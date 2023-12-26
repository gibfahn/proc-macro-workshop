use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma,
    AngleBracketedGenericArguments, Data, DeriveInput, Expr, ExprAssign, ExprLit, ExprPath, Field,
    Fields, GenericArgument, Ident, Lit, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let input_struct_name = input.ident;
    let builder_struct_name = format_ident!("{}Builder", input_struct_name);

    let input_fields = input_struct_fields(&input.data);

    let builder_fields = make_fields_optional(input_fields);

    let builder_methods = generate_builder_methods(input_fields);

    let build_method = generate_build_method(&input_struct_name, input_fields);

    let output = quote!(
        #[derive(std::default::Default)]
        pub struct #builder_struct_name {
            #builder_fields
        }

        impl #input_struct_name {
            pub fn builder() -> #builder_struct_name {
                #builder_struct_name::default()
            }
        }

        impl #builder_struct_name {
            #builder_methods

            #build_method
        }
    );

    proc_macro::TokenStream::from(output)
}

fn input_struct_fields(data: &Data) -> &Punctuated<Field, Comma> {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    // We take some care to use the span of each `syn::Field` as
                    // the span of the corresponding `heap_size_of_children`
                    // call. This way if one of the field types does not
                    // implement `HeapSize` then the compiler's error message
                    // underlines which field it is. An example is shown in the
                    // readme of the parent directory.
                    &fields.named
                }
                _ => unimplemented!(),
            }
        }
        _ => unimplemented!(),
    }
}

fn make_fields_optional(fields: &Punctuated<Field, Comma>) -> TokenStream {
    // We take some care to use the span of each `syn::Field` as
    // the span of the corresponding `heap_size_of_children`
    // call. This way if one of the field types does not
    // implement `HeapSize` then the compiler's error message
    // underlines which field it is. An example is shown in the
    // readme of the parent directory.
    let optional_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if strip_optional_wrapper_if_present(f).is_some() {
            quote_spanned!(f.span()=> #name: #ty)
        } else {
            quote_spanned!(f.span()=> #name: std::option::Option<#ty>)
        }
    });
    quote! {
        #(#optional_fields),*
    }
}

fn generate_builder_methods(fields: &Punctuated<Field, Comma>) -> TokenStream {
    let builder_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = strip_optional_wrapper_if_present(f).unwrap_or(&f.ty);
        let each_method = match create_each_method(f, name) {
            Ok(value) => value,
            Err(value) => value,
        };

        quote_spanned!(f.span()=>
            #each_method

            fn #name (&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        )
    });
    quote! {
        #(#builder_methods)*
    }
}

fn create_each_method(f: &Field, name: &Option<Ident>) -> Result<TokenStream, TokenStream> {
    Ok(match builder_each_method_name(f) {
        Ok(Some(each_name)) => {
            let each_type = match strip_vec_wrapper_if_present(f) {
                Some(ty) => ty,
                None => {
                    return Err(quote! {
                        compile_error!(
                            "We only support Vec types with the each attribute for field {each_name}"
                        )
                    });
                }
            };
            let each_ident = Ident::new(&each_name, f.span());
            if matches!(name, Some(name) if name == &each_ident) {
                eprintln!("WARN: Not generating 'each' attribute method for {each_name} as it would conflict, please choose a different method name than the field name.");
                return Err(quote!());
            }
            quote! {
                fn #each_ident(&mut self, #each_ident: #each_type) -> &mut Self {
                    match &mut self.#name {
                        Some(#name) => #name.push(#each_ident),
                        None => {
                            self.#name.replace(vec![#each_ident]);
                        },
                    }
                    self
                }
            }
        }
        Ok(None) => quote! {},
        Err(e) => {
            let e = e.into_compile_error();
            quote! {#e}
        }
    })
}

fn generate_build_method(
    input_struct_name: &Ident,
    fields: &Punctuated<Field, Comma>,
) -> TokenStream {
    let struct_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote!(#name)
    });

    let assignments = fields.iter().map(|f| {
        let name = &f.ident;
        let name_str = name.as_ref().map(|name| name.to_string());
        if strip_optional_wrapper_if_present(f).is_some() {
            quote! {
                let #name = self.#name.clone();
            }
        } else {
            quote! {
                let Some(#name) = self.#name.clone() else {
                    return Err(
                        format!("Required field {} not set", #name_str).into()
                    );
                };
            }
        }
    });

    quote! {
        pub fn build(&mut self) -> std::result::Result<#input_struct_name, std::boxed::Box<dyn std::error::Error>> {
            #(#assignments)*

            Ok(
                #input_struct_name {
                    #(#struct_fields),*
                }
            )
        }
    }
}

fn strip_optional_wrapper_if_present(f: &Field) -> Option<&Type> {
    let ty = &f.ty;
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(PathSegment { ident, arguments }) = path.segments.last() {
            if ident == &Ident::new("Option", f.span()) {
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = arguments
                {
                    if let Some(GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}

fn strip_vec_wrapper_if_present(f: &Field) -> Option<&Type> {
    let ty = &f.ty;

    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(PathSegment { ident, arguments }) = path.segments.last() {
            if ident == &Ident::new("Vec", f.span()) {
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = arguments
                {
                    if let Some(GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }

    None
}

fn builder_each_method_name(f: &Field) -> Result<Option<String>, syn::Error> {
    for attr in f.attrs.iter() {
        if attr.path().is_ident("builder") {
            let expr = attr.parse_args::<Expr>()?;
            if let Expr::Assign(ExprAssign { left, right, .. }) = expr {
                if let Expr::Path(ExprPath { path, .. }) = *left {
                    if path.is_ident("each") {
                        if let Expr::Lit(ExprLit {
                            lit: Lit::Str(lit_str),
                            ..
                        }) = *right
                        {
                            return Ok(Some(lit_str.value()));
                        }
                    } else {
                        return Err(syn::Error::new_spanned(
                            &attr.meta,
                            "expected `builder(each = \"...\")`",
                        ));
                    }
                }
            }
        }
    }

    Ok(None)
}
