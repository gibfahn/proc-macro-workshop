use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma, Data, DeriveInput,
    Field, Fields, Ident,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let input_struct_name = input.ident;
    let builder_struct_name = format_ident!("{}Builder", input_struct_name);

    let input_fields = input_struct_fields(&input.data);

    let builder_fields = make_fields_optional(input_fields);

    let builder_methods = generate_builder_methods(input_fields);

    let build_method = generate_build_method(&input_struct_name, input_fields);

    let output = quote!(
        #[derive(Default)]
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
        quote_spanned!(f.span()=> #name: Option<#ty>)
    });
    quote! {
        #(#optional_fields),*
    }
}

fn generate_builder_methods(fields: &Punctuated<Field, Comma>) -> TokenStream {
    let builder_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote_spanned!(f.span()=>
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
        quote! {
            let Some(#name) = self.#name.clone() else {
                return Err(
                    String::from("Required field #name not set").into()
                );
            };
        }
    });

    quote! {
        pub fn build(&mut self) -> Result<#input_struct_name, Box<dyn std::error::Error>> {
            #(#assignments)*

            Ok(
                #input_struct_name {
                    #(#struct_fields),*
                }
            )
        }
    }
}
