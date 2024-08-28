use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, ItemFn};
use syn::{Data, DeriveInput, Fields, Stmt};
extern crate proc_macro;

#[proc_macro_attribute]
pub fn system_fn2(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as AttributeArgs);

    let type_param = match args.first() {
        Some(syn::NestedMeta::Meta(syn::Meta::Path(path))) => path,
        _ => panic!("Expected a single type parameter"),
    }; // Parse the attribute arguments

    // Parse the function definition
    let input_fn = parse_macro_input!(item as ItemFn);

    // Extract function name and parameters
    let fn_name = &input_fn.sig.ident;
    let fn_name_string = fn_name.to_string();
    let fn_params = &input_fn.sig.inputs;

    // Generate macro invocation code
    let mut macro_invocation = quote! {};
    for _ in fn_params {
        macro_invocation = quote! {
            #macro_invocation _,
        };
    }

    // Generate macro_rules! macro
    let expanded = quote! {
        #[macro_export]
        macro_rules! #fn_name{
            ($runtime: expr) => {
                let funct_name = #fn_name_string;
                $runtime.get_resource::<#type_param>().register(Box::new( ( funct_name, Box::new(#fn_name) as Box<dyn Fn(#macro_invocation)>)))

            };
        }

        #input_fn
    };

    // Return the generated code as a TokenStream
    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn system_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the function definition
    let mut input_fn = parse_macro_input!(item as ItemFn);

    // Extract function name and parameters
    let fn_name_string = input_fn.sig.ident.to_string();

    // Generate the new statement to be added at the start of the function body
    let span_stmt: Stmt = syn::parse_quote! {
        crate::span!(span, #fn_name_string);
    };

    // Modify the function body to include the new statement at the start
    let block = &mut input_fn.block;
    block.stmts.insert(0, span_stmt);

    // Generate macro invocation code for parameters
    let fn_params = &input_fn.sig.inputs;
    let mut macro_invocation = quote! {};
    for _ in fn_params {
        macro_invocation = quote! {
            #macro_invocation _,
        };
    }

    let expanded = quote! {
        #input_fn
    };

    // Return the generated code as a TokenStream
    TokenStream::from(expanded)
}

#[proc_macro_derive(Comp)]
pub fn comp_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    impl_comp(&ast)
}

fn impl_comp(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl legion::internals::world::Comp for #name {}

    };
    gen.into()
}

#[proc_macro_derive(EngineResource)]
pub fn res_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    impl_res(&ast)
}

#[proc_macro]
pub fn impl_uuid(input: TokenStream) -> TokenStream {
    // Parse the input as a Path (which can represent full paths like `crate::ecs::systems::SomeStruct`)
    let input = parse_macro_input!(input as syn::Path);

    // Convert the full path to a string
    let path_string = quote!(#input).to_string();

    // Use a fixed namespace UUID for consistency
    let namespace_uuid = uuid::Uuid::NAMESPACE_DNS; // You can use any of the available namespaces
    let uuid = uuid::Uuid::new_v5(&namespace_uuid, path_string.as_bytes()).to_string();

    // Extract the last segment as the type name for the impl block
    let type_name = input.segments.last().unwrap().ident.clone();

    // Generate the implementation of the AbiTypeId trait for the given type
    let gen = quote! {
        impl crate::AbiTypeId for #type_name {
            fn uuid() -> &'static str {
                #uuid
            }
        }
    };

    // Convert the generated code into a TokenStream and return it
    gen.into()
}

#[proc_macro_derive(Uuid)]
pub fn uuid_derive(input: TokenStream) -> TokenStream {
    // Parse the input as a Path (which can represent full paths like `crate::ecs::systems::SomeStruct`)
    let input = parse_macro_input!(input as syn::Path);

    // Convert the full path to a string
    let path_string = quote!(#input).to_string();

    // Use a fixed namespace UUID for consistency
    let namespace_uuid = uuid::Uuid::NAMESPACE_DNS; // You can use any of the available namespaces
    let uuid = uuid::Uuid::new_v5(&namespace_uuid, path_string.as_bytes()).to_string();

    // Extract the last segment as the type name for the impl block
    let type_name = input.segments.last().unwrap().ident.clone();

    // Generate the implementation of the AbiTypeId trait for the given type
    let gen = quote! {
        impl crate::AbiTypeId for #type_name {
            fn uuid() -> &'static str {
                #uuid
            }
        }
    };

    // Convert the generated code into a TokenStream and return it
    gen.into()
}
fn impl_res(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        use crate::FromEngine as _;

        use crate::AbiTypeId as _;

        krajc_macros::impl_uuid!(#name);

        impl crate::engine_runtime::schedule_manager::system_params::system_resource::EngineResource for #name {
            fn get_mut(engine: &mut crate::EngineRuntime) -> &'static mut Self {
                crate::TypedAddr::new({
                    let op = engine.static_resource_map.get_mut(&Self::uuid());
                    match op {
                        Some(val) => *val,
                        None => {
                            let new = Box::leak(Box::new(#name::from_engine(crate::dupe(engine))));
                            let addr = crate::TypedAddr::new_with_ref(new).addr;
                            engine.static_resource_map.insert(Self::uuid(), addr);
                            addr
                        }
                    }
                })
                .get()
            }
            fn get(engine: &mut crate::EngineRuntime) -> &'static Self {
                crate::TypedAddr::new({
                    let op = engine.static_resource_map.get_mut(&Self::uuid());
                    match op {
                        Some(val) => *val,
                        None => {
                            let new = Box::leak(Box::new(#name::from_engine(crate::dupe(engine))));
                            let addr = crate::TypedAddr::new_with_ref(new).addr;
                            engine.static_resource_map.insert(Self::uuid(), addr);
                            addr
                        }
                    }
                })
                .get()
            }
            fn get_no_init(engine: &crate::EngineRuntime) -> &'static Self {
                crate::TypedAddr::new({
                    let op = engine.static_resource_map.get(&Self::uuid());
                    match op {
                        Some(val) => *val,
                        None => {
                            panic!("read an uninited resoruce without in get_no_init()");
                        }
                    }
                })
                .get()
            }
        }

    };
    gen.into()
}

use syn::Ident;

#[proc_macro]
pub fn not_prod(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Ident);
    let expanded = quote! {
        #[cfg(not(feature = "prod"))]
        {
            #input
        }
    };
    TokenStream::from(expanded)
}

#[proc_macro_derive(FromEngine)]
pub fn from_engine_macro(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;

    let expanded = match input.data {
        Data::Struct(data_struct) => {
            let field_inits = match data_struct.fields {
                Fields::Named(fields_named) => fields_named
                    .named
                    .iter()
                    .map(|field| {
                        let field_name = &field.ident;
                        let field_type = &field.ty;

                        quote! {
                            #field_name: <#field_type as crate::FromEngine>::from_engine(dupe(runtime))
                        }
                    })
                    .collect::<Vec<_>>(),
                _ => panic!("FromEngine can only be derived for structs with named fields"),
            };

            quote! {
                impl #name {
                    pub fn from_engine(runtime: &'static mut EngineRuntime) -> Self {
                        Self {
                            #(#field_inits),*
                        }
                    }
                }
            }
        }
        _ => panic!("FromEngine can only be derived for structs"),
    };

    TokenStream::from(expanded)
}
