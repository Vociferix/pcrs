use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Expr, Ident, Index, Token};

struct Args(Punctuated<Expr, Token![,]>);

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut punc = Punctuated::new();
        while !input.is_empty() {
            punc.push_value(input.parse()?);
            let comma: Result<Token![,]> = input.parse();
            match comma {
                Ok(comma) => {
                    punc.push_punct(comma);
                }
                Err(e) => {
                    if input.is_empty() {
                        break;
                    } else {
                        return Err(e);
                    }
                }
            }
        }
        Ok(Self(punc))
    }
}

#[proc_macro]
pub fn seq(args: TokenStream) -> TokenStream {
    let Args(args) = parse_macro_input!(args as Args);
    let Some(arg0) = args.first() else {
        return quote! {
            { ::pcrs::basic::constant(()) }
        }
        .into();
    };

    let mut expr = quote! {
        { #arg0 }
    };

    for arg in args.iter().skip(1) {
        expr = quote! {
            { ::pcrs::basic::pair(#expr, #arg) }
        };
    }

    let vars: Vec<Ident> = (0..args.len())
        .into_iter()
        .map(|idx| Ident::new(&format!("__pcrs_seq_var_{idx}"), Span::call_site()))
        .collect();
    let var0 = vars.first().unwrap();
    let mut capture = quote! { #var0 };
    for var in vars.iter().skip(1) {
        capture = quote! { (#capture, #var) };
    }

    quote! {
        { ::pcrs::basic::map(#expr, |#capture| (#(#vars,)*)) }
    }
    .into()
}

#[proc_macro]
pub fn alt(args: TokenStream) -> TokenStream {
    let Args(args) = parse_macro_input!(args as Args);
    let Some(arg0) = args.first() else {
        return quote! {
            ::core::compile_error!("`alt!` requires one or more arguments")
        }
        .into();
    };

    let mut expr = quote! {
        { #arg0 }
    };

    for arg in args.iter().skip(1) {
        expr = quote! {
            { ::pcrs::basic::either(#expr, #arg) }
        };
    }

    expr.into()
}

#[proc_macro]
pub fn permutation(args: TokenStream) -> TokenStream {
    let Args(args) = parse_macro_input!(args as Args);
    if args.is_empty() {
        return quote! {
            { ::pcrs::basic::constant(()) }
        }
        .into();
    }

    let ty_params_vec: Vec<Ident> = (0..args.len())
        .into_iter()
        .map(|idx| Ident::new(&format!("__PcrsPermutationP{idx}"), Span::call_site()))
        .collect();
    let ty_params = &ty_params_vec[..];

    let vars_vec: Vec<Ident> = (0..args.len())
        .into_iter()
        .map(|idx| Ident::new(&format!("__pcrs_permutation_var_{idx}"), Span::call_site()))
        .collect();
    let vars = &vars_vec[..];

    let state_vec: Vec<Ident> = (0..args.len())
        .into_iter()
        .map(|idx| {
            Ident::new(
                &format!("__pcrs_permutation_state_{idx}"),
                Span::call_site(),
            )
        })
        .collect();
    let state = &state_vec[..];

    let vars_init_vec: Vec<_> = vars
        .iter()
        .zip(ty_params.iter())
        .zip(state.iter())
        .map(|((var, ty), st)| {
            quote! {
                let mut #st: bool = false;
                let mut #var: ::core::mem::MaybeUninit<<#ty as ::pcrs::Parse<__PcrsPermutationInput>>::Parsed> = const { ::core::mem::MaybeUninit::uninit() };
            }
        })
        .collect();
    let vars_init = &vars_init_vec[..];

    let parse_impls_vec: Vec<_> = vars.iter().zip(state.iter()).enumerate().map(|(idx, (var, st))| {
        let idx = Index { index: idx as u32, span: Span::call_site(), };
        quote! {
            if !#st {
                match self.#idx.parse(__pcrs_permutation_rem) {
                    ::core::result::Result::Ok(::pcrs::Success(__pcrs_permutation_val, __pcrs_permutation_new_rem)) => {
                        __pcrs_permutation_rem = __pcrs_permutation_new_rem;
                        #var.write(__pcrs_permutation_val);
                        #st = true;
                        __pcrs_permutation_parsed += 1;
                        continue;
                    },
                    ::core::result::Result::Err(::pcrs::Failure(__pcrs_permutation_new_err, __pcrs_permutation_new_rem)) => {
                        __pcrs_permutation_err = Some(__pcrs_permutation_new_err);
                        __pcrs_permutation_rem = __pcrs_permutation_new_rem;
                    },
                }
            }
        }
    }).collect();
    let parse_impls = &parse_impls_vec[..];

    let drops_vec: Vec<_> = vars
        .iter()
        .zip(state.iter())
        .map(|(var, st)| {
            quote! {
                if #st {
                    unsafe { #var.assume_init_drop(); }
                }
            }
        })
        .collect();
    let drops = &drops_vec[..];

    let count = args.len();

    let arg_exprs = args.iter();

    quote! {
        {
            struct __PcrsPermutationParser<#(#ty_params,)* __PcrsPermutationInput>(#(#ty_params,)* ::core::marker::PhantomData<__PcrsPermutationInput>)
            where
                #(#ty_params: ::pcrs::Parse<__PcrsPermutationInput>,)*
                __PcrsPermutationInput: ::pcrs::Input;

            impl<#(#ty_params,)* __PcrsPermutationInput> ::pcrs::Parse<__PcrsPermutationInput> for __PcrsPermutationParser<#(#ty_params,)* __PcrsPermutationInput>
            where
                #(#ty_params: ::pcrs::Parse<__PcrsPermutationInput>,)*
                __PcrsPermutationInput: ::pcrs::Input,
            {
                type Parsed = (#(<#ty_params as ::pcrs::Parse<__PcrsPermutationInput>>::Parsed,)*);

                fn parse(&self, __pcrs_permutation_input: __PcrsPermutationInput) -> ::pcrs::PResult<Self::Parsed, __PcrsPermutationInput> {
                    let mut __pcrs_permutation_parsed = 0usize;
                    let mut __pcrs_permutation_rem = __pcrs_permutation_input.clone();
                    let mut __pcrs_permutation_err = None;
                    #(#vars_init)*

                    while __pcrs_permutation_parsed != #count {
                        #(#parse_impls)*
                        #(#drops)*
                        return ::core::result::Result::Err(::pcrs::Failure(match __pcrs_permutation_err {
                            Some(__pcrs_permutation_err) => __pcrs_permutation_err,
                            None => ::core::unreachable!(),
                        }, __pcrs_permutation_input));
                    }

                    ::core::result::Result::Ok(::pcrs::Success(unsafe {
                        (#(#vars.assume_init(),)*)
                    }, __pcrs_permutation_rem))
                }
            }

            __PcrsPermutationParser(#(#arg_exprs,)* ::core::marker::PhantomData)
        }
    }.into()
}
