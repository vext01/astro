// Copyright 2015 Nicholas Cameron.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
//
// Copyright 2017 King's College London
// Derivative work authored by Edd Barrett <vext01@gmail.com>
// Based on https://github.com/nrc/stupid-stats

#![feature(box_syntax)]
#![feature(plugin_registrar, rustc_private, plugin)]
#![feature(quote)]

extern crate getopts;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;
extern crate rustc_plugin;

use syntax::ast::{Item, ItemKind, MetaItem};
use syntax::ext::base::{ExtCtxt, SyntaxExtension, Annotatable};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::Span;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use rustc_plugin::Registry;


fn expand_inject_block_ids(cx: &mut ExtCtxt, _: Span,
                           _: &MetaItem, ann_item: Annotatable) -> Vec<Annotatable> {

    if let &Annotatable::Item(ref item_p) = &ann_item {
        let item = item_p.clone().unwrap();
        let node = item.node;
        if let ItemKind::Fn(decl_p, unsafety, spanned_const, abi, generics, blk_p) = node {
            let blk = blk_p.unwrap();
            let expr = quote_expr!(cx, {println!("In like flyn!"); $blk});
            let new_blk = cx.block_expr(expr);
            let decl = decl_p.unwrap();
            let new_fn_kind = ItemKind::Fn(P(decl), unsafety,
                                           spanned_const, abi, generics,
                                           new_blk);
            let new_fn = Item {
                ident: item.ident,
                attrs: item.attrs,
                id: item.id,
                node: new_fn_kind,
                vis: item.vis,
                span: item.span
            };
            return vec![Annotatable::Item(P(new_fn))]
        }
    }

    // default case, do nothing
    vec![ann_item]
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
        Symbol::intern("inject_block_id"),
        SyntaxExtension::MultiModifier(box(expand_inject_block_ids)));
}
