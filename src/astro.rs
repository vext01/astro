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

use syntax::ast::{Item, ItemKind, MetaItem, Mod, Block, Stmt, StmtKind};
use syntax::ext::base::{ExtCtxt, SyntaxExtension, Annotatable};
use syntax::ext::quote::rt::Span;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use rustc_plugin::Registry;

// Indented println! and print!
macro_rules! iprintln {
    ($slf:expr, $($ps:expr),*) => {{
        print!("{}", (0..$slf.indent_level).map(|_| "  ").collect::<String>());
        println!( $($ps),* );
    }};
}

macro_rules! iprint {
    ($slf:expr, $($ps:expr),*) => {{
        print!("{}", (0..$slf.indent_level).map(|_| "  ").collect::<String>());
        print!( $($ps),* );
    }};
}


struct ModifyCtxt<'a, 'ctx: 'a> {
    ext_ctxt: &'a ExtCtxt<'ctx>,
    next_blk_id: usize,
    indent_level: usize,
}

impl<'a, 'ctx> ModifyCtxt<'a, 'ctx> {
    fn new(cx: &'a ExtCtxt<'ctx>) -> Self {
        ModifyCtxt {
            ext_ctxt: cx,
            next_blk_id: 0,
            indent_level: 0,
        }
    }

    /* Format a span as a string (two lines at most) */
    fn span_str(&self, span: &Span) -> String {
        let codemap = self.ext_ctxt.codemap();
        let mut ret = String::new();
        ret.push_str(&codemap.span_to_string(*span));
        ret.push_str(": `");
        match codemap.span_to_snippet(*span) {
            Ok(snippet) => {
                let mut lines = snippet.split("\n");
                let mut left = 2;
                while left > 0 {
                    match lines.next() {
                        Some(x) => ret.push_str(x.trim()),
                        None => break
                    }
                    left -= 1;
                }
            },
            _ => ret.push_str("???"),
        }
        ret.push_str("`");
        ret
    }

    /*
     * Modification starts here
     */
    fn modify(&mut self, modu: Mod) -> Mod {
        let mut new_item_ps = Vec::new();
        for item_p in modu.items {
            new_item_ps.push(self.modify_item_p(item_p));
        }
        Mod{items: new_item_ps, .. modu}
    }

    fn modify_item(&mut self, item: Item) -> Item {
        iprintln!(self, "+Item: {}", self.span_str(&item.span));
        match item.node {
            ItemKind::Fn(decl_p, unsafety, spanned_const, abi, generics, block_p) => {
                iprintln!(self, "\n+Function {}", &item.ident);
                let new_blk = self.modify_block(block_p.clone().unwrap());
                let new_itemkind = ItemKind::Fn(decl_p, unsafety, spanned_const, abi, generics, P(new_blk));
                return Item{node: new_itemkind, ..item}
            },
            _ => {
                iprintln!(self, "unhandled item kind");
                return item;
            }
        }
    }

    fn modify_item_p(&mut self, item_p: P<Item>) -> P<Item> {
        let item = item_p.clone().unwrap();
        P(self.modify_item(item))
    }

    fn modify_block(&mut self, blk: Block) -> Block {
        iprintln!(self, "+Block {}", self.span_str(&blk.span));
        self.indent_level += 1;

        let id = self.next_blk_id;
        let rv = quote_block!(self.ext_ctxt, {
            println!("runtime: enter block: {}", $id); $blk
        }).unwrap();
        self.next_blk_id += 1;

        for stmt in blk.stmts {
            self.modify_stmt(stmt);
        }
        self.indent_level -= 1;
        rv
    }

    fn modify_stmt(&mut self, stmt: Stmt) -> Stmt {
        iprintln!(self, "+Stmt: {}", self.span_str(&stmt.span));
        match stmt.node {
            StmtKind::Item(item_p) => {
                let new_item = self.modify_item_p(item_p);
                let new_stmtkind = StmtKind::Item(new_item);
                return Stmt{node: new_stmtkind, ..stmt}
            },
            _ => {
                // XXX many other types
                iprintln!(self, "unhandled statement kind");
                return stmt
            },
        }
    }
}

fn expand_inject_block_ids(cx: &mut ExtCtxt, _: Span,
                           _: &MetaItem, ann_item: Annotatable) -> Vec<Annotatable> {
    if let &Annotatable::Item(ref item_p) = &ann_item {
        let item = item_p.clone().unwrap();
        if let ItemKind::Mod(modu) = item.node {
            let mut mc = ModifyCtxt::new(cx); //, &mod_name);
            let new_mod = mc.modify(modu);
            let new_item = Item{node: ItemKind::Mod(new_mod), .. item};
            return vec![Annotatable::Item(P(new_item))];
        } else {
            panic!("Plugin applies only at the module level");
        }
    }

    // default case, do nothing
    vec![ann_item]
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
        Symbol::intern("inject_block_id"),
        SyntaxExtension::MultiModifier(box expand_inject_block_ids));
}
