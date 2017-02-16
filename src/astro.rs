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

use rustc::session::Session;
use rustc::session::config::{self, Input, ErrorOutputType};
use rustc_driver::{driver, CompilerCalls, Compilation, RustcDefaultCalls};

use syntax::ast::{Expr, ExprKind, Stmt, StmtKind, Block, Item, ItemKind, Ident, FunctionRetTy};
use syntax::{ast, visit, errors};
use syntax::ext::quote::rt;

use std::path::{PathBuf, Path};
use std::fs::File;
use std::io::prelude::*;
use syntax::symbol::Symbol;
use syntax::ext::build::AstBuilder;
use syntax::ptr::P;

//use std::io::Write;

use syntax::ext::base::{ExtCtxt, SyntaxExtension, Annotatable};
use syntax::ext::quote::rt::Span;
use syntax::ast::MetaItem;
use rustc_plugin::Registry;
use syntax::codemap;


struct ASTDumper {
    default_calls: RustcDefaultCalls,
}

impl ASTDumper {
    fn new() -> ASTDumper {
        ASTDumper { default_calls: RustcDefaultCalls }
    }
}

impl<'a> CompilerCalls<'a> for ASTDumper {
    fn early_callback(&mut self,
                      _: &getopts::Matches,
                      _: &config::Options,
                      _: &ast::CrateConfig,
                      _: &errors::registry::Registry,
                      _: ErrorOutputType)
                      -> Compilation {
        Compilation::Continue
    }

    fn late_callback(&mut self,
                     m: &getopts::Matches,
                     s: &Session,
                     i: &Input,
                     odir: &Option<PathBuf>,
                     ofile: &Option<PathBuf>)
                     -> Compilation {
        self.default_calls.late_callback(m, s, i, odir, ofile);
        Compilation::Continue
    }

    fn some_input(&mut self, input: Input, input_path: Option<PathBuf>) -> (Input, Option<PathBuf>) {
        (input, input_path)
    }

    fn no_input(&mut self,
                m: &getopts::Matches,
                o: &config::Options,
                cc: &ast::CrateConfig,
                odir: &Option<PathBuf>,
                ofile: &Option<PathBuf>,
                r: &errors::registry::Registry)
                -> Option<(Input, Option<PathBuf>)> {
        self.default_calls.no_input(m, o, cc, odir, ofile, r);
        panic!("No input!");
    }

    fn build_controller(&mut self, _: &Session,  _: &getopts::Matches) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();

        control.after_parse.stop = Compilation::Continue;
        control.after_parse.callback = box |state: &mut driver::CompileState| {
            let krate = state.krate.as_ref();
            let filepath = match state.input {
                &Input::File(ref pb) => pb.as_path(),
                _ => panic!("not file"),
            };
            let mut visitor = ASTVisitor::new(filepath);
            visit::walk_crate(&mut visitor, &krate.unwrap());
        };

        control
    }
}

struct ASTVisitor {
    level: usize,
    source: String,
    next_id: usize,
}

// An indented println!
macro_rules! iprintln {
    ($slf:expr, $($ps:expr),*) => {{
        print!("{}", (0..$slf.level).map(|_| "  ").collect::<String>());
        println!( $($ps),* );
    }};
}

// An indented print!
macro_rules! iprint {
    ($slf:expr, $($ps:expr),*) => {{
        print!("{}", (0..$slf.level).map(|_| "  ").collect::<String>());
        print!( $($ps),* );
    }};
}

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

impl ASTVisitor {
    fn new(filepath: &Path) -> ASTVisitor {
        let mut fh = match File::open(filepath) {
            Ok(fh) => fh,
            _ => panic!("failed to open source file"),
        };
        let mut source = String::new();
        match fh.read_to_string(&mut source) {
            Ok(_) => {},
            _ => panic!("failed to read source file"),
        }
        ASTVisitor {
            level: 0,
            source: source,
            next_id: 0,
        }
    }

    fn indent(&mut self) {
        self.level += 1;
    }

    fn dedent(&mut self) {
        self.level -= 1;
    }

    fn traverse_item(&mut self, item: &Item) {
        match item.node {
            ItemKind::Fn(_, _, _, _, _, ref block_p) => {
                iprintln!(self, "+Function {}", item.ident);
                self.traverse_block(block_p.clone().unwrap());
            }
            _ => {},
        }
    }

    fn print_span(&self, span: &rt::Span) {
        // print the first line of a span to help me understand
        let slice = &self.source[(span.lo.0 as usize)..(span.hi.0 as usize)];
        let mut lines = slice.split("\n");
        println!("`{}`...", lines.next().unwrap());
    }

    fn traverse_stmt(&mut self, stmt: &Stmt) {
        iprint!(self, "+Stmt: ");
        match stmt.node {
            StmtKind::Item(ref item_p) => {
                print!("Item: ");
                self.print_span(&stmt.span);
                self.traverse_item(&item_p.clone().unwrap());
            }
            StmtKind::Expr(ref expr_p) => {
                print!("Expr: ");
                self.print_span(&stmt.span);
                self.traverse_expr(&expr_p.clone().unwrap());
            }
            _ => {
                print!("Unknown: ");
                self.print_span(&stmt.span);
            },
        }
    }

    fn traverse_expr(&mut self, expr: &Expr) {
        iprint!(self, "+Expr: ");
        match &expr.node {
            &ExprKind::If(_, ref blk_p, ref else_o) => {
                print!("If: ");
                self.print_span(&expr.span);
                self.traverse_block(blk_p.clone().unwrap());
                match else_o {
                    &Some(ref expr_p) => {
                        self.traverse_expr(&expr_p.clone().unwrap());
                    }
                    _ => {},
                }
            },
            &ExprKind::Block(ref blk_p) => {
                print!("Block: ");
                self.print_span(&expr.span);
                self.traverse_block(blk_p.clone().unwrap());
            }
            &ExprKind::Loop(ref blk_p, _) => {
                print!("Loop: ");
                self.print_span(&expr.span);
                self.traverse_block(blk_p.clone().unwrap());
            }
            &ExprKind::ForLoop(_, _, ref blk_p, _) => {
                print!("ForLoop: ");
                self.print_span(&expr.span);
                self.traverse_block(blk_p.clone().unwrap());
            }
            &ExprKind::While(_, ref blk_p, _) => {
                print!("While: ");
                self.print_span(&expr.span);
                self.traverse_block(blk_p.clone().unwrap());
            }
            _ => {
                print!("Unknown: ");
                self.print_span(&expr.span);
            },
        }
    }

    fn traverse_block(&mut self, blk: Block) {
        iprintln!(self, "+Block {}:", self.next_id);
        self.next_id += 1;
        self.indent();
        for stmt in blk.stmts {
            self.traverse_stmt(&stmt);
        }
        self.dedent();
    }
}

impl<'a> visit::Visitor<'a> for ASTVisitor {
    /*
     * Hook into the AST walker, looking for functions to analyse
     */

    fn visit_item(&mut self, item: &Item) {
        self.traverse_item(item);
        visit::walk_item(self, item)
    }

    fn visit_mac(&mut self, mac: &ast::Mac) {
        visit::walk_mac(self, mac)
    }
}

fn main() {
    let f = File::create("compiled").unwrap();
    let args: Vec<_> = std::env::args().collect();
    let (res, _) = rustc_driver::run_compiler(&args, &mut ASTDumper::new(),
                                              None, Some(Box::new(f)));
    res.expect("compile errors");
}
