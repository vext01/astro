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
#![feature(rustc_private)]

extern crate getopts;
extern crate rustc;
extern crate rustc_driver;
extern crate syntax;

use rustc::session::Session;
use rustc::session::config::{self, Input, ErrorOutputType};
use rustc_driver::{driver, CompilerCalls, Compilation, RustcDefaultCalls};

use syntax::ast::{Expr, ExprKind, Stmt, StmtKind, Block, Item, ItemKind};
use syntax::{ast, visit, errors};
use syntax::ext::quote::rt;

use std::path::{PathBuf, Path};
use std::fs::File;
use std::io::prelude::*;


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

        control.after_parse.stop = Compilation::Stop;
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
                self.indent();
                self.traverse_block(block_p.clone().unwrap());
                self.dedent();
            }
            _ => {},
        }
    }

    fn print_span(&self, span: &rt::Span) {
        // print the first line of a span to help me understand
        let slice = &self.source[(span.lo.0 as usize)..(span.hi.0 as usize)];
        let mut lines = slice.split("\n");
        iprintln!(self, "`{}`...", lines.next().unwrap());
    }

    fn traverse_stmt(&mut self, stmt: &Stmt) {
        iprint!(self, "+Statement: ");
        self.print_span(&stmt.span);
        self.indent();
        match stmt.node {
            StmtKind::Item(ref item_p) =>
                self.traverse_item(&item_p.clone().unwrap()),
            StmtKind::Expr(ref expr_p) =>
                self.traverse_expr(&expr_p.clone().unwrap()),
            _ => {},
        }
        self.dedent();

    }

    fn traverse_expr(&mut self, expr: &Expr) {
        iprint!(self, "+Expr: ");
        self.print_span(&expr.span);
        match &expr.node {
            &ExprKind::If(_, ref blk_p, ref else_o) => {
                self.indent();
                self.traverse_block(blk_p.clone().unwrap());
                match else_o {
                    &Some(ref expr_p) => {
                        self.traverse_expr(&expr_p.clone().unwrap());
                    }
                    _ => {},
                }
                self.dedent();
            },
            &ExprKind::Block(ref blk_p) => self.traverse_block(blk_p.clone().unwrap()),
            _ => {},
        }
    }

    fn traverse_block(&mut self, blk: Block) {
        iprintln!(self, "+Block");
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
    let args: Vec<_> = std::env::args().collect();
    rustc_driver::run_compiler(&args, &mut ASTDumper::new(), None, None);
}
