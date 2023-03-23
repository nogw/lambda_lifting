use std::collections::HashSet;

use crate::pass;
use crate::term::{Decl, Expr, Module};

pub struct ClosureConv {
    globals: Vec<String>,
}

impl Default for ClosureConv {
    fn default() -> ClosureConv {
        ClosureConv::new()
    }
}

impl pass::Pass for ClosureConv {
    type Input = Module;
    type Output = Module;

    fn run(mut self, source: Self::Input) -> Self::Output {
        source.map(|declaration| self.closure_decl(declaration))
    }
}

impl ClosureConv {
    fn new() -> ClosureConv {
        ClosureConv {
            globals: Vec::new(),
        }
    }

    fn closure_decl(&mut self, Decl(name, params, body): &Decl) -> Decl {
        let body = self.closure(body);
        let name = name.to_owned();
        let params = params.to_owned();

        Decl(name, params, body)
    }

    fn free_variables(expr: &Expr, free: HashSet<String>) -> HashSet<String> {
        match expr {
            Expr::Int(_) => free,
            Expr::Var(sym) => {
                let mut new_free = free;
                new_free.insert(sym.clone());
                new_free
            }
            Expr::App(fun, arg) => {
                let free_lambda = Self::free_variables(fun, free);
                let free_arg = Self::free_variables(arg, free_lambda);
                free_arg
            }
            Expr::Abs(params, body) => {
                let free_body = Self::free_variables(body, free);
                let free = Self::difference(&free_body, params);
                free
            }
            Expr::Bop(_, left, right) => {
                let free_left = Self::free_variables(left, free);
                let free_right = Self::free_variables(right, free_left);
                free_right
            }
            Expr::Let(name, bind, body) => {
                let free_bind = Self::free_variables(bind, free);
                let free_body = Self::free_variables(body, free_bind);
                let mut new_free = free_body;
                new_free.remove(name);
                new_free
            }
        }
    }

    fn difference(free: &HashSet<String>, remove: &Vec<String>) -> HashSet<String> {
        let mut result = free.clone();
        result.retain(|element| !remove.contains(element));
        result
    }

    fn apply_to(lambda: Expr, args: Vec<String>) -> Expr {
        args.iter().cloned().fold(lambda, |acc, arg| {
            Expr::App(Box::new(acc), Box::new(Expr::Var(arg)))
        })
    }

    fn closure(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::Int(int) => Expr::Int(int.clone()),
            Expr::Var(sym) => Expr::Var(sym.clone()),
            Expr::Bop(bop, left, right) => {
                let left = self.closure(left);
                let right = self.closure(right);
                Expr::Bop(bop.clone(), Box::new(left), Box::new(right))
            }
            Expr::App(fun, arg) => {
                let fun = self.closure(fun);
                let arg = self.closure(arg);
                Expr::App(Box::new(fun), Box::new(arg))
            }
            Expr::Abs(params, body) => {
                let to_remove: Vec<_> = self
                    .globals
                    .iter()
                    .cloned()
                    .chain(params.iter().cloned())
                    .collect();
                let variables = Self::free_variables(&body, HashSet::new());
                let variables = Self::difference(&variables, &to_remove);
                let variables = Vec::from_iter(variables);
                let params = variables
                    .iter()
                    .cloned()
                    .chain(params.iter().cloned())
                    .collect();
                let lambda = Expr::Abs(params, body.clone());
                Self::apply_to(lambda, variables)
            }
            Expr::Let(name, bind, body) => {
                let bind = self.closure(bind);
                let body = self.closure(body);
                Expr::Let(name.clone(), Box::new(bind), Box::new(body))
            }
        }
    }
}
