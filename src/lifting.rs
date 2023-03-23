use crate::pass;
use crate::scope;
use crate::state;
use crate::term::{Decl, Expr, Module};

pub struct LambdaLift {
    scope: scope::Scope,
    state: state::State,
    declarations: Vec<Decl>,
}

impl Default for LambdaLift {
    fn default() -> LambdaLift {
        LambdaLift::new()
    }
}

impl pass::Pass for LambdaLift {
    type Input = Module;
    type Output = Module;

    fn run(mut self, source: Self::Input) -> Self::Output {
        let module = source.map(|declaration| self.lifting_decl(declaration));
        let entries = self
            .declarations
            .iter()
            .cloned()
            .chain(module.entries.iter().cloned())
            .collect();

        Module { entries }
    }
}

impl LambdaLift {
    fn new() -> LambdaLift {
        LambdaLift {
            scope: scope::Scope::new(),
            state: state::State::new(),
            declarations: Vec::new(),
        }
    }

    fn lifting_decl(&mut self, Decl(name, params, body): &Decl) -> Decl {
        self.scope.new_scope(name);

        let body = self.lifting(body);
        let name = name.to_owned();
        let params = params.to_owned();

        Decl(name, params, body)
    }

    fn lifting(&mut self, expr: &Expr) -> Expr {
        match expr.clone() {
            Expr::Int(value) => Expr::Int(value),
            Expr::Var(value) => self.scope.lookup(value),
            Expr::Bop(op, left, right) => {
                let left = self.lifting(&left);
                let right = self.lifting(&right);
                Expr::Bop(op, Box::new(left), Box::new(right))
            }
            Expr::App(lambda, argument) => {
                let lambda = self.lifting(&lambda);
                let argument = self.lifting(&argument);
                Expr::App(Box::new(lambda), Box::new(argument))
            }
            Expr::Abs(params, body) => {
                let prefix = &self.scope.prefix;
                let fresh = self.state.fresh(prefix.to_string());
                let decl = Decl(fresh, params, *body);
                self.declarations.push(decl.clone());
                Expr::Var(decl.0)
            }
            Expr::Let(name, bind, body) => match *bind {
                Expr::Abs(..) => {
                    let bind = self.lifting(&bind);
                    self.scope.context.insert(name, bind);
                    self.lifting(&body)
                }
                _ => {
                    let bind = self.lifting(&bind);
                    let body = self.lifting(&body);
                    Expr::Let(name, Box::new(bind), Box::new(body))
                }
            },
        }
    }
}
