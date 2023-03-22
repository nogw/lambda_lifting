use core::fmt;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub enum Binary {
    Add,
    Sub,
}

#[derive(Debug, Clone)]
pub enum Expr {
    // integer
    Int(usize),
    // variable, x
    Var(String),
    // lambda, λx. x
    Abs(Vec<String>, Box<Expr>),
    // application, x y
    App(Box<Expr>, Box<Expr>),
    // let binding, let x = y in x
    Let(String, Box<Expr>, Box<Expr>),
    // binary expression, x + 1
    Bop(Binary, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Var(sym) => write!(f, "{}", sym),
            Expr::Int(int) => write!(f, "{}", int),
            Expr::Bop(Binary::Add, left, right) => write!(f, "({} + {})", left, right),
            Expr::Bop(Binary::Sub, left, right) => write!(f, "({} - {})", left, right),
            Expr::App(lam, arg) => write!(f, "({} {})", lam, arg),
            Expr::Abs(params, body) => {
                let params: Vec<String> = params.iter().map(|t| format!("{}", t)).collect();
                let params = params.join(", ");
                write!(f, "lambda [{}] => {}", params, body)
            }
            Expr::Let(name, bind, body) => write!(f, "let {} = {} in\n{}", name, bind, body),
        }
    }
}
#[derive(Debug)]
pub struct Decl(pub String, pub Vec<String>, pub Expr);

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {} [", self.0)?;
        let args: Vec<String> = self.1.iter().map(|t| format!("{}", t)).collect();
        let args = args.join(", ");
        let body = self.2.to_string().replace("\n", "\n\t");
        write!(f, "{}] =\n\t{}", args, body)
    }
}

#[derive(Debug, Clone)]
pub struct State {
    count: usize,
}

impl State {
    pub fn new() -> State {
        State { count: 0 }
    }

    pub fn fresh(&mut self) -> String {
        let fresh = self.count;
        self.count = self.count.checked_add(1).unwrap_or(0);

        format!("anno_{}", fresh)
    }
}

#[derive(Debug)]
pub struct Context {
    state: State,
    scope: HashMap<String, Expr>,
    decls: Vec<Decl>,
}

impl Context {
    fn new() -> Context {
        Context {
            state: State::new(),
            scope: HashMap::new(),
            decls: vec![],
        }
    }
}

macro_rules! lam {
    (($($params:ident),* $(,)?) => $body:expr) => {
        Expr::Abs( vec![$(stringify!($params).to_owned()),*], Box::new($body))
    };
}

macro_rules! app {
    ($lambda:expr, $arg:expr) => {
        Expr::App(Box::new($lambda), Box::new($arg))
    };
}

macro_rules! bop {
    (($left:expr) + ($right:expr)) => {
        Expr::Bop(Binary::Add, Box::new($left), Box::new($right))
    };
    (($left:expr) - ($right:expr)) => {
        Expr::Bop(Binary::Sub, Box::new($left), Box::new($right))
    };
}

macro_rules! def {
    ($name:ident = ($bind:expr) in $body:expr) => {
        Expr::Let(
            stringify!($name).to_owned(),
            Box::new($bind),
            Box::new($body),
        )
    };
}

macro_rules! λ {
    ($num:literal) => {
        Expr::Int($num)
    };
    ($name:ident) => {
        Expr::Var(String::from(stringify!($name)))
    };
}

macro_rules! decl {
    (let $name:ident [$($params:ident),* $(,)?] = $body:expr) => {
        Decl(stringify!($name).to_owned(), vec![$(stringify!($params).to_owned()),*], $body)
    };
}

fn difference(free: &HashSet<String>, remove: &Vec<String>) -> HashSet<String> {
    let mut result = free.clone();
    result.retain(|element| !remove.contains(element));
    result
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
            let free_lambda = free_variables(fun, free);
            let free_arg = free_variables(arg, free_lambda);
            free_arg
        }
        Expr::Abs(params, body) => {
            let free_body = free_variables(body, free);
            let free = difference(&free_body, params);
            free
        }
        Expr::Bop(_, left, right) => {
            let free_left = free_variables(left, free);
            let free_right = free_variables(right, free_left);
            free_right
        }
        Expr::Let(name, bind, body) => {
            let free_bind = free_variables(bind, free);
            let free_body = free_variables(body, free_bind);
            let mut new_free = free_body;
            new_free.remove(name);
            new_free
        }
    }
}

fn apply_to(lambda: Expr, args: Vec<String>) -> Expr {
    args.iter().cloned().fold(lambda, |acc, arg| {
        Expr::App(Box::new(acc), Box::new(Expr::Var(arg)))
    })
}

fn closure(globals: &[String], expr: Expr) -> Expr {
    match expr {
        Expr::Int(int) => Expr::Int(int),
        Expr::Var(sym) => Expr::Var(sym),
        Expr::Abs(params, body) => {
            let to_remove: Vec<_> = globals
                .iter()
                .cloned()
                .chain(params.iter().cloned())
                .collect();
            let variables = free_variables(&body, HashSet::new());
            let variables = difference(&variables, &to_remove);
            let variables = Vec::from_iter(variables);
            let params = variables
                .iter()
                .cloned()
                .chain(params.into_iter())
                .collect();
            let lambda = Expr::Abs(params, body);
            apply_to(lambda, variables)
        }
        Expr::Bop(bop, left, right) => {
            let left = closure(globals, *left);
            let right = closure(globals, *right);
            Expr::Bop(bop, Box::new(left), Box::new(right))
        }
        Expr::App(fun, arg) => {
            let fun = closure(globals, *fun);
            let arg = closure(globals, *arg);
            Expr::App(Box::new(fun), Box::new(arg))
        }
        Expr::Let(name, bind, body) => {
            let bind = closure(globals, *bind);
            let body = closure(globals, *body);
            Expr::Let(name, Box::new(bind), Box::new(body))
        }
    }
}

fn lifting(expr: Expr, context: &mut Context) -> Expr {
    match expr {
        Expr::Int(int) => Expr::Int(int),
        Expr::Var(sym) => {
            if let Some(fresh) = context.scope.get(&sym) {
                return fresh.clone();
            }

            Expr::Var(sym)
        }
        Expr::Abs(params, body) => {
            let fresh = context.state.fresh();
            let body = lifting(*body, context);
            let decl = Decl(fresh.clone(), params, body);
            context.decls.push(decl);
            Expr::Var(fresh)
        }
        Expr::App(fun, arg) => {
            let fun = lifting(*fun, context);
            let arg = lifting(*arg, context);
            Expr::App(Box::new(fun), Box::new(arg))
        }
        Expr::Bop(bop, left, right) => {
            let left = lifting(*left, context);
            let right = lifting(*right, context);
            Expr::Bop(bop, Box::new(left), Box::new(right))
        }
        Expr::Let(name, bind, body) => {
            if let Expr::Abs(..) = *bind {
                let bind = lifting(*bind, context);
                context.scope.insert(name, bind);

                return lifting(*body, context);
            }

            let bind = lifting(*bind, context);
            let body = lifting(*body, context);
            Expr::Let(name, Box::new(bind), Box::new(body))
        }
    }
}

fn eliminate(globals: &[String], expr: Decl) -> Vec<Decl> {
    let mut context = Context::new();
    let Decl(name, variables, body) = expr;

    let clos_conv = closure(globals, body);
    let lift_conv = lifting(clos_conv, &mut context);

    let mut decls = context.decls;
    let declation = Decl(name, variables, lift_conv);

    decls.push(declation);
    decls
}

pub fn run() {
    let tree = decl!(
        let add [x] =
            def!(incr = (
                lam!((a) => bop!((λ!(a)) + (λ!(1))))
            ) in
            def!(decr = (
                lam!((a) => bop!((λ!(a)) + (λ!(1))))
            ) in
            bop!((app!(λ!(incr), λ!(x))) + (app!(λ!(decr), λ!(x))))))
    );

    // input
    // let add x =
    //     let incr = fun x -> x + 1 in
    //     let decr = fun x -> x - 1 in
    //     (incr x) + (decr x)

    // output
    // let anno_0 a = a + 1
    // let anno_1 a = a + 1
    // let add x = (anno_0 x) + (anno_1 x)

    let tree = eliminate(&[], tree);

    for t in tree {
        println!("{t}")
    }
}
