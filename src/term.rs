use std::fmt;

#[derive(Debug, Clone)]
pub struct Module {
    pub entries: Vec<Decl>,
}

impl Module {
    pub fn new(entries: Vec<Decl>) -> Module {
        Module { entries }
    }

    pub fn map<F>(self, f: F) -> Module
    where
        F: FnMut(&Decl) -> Decl,
    {
        Module {
            entries: self.entries.iter().map(f).collect::<Vec<_>>(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Decl(pub String, pub Vec<String>, pub Expr);

#[derive(Debug, Clone)]
pub enum Expr {
    Int(usize),
    Var(String),
    Abs(Vec<String>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Bop(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "module [\n")?;
        let entries: Vec<String> = self.entries.iter().map(|t| format!("  {}", t)).collect();
        write!(f, "{}\n", entries.join("\n"))?;
        write!(f, "]")
    }
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {} [", self.0)?;
        let args: Vec<String> = self.1.iter().map(|t| format!("{}", t)).collect();
        let args = args.join(", ");
        let body = self.2.to_string().replace("\n", "\n\t");
        write!(f, "{}] =", args)?;
        write!(f, "\n    {}", body)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Var(sym) => write!(f, "{}", sym),
            Expr::Int(int) => write!(f, "{}", int),
            Expr::Bop(Op::Add, left, right) => write!(f, "({} + {})", left, right),
            Expr::Bop(Op::Sub, left, right) => write!(f, "({} - {})", left, right),
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
