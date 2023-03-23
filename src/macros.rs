macro_rules! lam {
  (($($params:ident),* $(,)?) => $body:expr) => {
      term::Expr::Abs( vec![$(stringify!($params).to_owned()),*], Box::new($body))
  };
}

macro_rules! app {
    ($lambda:expr, $arg:expr) => {
        term::Expr::App(Box::new($lambda), Box::new($arg))
    };
}

macro_rules! bop {
    (($left:expr) + ($right:expr)) => {
        term::Expr::Bop(term::Op::Add, Box::new($left), Box::new($right))
    };
    (($left:expr) - ($right:expr)) => {
        term::Expr::Bop(term::Op::Sub, Box::new($left), Box::new($right))
    };
}

macro_rules! def {
    ($name:ident = ($bind:expr) in $body:expr) => {
        term::Expr::Let(
            stringify!($name).to_owned(),
            Box::new($bind),
            Box::new($body),
        )
    };
}

macro_rules! λ {
    ($num:literal) => {
        term::Expr::Int($num)
    };
    ($name:ident) => {
        term::Expr::Var(stringify!($name).to_owned())
    };
}

macro_rules! decl {
  (let $name:ident [$($params:ident),* $(,)?] = $body:expr) => {
      term::Decl(stringify!($name).to_owned(), vec![$(stringify!($params).to_owned()),*], $body)
  };
}

macro_rules! module {
    ([$($entries:expr),* $(,)?]) => {
        term::Module {
            entries: vec![$($entries.to_owned()),*],
        }
    };
}

pub(crate) use app;
pub(crate) use bop;
pub(crate) use decl;
pub(crate) use def;
pub(crate) use lam;
pub(crate) use module;
pub(crate) use λ;
