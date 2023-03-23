pub mod closure;
pub mod lifting;
pub mod macros;
pub mod pass;
pub mod scope;
pub mod state;
pub mod term;

use closure::ClosureConv;
use lifting::LambdaLift;
use macros::{app, bop, decl, def, lam, module, λ};
use pass::Pass;

fn main() {
    let module = module!([decl!(
        let add [x] =
            def!(incr = (lam!((a) => bop!((λ!(a)) + (λ!(1))))) in
            def!(decr = (lam!((a) => bop!((λ!(a)) - (λ!(1))))) in
            bop!((app!(λ!(incr), λ!(x))) + (app!(λ!(decr), λ!(x))))))
    )]);

    let module = Pass::run(LambdaLift::default(), module);
    let module = Pass::run(ClosureConv::default(), module);

    println!("{}", module);
}
