pub trait Pass {
    type Input;
    type Output;

    fn run(self, source: Self::Input) -> Self::Output;
}
