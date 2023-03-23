#[derive(Debug, Clone)]
pub struct State {
    count: usize,
}

impl State {
    pub fn new() -> State {
        State { count: 0 }
    }

    pub fn fresh(&mut self, prefix: String) -> String {
        let fresh = self.count;
        let fresh = self.count.checked_add(1).unwrap_or(fresh);

        self.count = fresh;
        format!("fresh@{}{}", prefix, fresh)
    }
}
