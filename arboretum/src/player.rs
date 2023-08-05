pub struct Player {
    human: bool,
}

impl Player {
    pub fn new_human() -> Self {
        Self { human: true }
    }

    pub fn is_human(&self) -> bool {
        self.human
    }
}
