use std::process::{Child, Command, Stdio};

use crate::process::Process;

pub struct HumanPlayer {}

pub struct UciPlayer {
    pub process: Process,
    pub stdout: Vec<String>,
    pub stderr: Vec<String>,
}

pub enum Player {
    Human(HumanPlayer),
    Uci(UciPlayer),
}

impl Player {
    pub fn new_human() -> Self {
        Self::Human(HumanPlayer {})
    }

    pub fn is_human(&self) -> bool {
        matches!(self, Player::Human(_))
    }

    pub fn new_uci() -> Self {
        let process = Process::new("stockfish");

        Self::Uci(UciPlayer {
            process,
            stderr: vec![],
            stdout: vec![],
        })
    }
}
