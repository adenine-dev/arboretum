use crate::{
    board::{Board, Move},
    ui::Theme,
};

pub struct Context {
    pub player_1: String,
    pub player_2: String,

    pub board: Board,

    pub current_moves: Vec<Move>,

    pub white_on_bottom: bool,

    pub theme: Theme,
}

impl Context {
    pub fn apply_move(&mut self, mov: Move) {
        self.board = self.board.apply_move(mov);
        self.current_moves = self.board.moves();
    }
}
