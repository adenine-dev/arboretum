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
