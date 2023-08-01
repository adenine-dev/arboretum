use crate::{board::Board, ui::Theme};

pub struct Context {
    pub player_1: String,
    pub player_2: String,

    pub board: Board,

    pub white_on_bottom: bool,

    pub theme: Theme,
}
