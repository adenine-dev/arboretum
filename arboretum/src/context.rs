use crate::{
    board::{Board, Color, Move, Square},
    player::Player,
    ui::Theme,
};

pub struct Context {
    pub black: Player,
    pub white: Player,

    pub board: Board,

    pub current_moves: Vec<Move>,
    pub current_fen: String,
    pub movable: [bool; 64],

    pub white_on_bottom: bool,

    pub theme: Theme,
}

fn make_movable_array(moves: &[Move]) -> [bool; 64] {
    let mut n = 0;
    [false; 64].map(|_| {
        let res = moves.iter().any(|mov| mov.from() == Square::new(n));
        n += 1;
        res
    })
}

impl Context {
    pub fn new(player_1: Player, player_2: Player, board: Board, theme: Theme) -> Self {
        let current_moves = board.moves();

        Self {
            black: player_1,
            white: player_2,
            board,
            movable: make_movable_array(&current_moves),
            current_fen: board.make_fen(),
            current_moves,
            white_on_bottom: true,
            theme,
        }
    }

    fn update_extra_board_data(&mut self) {
        self.current_fen = self.board.make_fen();
        self.current_moves = self.board.moves();
        self.movable = make_movable_array(&self.current_moves);
    }

    pub fn apply_move(&mut self, mov: Move) {
        self.board = self.board.apply_move(mov);
        self.update_extra_board_data()
    }

    pub fn get_player(&self, color: Color) -> &Player {
        match color {
            Color::Black => &self.black,
            Color::White => &self.white,
        }
    }

    pub fn get_player_mut(&mut self, color: Color) -> &mut Player {
        match color {
            Color::Black => &mut self.black,
            Color::White => &mut self.white,
        }
    }

    pub fn set_fen(&mut self, fen: &str) -> anyhow::Result<()> {
        let new_board = Board::from_fen(fen)?;

        self.board = new_board;
        self.update_extra_board_data();

        Ok(())
    }

    pub fn tick(&mut self) {
        self.black.tick();
        self.white.tick();
    }
}
