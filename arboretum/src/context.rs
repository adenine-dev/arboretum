use crate::{
    board::{Board, Color, Move, Square},
    player::Player,
    ui::Theme,
};

pub struct Winner {
    pub color: Color,
}

pub struct Context {
    pub black: Player,
    pub white: Player,

    pub board: Board,
    pub past_moves: Vec<String>,

    pub current_moves: Vec<Move>,
    pub current_fen: String,
    pub movable: [bool; 64],
    pub paused: bool,
    pub winner: Option<Winner>,

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
            past_moves: vec![],
            movable: make_movable_array(&current_moves),
            current_fen: board.make_fen(),
            current_moves,
            paused: false,
            winner: None,
            white_on_bottom: true,
            theme,
        }
    }

    fn update_extra_board_data(&mut self) {
        self.current_fen = self.board.make_fen();
        self.current_moves = self.board.moves();
        self.movable = make_movable_array(&self.current_moves);
        if self.current_moves.is_empty() {
            self.winner = Some(Winner {
                color: self.board.active_color.opponent(),
            })
        }
    }

    /// apply the move to the active game, `mov` may not have the appropriate
    /// flags, only to and from, and promotion (if needed) are required
    pub fn apply_move(&mut self, mov: Move) {
        let mov = self.current_moves.iter().find(|m| {
            // find move with correct flags
            m.from() == mov.from() && m.to() == mov.to() && m.flags().contains(mov.flags())
        });

        if let Some(&mov) = mov {
            self.past_moves.push(mov.to_long_algebraic());
            self.board = self.board.apply_move(mov);
            self.update_extra_board_data();
            if self.is_in_game() {
                self.get_player_mut(self.board.active_color).request_move();
            }
        }
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

        if self.is_in_game() && !self.paused {
            if let Some(mov) = self.get_player_mut(self.board.active_color).take_move() {
                self.apply_move(mov);
                self.next_move();
            }
        }
    }

    pub fn is_ready(&self) -> bool {
        self.black.is_ready() && self.white.is_ready()
    }

    pub fn is_in_game(&self) -> bool {
        self.black.is_in_game() && self.white.is_in_game() && self.winner.is_none()
    }

    pub fn ready(&mut self) {
        self.black.ready();
        self.white.ready();
    }

    pub fn start_new_game(&mut self) {
        if !self.is_ready() {
            self.ready();
            return;
        }

        self.black.start_new_game();
        self.white.start_new_game();

        self.next_move();
    }

    fn next_move(&mut self) {
        // do this manually for borrow checker reasons
        //TODO: don't 🙃

        let player = match self.board.active_color {
            Color::Black => &mut self.black,
            Color::White => &mut self.white,
        };
        player.next_move(&self.past_moves);
        player.request_move();
    }

    pub fn pause(&mut self) {
        self.paused = true;
    }

    pub fn unpause(&mut self) {
        self.paused = false;
    }
}
