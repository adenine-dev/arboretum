/// 8 bits designating the piece type:
/// the bits are `wbkqrbnp` where:
/// - `w`: set if the piece is white
/// - first `b`: set if the piece is black
/// - `k`: set if the piece is a king
/// - `q`: set if the piece is a queen
/// - `r`: set if the piece is a rook
/// - second `b`: set if the piece is a bishop
/// - `n`: set if the piece is a knight
/// - `p`: set if the piece is a pawn
///
/// Only one of `w` and the first `b` may set, and only one of `kqrbnp` may be set.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Piece(u8);

impl Piece {
    pub const WHITE_KING: Piece = Piece(0b10_100000);
    pub const WHITE_QUEEN: Piece = Piece(0b10_010000);
    pub const WHITE_ROOK: Piece = Piece(0b10_001000);
    pub const WHITE_BISHOP: Piece = Piece(0b10_000100);
    pub const WHITE_KNIGHT: Piece = Piece(0b10_000010);
    pub const WHITE_PAWN: Piece = Piece(0b10_000001);

    pub const BLACK_KING: Piece = Piece(0b01_100000);
    pub const BLACK_QUEEN: Piece = Piece(0b01_010000);
    pub const BLACK_ROOK: Piece = Piece(0b01_001000);
    pub const BLACK_BISHOP: Piece = Piece(0b01_000100);
    pub const BLACK_KNIGHT: Piece = Piece(0b01_000010);
    pub const BLACK_PAWN: Piece = Piece(0b01_000001);

    pub const EMPTY: Piece = Piece(0b00_000000);

    pub fn is_white(self) -> bool {
        let mask = 0b10_000000;
        (self.0 & mask) == mask
    }

    pub fn is_black(self) -> bool {
        let mask = 0b01_000000;
        (self.0 & mask) == mask
    }

    pub fn is_king(self) -> bool {
        let mask = 0b00_100000;
        (self.0 & mask) == mask
    }

    pub fn is_queen(self) -> bool {
        let mask = 0b00_010000;
        (self.0 & mask) == mask
    }

    pub fn is_rook(self) -> bool {
        let mask = 0b00_001000;
        (self.0 & mask) == mask
    }

    pub fn is_bishop(self) -> bool {
        let mask = 0b00_000100;
        (self.0 & mask) == mask
    }

    pub fn is_knight(self) -> bool {
        let mask = 0b00_000010;
        (self.0 & mask) == mask
    }

    pub fn is_pawn(self) -> bool {
        let mask = 0b00_000001;
        (self.0 & mask) == mask
    }

    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub fn to_string(self) -> &'static str {
        match self {
            Piece::BLACK_ROOK => "r",
            Piece::BLACK_KNIGHT => "n",
            Piece::BLACK_BISHOP => "b",
            Piece::BLACK_QUEEN => "q",
            Piece::BLACK_KING => "k",
            Piece::BLACK_PAWN => "p",

            Piece::WHITE_ROOK => "R",
            Piece::WHITE_KNIGHT => "N",
            Piece::WHITE_BISHOP => "B",
            Piece::WHITE_QUEEN => "Q",
            Piece::WHITE_KING => "K",
            Piece::WHITE_PAWN => "P",

            Piece::EMPTY => "",

            _ => "INVALID PIECE",
        }
    }
}

pub struct Board {
    pieces: [Piece; 64],
}

impl Board {
    fn from_fen(fen: &str) -> Self {
        let mut pieces = [Piece::EMPTY; 64];

        let mut file = 0;
        let mut rank = 7;
        for c in fen.chars() {
            let p = match c {
                'r' => Piece::BLACK_ROOK,
                'n' => Piece::BLACK_KNIGHT,
                'b' => Piece::BLACK_BISHOP,
                'q' => Piece::BLACK_QUEEN,
                'k' => Piece::BLACK_KING,
                'p' => Piece::BLACK_PAWN,

                'R' => Piece::WHITE_ROOK,
                'N' => Piece::WHITE_KNIGHT,
                'B' => Piece::WHITE_BISHOP,
                'Q' => Piece::WHITE_QUEEN,
                'K' => Piece::WHITE_KING,
                'P' => Piece::WHITE_PAWN,

                '/' => {
                    if file != 8 {
                        dbg!(file);
                        panic!("invalid fen: `{fen}`");
                    }
                    file = 0;
                    rank -= 1;
                    continue;
                }
                _ => {
                    if c.is_numeric() {
                        file += c.to_digit(10).unwrap();
                    }
                    continue;
                }
            };

            pieces[(rank * 8 + file) as usize] = p;
            file += 1;
        }

        Self { pieces }
    }

    /// Returns the piece at rank, file, both indexed from 0-7 corresponding to 1-8 and a-h respectively
    ///
    /// ## Panics
    ///
    /// Panics if either rank or file is >= 8
    #[inline]
    pub fn get(&self, rank: u8, file: u8) -> Piece {
        self.pieces[rank as usize * 8 + file as usize]
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pieces_are_what_they_say_they_are() {
        assert!(Piece::WHITE_KING.is_white());
        assert!(Piece::WHITE_KING.is_king());
        assert!(Piece::WHITE_QUEEN.is_white());
        assert!(Piece::WHITE_QUEEN.is_queen());
        assert!(Piece::WHITE_ROOK.is_white());
        assert!(Piece::WHITE_ROOK.is_rook());
        assert!(Piece::WHITE_BISHOP.is_white());
        assert!(Piece::WHITE_BISHOP.is_bishop());
        assert!(Piece::WHITE_KNIGHT.is_white());
        assert!(Piece::WHITE_KNIGHT.is_knight());
        assert!(Piece::WHITE_PAWN.is_white());
        assert!(Piece::WHITE_PAWN.is_pawn());

        assert!(Piece::BLACK_KING.is_black());
        assert!(Piece::BLACK_KING.is_king());
        assert!(Piece::BLACK_QUEEN.is_black());
        assert!(Piece::BLACK_QUEEN.is_queen());
        assert!(Piece::BLACK_ROOK.is_black());
        assert!(Piece::BLACK_ROOK.is_rook());
        assert!(Piece::BLACK_BISHOP.is_black());
        assert!(Piece::BLACK_BISHOP.is_bishop());
        assert!(Piece::BLACK_KNIGHT.is_black());
        assert!(Piece::BLACK_KNIGHT.is_knight());
        assert!(Piece::BLACK_PAWN.is_black());
        assert!(Piece::BLACK_PAWN.is_pawn());

        assert!(Piece::EMPTY.is_empty())
    }

    #[test]
    fn board_default_is_default_position() {
        let board = Board::default();

        assert_eq!(board.get(0, 0), Piece::WHITE_ROOK);
        assert_eq!(board.get(0, 1), Piece::WHITE_KNIGHT);
        assert_eq!(board.get(0, 2), Piece::WHITE_BISHOP);
        assert_eq!(board.get(0, 3), Piece::WHITE_QUEEN);
        assert_eq!(board.get(0, 4), Piece::WHITE_KING);
        assert_eq!(board.get(0, 5), Piece::WHITE_BISHOP);
        assert_eq!(board.get(0, 6), Piece::WHITE_KNIGHT);
        assert_eq!(board.get(0, 7), Piece::WHITE_ROOK);

        for file in 0..7 {
            assert_eq!(board.get(1, file), Piece::WHITE_PAWN);
        }

        assert_eq!(board.get(7, 0), Piece::BLACK_ROOK);
        assert_eq!(board.get(7, 1), Piece::BLACK_KNIGHT);
        assert_eq!(board.get(7, 2), Piece::BLACK_BISHOP);
        assert_eq!(board.get(7, 3), Piece::BLACK_QUEEN);
        assert_eq!(board.get(7, 4), Piece::BLACK_KING);
        assert_eq!(board.get(7, 5), Piece::BLACK_BISHOP);
        assert_eq!(board.get(7, 6), Piece::BLACK_KNIGHT);
        assert_eq!(board.get(7, 7), Piece::BLACK_ROOK);

        for file in 0..7 {
            assert_eq!(board.get(6, file), Piece::BLACK_PAWN);
        }

        for rank in 2..=5 {
            for file in 0..7 {
                assert!(board.get(rank, file).is_empty());
            }
        }
    }
}
