pub const RANKS: [&str; 8] = ["1", "2", "3", "4", "5", "6", "7", "8"];
pub const FILES: [&str; 8] = ["a", "b", "c", "d", "e", "f", "g", "h"];

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

    pub fn is_color(self, color: Color) -> bool {
        match color {
            Color::Black => self.is_black(),
            Color::White => self.is_white(),
        }
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

    pub fn to_algebraic(self) -> &'static str {
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

            _ => panic!("invalid piece {self}"),
        }
    }

    /// Returns the figurine algebraic notation symbol for the given piece.
    pub fn to_figurine(self) -> &'static str {
        match self {
            Piece::BLACK_ROOK => "♜",
            Piece::BLACK_KNIGHT => "♞",
            Piece::BLACK_BISHOP => "♝",
            Piece::BLACK_QUEEN => "♛",
            Piece::BLACK_KING => "♚",
            Piece::BLACK_PAWN => "♟",

            Piece::WHITE_ROOK => "♖",
            Piece::WHITE_KNIGHT => "♘",
            Piece::WHITE_BISHOP => "♗",
            Piece::WHITE_QUEEN => "♕",
            Piece::WHITE_KING => "♔",
            Piece::WHITE_PAWN => "♙",

            Piece::EMPTY => "",

            _ => panic!("invalid piece {self}"),
        }
    }

    /// Returns the figurine algebraic notation symbol for the given piece, but
    /// all pieces are black, useful for displaying pieces that will be colored
    /// later.
    pub fn to_black_figurine(self) -> &'static str {
        match self {
            Piece::BLACK_ROOK | Piece::WHITE_ROOK => "♜",
            Piece::BLACK_KNIGHT | Piece::WHITE_KNIGHT => "♞",
            Piece::BLACK_BISHOP | Piece::WHITE_BISHOP => "♝",
            Piece::BLACK_QUEEN | Piece::WHITE_QUEEN => "♛",
            Piece::BLACK_KING | Piece::WHITE_KING => "♚",
            Piece::BLACK_PAWN | Piece::WHITE_PAWN => "♟",

            Piece::EMPTY => "",

            _ => panic!("invalid piece {self}"),
        }
    }
}

impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match *self {
            Piece::BLACK_ROOK => "black rook",
            Piece::BLACK_KNIGHT => "black knight",
            Piece::BLACK_BISHOP => "black bishop",
            Piece::BLACK_QUEEN => "black queen",
            Piece::BLACK_KING => "black king",
            Piece::BLACK_PAWN => "black pawn",

            Piece::WHITE_ROOK => "white rook",
            Piece::WHITE_KNIGHT => "white knight",
            Piece::WHITE_BISHOP => "white bishop",
            Piece::WHITE_QUEEN => "white queen",
            Piece::WHITE_KING => "white king",
            Piece::WHITE_PAWN => "white pawn",

            Piece::EMPTY => "empty",

            _ => "Invalid Piece",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Color {
    Black,
    White,
}

/// 8 bits designating how players can castle. Only 4 are used
/// the bits are `___KQkq` where:
/// - `_` is an unused bit
/// - `K` set if white can castle kingside
/// - `Q` set if white can castle queenside
/// - `k` set if black can castle kingside
/// - `q` set if black can castle queenside`
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CastlingAvailability(u8);

impl core::ops::BitOr for CastlingAvailability {
    type Output = CastlingAvailability;
    fn bitor(self, rhs: Self) -> Self::Output {
        CastlingAvailability(self.0 | rhs.0)
    }
}

impl core::ops::BitOrAssign for CastlingAvailability {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

impl core::ops::BitAnd for CastlingAvailability {
    type Output = CastlingAvailability;
    fn bitand(self, rhs: Self) -> Self::Output {
        CastlingAvailability(self.0 & rhs.0)
    }
}

impl core::ops::BitAndAssign for CastlingAvailability {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0
    }
}

impl core::ops::BitXor for CastlingAvailability {
    type Output = CastlingAvailability;
    fn bitxor(self, rhs: Self) -> Self::Output {
        CastlingAvailability(self.0 ^ rhs.0)
    }
}

impl core::ops::BitXorAssign for CastlingAvailability {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0
    }
}

impl CastlingAvailability {
    pub const WHITE_KINGSIDE: CastlingAvailability = CastlingAvailability(0b0000_1000);
    pub const WHITE_QUEENSIDE: CastlingAvailability = CastlingAvailability(0b0000_0100);
    pub const BLACK_KINGSIDE: CastlingAvailability = CastlingAvailability(0b0000_0010);
    pub const BLACK_QUEENSIDE: CastlingAvailability = CastlingAvailability(0b0000_0001);
    pub const ALL: CastlingAvailability = CastlingAvailability(0b0000_1111);
    pub const NONE: CastlingAvailability = CastlingAvailability(0b0000_0000);
}

/// Represents a square on the board. Stored as a rank-major index
/// Values >= 64 are invalid.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Square(u8);

impl Square {
    pub fn new(idx: u8) -> Self {
        Square(idx)
    }

    /// Creates a new square from a rank and file, both indexed from 0-7 corresponding to 1-8 and a-h respectively.
    ///
    /// ## Panics
    ///
    /// Panics if either rank or file is >= 8
    pub fn from_rank_file(rank: u8, file: u8) -> Self {
        debug_assert!(rank < 8 && file < 8);
        Square(rank * 8 + file)
    }

    /// Creates a new square from algebraic notation.
    pub fn from_algebraic(position: &str) -> Self {
        if !position.is_ascii() || position.len() != 2 {
            return Self::invalid();
        }
        let mut bytes = position.bytes();

        let file = bytes.next().unwrap() - b'a';
        let rank = bytes.next().unwrap() - b'1';

        Self::from_rank_file(rank, file)
    }

    /// Creates a new invalid square
    pub fn invalid() -> Self {
        Square(64)
    }

    /// Returns true if the square is valid
    pub fn valid(self) -> bool {
        64 > self.0
    }

    /// Returns the rank of the square
    pub fn rank(self) -> u8 {
        self.0 / 8
    }

    /// Returns the file of the square
    pub fn file(self) -> u8 {
        self.0 % 8
    }
}

impl ToString for Square {
    fn to_string(&self) -> String {
        if !self.valid() {
            return "invalid square".to_owned();
        }

        let rank = self.rank();
        let file = self.file();

        RANKS[rank as usize].to_owned() + FILES[file as usize]
    }
}

/// 16 bits representing a move
/// the bits are `FFFFFFTTTTTTKBRQ` where:
/// - `FFFFFF` is the index of the from square
/// - `TTTTTT` is the index of the to square
/// - `K` is set if a pawn is promoting to a knight
/// - `B` is set if a pawn is promoting to a bishop
/// - `R` is set if a pawn is promoting to a rook
/// - `Q` is set if a pawn is promoting to a queen
///
/// Castling is represented by the from and to squares being the king's.
///
/// Only one of `KBRQ` may be set.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Move(u16);

impl Move {
    pub fn new(from: Square, to: Square) -> Self {
        assert!(from.valid() && to.valid());
        let from = from.0 as u16;
        let to = to.0 as u16;

        Self(from << 10 | to << 4)
    }

    pub fn from(self) -> Square {
        Square::new((self.0 >> 10 & 0b111111) as u8)
    }

    pub fn to(self) -> Square {
        Square::new((self.0 >> 4 & 0b111111) as u8)
    }
}

pub struct Board {
    pieces: [Piece; 64],
    pub castling_availability: CastlingAvailability,
    pub active_color: Color,
    // if there is no en passant target then this square is invalid
    pub en_passant_target: Square,
    pub halfmove_clock: u8,
    pub fullmoves: u32,
}

impl Board {
    /// Creates a board from a fen string
    fn from_fen(fen: &str) -> Self {
        let mut pieces = [Piece::EMPTY; 64];

        let mut field_iter = fen.split(' ');

        let mut file = 0;
        let mut rank = 7;
        for c in field_iter
            .next()
            .expect("invalid fen string could not get piece placement data")
            .chars()
        {
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

        let active_color = field_iter
            .next()
            .expect("invalid fen string could not get active color");
        let active_color = match active_color {
            "w" => Color::White,
            "b" => Color::Black,
            _ => panic!("invalid fen string, active color is invalid. expected `b` or `w` found `{active_color}`"),
        };

        let castling_availability = field_iter
            .next()
            .expect("invalid fen string could not get castling availability");

        let castling_availability = {
            let mut ca = CastlingAvailability::NONE;
            if castling_availability != "-" {
                for c in castling_availability.chars() {
                    match c {
                        'K' => ca |= CastlingAvailability::WHITE_KINGSIDE,
                        'Q' => ca |= CastlingAvailability::WHITE_QUEENSIDE,
                        'k' => ca |= CastlingAvailability::BLACK_KINGSIDE,
                        'q' => ca |= CastlingAvailability::BLACK_QUEENSIDE,
                        _ => panic!("invalid fen string, castling availability is invalid. expected some combination of `K`, `Q`, `k`, `q` found `{c}`")
                    }
                }
            }

            ca
        };

        let en_passant_target = field_iter
            .next()
            .expect("invalid fen string could not get en passant target square");
        let en_passant_target = if en_passant_target == "-" {
            Square::invalid()
        } else {
            Square::from_algebraic(en_passant_target)
        };

        let halfmove_clock = field_iter
            .next()
            .expect("invalid fen string could not get halfmove clock")
            .parse()
            .expect("invalid fen string expected number for halfmove clock");

        let fullmoves = field_iter
            .next()
            .expect("invalid fen string could not get fullmove number")
            .parse()
            .expect("invalid fen string expected number for fullmove number");

        Self {
            pieces,
            active_color,
            castling_availability,
            en_passant_target,
            halfmove_clock,
            fullmoves,
        }
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
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
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

        assert_eq!(board.active_color, Color::White);

        assert_eq!(board.castling_availability, CastlingAvailability::ALL);

        assert!(!board.en_passant_target.valid());

        assert_eq!(board.halfmove_clock, 0);
        assert_eq!(board.fullmoves, 1);
    }

    #[test]
    fn square_algebraic_notation_parsing_works() {
        for (r, &rank) in RANKS.iter().enumerate() {
            for (f, &file) in FILES.iter().enumerate() {
                let square = Square::from_algebraic(&(file.to_owned() + rank));
                assert_eq!(square.rank(), r as u8);
                assert_eq!(square.file(), f as u8);
            }
        }
    }
}
