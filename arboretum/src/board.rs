use anyhow::Result;
use rayon::prelude::*;
use thiserror::Error;

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

    // the dummy is neither color so nothing should process it as a movable piece, but is capturable by all.
    pub const DUMMY: Piece = Piece(0b00_111111);

    pub fn king(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_KING,
            Color::White => Self::WHITE_KING,
        }
    }

    pub fn queen(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_QUEEN,
            Color::White => Self::WHITE_QUEEN,
        }
    }

    pub fn rook(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_ROOK,
            Color::White => Self::WHITE_ROOK,
        }
    }

    pub fn bishop(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_BISHOP,
            Color::White => Self::WHITE_BISHOP,
        }
    }

    pub fn knight(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_KNIGHT,
            Color::White => Self::WHITE_KNIGHT,
        }
    }

    pub fn pawn(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_PAWN,
            Color::White => Self::WHITE_PAWN,
        }
    }

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

    pub fn moves_diagonal(self) -> bool {
        self.is_bishop() || self.is_queen()
    }

    pub fn moves_forthright(self) -> bool {
        self.is_rook() || self.is_queen()
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

    fn is_capturable_by(self, color: Color) -> bool {
        self == Self::DUMMY || self.is_color(color.opponent())
    }

    fn color_can_move_to(self, color: Color) -> bool {
        self.is_empty() || self.is_capturable_by(color)
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn opponent(self) -> Self {
        match self {
            Color::Black => Color::White,
            Color::White => Color::Black,
        }
    }

    pub fn double_push_pawn_rank(self) -> u8 {
        match self {
            Color::Black => 6,
            Color::White => 1,
        }
    }

    pub fn promotion_rank(self) -> u8 {
        match self {
            Color::Black => 0,
            Color::White => 7,
        }
    }

    pub fn pawn_move_direction(self) -> i8 {
        match self {
            Color::Black => -1,
            Color::White => 1,
        }
    }

    pub fn kingside_castle_path(self) -> [Square; 2] {
        [
            self.kingside_castle_position_king(),
            self.kingside_castle_position_rook(),
        ]
    }

    pub fn kingside_castle_position_king(&self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 6),
            Color::White => Square::from_rank_file(0, 6),
        }
    }

    pub fn kingside_castle_position_rook(&self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 5),
            Color::White => Square::from_rank_file(0, 5),
        }
    }

    pub fn kingside_rook_position(self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 7),
            Color::White => Square::from_rank_file(0, 7),
        }
    }

    pub fn kingside_knight_position(self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 6),
            Color::White => Square::from_rank_file(0, 6),
        }
    }

    pub fn queenside_castle_path(self) -> [Square; 2] {
        [
            self.queenside_castle_position_king(),
            self.queenside_castle_position_rook(),
        ]
    }

    pub fn queenside_castle_position_king(self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 2),
            Color::White => Square::from_rank_file(0, 2),
        }
    }

    pub fn queenside_castle_position_rook(self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 3),
            Color::White => Square::from_rank_file(0, 3),
        }
    }

    pub fn queenside_rook_position(self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 0),
            Color::White => Square::from_rank_file(0, 0),
        }
    }

    pub fn queenside_knight_position(self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 1),
            Color::White => Square::from_rank_file(0, 1),
        }
    }

    pub fn rook_position(self, side: Side) -> Square {
        match side {
            Side::King => self.kingside_rook_position(),
            Side::Queen => self.queenside_rook_position(),
        }
    }

    pub fn castle_position_rook(self, castle_side: Side) -> Square {
        match castle_side {
            Side::King => self.kingside_castle_position_rook(),
            Side::Queen => self.queenside_castle_position_rook(),
        }
    }

    pub fn castle_position_king(self, castle_side: Side) -> Square {
        match castle_side {
            Side::King => self.kingside_castle_position_king(),
            Side::Queen => self.queenside_castle_position_king(),
        }
    }

    fn initial_king_position(&self) -> Square {
        match self {
            Color::Black => Square::from_rank_file(7, 4),
            Color::White => Square::from_rank_file(0, 4),
        }
    }
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

impl core::ops::Not for CastlingAvailability {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl CastlingAvailability {
    pub const WHITE_KINGSIDE: CastlingAvailability = CastlingAvailability(0b0000_1000);
    pub const WHITE_QUEENSIDE: CastlingAvailability = CastlingAvailability(0b0000_0100);
    pub const BLACK_KINGSIDE: CastlingAvailability = CastlingAvailability(0b0000_0010);
    pub const BLACK_QUEENSIDE: CastlingAvailability = CastlingAvailability(0b0000_0001);
    pub const ALL: CastlingAvailability = CastlingAvailability(0b0000_1111);
    pub const NONE: CastlingAvailability = CastlingAvailability(0b0000_0000);

    pub fn contains(self, other: Self) -> bool {
        (self & other) == other
    }

    pub fn kingside_color(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_KINGSIDE,
            Color::White => Self::WHITE_KINGSIDE,
        }
    }

    pub fn queenside_color(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_QUEENSIDE,
            Color::White => Self::WHITE_QUEENSIDE,
        }
    }

    pub fn has_kingside_with(self, color: Color) -> bool {
        match color {
            Color::Black => self.contains(Self::BLACK_KINGSIDE),
            Color::White => self.contains(Self::WHITE_KINGSIDE),
        }
    }

    pub fn has_queenside_with(self, color: Color) -> bool {
        match color {
            Color::Black => self.contains(Self::BLACK_QUEENSIDE),
            Color::White => self.contains(Self::WHITE_QUEENSIDE),
        }
    }
}

impl std::fmt::Display for CastlingAvailability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Self::NONE {
            return f.write_str("-");
        }

        if self.contains(Self::WHITE_KINGSIDE) {
            f.write_str("K")?;
        }
        if self.contains(Self::WHITE_QUEENSIDE) {
            f.write_str("Q")?;
        }

        if self.contains(Self::BLACK_KINGSIDE) {
            f.write_str("k")?;
        }
        if self.contains(Self::BLACK_QUEENSIDE) {
            f.write_str("q")?;
        }

        Ok(())
    }
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

    /// Returns a new square moved by the specified number of ranks and files if it is valid.
    pub fn mov(self, d_rank: i8, d_file: i8) -> Option<Self> {
        let rank = self.rank() as i8;
        let file = self.file() as i8;

        let new_rank = rank + d_rank;
        let new_file = file + d_file;

        if !(0..8).contains(&new_rank) || !(0..8).contains(&new_file) {
            return None;
        }

        Some(Self::from_rank_file(new_rank as u8, new_file as u8))
    }
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.valid() {
            return f.write_str("invalid square");
        }

        let rank = self.rank();
        let file = self.file();

        f.write_str(&(FILES[file as usize].to_owned() + RANKS[rank as usize]))
    }
}

pub enum Side {
    King,
    Queen,
}

/// represents move flags
/// the bits are `____FFFF` where:
/// - `_` is an unused bit
/// - `FFFF` are the flags, denoting various properties about the move (see associated constants)
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct MoveFlags(u8);

impl MoveFlags {
    pub const NONE: MoveFlags = Self(0b0000);

    pub const PROMOTE_TO_KNIGHT: MoveFlags = Self(0b1000);
    pub const PROMOTE_TO_BISHOP: MoveFlags = Self(0b0100);
    pub const PROMOTE_TO_ROOK: MoveFlags = Self(0b0010);
    pub const PROMOTE_TO_QUEEN: MoveFlags = Self(0b0001);

    pub const CASTLE_QUEENSIDE: MoveFlags = Self(0b0011);
    pub const CASTLE_KINGSIDE: MoveFlags = Self(0b0101);

    pub const DOUBLE_PUSH: MoveFlags = Self(0b1100);
    pub const EN_PASSANT: MoveFlags = Self(0b1010);

    pub fn is_promotion(self) -> bool {
        (self.0 & 0b1111).is_power_of_two()
    }

    pub fn is_castle(self) -> bool {
        self == Self::CASTLE_KINGSIDE || self == Self::CASTLE_QUEENSIDE
    }

    pub fn is_double_push(self) -> bool {
        self == Self::DOUBLE_PUSH
    }

    pub fn is_en_passant(self) -> bool {
        self == Self::EN_PASSANT
    }

    fn castle_side(self) -> Side {
        assert!(self.is_castle());
        if self == Self::CASTLE_KINGSIDE {
            Side::King
        } else {
            Side::Queen
        }
    }

    fn promotion_piece(self, color: Color) -> Option<Piece> {
        match color {
            Color::Black => match self {
                MoveFlags::PROMOTE_TO_KNIGHT => Some(Piece::BLACK_KNIGHT),
                MoveFlags::PROMOTE_TO_BISHOP => Some(Piece::BLACK_BISHOP),
                MoveFlags::PROMOTE_TO_ROOK => Some(Piece::BLACK_ROOK),
                MoveFlags::PROMOTE_TO_QUEEN => Some(Piece::BLACK_QUEEN),
                _ => None,
            },
            Color::White => match self {
                MoveFlags::PROMOTE_TO_KNIGHT => Some(Piece::WHITE_KNIGHT),
                MoveFlags::PROMOTE_TO_BISHOP => Some(Piece::WHITE_BISHOP),
                MoveFlags::PROMOTE_TO_ROOK => Some(Piece::WHITE_ROOK),
                MoveFlags::PROMOTE_TO_QUEEN => Some(Piece::WHITE_QUEEN),
                _ => None,
            },
        }
    }

    pub fn contains(self, flags: MoveFlags) -> bool {
        (self.0 & flags.0) == flags.0
    }
}

/// 16 bits representing a move
/// the bits are `FFFFFFTTTTTTXXXX` where:
/// - `FFFFFF` is the index of the from square
/// - `TTTTTT` is the index of the to square
/// - `XXXX` are the flags which may be any of: [`MoveFlags`]
///
/// Castling is represented by the from and to squares being the king's.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Move(u16);

impl Move {
    pub const NULLMOVE: Move = Move(0);

    pub fn new(from: Square, to: Square) -> Self {
        assert!(from.valid() && to.valid());
        let from = from.0 as u16;
        let to = to.0 as u16;

        Self(((from & 0b111111) << 10) | ((to & 0b111111) << 4))
    }

    pub fn new_with_flags(from: Square, to: Square, flags: MoveFlags) -> Self {
        assert!(from.valid() && to.valid());
        let from = from.0 as u16;
        let to = to.0 as u16;

        Self(((from & 0b111111) << 10) | ((to & 0b111111) << 4) | (flags.0 as u16 & 0b1111))
    }

    pub fn from_long_algebraic(mov: &str) -> Self {
        if mov == "(none)" || mov == "0000" {
            return Self::NULLMOVE;
        }

        assert!(4 <= mov.len() && mov.len() <= 5);
        let from = &mov[0..2];
        let to = &mov[2..4];

        let promotion = mov.as_bytes().get(4).copied();

        Self::new_with_flags(
            Square::from_algebraic(from),
            Square::from_algebraic(to),
            promotion
                .map(|p| match p {
                    b'q' => MoveFlags::PROMOTE_TO_QUEEN,
                    b'r' => MoveFlags::PROMOTE_TO_ROOK,
                    b'b' => MoveFlags::PROMOTE_TO_BISHOP,
                    b'k' => MoveFlags::PROMOTE_TO_KNIGHT,
                    _ => panic!("oof"),
                })
                .unwrap_or(MoveFlags::NONE),
        )
    }

    pub fn from(self) -> Square {
        Square::new((self.0 >> 10 & 0b111111) as u8)
    }

    pub fn to(self) -> Square {
        Square::new((self.0 >> 4 & 0b111111) as u8)
    }

    pub fn flags(self) -> MoveFlags {
        MoveFlags((self.0 & 0b1111) as u8)
    }

    pub(crate) fn to_long_algebraic(self) -> String {
        format!(
            "{}{}{}",
            self.from(),
            self.to(),
            self.flags()
                .promotion_piece(Color::Black)
                .map(|p| p.to_algebraic())
                .unwrap_or("")
        )
    }
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&(self.from().to_string() + &self.to().to_string()))
    }
}

#[derive(Debug, Error)]
pub enum FenParseError {
    #[error("invalid fen string, missing piece placement data")]
    MissingPiecePlacementData,
    #[error("invalid fen string, invalid piece placement data")]
    InvalidPiecePlacementData,
    #[error("invalid fen string, missing active color")]
    MissingActiveColor,
    #[error("invalid fen string, invalid active color. expected `b` or `w` found {0}")]
    InvalidActiveColor(String),
    #[error("invalid fen string, missing castling availability")]
    MissingCastlingAvailability,
    #[error("invalid fen string, castling availability is invalid. expected some combination of `K`, `Q`, `k`, and `q` or `-` found `{0}")]
    InvalidCastlingAvailability(char),
    #[error("invalid fen string, missing target en passant square")]
    MissingTargetEnPassantSquare,
    #[error("invalid fen string, missing halfmove clock")]
    MissingHalfmoveClock,
    #[error("invalid fen string, missing fullmove count")]
    MissingFullmoveCount,
}

#[derive(Debug, Clone, Copy)]
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
    pub fn from_fen(fen: &str) -> Result<Self> {
        let mut pieces = [Piece::EMPTY; 64];

        let mut field_iter = fen.split(' ');

        let mut file = 0;
        let mut rank = 7;
        for c in field_iter
            .next()
            .ok_or(FenParseError::MissingPiecePlacementData)?
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
                        return Err(anyhow::Error::new(FenParseError::InvalidPiecePlacementData));
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

            if file > 7 || rank > 7 {
                return Err(anyhow::Error::new(FenParseError::InvalidPiecePlacementData));
            }

            pieces[(rank * 8 + file) as usize] = p;
            file += 1;
        }

        let active_color = field_iter.next().ok_or(FenParseError::MissingActiveColor)?;
        let active_color = match active_color {
            "w" => Color::White,
            "b" => Color::Black,
            _ => {
                return Err(anyhow::Error::new(FenParseError::InvalidActiveColor(
                    active_color.to_owned(),
                )))
            }
        };

        let castling_availability = field_iter
            .next()
            .ok_or(FenParseError::MissingCastlingAvailability)?;

        let castling_availability = {
            let mut ca = CastlingAvailability::NONE;
            if castling_availability != "-" {
                for c in castling_availability.chars() {
                    match c {
                        'K' => ca |= CastlingAvailability::WHITE_KINGSIDE,
                        'Q' => ca |= CastlingAvailability::WHITE_QUEENSIDE,
                        'k' => ca |= CastlingAvailability::BLACK_KINGSIDE,
                        'q' => ca |= CastlingAvailability::BLACK_QUEENSIDE,
                        _ => {
                            return Err(anyhow::Error::new(
                                FenParseError::InvalidCastlingAvailability(c),
                            ))
                        }
                    }
                }
            }

            ca
        };

        let en_passant_target = field_iter
            .next()
            .ok_or(FenParseError::MissingTargetEnPassantSquare)?;
        let en_passant_target = if en_passant_target == "-" {
            Square::invalid()
        } else {
            Square::from_algebraic(en_passant_target)
        };

        let halfmove_clock = field_iter
            .next()
            .ok_or(FenParseError::MissingHalfmoveClock)?
            .parse()?;

        let fullmoves = field_iter
            .next()
            .ok_or(FenParseError::MissingFullmoveCount)?
            .parse()?;

        Ok(Self {
            pieces,
            active_color,
            castling_availability,
            en_passant_target,
            halfmove_clock,
            fullmoves,
        })
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

    #[inline]
    pub fn get_mut(&mut self, rank: u8, file: u8) -> &mut Piece {
        &mut self.pieces[rank as usize * 8 + file as usize]
    }

    pub fn at(&self, square: Square) -> Piece {
        self.pieces[square.0 as usize]
    }

    pub fn at_mut(&mut self, square: Square) -> &mut Piece {
        &mut self.pieces[square.0 as usize]
    }

    fn under_attack_by(&self, color: Color, square: Square) -> bool {
        // construct new board
        let mut board = *self;
        board.active_color = color; // attacking color

        // place a piece at the attacked square, pawns cannot attack a square if
        // a piece isn't there. Bit of a hack but meh
        if board.at(square).is_empty() {}
        *board.at_mut(square) = Piece::DUMMY;

        board
            .pseudolegal_moves()
            .into_iter()
            .any(|mov| mov.to() == square)
    }

    fn in_check(&self, color: Color) -> bool {
        if let Some(king_square) = self
            .pieces
            .iter()
            .position(|p| p.is_king() && p.is_color(color))
        {
            return self.under_attack_by(color.opponent(), Square::new(king_square as u8));
        }

        false
    }

    fn extend_pseudo_legal_king_moves_at(&self, pseudo_legal: &mut Vec<Move>, from_square: Square) {
        // basic king movement
        for r in -1..=1 {
            for f in -1..=1 {
                if let Some(to) = from_square.mov(r, f) {
                    if self.pieces[to.0 as usize].color_can_move_to(self.active_color) {
                        pseudo_legal.push(Move::new(from_square, to));
                    }
                }
            }
        }

        // castling
        if self
            .castling_availability
            .has_kingside_with(self.active_color)
            && self
                .active_color
                .kingside_castle_path()
                .into_iter()
                .all(|square| {
                    self.at(square).is_empty()
                        && !self.under_attack_by(self.active_color.opponent(), square)
                        && self.at(self.active_color.kingside_rook_position())
                            == Piece::rook(self.active_color)
                })
            && !self.in_check(self.active_color)
        {
            pseudo_legal.push(Move::new_with_flags(
                from_square,
                self.active_color.kingside_castle_position_king(),
                MoveFlags::CASTLE_KINGSIDE,
            ))
        }

        if self
            .castling_availability
            .has_queenside_with(self.active_color)
            && self
                .active_color
                .queenside_castle_path()
                .into_iter()
                .all(|square| {
                    self.at(square).is_empty()
                        && !self.under_attack_by(self.active_color.opponent(), square)
                        && self
                            .at(self.active_color.queenside_knight_position())
                            .is_empty()
                        && self.at(self.active_color.queenside_rook_position())
                            == Piece::rook(self.active_color)
                })
            && !self.in_check(self.active_color)
        {
            pseudo_legal.push(Move::new_with_flags(
                from_square,
                self.active_color.queenside_castle_position_king(),
                MoveFlags::CASTLE_QUEENSIDE,
            ))
        }
    }

    fn extend_pseudo_legal_pawn_moves_at(&self, pseudo_legal: &mut Vec<Move>, from_square: Square) {
        let direction = self.active_color.pawn_move_direction();

        for f in -1..=1 {
            if let Some(to) = from_square.mov(direction, f) {
                if !self.pieces[to.0 as usize].is_color(self.active_color) {
                    // can only capture diagonally
                    if (f == 0 && self.pieces[to.0 as usize].is_empty())
                        || (f != 0
                            && (self.pieces[to.0 as usize].is_capturable_by(self.active_color)
                                || self.en_passant_target == to))
                    {
                        if to.rank() == self.active_color.promotion_rank() {
                            pseudo_legal.extend([
                                Move::new_with_flags(from_square, to, MoveFlags::PROMOTE_TO_QUEEN),
                                Move::new_with_flags(from_square, to, MoveFlags::PROMOTE_TO_ROOK),
                                Move::new_with_flags(from_square, to, MoveFlags::PROMOTE_TO_BISHOP),
                                Move::new_with_flags(from_square, to, MoveFlags::PROMOTE_TO_KNIGHT),
                            ]);
                        } else {
                            pseudo_legal.push(Move::new_with_flags(
                                from_square,
                                to,
                                if self.en_passant_target == to {
                                    MoveFlags::EN_PASSANT
                                } else {
                                    MoveFlags::NONE
                                },
                            ));
                        }
                    }
                }
            }
        }

        if self.active_color.double_push_pawn_rank() == from_square.rank() {
            if let Some(to) = from_square.mov(2 * direction, 0) {
                if self.pieces[to.0 as usize].is_empty()
                    && self.pieces
                        [(to.0 as i8 - (8 * self.active_color.pawn_move_direction())) as usize]
                        .is_empty()
                {
                    pseudo_legal.push(Move::new_with_flags(
                        from_square,
                        to,
                        MoveFlags::DOUBLE_PUSH,
                    ));
                }
            }
        }
    }

    fn extend_pseudo_legal_knight_moves_at(
        &self,
        pseudo_legal: &mut Vec<Move>,
        from_square: Square,
    ) {
        let deltas = [
            (-1, -2),
            (-2, -1),
            (-1, 2),
            (2, -1),
            (1, -2),
            (-2, 1),
            (1, 2),
            (2, 1),
        ];
        for (r, f) in deltas {
            if let Some(to) = from_square.mov(r, f) {
                if self.pieces[to.0 as usize].color_can_move_to(self.active_color) {
                    pseudo_legal.push(Move::new(from_square, to));
                }
            }
        }
    }

    fn extend_pseudo_legal_slider_moves_at(
        &self,
        pseudo_legal: &mut Vec<Move>,
        from_square: Square,
        forthright: bool,
        diagonal: bool,
    ) {
        let deltas = [
            // forthright deltas
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
            // diagonal deltas
            (1, 1),
            (-1, 1),
            (1, -1),
            (-1, -1),
        ];

        let start = if forthright { 0 } else { 4 };
        let end = if diagonal { 8 } else { 4 };

        for &(dr, df) in deltas[start..end].iter() {
            for n in 1..=8 {
                let dr = dr * n;
                let df = df * n;
                if let Some(to) = from_square.mov(dr, df) {
                    if self.pieces[to.0 as usize].is_color(self.active_color) {
                        break;
                    }
                    pseudo_legal.push(Move::new(from_square, to));
                    if self.pieces[to.0 as usize].is_capturable_by(self.active_color) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
    }

    pub fn pseudolegal_moves(&self) -> Vec<Move> {
        // preallocate vec with some maximum, the maximum legal move count is 218,
        // this is pseudolegal, but its a good enough estimate.
        let mut pseudo_legal = Vec::with_capacity(218);

        for (idx, piece) in self.pieces.iter().enumerate() {
            let idx = idx;
            let from_square = Square::new(idx as u8);
            if piece.is_color(self.active_color) {
                if piece.is_king() {
                    self.extend_pseudo_legal_king_moves_at(&mut pseudo_legal, from_square);
                } else if piece.is_pawn() {
                    self.extend_pseudo_legal_pawn_moves_at(&mut pseudo_legal, from_square);
                } else if piece.is_knight() {
                    self.extend_pseudo_legal_knight_moves_at(&mut pseudo_legal, from_square);
                } else {
                    self.extend_pseudo_legal_slider_moves_at(
                        &mut pseudo_legal,
                        from_square,
                        piece.moves_forthright(),
                        piece.moves_diagonal(),
                    )
                }
            }
        }

        pseudo_legal
    }

    pub fn moves(&self) -> Vec<Move> {
        self.pseudolegal_moves()
            .into_iter()
            .filter(|&mov| !self.apply_move(mov).in_check(self.active_color))
            .collect()
    }

    /// runs the perft function. This is purely used for debugging and testing and should not be
    /// used as an actual move generator because it is so slow
    pub fn perft(&self, depth: usize, print: bool) -> u128 {
        let moves = self.moves();

        if depth == 1 {
            if print {
                for mov in moves.iter() {
                    println!("{mov} 1");
                }
            }
            return moves.len() as u128;
        }

        moves
            .into_par_iter()
            // .into_iter()
            .map(|mov| {
                let n = self.apply_move(mov).perft(depth - 1, false);
                if print {
                    println!("{mov} {n}");
                }
                n
            })
            .sum()
    }

    pub fn apply_move(&self, mov: Move) -> Board {
        let mut ret = *self;

        ret.active_color = self.active_color.opponent();

        if self.active_color == Color::Black {
            ret.fullmoves += 1;
        }

        // take care of the 50 move rule
        if !self.at(mov.from()).is_pawn() && self.at(mov.to()).is_empty() {
            ret.halfmove_clock += 1;
        } else {
            ret.halfmove_clock = 0;
        }

        // find en passant target square
        if mov.flags().is_double_push() {
            ret.en_passant_target = mov
                .from()
                .mov(self.active_color.pawn_move_direction(), 0)
                .unwrap();
        } else {
            ret.en_passant_target = Square::invalid();
        }

        // move piece
        *ret.at_mut(mov.to()) = self.at(mov.from());
        *ret.at_mut(mov.from()) = Piece::EMPTY;

        // promotion
        if mov.flags().is_promotion() {
            *ret.at_mut(mov.to()) = mov.flags().promotion_piece(self.active_color).unwrap();
        }

        // handle en passant
        if mov.flags().is_en_passant() {
            *ret.at_mut(
                self.en_passant_target
                    .mov(self.active_color.opponent().pawn_move_direction(), 0)
                    .unwrap(),
            ) = Piece::EMPTY;
        }

        // update castling flags
        if mov.from() == self.active_color.kingside_rook_position()
            || mov.to() == self.active_color.kingside_rook_position()
            || mov.from() == self.active_color.initial_king_position()
        {
            ret.castling_availability &= !CastlingAvailability::kingside_color(self.active_color);
        }

        if mov.from() == self.active_color.queenside_rook_position()
            || mov.to() == self.active_color.queenside_rook_position()
            || mov.from() == self.active_color.initial_king_position()
        {
            ret.castling_availability &= !CastlingAvailability::queenside_color(self.active_color);
        }

        // handle castling (note because of move encoding, king has already been moved)
        if mov.flags().is_castle() {
            *ret.at_mut(self.active_color.rook_position(mov.flags().castle_side())) = Piece::EMPTY;
            *ret.at_mut(
                self.active_color
                    .castle_position_rook(mov.flags().castle_side()),
            ) = Piece::rook(self.active_color);
        }

        ret
    }

    pub fn make_fen(&self) -> String {
        let mut piece_placement = String::with_capacity(32); // shortest possible string + a bit extra

        // piece placement data
        let mut n = 0;
        for r in 0..8 {
            for f in 0..8 {
                let i = (7 - r) * 8 + f;
                let p = self.pieces[i];
                if i % 8 == 0 && i != 56 {
                    if n != 0 {
                        piece_placement += &n.to_string();
                        n = 0;
                    }
                    piece_placement += "/";
                }
                if p.is_empty() {
                    n += 1;
                } else {
                    if n != 0 {
                        piece_placement += &n.to_string();
                        n = 0;
                    }
                    piece_placement += p.to_algebraic();
                }
            }
        }
        if n != 0 {
            piece_placement += &n.to_string();
        }

        let mut en_passant_target = "-".to_owned();
        if self.en_passant_target.valid() {
            en_passant_target = self.en_passant_target.to_string();
        }

        format!(
            "{} {} {} {} {} {}",
            piece_placement,
            match self.active_color {
                Color::Black => "b",
                Color::White => "w",
            },
            self.castling_availability,
            en_passant_target,
            self.halfmove_clock,
            self.fullmoves
        )
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
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
    fn fen_generation() {
        let check_same = |fen| {
            assert_eq!(Board::from_fen(fen).unwrap().make_fen(), fen);
        };
        check_same("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        check_same("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1");
        check_same("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1");
        check_same("rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2");
        check_same("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2");

        check_same("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2");
        check_same("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1");
        check_same("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1");
        check_same("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1");
        check_same("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
        check_same("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8");
    }

    #[test]
    fn square_algebraic_notation_parsing_and_stringify() {
        for (r, &rank) in RANKS.iter().enumerate() {
            for (f, &file) in FILES.iter().enumerate() {
                let algebraic = file.to_owned() + rank;
                let square = Square::from_algebraic(&algebraic);
                assert_eq!(square.rank(), r as u8);
                assert_eq!(square.file(), f as u8);
                assert_eq!(square.to_string(), algebraic);
            }
        }
    }

    fn expect_move_len(fen: &str, len: usize) {
        let board = Board::from_fen(fen).unwrap();
        let moves = board.moves();

        for mov in moves.iter() {
            println!("{mov}");
        }
        assert_eq!(moves.len(), len, "for fen `{fen}`");
    }

    #[test]
    fn move_generation_king() {
        expect_move_len("8/8/8/8/3K4/8/8/8 w - - 0 1", 8);
        expect_move_len("8/8/8/4p3/3KP3/8/8/8 w - - 0 1", 7);
        expect_move_len("8/8/3r4/2rPr3/2PKP3/8/8/8 w - - 0 1", 5);
        expect_move_len("8/8/8/8/8/8/8/K7 w - - 0 1", 3);

        expect_move_len("8/8/8/8/3k4/8/8/8 b - - 0 1", 8);
        expect_move_len("8/8/8/8/3kp3/4P3/8/8 b - - 0 1", 7);
        expect_move_len("8/8/8/2pkp3/2RpR3/3R4/8/8 b - - 0 1", 5);
        expect_move_len("8/8/8/8/8/8/8/k7 b - - 0 1", 3);
    }

    #[test]
    fn move_generation_castle() {
        expect_move_len("8/8/8/8/8/8/8/R3K2R w KQkq - 0 1", 26);
        expect_move_len("8/8/8/8/8/8/8/R3K2R w kq - 0 1", 24);
        expect_move_len("8/8/8/8/8/8/8/R3K2R w Kkq - 0 1", 25);
        expect_move_len("8/8/8/8/8/8/8/R3K2R w Qkq - 0 1", 25);

        expect_move_len("8/8/8/8/8/8/7p/R3K2R w KQkq - 0 1", 19);
        expect_move_len("8/8/8/8/8/8/7p/R3K2R w kq - 0 1", 18);
        expect_move_len("8/8/8/8/8/8/7p/R3K2R w Kkq - 0 1", 18);
        expect_move_len("8/8/8/8/8/8/7p/R3K2R w Qkq - 0 1", 19);

        expect_move_len("r3k2r/8/8/8/8/8/8/8 b KQkq - 0 1", 26);
        expect_move_len("r3k2r/8/8/8/8/8/8/8 b KQ - 0 1", 24);
        expect_move_len("r3k2r/8/8/8/8/8/8/8 b KQk - 0 1", 25);
        expect_move_len("r3k2r/8/8/8/8/8/8/8 b KQq - 0 1", 25);
    }

    #[test]
    fn move_generation_pawn() {
        expect_move_len("8/8/8/8/8/8/4P3/8 w - - 0 1", 2);
        expect_move_len("8/8/8/8/8/8/PPPPPPPP/8 w - - 0 1", 16);
        expect_move_len("8/8/8/8/8/4p3/4P3/8 w - - 0 1", 0);
        expect_move_len("8/8/8/8/4p3/8/4P3/8 b - - 0 1", 1);
        expect_move_len("8/8/8/8/8/3p1p2/4P3/8 w - - 0 1", 4);
        expect_move_len("8/2P5/3P4/4P3/5P2/6P1/7P/8 w - - 0 1", 10);
        expect_move_len("8/8/8/1Pp5/8/8/8/8 w - c6 0 1", 2);

        expect_move_len("8/4p3/8/8/8/8/8/8 b - - 0 1", 2);
        expect_move_len("8/pppppppp/8/8/8/8/8/8 b - - 0 1", 16);
        expect_move_len("8/5p2/5P2/8/8/8/8/8 b - - 0 1", 0);
        expect_move_len("8/5p2/8/5P2/8/8/8/8 b - - 0 1", 1);
        expect_move_len("8/3p4/2P1P3/8/8/8/8/8 b - - 0 1", 4);
        expect_move_len("8/1p6/2p5/3p4/4p3/5p2/6p1/8 b - - 0 1", 10);
        expect_move_len("8/8/8/8/5Pp1/8/8/8 b - f3 0 1", 2);
    }

    #[test]
    fn move_generation_knight() {
        expect_move_len("8/8/8/8/4N3/8/8/8 w - - 0 1", 8);
        expect_move_len("N7/8/8/8/8/8/8/8 w - - 0 1", 2);
        expect_move_len("8/8/2ppp3/2pNp3/2ppp3/8/8/8 w - - 0 1", 8);
        expect_move_len("8/8/8/8/4N3/2N5/8/8 w - - 0 1", 14);

        expect_move_len("8/8/8/8/4n3/8/8/8 b - - 0 1", 8);
        expect_move_len("n7/8/8/8/8/8/8/8 b - - 0 1", 2);
        expect_move_len("8/8/2PPP3/2PnP3/2PPP3/8/8/8 b - - 0 1", 8);
        expect_move_len("8/8/8/8/4n3/2n5/8/8 b - - 0 1", 14);
    }

    #[test]
    fn move_generation_rook() {
        expect_move_len("8/8/8/8/8/8/rr6/Rr6 w - - 0 1", 2);
        expect_move_len("8/8/8/8/8/8/RR6/rR6 b - - 0 1", 2);

        // no matter what, in a board with only one rook it will always only be able to move to 14 places
        let mut board = Board {
            pieces: [Piece::EMPTY; 64],
            ..Board::default()
        };

        for r in 0..8 {
            for f in 0..8 {
                *board.get_mut(r, f) = Piece::WHITE_ROOK;
                let moves = board.pseudolegal_moves();
                assert_eq!(moves.len(), 14);
                *board.get_mut(r, f) = Piece::EMPTY;
            }
        }
    }

    #[test]
    fn move_generation_bishop() {
        expect_move_len("8/8/8/8/8/8/8/B7 w - - 0 1", 7);
        expect_move_len("8/8/8/8/8/2r5/8/B7 w - - 0 1", 2);

        expect_move_len("8/8/8/8/8/8/8/b7 b - - 0 1", 7);
        expect_move_len("8/8/8/8/8/2R5/8/b7 b - - 0 1", 2);
    }

    // perft results sourced from: https://www.chessprogramming.org/Perft_Results#Initial_Position

    #[test]
    fn perft_default_fastish() {
        let board = Board::default();
        assert_eq!(board.perft(1, false), 20);
        assert_eq!(board.perft(2, false), 400);
        assert_eq!(board.perft(3, false), 8902);
        assert_eq!(board.perft(4, false), 197281);
    }

    #[ignore]
    #[test]
    fn perft_default_full() {
        let board = Board::default();
        assert_eq!(board.perft(5, false), 4865609);
        assert_eq!(board.perft(6, false), 119060324);
        // assert_eq!(board.perft(7, false), 3195901860);
        // assert_eq!(board.perft(8, false), 84998978956);
        // assert_eq!(board.perft(9, false), 2439530234167);
        // assert_eq!(board.perft(10, false), 69352859712417);
        // assert_eq!(board.perft(11, false), 2097651003696806);
        // assert_eq!(board.perft(12, false), 62854969236701747);
        // assert_eq!(board.perft(13, false), 1981066775000396239);
        // assert_eq!(board.perft(14, false), 61885021521585529237);
        // assert_eq!(board.perft(15, false), 2015099950053364471960);
    }

    #[test]
    fn perft_kiwipete_fastish() {
        let board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();
        assert_eq!(board.perft(1, false), 48);
        assert_eq!(board.perft(2, false), 2039);
        assert_eq!(board.perft(3, false), 97862);
    }

    #[ignore]
    #[test]
    fn perft_kiwipete_full() {
        let board =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .unwrap();

        assert_eq!(board.perft(4, false), 4085603);
        assert_eq!(board.perft(5, false), 193690690);
        // assert_eq!(board.perft(6, false), 8031647685);
    }

    #[test]
    fn perft_position_3_fastish() {
        let board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();

        assert_eq!(board.perft(1, false), 14);
        assert_eq!(board.perft(2, false), 191);
        assert_eq!(board.perft(3, false), 2812);
        assert_eq!(board.perft(4, false), 43238);
        assert_eq!(board.perft(5, false), 674624);
        assert_eq!(board.perft(6, false), 11030083);
    }

    #[ignore]
    #[test]
    fn perft_position_3_full() {
        let board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();

        assert_eq!(board.perft(7, false), 178633661);
        // assert_eq!(board.perft(8, false), 3009794393);
    }

    #[test]
    fn perft_position_4_fastish() {
        let board =
            Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .unwrap();
        assert_eq!(board.perft(1, false), 6);
        assert_eq!(board.perft(2, false), 264);
        assert_eq!(board.perft(3, false), 9467);
        assert_eq!(board.perft(4, false), 422333);
        assert_eq!(board.perft(5, false), 15833292);
    }

    #[ignore]
    #[test]
    fn perft_position_4_full() {
        let board =
            Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .unwrap();

        assert_eq!(board.perft(6, false), 706045033);
    }

    #[test]
    fn perft_position_5_fastish() {
        let board =
            Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
        assert_eq!(board.perft(1, false), 44);
        assert_eq!(board.perft(2, false), 1486);
        assert_eq!(board.perft(3, false), 62379);
        assert_eq!(board.perft(4, false), 2103487);
        assert_eq!(board.perft(5, false), 89941194);
    }

    #[test]
    fn perft_position_6_fastish() {
        let board = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .unwrap();
        assert_eq!(board.perft(1, false), 46);
        assert_eq!(board.perft(2, false), 2079);
        assert_eq!(board.perft(3, false), 89890);
        assert_eq!(board.perft(4, false), 3894594);
        assert_eq!(board.perft(5, false), 164075551);
    }

    //NOTE: this is too slow to run on my machine in a reasonable amount of time with current performance
    // #[ignore]
    // #[test]
    // fn perft_position_6_full() {
    //     let board = Board::from_fen(
    //         "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
    //     );
    //     assert_eq!(board.perft(6, false), 6923051137);
    //     // assert_eq!(board.perft(7, false), 287188994746);
    //     // assert_eq!(board.perft(8, false), 11923589843526);
    //     // assert_eq!(board.perft(9, false), 490154852788714);
    // }
}
