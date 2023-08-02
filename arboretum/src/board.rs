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

    pub fn rook(color: Color) -> Self {
        match color {
            Color::Black => Self::BLACK_ROOK,
            Color::White => Self::WHITE_ROOK,
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

    pub fn contains(self, other: Self) -> bool {
        (self & other) == other
    }

    pub fn kingside_color(self, color: Color) -> bool {
        match color {
            Color::Black => self.contains(Self::BLACK_KINGSIDE),
            Color::White => self.contains(Self::WHITE_KINGSIDE),
        }
    }

    pub fn queenside_color(self, color: Color) -> bool {
        match color {
            Color::Black => self.contains(Self::BLACK_QUEENSIDE),
            Color::White => self.contains(Self::WHITE_QUEENSIDE),
        }
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

    pub fn from(self) -> Square {
        Square::new((self.0 >> 10 & 0b111111) as u8)
    }

    pub fn to(self) -> Square {
        Square::new((self.0 >> 4 & 0b111111) as u8)
    }

    pub fn flags(self) -> MoveFlags {
        MoveFlags((self.0 & 0b1111) as u8)
    }
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&(self.from().to_string() + " -> " + &self.to().to_string()))
    }
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

    fn extend_pseudo_legal_king_moves_at(&self, pseudo_legal: &mut Vec<Move>, from_square: Square) {
        // basic king movement
        for r in -1..=1 {
            for f in -1..=1 {
                if let Some(to) = from_square.mov(r, f) {
                    if !self.pieces[to.0 as usize].is_color(self.active_color) {
                        pseudo_legal.push(Move::new(from_square, to));
                    }
                }
            }
        }

        // castling
        if self.castling_availability.kingside_color(self.active_color)
            && self
                .active_color
                .kingside_castle_path()
                .into_iter()
                .all(|square| self.at(square).is_empty())
        {
            pseudo_legal.push(Move::new_with_flags(
                from_square,
                self.active_color.kingside_castle_position_king(),
                MoveFlags::CASTLE_KINGSIDE,
            ))
        }

        if self
            .castling_availability
            .queenside_color(self.active_color)
            && self
                .active_color
                .queenside_castle_path()
                .into_iter()
                .all(|square| self.at(square).is_empty())
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
                            && (self.pieces[to.0 as usize].is_color(self.active_color.opponent())
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
                if !self.pieces[to.0 as usize].is_color(self.active_color) {
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
                    if self.pieces[to.0 as usize].is_color(self.active_color.opponent()) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
    }

    pub fn moves(&self) -> Vec<Move> {
        let mut pseudo_legal = vec![];

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

        // handle en passant
        if mov.flags().is_en_passant() {
            *ret.at_mut(
                self.en_passant_target
                    .mov(self.active_color.opponent().pawn_move_direction(), 0)
                    .unwrap(),
            ) = Piece::EMPTY;
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
        let board = Board::from_fen(fen);
        let moves = board.moves();
        println!("---");

        for mov in moves.iter() {
            println!("{mov}");
        }
        assert_eq!(moves.len(), len, "for fen `{fen}`");
    }

    #[test]
    fn move_generation_king() {
        expect_move_len("8/8/8/8/3K4/8/8/8 w - - 0 1", 8);
        expect_move_len("8/8/8/4r3/3KP3/8/8/8 w - - 0 1", 7);
        expect_move_len("8/8/3r4/2rPr3/2PKP3/8/8/8 w - - 0 1", 5);
        expect_move_len("8/8/8/8/8/8/8/K7 w - - 0 1", 3);

        expect_move_len("8/8/8/8/3k4/8/8/8 b - - 0 1", 8);
        expect_move_len("8/8/8/8/3kp3/4R3/8/8 b - - 0 1", 7);
        expect_move_len("8/8/8/2pkp3/2RpR3/3R4/8/8 b - - 0 1", 5);
        expect_move_len("8/8/8/8/8/8/8/k7 b - - 0 1", 3);
    }

    #[test]
    fn move_generation_castle() {
        expect_move_len("8/8/8/8/8/8/8/R3K2R w KQkq - 0 1", 26);
        expect_move_len("8/8/8/8/8/8/8/R3K2R w kq - 0 1", 24);
        expect_move_len("8/8/8/8/8/8/8/R3K2R w Kkq - 0 1", 25);
        expect_move_len("8/8/8/8/8/8/8/R3K2R w Qkq - 0 1", 25);

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

        // no matter what in a board with only one rook it will always only be able to move to 14 places
        let mut board = Board {
            pieces: [Piece::EMPTY; 64],
            ..Board::default()
        };

        for r in 0..8 {
            for f in 0..8 {
                *board.get_mut(r, f) = Piece::WHITE_ROOK;
                let moves = board.moves();
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
}
