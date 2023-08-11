use bitflags::bitflags;
use serde::{Deserialize, Serialize};

use crate::{board::Move, process::Process};

pub struct HumanPlayer {
    pub played_move: Option<Move>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UciOptionSpin {
    pub value: i64,
    pub min: i64,
    pub max: i64,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UciOptionData {
    Check(bool),
    Spin(UciOptionSpin),
    Combo,
    Button,
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UciOption {
    pub name: String,
    pub data: UciOptionData,
}

impl UciOption {
    /// Creates a uci option from line, the line should be the raw line gotten from a uci command with the option prefix.
    #[inline]
    pub fn new_from_uci_params(line: &str) -> Option<Self> {
        let (command, params) = line.split_once(' ')?;
        if command != "option" {
            return None;
        }

        let mut name = None;
        let mut option = None;

        let get_value = |param_iter: &mut std::iter::Peekable<std::str::SplitWhitespace<'_>>| {
            let mut out = "".to_owned();
            while let Some(peeked) = param_iter.peek() {
                if ["name", "type", "default", "min", "max"].contains(peeked) {
                    break;
                }

                out += " ";
                out += param_iter.next().unwrap();
            }
            out.trim().to_owned()
        };

        let mut param_iter = params.split_whitespace().peekable();

        while let Some(parameter_kind) = param_iter.next() {
            match parameter_kind {
                "name" => {
                    name = Some(get_value(&mut param_iter));
                    //TODO: predefined names
                }
                "type" => {
                    let ty = param_iter.next().unwrap();
                    option = Some(match ty {
                        "check" => UciOptionData::Check(false),
                        "spin" => UciOptionData::Spin(UciOptionSpin {
                            value: 0,
                            min: 0,
                            max: 0,
                        }),
                        "combo" => unimplemented!(),
                        "button" => UciOptionData::Button,
                        "string" => UciOptionData::String("value".to_owned()),
                        _ => {
                            println!("unknown option type {ty}");
                            return None;
                        }
                    });
                }
                "default" => {
                    let val = get_value(&mut param_iter);
                    match &mut option {
                        Some(UciOptionData::Check(opt)) => *opt = val.parse().ok()?,
                        Some(UciOptionData::Spin(opt)) => opt.value = val.parse().ok()?,
                        Some(UciOptionData::String(opt)) => *opt = val.to_owned(),
                        _ => {
                            println!("default value called on option type without default");
                        }
                    }
                }
                "min" => {
                    let val = get_value(&mut param_iter);
                    match &mut option {
                        Some(UciOptionData::Spin(opt)) => opt.min = val.parse().ok()?,
                        _ => {
                            println!("min value called on option type without min");
                        }
                    }
                }
                "max" => {
                    let val = get_value(&mut param_iter);
                    match &mut option {
                        Some(UciOptionData::Spin(opt)) => opt.max = val.parse().ok()?,
                        _ => {
                            println!("max value called on option type without max");
                        }
                    }
                }
                _ => {
                    println!("unknown uci command option `{line}`");
                }
            }
        }

        option.map(|data| UciOption {
            name: name.unwrap_or("unknown option".to_owned()),
            data,
        })
    }

    pub fn to_uci_value_string(&self) -> String {
        match &self.data {
            UciOptionData::Button => format!("setoption name {}", self.name),
            UciOptionData::Spin(val) => format!("setoption name {} value {}", self.name, val.value),
            UciOptionData::String(val) => format!("setoption name {} value {val}", self.name),
            UciOptionData::Check(val) => format!("setoption name {} value {val}", self.name),
            UciOptionData::Combo => unimplemented!(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UciState {
    Booted,
    UciOk,
    ReadyOk,
    InGame,
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
    pub struct IoKind: u8 {
        const Stdout = 0b001;
        const Stdin = 0b010;
        const Stderr = 0b100;
    }
}

impl IoKind {
    pub fn to_prefix(self) -> &'static str {
        match self {
            IoKind::Stdout => "<<<",
            IoKind::Stdin => ">>>",
            IoKind::Stderr => "<!<",
            _ => "??",
        }
    }
}

pub struct IoLine {
    pub kind: IoKind,
    pub line: String,
}

pub struct UciPlayer {
    pub state: UciState,
    process: Process,
    pub id_name: String,
    pub id_author: String,
    pub io: Vec<IoLine>,
    pub options: Vec<UciOption>,
    pub best_move: Option<Move>,
}

impl UciPlayer {
    fn process_uci_command(&mut self, line: &str) {
        let (command, params) = line.split_once(' ').unwrap_or((line, ""));
        let mut param_iter = params.split_whitespace().peekable();

        match command {
            "id" => match param_iter.next() {
                Some("name") => {
                    self.id_name = param_iter.next().unwrap().to_owned();
                }
                Some("author") => {
                    self.id_author = param_iter.next().unwrap().to_owned();
                }
                _ => println!("unknown uci command {command}"),
            },
            "uciok" => self.state = UciState::UciOk,
            "readyok" => self.state = UciState::ReadyOk,
            "bestmove" => {
                let mov = param_iter.next().unwrap_or("0000");
                let mov = Move::from_long_algebraic(mov);
                //TODO: ponder
                self.best_move = Some(mov);
            }
            "info" => {
                //TODO: just putting this here to remove the messages
            }
            "option" => {
                if let Some(option) = UciOption::new_from_uci_params(line) {
                    self.options.push(option);
                } else {
                    println!("could not create option from uci command {line}")
                }
            }
            "" => {}
            _ => println!("unknown uci command `{line}`"),
        }
    }

    pub fn update_option(&mut self, idx: usize) {
        self.send_stdin_line(self.options[idx].to_uci_value_string());
    }

    pub fn send_stdin_line(&mut self, line: String) {
        self.process.send_stdin_line(&line);

        self.io.push(IoLine {
            kind: IoKind::Stdin,
            line,
        });
    }

    fn tick(&mut self) {
        while let Some(line) = self.process.get_stdout_line() {
            self.process_uci_command(&line);

            self.io.push(IoLine {
                kind: IoKind::Stdout,
                line,
            });
        }

        while let Some(line) = self.process.get_stderr_line() {
            self.io.push(IoLine {
                kind: IoKind::Stderr,
                line,
            });
        }
    }
}

pub enum Player {
    Human(HumanPlayer),
    Uci(UciPlayer),
}

impl Player {
    pub fn new_human() -> Self {
        Self::Human(HumanPlayer { played_move: None })
    }

    pub fn is_human(&self) -> bool {
        matches!(self, Player::Human(_))
    }

    pub fn new_uci() -> Self {
        let process = Process::new("stockfish");

        let mut player = UciPlayer {
            state: UciState::Booted,
            best_move: None,
            id_name: "".to_owned(),
            id_author: "".to_owned(),
            process,
            io: vec![],

            options: vec![],
        };

        player.send_stdin_line("uci".to_owned());

        Self::Uci(player)
    }

    pub fn tick(&mut self) {
        match self {
            Player::Human(_) => {}
            Player::Uci(engine) => {
                engine.tick();
            }
        }
    }

    pub fn is_ready(&self) -> bool {
        match self {
            Player::Human(_) => true,
            Player::Uci(engine) => engine.state == UciState::ReadyOk,
        }
    }

    pub fn is_in_game(&self) -> bool {
        match self {
            Player::Human(_) => true,
            Player::Uci(engine) => engine.state == UciState::InGame,
        }
    }

    pub fn ready(&mut self) {
        match self {
            Player::Human(_) => {}
            Player::Uci(engine) => {
                engine.send_stdin_line("isready".to_owned());
            }
        }
    }

    pub fn start_new_game(&mut self) {
        match self {
            Player::Human(_) => {}
            Player::Uci(engine) => {
                engine.send_stdin_line("ucinewgame".to_owned());
                engine.state = UciState::InGame;
            }
        }
    }

    pub fn request_move(&mut self) {
        match self {
            Player::Human(_) => {}
            Player::Uci(engine) => {
                engine.send_stdin_line("go wtime 1000 btime 1000 winc 0 binc 0".to_owned());
            }
        }
    }

    pub fn get_move(&self) -> Option<Move> {
        match self {
            Player::Human(human) => human.played_move,
            Player::Uci(engine) => engine.best_move,
        }
    }

    pub fn take_move(&mut self) -> Option<Move> {
        match self {
            Player::Human(human) => human.played_move.take(),
            Player::Uci(engine) => engine.best_move.take(),
        }
    }

    pub fn next_move(&mut self, past_moves: &[String]) {
        match self {
            Player::Human(_) => {}
            Player::Uci(engine) => {
                engine.send_stdin_line(format!("position startpos moves {}", past_moves.join(" ")))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn uci_option_parse() {
        assert_eq!(
            UciOption::new_from_uci_params("option name Clear Hash type button"),
            Some(UciOption {
                name: "Clear Hash".to_owned(),
                data: UciOptionData::Button
            })
        );

        assert_eq!(
            UciOption::new_from_uci_params("option name Nullmove type check default true"),
            Some(UciOption {
                name: "Nullmove".to_owned(),
                data: UciOptionData::Check(true)
            })
        );

        assert_eq!(
            UciOption::new_from_uci_params(
                "option name Selectivity type spin default 2 min 0 max 4"
            ),
            Some(UciOption {
                name: "Selectivity".to_owned(),
                data: UciOptionData::Spin(UciOptionSpin {
                    value: 2,
                    min: 0,
                    max: 4
                })
            })
        );

        assert_eq!(
            UciOption::new_from_uci_params("option name NalimovPath type string default c:\\"),
            Some(UciOption {
                name: "NalimovPath".to_owned(),
                data: UciOptionData::String("c:\\".to_owned())
            })
        );
    }
}
