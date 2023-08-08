use eframe::epaint::ahash::HashMap;

use crate::process::Process;

pub struct HumanPlayer {}

pub struct UciOptionSpin {
    pub value: i64,
    pub min: i64,
    pub max: i64,
}

pub enum UciOption {
    Check(bool),
    Spin(UciOptionSpin),
    Combo,
    Button,
    String(String),
}

pub enum UciState {
    Booted,
    UciOk,
    Ready,
}

pub struct UciPlayer {
    pub state: UciState,
    pub process: Process,
    pub id_name: String,
    pub id_author: String,
    pub stdout: Vec<String>,
    pub stderr: Vec<String>,
    pub options: HashMap<String, UciOption>,
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
            "option" => {
                let mut name = None;
                let mut option = None;

                let get_value =
                    |param_iter: &mut std::iter::Peekable<std::str::SplitWhitespace<'_>>| {
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

                while let Some(parameter_kind) = param_iter.next() {
                    match parameter_kind {
                        "name" => {
                            name = Some(get_value(&mut param_iter));
                            //TODO: predefined names
                        }
                        "type" => {
                            let ty = param_iter.next().unwrap();
                            option = Some(match ty {
                                "check" => UciOption::Check(false),
                                "spin" => UciOption::Spin(UciOptionSpin {
                                    value: 0,
                                    min: 0,
                                    max: 0,
                                }),
                                "combo" => UciOption::Combo,
                                "button" => UciOption::Button,
                                "string" => UciOption::String("value".to_owned()),
                                _ => {
                                    println!("unknown option type {ty}");
                                    break;
                                }
                            });
                        }
                        "default" => {
                            let val = get_value(&mut param_iter);
                            match &mut option {
                                Some(UciOption::Check(opt)) => *opt = val.parse().unwrap(),
                                Some(UciOption::Spin(opt)) => opt.value = val.parse().unwrap(),
                                Some(UciOption::String(opt)) => *opt = val.to_owned(),
                                _ => {
                                    println!("default value called on option type without default");
                                }
                            }
                        }
                        "min" => {
                            let val = get_value(&mut param_iter);
                            match &mut option {
                                Some(UciOption::Spin(opt)) => opt.min = val.parse().unwrap(),
                                _ => {
                                    println!("min value called on option type without min");
                                }
                            }
                        }
                        "max" => {
                            let val = get_value(&mut param_iter);
                            match &mut option {
                                Some(UciOption::Spin(opt)) => opt.max = val.parse().unwrap(),
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
                self.options
                    .insert(name.unwrap_or("unknown option".to_owned()), option.unwrap());
            }
            "" => {}
            _ => println!("unknown uci command `{line}`"),
        }
    }
}

pub enum Player {
    Human(HumanPlayer),
    Uci(UciPlayer),
}

impl Player {
    pub fn new_human() -> Self {
        Self::Human(HumanPlayer {})
    }

    pub fn is_human(&self) -> bool {
        matches!(self, Player::Human(_))
    }

    pub fn new_uci() -> Self {
        let process = Process::new("stockfish");
        process.send_stdin_line("uci");
        Self::Uci(UciPlayer {
            state: UciState::Booted,
            id_name: "".to_owned(),
            id_author: "".to_owned(),
            process,
            stderr: vec![],
            stdout: vec![],
            options: HashMap::default(),
        })
    }

    pub fn tick(&mut self) {
        match self {
            Player::Human(_) => {}
            Player::Uci(engine) => {
                while let Some(line) = engine.process.get_stdout_line() {
                    engine.process_uci_command(&line);
                    engine.stdout.push(line);
                }

                while let Some(line) = engine.process.get_stderr_line() {
                    engine.stderr.push(line);
                }
            }
        }
    }
}
