use core::panic;
use std::io::{BufRead, BufReader, Read};

use eframe::{
    egui::{self, Ui, WidgetText},
    epaint::Vec2,
};

use crate::{
    board::Color,
    player::{Player, UciOption},
    AppData,
};

use super::PanelT;
pub struct PlayersPanel {}

impl PanelT for PlayersPanel {
    fn update(app_data: &mut AppData, ui: &mut Ui) {
        display_player(Color::Black, app_data, ui);
        display_player(Color::White, app_data, ui);
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Players".into()
    }
}

fn display_player(color: Color, app_data: &mut AppData, ui: &mut Ui) {
    let player = app_data.context.get_player_mut(color);

    ui.collapsing(
        match color {
            Color::Black => "Black",
            Color::White => "White",
        },
        |ui| match player {
            Player::Human(_human) => {
                ui.label("human player");
            }
            Player::Uci(engine) => {
                if ui.button("uci").clicked() {
                    //
                    engine.process.send_stdin_line("uci");
                }

                ui.add_sized(
                    Vec2::new(ui.available_width(), 0.0),
                    egui::TextEdit::multiline(&mut engine.stdout.join("\n"))
                        .interactive(false)
                        // .font(egui::TextStyle::Monospace)
                        .code_editor()
                        .lock_focus(true),
                );

                for (name, ty) in engine.options.iter_mut() {
                    ui.horizontal(|ui| {
                        //
                        match ty {
                            UciOption::Button => {
                                ui.button(name);
                            }
                            UciOption::Check(val) => {
                                ui.label(name);
                                ui.checkbox(val, "");
                            }
                            UciOption::Combo => {
                                ui.label(name);
                                ui.label("combo box goes here...");
                            }
                            UciOption::Spin(spin) => {
                                ui.label(name);
                                ui.add(
                                    egui::DragValue::new(&mut spin.value)
                                        .clamp_range(spin.min..=spin.max),
                                );
                            }
                            UciOption::String(val) => {
                                ui.label(name);
                                ui.text_edit_singleline(val);
                            }
                        }
                    });
                }

                // ui.code();
            }
        },
    );
}
