use core::panic;
use std::io::{BufRead, BufReader, Read};

use eframe::{
    egui::{self, Ui, WidgetText},
    epaint::Vec2,
};

use crate::{board::Color, player::Player, AppData};

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
                while let Some(line) = engine.process.get_stdout_line() {
                    engine.stdout.push(line);
                }
                ui.add_sized(
                    Vec2::new(ui.available_width(), 0.0),
                    egui::TextEdit::multiline(&mut engine.stdout.join("\n"))
                        .interactive(false)
                        // .font(egui::TextStyle::Monospace)
                        .code_editor()
                        .lock_focus(true),
                );

                // ui.code();
            }
        },
    );
}
