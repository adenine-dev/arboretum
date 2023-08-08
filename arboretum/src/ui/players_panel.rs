use eframe::{
    egui::{self, Id, Ui, WidgetText},
    epaint::Vec2,
};

use crate::{
    board::Color,
    player::{IoKind, Player, UciOptionData},
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
                ui.collapsing("I/O", |ui| {
                    let id = Id::new("I/O MASK").with(color);
                    let mut kind_mask = ui
                        .data_mut(|data| data.get_persisted(id))
                        .unwrap_or(IoKind::all());

                    ui.horizontal(|ui| {
                        for (kind, name) in [
                            (IoKind::Stdin, "stdin"),
                            (IoKind::Stdout, "stdout"),
                            (IoKind::Stderr, "stderr"),
                        ] {
                            let mut res = kind_mask.contains(kind);
                            ui.checkbox(&mut res, name);
                            kind_mask.set(kind, res);
                        }
                    });
                    ui.data_mut(|data| data.insert_persisted(id, kind_mask));

                    egui::ScrollArea::vertical()
                        .max_height(300.0)
                        .drag_to_scroll(false)
                        .auto_shrink([true; 2])
                        .show(ui, |ui| {
                            ui.add_sized(
                                Vec2::new(ui.available_width(), 0.0),
                                egui::TextEdit::multiline(&mut engine.io.iter().fold(
                                    "".to_owned(),
                                    |acc, line| {
                                        if kind_mask.contains(line.kind) {
                                            format!(
                                                "{acc}{} {}\n",
                                                line.kind.to_prefix(),
                                                line.line
                                            )
                                        } else {
                                            acc
                                        }
                                    },
                                ))
                                .interactive(false)
                                .code_editor()
                                .lock_focus(true),
                            );
                        });
                });

                ui.collapsing("Options", |ui| {
                    let mut updated_option = None;
                    egui::Grid::new("my_grid")
                        .num_columns(2)
                        .striped(true)
                        .show(ui, |ui| {
                            for (idx, option) in engine.options.iter_mut().enumerate() {
                                let name = &option.name;
                                match &mut option.data {
                                    UciOptionData::Button => {
                                        if ui.button(name).clicked() {
                                            updated_option = Some(idx);
                                        }
                                    }
                                    UciOptionData::Check(val) => {
                                        ui.label(name);
                                        if ui.checkbox(val, "").changed() {
                                            updated_option = Some(idx);
                                        }
                                    }
                                    UciOptionData::Combo => {
                                        ui.label(name);
                                        unimplemented!();
                                    }
                                    UciOptionData::Spin(spin) => {
                                        ui.label(name);
                                        if ui
                                            .add(egui::Slider::new(
                                                &mut spin.value,
                                                spin.min..=spin.max,
                                            ))
                                            .drag_released()
                                        {
                                            updated_option = Some(idx);
                                        }
                                    }
                                    UciOptionData::String(val) => {
                                        ui.label(name);
                                        if ui.text_edit_singleline(val).lost_focus() {
                                            updated_option = Some(idx);
                                        }
                                    }
                                }
                                ui.end_row();
                            }
                        });

                    if let Some(idx) = updated_option {
                        engine.update_option(idx)
                    }
                });
            }
        },
    );
}
