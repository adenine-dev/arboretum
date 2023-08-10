use eframe::{
    egui::{self, Id, TextFormat, Ui, WidgetText},
    epaint::{
        text::{LayoutJob, LayoutSection, TextWrapping},
        Color32, FontId, Vec2,
    },
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
    egui::CollapsingHeader::new(match color {
        Color::Black => "Black",
        Color::White => "White",
    })
    .show_unindented(ui, |ui| {
        // ui.collapsing(
        //     match color {
        //         Color::Black => "Black",
        //         Color::White => "White",
        //     },
        // |ui| {
        //
        match player {
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
                        .stick_to_bottom(true)
                        .show(ui, |ui| {
                            let mut layouter = |ui: &Ui, text: &str, wrap_width: f32| {
                                let mut job = LayoutJob {
                                    text: text.into(),
                                    wrap: TextWrapping {
                                        max_width: wrap_width,
                                        ..Default::default()
                                    },
                                    ..Default::default()
                                };

                                for line in text.split_inclusive(|x| x == '\n') {
                                    let (mark, _) = line.split_once(' ').unwrap();
                                    let color = match mark {
                                        "<<<" => Color32::from_rgb(0xe1, 0xca, 0xff),
                                        ">>>" => Color32::from_rgb(0x98, 0x6e, 0xa9),
                                        "<!<" => Color32::from_rgb(126, 126, 221),
                                        _ => Color32::RED,
                                    };

                                    let start = line.as_ptr() as usize - text.as_ptr() as usize;
                                    let line_len = line.as_bytes().len();
                                    job.sections.push(LayoutSection {
                                        leading_space: 0.0,
                                        byte_range: start..start + line_len,
                                        format: TextFormat {
                                            font_id: egui::FontId::monospace(14.0),
                                            color,
                                            ..Default::default()
                                        },
                                    });
                                }

                                ui.fonts(|f| f.layout_job(job))
                            };

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
                                .layouter(&mut layouter)
                                .interactive(false)
                                .code_editor()
                                .lock_focus(true),
                            );
                        });

                    let id = Id::new("command text").with(color);
                    let mut command_text = ui
                        .data_mut(|data| data.get_temp(id))
                        .unwrap_or("".to_owned());

                    if ui.text_edit_singleline(&mut command_text).lost_focus()
                        && ui.input(|i| i.key_down(egui::Key::Enter))
                    {
                        engine.send_stdin_line(command_text.clone());
                        command_text = "".to_owned();
                    }
                    ui.data_mut(|data| data.insert_temp(id, command_text));
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
        }
    });
}
