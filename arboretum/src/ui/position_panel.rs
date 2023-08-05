use eframe::egui::{self, Ui, WidgetText};
use egui_toast::{Toast, ToastKind, ToastOptions};

use crate::AppData;

use super::{Panel, PanelT};
pub struct PositionPanel {}

impl PanelT for PositionPanel {
    fn update(app_data: &mut AppData, ui: &mut Ui) {
        ui.horizontal(|ui| {
            ui.strong("fen:");
            if app_data.tabs.super_focused == Some(Panel::Position) {
                let response = ui.add(
                    egui::TextEdit::singleline(&mut app_data.context.current_fen)
                        .hint_text("fen")
                        .clip_text(false)
                        .desired_width(300.0),
                );

                if (response.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)))
                    || ui.button("✔").on_hover_text_at_pointer("done").clicked()
                {
                    if let Err(err) = app_data
                        .context
                        .set_fen(&app_data.context.current_fen.clone())
                    {
                        app_data.tabs.toasts.add(Toast {
                            text: err.to_string().into(),
                            kind: ToastKind::Error,
                            options: ToastOptions::default()
                                .duration_in_seconds(3.0)
                                .show_progress(false)
                                .show_icon(true),
                        });
                    } else {
                        app_data.tabs.super_focused = None;
                    }
                }

                if ui.input(|i| i.key_pressed(egui::Key::Escape))
                    || ui.button("✖").on_hover_text_at_pointer("cancel").clicked()
                {
                    app_data.context.current_fen = app_data.context.board.make_fen();
                    app_data.tabs.super_focused = None;
                }

                if app_data.tabs.super_focused.is_none() {
                    app_data.tabs.clear_toasts(ui);
                } else {
                    // this is the only thing that matters so we just continually refocus it
                    response.request_focus();
                }
            } else {
                let fen = app_data.context.board.make_fen();
                if ui
                    .label(&fen)
                    .on_hover_text_at_pointer("click to copy")
                    .on_hover_cursor(egui::CursorIcon::PointingHand)
                    .clicked()
                {
                    ui.output_mut(|o| o.copied_text = fen);
                }

                if ui
                    .button("✏")
                    .on_hover_text_at_pointer("edit fen")
                    .clicked()
                {
                    app_data.tabs.super_focused = app_data
                        .tabs
                        .super_focused
                        .map_or(Some(Panel::Position), |_| None);
                }
            }

            ui.end_row()
        });
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Position".into()
    }
}
