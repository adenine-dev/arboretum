use eframe::egui;
use egui::{
    color_picker::{color_edit_button_srgba, Alpha},
    Ui, WidgetText,
};

use crate::AppData;

use super::PanelT;
pub struct StyleEditorPanel {}

impl PanelT for StyleEditorPanel {
    fn update(app_data: &mut AppData, ui: &mut Ui) {
        ui.heading("Style Editor");
        ui.collapsing("Board", |ui| {
            egui::Grid::new("board").show(ui, |ui| {
                ui.label("dark squares:");
                color_edit_button_srgba(
                    ui,
                    &mut app_data.context.theme.dark_square_color,
                    Alpha::OnlyBlend,
                );
                ui.end_row();

                ui.label("light squares:");
                color_edit_button_srgba(
                    ui,
                    &mut app_data.context.theme.light_square_color,
                    Alpha::OnlyBlend,
                );
                ui.end_row();

                ui.label("black pieces:");
                color_edit_button_srgba(
                    ui,
                    &mut app_data.context.theme.black_piece_color,
                    Alpha::OnlyBlend,
                );
                ui.end_row();

                ui.label("white pieces:");
                color_edit_button_srgba(
                    ui,
                    &mut app_data.context.theme.white_piece_color,
                    Alpha::OnlyBlend,
                );
                ui.end_row();
            });
        });

        ui.checkbox(&mut app_data.context.white_on_bottom, "white on bottom");
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Style Editor".into()
    }
}
