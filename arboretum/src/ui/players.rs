use eframe::egui::{Ui, WidgetText};

use crate::AppData;

use super::PanelT;
pub struct PlayersPanel {}

impl PanelT for PlayersPanel {
    fn update(app_data: &mut AppData, ui: &mut Ui) {
        ui.label(&app_data.context.player_1);
        ui.label("vs");
        ui.label(&app_data.context.player_2);
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Players".into()
    }
}
