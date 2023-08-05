use eframe::egui::{Ui, WidgetText};

use crate::AppData;

use super::PanelT;
pub struct PlayersPanel {}

impl PanelT for PlayersPanel {
    fn update(app_data: &mut AppData, ui: &mut Ui) {
        //
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Players".into()
    }
}
