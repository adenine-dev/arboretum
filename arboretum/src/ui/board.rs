use eframe::egui::{Ui, WidgetText};

use crate::AppData;

use super::PanelT;

pub struct BoardPanel {}

impl PanelT for BoardPanel {
    fn update(_app_data: &mut AppData, ui: &mut Ui) {
        ui.label("board goes here");
    }

    fn on_close(_app_data: &mut AppData) {
        println!("closing board panel")
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Board".into()
    }
}
