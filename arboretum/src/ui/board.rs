use eframe::{
    egui::{Painter, Ui, WidgetText},
    emath::Align2,
    epaint::{Color32, FontFamily, FontId, Pos2, Rect, Stroke, Vec2},
};

use crate::AppData;

use super::PanelT;

pub struct BoardPanel {}

impl PanelT for BoardPanel {
    fn update(_app_data: &mut AppData, ui: &mut Ui) {
        let clip_rect = ui.available_rect_before_wrap();
        let painter = Painter::new(ui.ctx().clone(), ui.layer_id(), clip_rect);

        let size = clip_rect.size().min_elem() / 8.0;
        for rank in (0..8).rev() {
            for file in 0..8 {
                let min = Pos2::new(rank as f32 * size, file as f32 * size)
                    + ui.next_widget_position().to_vec2();
                let max = min + Vec2::new(size, size);
                painter.rect_filled(
                    Rect::from_two_pos(min, max),
                    0.0,
                    if (rank + file) % 2 == 0 {
                        Color32::WHITE
                    } else {
                        Color32::BLACK
                    },
                );

                if file == 7 {
                    let ranks = ["A", "B", "C", "D", "E", "F", "G", "H"];

                    painter.text(
                        max,
                        Align2::RIGHT_BOTTOM,
                        ranks[rank],
                        FontId::new(size / 4.0, FontFamily::Proportional),
                        if (rank + file) % 2 == 1 {
                            Color32::WHITE
                        } else {
                            Color32::BLACK
                        },
                    );
                }
                if rank == 0 {
                    let ranks = ["1", "2", "3", "4", "5", "6", "7", "8"];

                    painter.text(
                        min,
                        Align2::LEFT_TOP,
                        ranks[file],
                        FontId::new(size / 4.0, FontFamily::Proportional),
                        if (rank + file) % 2 == 1 {
                            Color32::WHITE
                        } else {
                            Color32::BLACK
                        },
                    );
                }
            }
        }

        ui.expand_to_include_rect(painter.clip_rect());
    }

    fn on_close(_app_data: &mut AppData) {
        println!("closing board panel")
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Board".into()
    }
}
