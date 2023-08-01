use eframe::{
    egui::{Painter, Ui, WidgetText},
    emath::Align2,
    epaint::{FontFamily, FontId, Pos2, Rect, Vec2},
};

use crate::AppData;

use super::PanelT;

pub struct BoardPanel {}

impl PanelT for BoardPanel {
    fn update(app_data: &mut AppData, ui: &mut Ui) {
        let white_on_bottom = app_data.context.white_on_bottom;

        let clip_rect = ui.available_rect_before_wrap();
        let painter = Painter::new(ui.ctx().clone(), ui.layer_id(), clip_rect);

        let size = clip_rect.size().min_elem() / 8.0;
        let origin = ui.next_widget_position().to_vec2();
        let rank_file_indicator_rank_file = if white_on_bottom { 0 } else { 7 };
        for rank in 0..8 {
            for file in 0..8 {
                // get colors
                let bg = app_data.context.theme.dark_square_color;
                let fg = app_data.context.theme.light_square_color;
                let (bg, fg) = if (rank + file) % 2 == 1 {
                    (fg, bg)
                } else {
                    (bg, fg)
                };

                // paint rect
                let min = {
                    let mut min = Pos2::new((7 - file) as f32 * size, rank as f32 * size);
                    if white_on_bottom {
                        min.y = 7.0 * size - min.y;
                        min.x = 7.0 * size - min.x;
                    }
                    min + origin
                };

                let max = min + Vec2::splat(size);
                painter.rect_filled(Rect::from_two_pos(min, max), 0.0, bg);

                // paint piece
                let piece = app_data.context.board.get(rank as u8, file as u8);
                if !piece.is_empty() {
                    painter.text(
                        min + Vec2::splat(size) / 2.0,
                        Align2::CENTER_CENTER,
                        piece.to_black_figurine(),
                        FontId::new(size * 0.66, FontFamily::Proportional),
                        if piece.is_black() {
                            app_data.context.theme.black_piece_color
                        } else {
                            app_data.context.theme.white_piece_color
                        },
                    );
                }

                // paint rank/file indicators
                if rank == rank_file_indicator_rank_file {
                    let files = ["A", "B", "C", "D", "E", "F", "G", "H"];

                    painter.text(
                        max,
                        Align2::RIGHT_BOTTOM,
                        files[file],
                        FontId::new(size / 4.0, FontFamily::Proportional),
                        fg,
                    );
                }
                if file == rank_file_indicator_rank_file {
                    let ranks = ["1", "2", "3", "4", "5", "6", "7", "8"];

                    painter.text(
                        min,
                        Align2::LEFT_TOP,
                        ranks[rank],
                        FontId::new(size / 4.0, FontFamily::Proportional),
                        fg,
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
