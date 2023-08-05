use eframe::{
    egui::{self, CursorIcon, Id, InnerResponse, LayerId, Order, Sense, Ui, WidgetText},
    emath::Align2,
    epaint::{self, Color32, FontFamily, FontId, Pos2, Rect, Shape, Stroke, Vec2},
};

use crate::{
    board::{Square, FILES, RANKS},
    AppData,
};

use super::PanelT;

pub struct BoardPanel {}

pub fn drag_source(ui: &mut Ui, id: Id, body: impl FnOnce(&mut Ui)) {
    let is_being_dragged = ui.memory(|mem| mem.is_being_dragged(id));

    if !is_being_dragged {
        let response = ui.scope(body).response;

        let response = ui.interact(response.rect, id, Sense::drag());
        if response.hovered() {
            ui.ctx().set_cursor_icon(CursorIcon::Grab);
        }
    } else {
        ui.ctx().set_cursor_icon(CursorIcon::Grabbing);

        let layer_id = LayerId::new(Order::Tooltip, id);
        let response = ui.with_layer_id(layer_id, body).response;

        if let Some(pointer_pos) = ui.ctx().pointer_interact_pos() {
            let delta = pointer_pos - response.rect.center();
            ui.ctx().translate_layer(layer_id, delta);
        }
    }
}

pub fn square_drop_target<R>(
    ui: &mut Ui,
    can_accept_what_is_being_dragged: bool,
    is_being_dragged: bool,
    rank: u8,
    file: u8,
    size: f32,
    white_on_bottom: bool,
    dark_color: Color32,
    light_color: Color32,
    body: impl FnOnce(&mut Ui) -> R,
) -> InnerResponse<R> {
    let bg = dark_color;
    let fg = light_color;
    let (bg, fg) = if (rank + file) % 2 == 1 {
        (fg, bg)
    } else {
        (bg, fg)
    };

    // let is_being_dragged = ui.memory(|mem| mem.is_anything_being_dragged());

    let min = {
        let mut min = Pos2::new((7 - file) as f32 * size, rank as f32 * size);
        if white_on_bottom {
            min.y = 7.0 * size - min.y;
            min.x = 7.0 * size - min.x;
        }
        min
    };

    let max = min + Vec2::splat(size);
    let rect = Rect::from_min_max(min, max);

    let outer_rect_bounds = ui.available_rect_before_wrap();
    let where_to_put_background = ui.painter().add(Shape::Noop);
    let mut content_ui = ui.child_ui(outer_rect_bounds, *ui.layout());
    let ret = body(&mut content_ui);
    let (rect, response) = ui.allocate_at_least(rect.size(), Sense::hover());

    let style = if is_being_dragged && can_accept_what_is_being_dragged && response.hovered() {
        ui.visuals().widgets.active
    } else {
        ui.visuals().widgets.inactive
    };

    let fill = bg;
    let stroke = style.bg_stroke;

    ui.painter().set(
        where_to_put_background,
        epaint::RectShape {
            rounding: epaint::Rounding::none(),
            fill,
            stroke,
            rect,
        },
    );

    if is_being_dragged && can_accept_what_is_being_dragged {
        ui.painter().circle(
            ((rect.min + rect.max.to_vec2()).to_vec2() * 0.5).to_pos2(),
            size * 0.25,
            Color32::WHITE.linear_multiply(0.01),
            Stroke::NONE,
        )
    }

    // paint rank/file indicators
    let rank_file_indicator_rank_file = if white_on_bottom { 0 } else { 7 };
    if rank == rank_file_indicator_rank_file {
        ui.painter().text(
            rect.max,
            Align2::RIGHT_BOTTOM,
            FILES[file as usize],
            FontId::new(size / 4.0, FontFamily::Proportional),
            fg,
        );
    }
    if file == rank_file_indicator_rank_file {
        ui.painter().text(
            rect.min,
            Align2::LEFT_TOP,
            RANKS[rank as usize],
            FontId::new(size / 4.0, FontFamily::Proportional),
            fg,
        );
    }

    InnerResponse::new(ret, response)
}

impl PanelT for BoardPanel {
    fn update(app_data: &mut AppData, ui: &mut Ui) {
        let white_on_bottom = app_data.context.white_on_bottom;

        let clip_rect = ui.available_rect_before_wrap();

        let size = clip_rect.size().min_elem() / 8.0;
        let origin = ui.next_widget_position().to_vec2();

        ui.spacing_mut().item_spacing = Vec2::ZERO;

        egui::Grid::new("board").min_col_width(size).show(ui, |ui| {
            let dragged_source_piece_id = Id::new("dragged_source_piece");
            if !ui.memory(|mem| mem.is_anything_being_dragged()) {
                ui.memory_mut(|mem| {
                    mem.data
                        .insert_temp(dragged_source_piece_id, None::<Square>)
                });
            }
            let from_square = ui
                .memory_mut(|mem| mem.data.get_temp(dragged_source_piece_id))
                .unwrap_or(None);

            let mut human_move = None;

            for mut rank in (0..8).rev() {
                if !white_on_bottom {
                    rank = 7 - rank;
                }

                for mut file in 0..8 {
                    if !white_on_bottom {
                        file = 7 - file;
                    }
                    let drop_move = app_data.context.current_moves.iter().find(|m| {
                        from_square.is_none()
                            || (m.from() == from_square.unwrap()
                                && m.to() == Square::from_rank_file(rank, file))
                    });

                    let can_accept_what_is_being_dragged = drop_move.is_some();

                    let response = square_drop_target(
                        ui,
                        can_accept_what_is_being_dragged,
                        from_square.is_some(),
                        rank,
                        file,
                        size,
                        white_on_bottom,
                        app_data.context.theme.dark_square_color,
                        app_data.context.theme.light_square_color,
                        |ui| {
                            let item_id = Id::new("board_squares").with(rank).with(file);
                            let piece = app_data.context.board.get(rank, file);

                            let min = {
                                let mut min =
                                    Pos2::new((7 - file) as f32 * size, rank as f32 * size);
                                if white_on_bottom {
                                    min.y = 7.0 * size - min.y;
                                    min.x = 7.0 * size - min.x;
                                }
                                min
                            };

                            let render = |ui: &mut Ui| {
                                let (_response, painter) =
                                    ui.allocate_painter(Vec2::splat(size), Sense::click());

                                painter.text(
                                    min + Vec2::splat(size) / 2.0 + origin,
                                    Align2::CENTER_CENTER,
                                    piece.to_black_figurine(),
                                    FontId::new(size * 0.66, FontFamily::Proportional),
                                    if piece.is_black() {
                                        app_data.context.theme.black_piece_color
                                    } else {
                                        app_data.context.theme.white_piece_color
                                    },
                                );

                                if ui.memory(|mem| mem.is_being_dragged(item_id)) {
                                    ui.memory_mut(|mem| {
                                        mem.data.insert_temp(
                                            dragged_source_piece_id,
                                            Some(Square::from_rank_file(rank, file)),
                                        );
                                    });
                                }

                                ui.end_row();
                            };

                            if app_data
                                .context
                                .get_player(app_data.context.board.active_color)
                                .is_human()
                                && app_data.context.movable[(rank * 8 + file) as usize]
                            {
                                drag_source(ui, item_id, render);
                            } else {
                                render(ui);
                            }
                        },
                    )
                    .response;

                    let is_being_dragged = ui.memory(|mem| mem.is_anything_being_dragged());
                    if is_being_dragged && can_accept_what_is_being_dragged && response.hovered() {
                        human_move = drop_move;
                    }
                }
                ui.end_row();

                if let Some(&mov) = human_move {
                    if ui.input(|i| i.pointer.any_released()) {
                        app_data.context.apply_move(mov);

                        human_move = None;
                        ui.memory_mut(|mem| {
                            mem.data
                                .insert_temp(dragged_source_piece_id, None::<Square>)
                        });
                    }
                }
            }
        });

        ui.reset_style();
    }

    fn on_close(_app_data: &mut AppData) {
        println!("closing board panel")
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Board".into()
    }
}
