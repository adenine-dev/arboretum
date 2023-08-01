use eframe::egui;
use egui::{
    color_picker::{color_edit_button_srgba, Alpha},
    ComboBox, Slider, Ui, WidgetText,
};

use crate::AppData;

use super::PanelT;
pub struct StyleEditorPanel {}

impl PanelT for StyleEditorPanel {
    fn update(app_data: &mut AppData, ui: &mut Ui) {
        ui.heading("Style Editor");

        let style = app_data.tabs.style.as_mut().unwrap();

        ui.collapsing("Border", |ui| {
            egui::Grid::new("border").show(ui, |ui| {
                ui.label("Width:");
                ui.add(Slider::new(&mut style.border.width, 1.0..=50.0));
                ui.end_row();

                ui.label("Color:");
                color_edit_button_srgba(ui, &mut style.border.color, Alpha::OnlyBlend);
                ui.end_row();
            });
        });

        ui.collapsing("Selection", |ui| {
            egui::Grid::new("selection").show(ui, |ui| {
                ui.label("Color:");
                color_edit_button_srgba(ui, &mut style.selection_color, Alpha::OnlyBlend);
                ui.end_row();
            });
        });

        ui.collapsing("Separator", |ui| {
            egui::Grid::new("separator").show(ui, |ui| {
                ui.label("Width:");
                ui.add(Slider::new(&mut style.separator.width, 1.0..=50.0));
                ui.end_row();

                ui.label("Extra Interact Width:");
                ui.add(Slider::new(
                    &mut style.separator.extra_interact_width,
                    0.0..=50.0,
                ));
                ui.end_row();

                ui.label("Offset limit:");
                ui.add(Slider::new(&mut style.separator.extra, 1.0..=300.0));
                ui.end_row();

                ui.label("Idle color:");
                color_edit_button_srgba(ui, &mut style.separator.color_idle, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Hovered color:");
                color_edit_button_srgba(ui, &mut style.separator.color_hovered, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Dragged color:");
                color_edit_button_srgba(ui, &mut style.separator.color_dragged, Alpha::OnlyBlend);
                ui.end_row();
            });
        });

        ui.collapsing("Tabs", |ui| {
            ui.separator();

            ui.checkbox(&mut style.tabs.fill_tab_bar, "Expand tabs");
            ui.checkbox(
                &mut style.tabs.hline_below_active_tab_name,
                "Show a line below the active tab name",
            );

            ui.separator();

            ui.checkbox(
                &mut style.tab_bar.show_scroll_bar_on_overflow,
                "Show scroll bar on tab overflow",
            );
            ui.horizontal(|ui| {
                ui.add(Slider::new(&mut style.tab_bar.height, 20.0..=50.0));
                ui.label("Tab bar height");
            });

            ComboBox::new("add_button_align", "Add button align")
                .selected_text(format!("{:?}", style.buttons.add_tab_align))
                .show_ui(ui, |ui| {
                    for align in [egui_dock::TabAddAlign::Left, egui_dock::TabAddAlign::Right] {
                        ui.selectable_value(
                            &mut style.buttons.add_tab_align,
                            align,
                            format!("{:?}", align),
                        );
                    }
                });

            ui.separator();

            ui.label("Rounding");
            ui.horizontal(|ui| {
                ui.add(Slider::new(&mut style.tabs.rounding.nw, 0.0..=15.0));
                ui.label("North-West");
            });
            ui.horizontal(|ui| {
                ui.add(Slider::new(&mut style.tabs.rounding.ne, 0.0..=15.0));
                ui.label("North-East");
            });
            ui.horizontal(|ui| {
                ui.add(Slider::new(&mut style.tabs.rounding.sw, 0.0..=15.0));
                ui.label("South-West");
            });
            ui.horizontal(|ui| {
                ui.add(Slider::new(&mut style.tabs.rounding.se, 0.0..=15.0));
                ui.label("South-East");
            });

            ui.separator();

            egui::Grid::new("tabs_colors").show(ui, |ui| {
                ui.label("Title text color, inactive and unfocused:");
                color_edit_button_srgba(ui, &mut style.tabs.text_color_unfocused, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Title text color, inactive and focused:");
                color_edit_button_srgba(ui, &mut style.tabs.text_color_focused, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Title text color, active and unfocused:");
                color_edit_button_srgba(
                    ui,
                    &mut style.tabs.text_color_active_unfocused,
                    Alpha::OnlyBlend,
                );
                ui.end_row();

                ui.label("Title text color, active and focused:");
                color_edit_button_srgba(
                    ui,
                    &mut style.tabs.text_color_active_focused,
                    Alpha::OnlyBlend,
                );
                ui.end_row();

                ui.label("Close button color unfocused:");
                color_edit_button_srgba(ui, &mut style.buttons.close_tab_color, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Close button color focused:");
                color_edit_button_srgba(
                    ui,
                    &mut style.buttons.close_tab_active_color,
                    Alpha::OnlyBlend,
                );
                ui.end_row();

                ui.label("Close button background color:");
                color_edit_button_srgba(ui, &mut style.buttons.close_tab_bg_fill, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Bar background color:");
                color_edit_button_srgba(ui, &mut style.tab_bar.bg_fill, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Outline color:")
                    .on_hover_text("The outline around the active tab name.");
                color_edit_button_srgba(ui, &mut style.tabs.outline_color, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Horizontal line color:").on_hover_text(
                    "The line separating the tab name area from the tab content area",
                );
                color_edit_button_srgba(ui, &mut style.tab_bar.hline_color, Alpha::OnlyBlend);
                ui.end_row();

                ui.label("Background color:");
                color_edit_button_srgba(ui, &mut style.tabs.bg_fill, Alpha::OnlyBlend);
                ui.end_row();
            });
        });
    }

    fn title(_app_data: &mut AppData) -> WidgetText {
        "Style Editor".into()
    }
}
