#![feature(macro_metavar_expr)]
#![feature(let_chains)]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use std::collections::HashSet;

use board::Board;
use eframe::{
    egui::{self, Id},
    emath::Align2,
    CreationContext, NativeOptions,
};
use egui::{CentralPanel, Frame, Ui, WidgetText};

use egui_dock::{DockArea, Node, NodeIndex, Style, TabViewer, Tree};

mod context;
use context::Context;

mod ui;
use egui_toast::{Toast, Toasts};
use ui::*;

mod board;
mod process;
mod result;

mod player;
use player::Player;

fn main() -> eframe::Result<()> {
    let options = NativeOptions {
        maximized: true,
        ..Default::default()
    };
    eframe::run_native(
        "Arboretum",
        options,
        Box::new(|cc| Box::<App>::new(App::new(cc))),
    )
}

pub struct TabData {
    open: HashSet<Panel>,
    to_be_added: Vec<(NodeIndex, Panel)>,
    super_focused: Option<Panel>,
    toasts: Toasts,
}
impl TabData {
    fn clear_toasts(&self, ui: &mut Ui) {
        ui.data_mut(|ui| {
            // HACK: egui toasts doesn't allow for clearing of toasts yet, so we have to go into the internals of it.
            *ui.get_temp_mut_or_default(Id::new("__toasts")) = Vec::<Toast>::new();
        });
    }
}

pub struct AppData {
    pub context: Context,
    pub tabs: TabData,
}

struct App {
    app_data: AppData,
    tree: Tree<Panel>,
}

impl TabViewer for AppData {
    type Tab = Panel;

    fn ui(&mut self, ui: &mut Ui, tab: &mut Self::Tab) {
        // ui.next_widget_position();
        if self.tabs.super_focused.is_some() && *tab != self.tabs.super_focused.unwrap() {
            ui.set_enabled(false);
        }
        tab.update(self, ui);
    }

    fn title(&mut self, tab: &mut Self::Tab) -> WidgetText {
        tab.title(self)
    }

    fn on_close(&mut self, tab: &mut Self::Tab) -> bool {
        tab.on_close(self);

        // don't allow the last tab to close, we don't have anything to handle opening new tabs lol
        if self.tabs.open.len() <= 1 {
            return false;
        }

        self.tabs.open.remove(tab);
        true
    }

    fn add_popup(&mut self, ui: &mut Ui, node: NodeIndex) {
        ui.set_min_width(100.0);

        for tab in &[Panel::Board, Panel::Players, Panel::StyleEditor] {
            ui.add_enabled_ui(!self.tabs.open.contains(tab), |ui| {
                if ui.button(tab.to_string()).clicked() {
                    self.tabs.to_be_added.push((node, *tab));

                    ui.close_menu();
                }
            });
        }
    }
}

impl App {
    fn new(_cc: &CreationContext) -> Self {
        let mut tree = Tree::new(vec![Panel::Board]);

        let [a, _] = tree.split_left(NodeIndex::root(), 0.15, vec![Panel::StyleEditor]);
        let [a, _] = tree.split_below(a, 0.8, vec![Panel::Position]);
        tree.split_right(a, 0.6, vec![Panel::Players]);

        let mut open_tabs = HashSet::new();

        for node in tree.iter() {
            if let Node::Leaf { tabs, .. } = node {
                for tab in tabs {
                    open_tabs.insert(*tab);
                }
            }
        }

        Self {
            app_data: AppData {
                context: Context::new(
                    Player::new_human(),
                    Player::new_uci(),
                    Board::default(),
                    Theme::default(),
                ),
                tabs: TabData {
                    open: open_tabs,
                    to_be_added: vec![],
                    super_focused: None,
                    toasts: Toasts::new()
                        .anchor(Align2::LEFT_TOP, (10.0, 10.0))
                        .direction(egui::Direction::TopDown),
                },
            },
            tree,
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.app_data
            .tabs
            .to_be_added
            .drain(..)
            .for_each(|(node, tab)| {
                if self.tree.find_tab(&tab).is_none() {
                    self.tree.set_focused_node(node);
                    self.tree.push_to_focused_leaf(tab);
                    self.app_data.tabs.open.insert(tab);
                }
            });
        self.app_data.tabs.toasts.show(ctx);

        CentralPanel::default()
            .frame(Frame::central_panel(&ctx.style()).inner_margin(0.))
            .show(ctx, |ui| {
                let mut style = Style::from_egui(ui.style());
                style.tabs.fill_tab_bar = true;

                DockArea::new(&mut self.tree)
                    .style(style)
                    .show_close_buttons(self.app_data.tabs.open.len() > 1)
                    .show_add_buttons(true)
                    .show_add_popup(true)
                    .show_inside(ui, &mut self.app_data);
            });
    }
}
