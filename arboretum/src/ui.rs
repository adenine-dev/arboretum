use eframe::egui::{Ui, WidgetText};
use std::fmt::Display;

mod board;
use board::*;
mod players;
use players::*;
mod style;
use style::*;

use crate::AppData;

macro_rules! dispatched {
    (
        $(#[$meta:meta])?
        $vis_e:vis enum $ident_e:ident {
            $( $variant:ident($variant_ty:ty), )+
        }

        $vis_t:vis trait $ident_t:ident {
            $( $vis_f:vis fn $name:ident ($( $arg_n:ident : $arg_t:ty ),*) $(-> $return_ty:ty)? $($default_impl:block)? );+;
        }
    ) => {
        $(#[$meta])?
        $vis_e enum $ident_e {
            $( $variant ),+
        }

        $vis_t trait $ident_t {
            $(fn $name ($($arg_n: $arg_t),*) $(-> $return_ty)? $($default_impl)?);+
        }

        macro_rules! _dispatched_internal_make_fn {
            ($$vis:vis $$name:ident, $$($$arg_n:ident)* @ $$($$arg_t:ty)*; $$(-> $$return_ty:ty)?) => {
                $$vis fn $$name(&mut self, $$( $$arg_n : $$arg_t ),*) $$(-> $$return_ty)? {
                    match self {
                        $($ident_e::$variant => <$variant_ty>::$$name($$( $$arg_n),*)),+
                    }
                }
            }
        }
        impl $ident_e {
            $(_dispatched_internal_make_fn!($vis_f $name, $($arg_n)* @ $($arg_t)*; $(-> $return_ty)?); )*
        }
    };
}

dispatched!(
    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    pub enum Panel {
        Board(BoardPanel),
        Players(PlayersPanel),
        StyleEditor(StyleEditorPanel),
    }

    pub trait PanelT {
        pub fn update(app_data: &mut AppData, ui: &mut Ui);
        pub fn title(app_data: &mut AppData) -> WidgetText;
        pub fn on_close(_app_data: &mut AppData) {};
    }
);

impl Display for Panel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
