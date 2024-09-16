use super::{Property, UnicodeInput};
use crate::{Failure, PResult, Parse, Success};
#[allow(unused_imports)]
use icu_properties::{self as icup, maps, sets};

#[doc(inline)]
pub use crate::{unicode_prop_all as all, unicode_prop_any as any};

#[doc(hidden)]
#[macro_export]
macro_rules! unicode_prop_all {
    ($prop:expr) => {{
        $prop
    }};
    ($prop0:expr, $($propn:expr),+) => {{
        $crate::unicode::prop::and($prop0, $crate::unicode::prop::all!($($propn),+))
    }};
    ($($prop:expr,)+) => {{
        $crate::unicode::prop::all!($($prop),*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! unicode_prop_any {
    ($prop:expr) => {{
        $prop
    }};
    ($prop0:expr, $($propn:expr),+) => {{
        $crate::unicode::prop::or($prop0, $crate::unicode::prop::any!($($propn),+))
    }};
    ($($prop:expr,)+) => {{
        $crate::unicode::prop::any!($($prop),*)
    }};
}

#[derive(Debug, Clone, Copy)]
pub struct Not<P: Property>(P);

#[derive(Debug, Clone, Copy)]
pub struct And<L: Property, R: Property>(L, R);

#[derive(Debug, Clone, Copy)]
pub struct Or<L: Property, R: Property>(L, R);

pub const fn not<P: Property>(property: P) -> Not<P> {
    Not(property)
}

pub const fn and<L: Property, R: Property>(lhs: L, rhs: R) -> And<L, R> {
    And(lhs, rhs)
}

pub const fn or<L: Property, R: Property>(lhs: L, rhs: R) -> Or<L, R> {
    Or(lhs, rhs)
}

impl<P: Property> Property for Not<P> {
    fn contains(self, ch: char) -> bool {
        !self.0.contains(ch)
    }
}

impl<P: Property, I: UnicodeInput> Parse<I> for Not<P> {
    type Parsed = char;

    fn parse(&self, input: I) -> PResult<char, I> {
        match input.clone().parse_char() {
            Ok(Success(ch, rem)) if self.contains(ch) => Ok(Success(ch, rem)),
            _ => Err(Failure(input)),
        }
    }
}

impl<L: Property, R: Property> core::ops::BitAnd<R> for Not<L> {
    type Output = And<Self, R>;

    fn bitand(self, rhs: R) -> And<Self, R> {
        And(self, rhs)
    }
}

impl<L: Property, R: Property> core::ops::BitOr<R> for Not<L> {
    type Output = Or<Self, R>;

    fn bitor(self, rhs: R) -> Or<Self, R> {
        Or(self, rhs)
    }
}

impl<L: Property, R: Property> Property for And<L, R> {
    fn contains(self, ch: char) -> bool {
        self.0.contains(ch) && self.1.contains(ch)
    }
}

impl<L: Property, R: Property, I: UnicodeInput> Parse<I> for And<L, R> {
    type Parsed = char;

    fn parse(&self, input: I) -> PResult<char, I> {
        match input.clone().parse_char() {
            Ok(Success(ch, rem)) if self.contains(ch) => Ok(Success(ch, rem)),
            _ => Err(Failure(input)),
        }
    }
}

impl<L: Property, R: Property> core::ops::Not for And<L, R> {
    type Output = Not<Self>;

    fn not(self) -> Not<Self> {
        Not(self)
    }
}

impl<LL: Property, LR: Property, R: Property> core::ops::BitAnd<R> for And<LL, LR> {
    type Output = And<Self, R>;

    fn bitand(self, rhs: R) -> And<Self, R> {
        And(self, rhs)
    }
}

impl<LL: Property, LR: Property, R: Property> core::ops::BitOr<R> for And<LL, LR> {
    type Output = Or<Self, R>;

    fn bitor(self, rhs: R) -> Or<Self, R> {
        Or(self, rhs)
    }
}

impl<L: Property, R: Property> Property for Or<L, R> {
    fn contains(self, ch: char) -> bool {
        self.0.contains(ch) || self.1.contains(ch)
    }
}

impl<L: Property, R: Property, I: UnicodeInput> Parse<I> for Or<L, R> {
    type Parsed = char;

    fn parse(&self, input: I) -> PResult<char, I> {
        match input.clone().parse_char() {
            Ok(Success(ch, rem)) if self.contains(ch) => Ok(Success(ch, rem)),
            _ => Err(Failure(input)),
        }
    }
}

impl<L: Property, R: Property> core::ops::Not for Or<L, R> {
    type Output = Not<Self>;

    fn not(self) -> Not<Self> {
        Not(self)
    }
}

impl<LL: Property, LR: Property, R: Property> core::ops::BitAnd<R> for Or<LL, LR> {
    type Output = And<Self, R>;

    fn bitand(self, rhs: R) -> And<Self, R> {
        And(self, rhs)
    }
}

impl<LL: Property, LR: Property, R: Property> core::ops::BitOr<R> for Or<LL, LR> {
    type Output = Or<Self, R>;

    fn bitor(self, rhs: R) -> Or<Self, R> {
        Or(self, rhs)
    }
}

macro_rules! def_bool_prop {
    ($(#[$($attr:tt)*])* $ty:ident => $set:expr) => {
        $(#[$($attr)*])*
        #[derive(Debug, Clone, Copy)]
        pub struct $ty;

        impl Property for $ty {
            fn contains(self, ch: char) -> bool {
                const { $set }.contains(ch)
            }
        }

        impl<I: UnicodeInput> Parse<I> for $ty {
            type Parsed = char;

            fn parse(&self, input: I) -> PResult<char, I> {
                match input.clone().parse_char() {
                    Ok(Success(ch, rem)) if self.contains(ch) => Ok(Success(ch, rem)),
                    _ => Err(Failure(input)),
                }
            }
        }

        impl core::ops::Not for $ty {
            type Output = Not<Self>;

            fn not(self) -> Not<Self> {
                Not(self)
            }
        }

        impl<R: Property> core::ops::BitAnd<R> for $ty {
            type Output = And<Self, R>;

            fn bitand(self, rhs: R) -> And<Self, R> {
                And(self, rhs)
            }
        }

        impl<R: Property> core::ops::BitOr<R> for $ty {
            type Output = Or<Self, R>;

            fn bitor(self, rhs: R) -> Or<Self, R> {
                Or(self, rhs)
            }
        }
    };
}

def_bool_prop!(Alphabetic => sets::alphabetic());
def_bool_prop!(AsciiHexDigit => sets::ascii_hex_digit());
def_bool_prop!(BidiControl => sets::bidi_control());
def_bool_prop!(BidiMirrored => sets::bidi_mirrored());
def_bool_prop!(CaseIgnorable => sets::case_ignorable());
def_bool_prop!(Cased => sets::cased());
def_bool_prop!(ChangesWhenCasefolded => sets::changes_when_casefolded());
def_bool_prop!(ChangesWhenCasemapped => sets::changes_when_casemapped());
def_bool_prop!(ChangesWhenLowercased => sets::changes_when_lowercased());
def_bool_prop!(ChangesWhenNfkcCasefolded => sets::changes_when_nfkc_casefolded());
def_bool_prop!(ChangesWhenTitlecased => sets::changes_when_titlecased());
def_bool_prop!(ChangesWhenUppercased => sets::changes_when_uppercased());
def_bool_prop!(Dash => sets::dash());
def_bool_prop!(DefaultIgnorableCodePoint => sets::default_ignorable_code_point());
def_bool_prop!(Deprecated => sets::deprecated());
def_bool_prop!(Diacritic => sets::diacritic());
def_bool_prop!(Emoji => sets::emoji());
def_bool_prop!(EmojiComponent => sets::emoji_component());
def_bool_prop!(EmojiModifier => sets::emoji_modifier());
def_bool_prop!(EmojiModifierBase => sets::emoji_modifier_base());
def_bool_prop!(EmojiPresentation => sets::emoji_presentation());
def_bool_prop!(ExtendedPictographic => sets::extended_pictographic());
def_bool_prop!(Extender => sets::extender());
def_bool_prop!(FullCompositionExclusion => sets::full_composition_exclusion());
def_bool_prop!(GraphemeBase => sets::grapheme_base());
def_bool_prop!(GraphemeExtend => sets::grapheme_extend());
def_bool_prop!(GraphemeLink => sets::grapheme_link());
def_bool_prop!(HexDigit => sets::hex_digit());
def_bool_prop!(Hyphen => sets::hyphen());
def_bool_prop!(IdContinue => sets::id_continue());
def_bool_prop!(IdStart => sets::id_start());
def_bool_prop!(Ideographic => sets::ideographic());
def_bool_prop!(IdsBinaryOperator => sets::ids_binary_operator());
def_bool_prop!(IdsTrinaryOperator => sets::ids_trinary_operator());
def_bool_prop!(JoinControl => sets::join_control());
def_bool_prop!(LogicalOrderException => sets::logical_order_exception());
def_bool_prop!(Lowercase => sets::lowercase());
def_bool_prop!(Math => sets::math());
def_bool_prop!(NoncharacterCodePoint => sets::noncharacter_code_point());
def_bool_prop!(PatternSyntax => sets::pattern_syntax());
def_bool_prop!(PatternWhiteSpace => sets::pattern_white_space());
def_bool_prop!(PrependedConcatenationMark => sets::prepended_concatenation_mark());
def_bool_prop!(QuotationMark => sets::quotation_mark());
def_bool_prop!(Radical => sets::radical());
def_bool_prop!(RegionalIndicator => sets::regional_indicator());
def_bool_prop!(SentenceTerminal => sets::sentence_terminal());
def_bool_prop!(SoftDotted => sets::soft_dotted());
def_bool_prop!(TerminalPunctuation => sets::terminal_punctuation());
def_bool_prop!(UnifiedIdeograph => sets::unified_ideograph());
def_bool_prop!(Uppercase => sets::uppercase());
def_bool_prop!(VariationSelector => sets::variation_selector());
def_bool_prop!(WhiteSpace => sets::white_space());
def_bool_prop!(XidContinue => sets::xid_continue());
def_bool_prop!(XidStart => sets::xid_start());

macro_rules! def_enum_prop {
    ($(#[$($attr:tt)*])* enum $name:ident { $($(#[$($vattr:tt)*])* $var:ident => $val:expr),* $(,)* } => $map:expr) => {
        $(#[$($attr)*])*
        #[derive(Debug, Clone, Copy)]
        pub enum $name {
            $(
                $(#[$($vattr)*])*
                $var,
            )*
        }

        impl Property for $name {
            fn contains(self, ch: char) -> bool {
                let val = const { &($map) }.get(ch);
                match self {
                    $(
                        Self::$var => (val == ($val)),
                    )*
                }
            }
        }

        impl<I: UnicodeInput> Parse<I> for $name {
            type Parsed = char;

            fn parse(&self, input: I) -> PResult<char, I> {
                match input.clone().parse_char() {
                    Ok(Success(ch, rem)) if self.contains(ch) => Ok(Success(ch, rem)),
                    _ => Err(Failure(input)),
                }
            }
        }

        impl core::ops::Not for $name {
            type Output = Not<Self>;

            fn not(self) -> Not<Self> {
                Not(self)
            }
        }

        impl<R: Property> core::ops::BitAnd<R> for $name {
            type Output = And<Self, R>;

            fn bitand(self, rhs: R) -> And<Self, R> {
                And(self, rhs)
            }
        }

        impl<R: Property> core::ops::BitOr<R> for $name {
            type Output = Or<Self, R>;

            fn bitor(self, rhs: R) -> Or<Self, R> {
                Or(self, rhs)
            }
        }
    };
}

def_enum_prop! {
    enum BidiClass {
        LeftToRight => icup::BidiClass::LeftToRight,
        RightToLeft => icup::BidiClass::RightToLeft,
        EuropeanNumber => icup::BidiClass::EuropeanNumber,
        EuropeanSeparator => icup::BidiClass::EuropeanSeparator,
        EuropeanTerminator => icup::BidiClass::EuropeanSeparator,
        ArabicNumber => icup::BidiClass::ArabicNumber,
        CommonSeparator => icup::BidiClass::CommonSeparator,
        ParagraphSeparator => icup::BidiClass::ParagraphSeparator,
        SegmentSeparator => icup::BidiClass::SegmentSeparator,
        WhiteSpace => icup::BidiClass::WhiteSpace,
        OtherNeutral => icup::BidiClass::OtherNeutral,
        LeftToRightEmbedding => icup::BidiClass::LeftToRightEmbedding,
        LeftToRightOverride => icup::BidiClass::LeftToRightOverride,
        ArabicLetter => icup::BidiClass::ArabicLetter,
        RightToLeftEmbedding => icup::BidiClass::RightToLeftEmbedding,
        RightToLeftOverride => icup::BidiClass::RightToLeftOverride,
        PopDirectionalFormat => icup::BidiClass::PopDirectionalFormat,
        NonspacingMark => icup::BidiClass::NonspacingMark,
        BoundaryNeutral => icup::BidiClass::BoundaryNeutral,
        FirstStrongIsolate => icup::BidiClass::FirstStrongIsolate,
        LeftToRightIsolate => icup::BidiClass::LeftToRightIsolate,
        RightToLeftIsolate => icup::BidiClass::RightToLeftIsolate,
        PopDirectionalIsolate => icup::BidiClass::PopDirectionalIsolate,
    } => maps::bidi_class()
}
