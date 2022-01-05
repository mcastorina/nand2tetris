use bitflags::bitflags;
use std::num::ParseIntError;
use thiserror::Error;

use super::lexer::Lexer;
use super::token::{Kind, Token};
use crate::T;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Eq)]
pub enum ADest<'a> {
    Const(u16),
    Ident(&'a str),
}

bitflags! {
    pub struct Dest: u8 {
        const M = 0b001;
        const D = 0b010;
        const A = 0b100;
    }
}

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq)]
pub enum Jump {
    JGT, JEQ, JGE,
    JLT, JNE, JLE,
    JMP,
}

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq)]
pub enum Comp {
    Zero, One, NegOne,
    D, A, M,
    NotD, NotA, NotM,
    NegD, NegA, NegM,
    IncD, IncA, IncM,
    DecD, DecA, DecM,
    DPlusA, DPlusM, DMinusA,
    DMinusM, AMinusD, MMinusD,
    DAndA, DAndM,
    DOrA, DOrM,
}

#[macro_export]
macro_rules! cmp {
    [0] => { Comp::Zero };
    [1] => { Comp::One };
    [-1] => { Comp::NegOne };
    [D] => { Comp::D };
    [A] => { Comp::A };
    [M] => { Comp::M };
    [!D] => { Comp::NotD };
    [!A] => { Comp::NotA };
    [!M] => { Comp::NotM };
    [-D] => { Comp::NegD };
    [-A] => { Comp::NegA };
    [-M] => { Comp::NegM };
    [D+1] => { Comp::IncD };
    [A+1] => { Comp::IncA };
    [M+1] => { Comp::IncM };
    [D-1] => { Comp::DecD };
    [A-1] => { Comp::DecA };
    [M-1] => { Comp::DecM };
    [D+A] => { Comp::DPlusA };
    [A+D] => { Comp::DPlusA };
    [D+M] => { Comp::DPlusM };
    [M+D] => { Comp::DPlusM };
    [D-A] => { Comp::DMinusA };
    [D-M] => { Comp::DMinusM };
    [A-D] => { Comp::AMinusD };
    [M-D] => { Comp::MMinusD };
    [D&A] => { Comp::DAndA };
    [D&M] => { Comp::DAndM };
    [D|A] => { Comp::DOrA };
    [D|M] => { Comp::DOrM };
}
