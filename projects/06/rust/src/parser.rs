use bitflags::bitflags;
use std::num::ParseIntError;
use thiserror::Error;

use super::lexer::Lexer;
use crate::T;
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            lexer: Lexer::new(s).peekable(),
        }
    }
    pub fn parse(&mut self) -> Vec<Line> {
        let mut lines = vec![];
        loop {
            let line = self.parse_line();
            if line.is_none() {
                break;
            }
            lines.push(line.unwrap());
        }
        lines
    }

    /// gets the next non-comment token from the lexer
    fn next(&mut self) -> Option<(Token, &str)> {
        // call peek() to skip over comments
        self.peek()?;
        self.lexer.next()
    }

    /// peeks the next non-comment token from the lexer
    fn peek(&mut self) -> Option<(Token, &str)> {
        loop {
            let token = self.lexer.peek()?;
            if token.0 != Token::Comment {
                return Some(*token);
            }
            self.lexer.next();
        }
    }

    /// consumes and throws away tokens until the next line
    fn sync(&mut self) {
        loop {
            match self.next() {
                None | Some(T!['\n']) => match self.peek() {
                    Some(T!['\n']) => continue,
                    _ => break,
                },
                _ => (),
            }
        }
    }

    fn parse_line(&mut self) -> Option<Line> {
        // check for label
        let mut label = None;
        if self.peek()? == T!['('] {
            match self.parse_label() {
                Ok(s) => label = Some(s),
                Err(err) => {
                    return Some(Line {
                        label: None,
                        inst: Err(err),
                    });
                }
            };
            // sync to start of next instruction
            self.sync();
        }
        // parse instruction
        let inst = match self.peek()? {
            T![@] => self.parse_at(),
            T![ident(_)] => self.parse_expr(),
            _ => todo!(),
        };
        // finished parsing line
        let mut line = Line { label, inst: inst };
        // check for trailing tokens
        match self.next() {
            None | Some(T!['\n']) => (),
            _ => {
                line.inst = Err(ParseError::Trailing);
                self.sync();
            }
        }
        Some(line)
    }

    fn parse_at(&mut self) -> Result<Inst, ParseError> {
        let mut next = || self.lexer.next().ok_or(ParseError::A);
        assert_eq!(next()?, T![@], "programmer error: parse_at called not at @");
        let dest = match next()? {
            T![number(s)] => ADest::Const(s.parse()?),
            T![ident(s)] => ADest::Ident(s.to_string()),
            _ => return Err(ParseError::A),
        };
        Ok(Inst::A(dest))
    }

    fn parse_label(&mut self) -> Result<String, ParseError> {
        let mut next = || self.lexer.next().ok_or(ParseError::Label);
        assert_eq!(
            next()?,
            T!['('],
            "programmer error: parse_label called not at label"
        );
        let label = match next()? {
            T![ident(s)] => s.to_string(),
            _ => return Err(ParseError::Label),
        };
        match next()? {
            T![')'] => Ok(label),
            _ => Err(ParseError::Label),
        }
    }

    fn parse_expr(&mut self) -> Result<Inst, ParseError> {
        todo!()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Line {
    label: Option<String>,
    inst: Result<Inst, ParseError>,
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParseError {
    #[error("unexpected EOF")]
    EOF,
    #[error("unexpected EOL")]
    EOL,
    #[error("trailing data")]
    Trailing,
    #[error("error parsing integer")]
    ParseInt(#[from] ParseIntError),
    #[error("error parsing A instruction")]
    A,
    #[error("error parsing label")]
    Label,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Inst {
    A(ADest),
    C(Dest, Comp, Option<Jump>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ADest {
    Const(u16),
    Ident(String),
}

bitflags! {
    pub struct Dest: u8 {
        const M = 0b001;
        const D = 0b010;
        const A = 0b100;
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Jump {
    JGT,
    JEQ,
    JGE,
    JLT,
    JNE,
    JLE,
    JMP,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Comp {
    Zero,
    One,
    NegOne,
    D,
    A,
    M,
    NotD,
    NotA,
    NotM,
    NegD,
    NegA,
    NegM,
    IncD,
    IncA,
    IncM,
    DecD,
    DecA,
    DecM,
    DPlusA,
    DPlusM,
    DMinusA,
    DMinusM,
    AMinusD,
    MMinusD,
    DAndA,
    DAndM,
    DOrA,
    DOrM,
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

#[macro_export]
macro_rules! inst {
    [A_num($dest:literal)] => { Ok(Inst::A(ADest::Const($dest))) };
    [A_ident($dest:literal)] => { Ok(Inst::A(ADest::Ident($dest.to_string()))) };
}

#[cfg(test)]
mod test {
    use super::{ADest, Inst, Line, ParseError, Parser};

    #[test]
    fn parse_at() {
        let mut parser = Parser::new("@ident\n@err; oops\n@100");
        assert_eq!(
            parser.parse(),
            vec![
                Line {
                    label: None,
                    inst: inst![A_ident("ident")],
                },
                Line {
                    label: None,
                    inst: Err(ParseError::Trailing),
                },
                Line {
                    label: None,
                    inst: inst![A_num(100)],
                },
            ]
        );
    }

    #[test]
    fn parse_label() {
        let mut parser = Parser::new("(foo)\n\n@123");
        assert_eq!(
            parser.parse(),
            vec![Line {
                label: Some("foo".to_string()),
                inst: inst![A_num(123)],
            }]
        );
    }
}
