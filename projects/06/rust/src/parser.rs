use std::num::ParseIntError;
use thiserror::Error;

use super::instruction::{ADest, Comp, Dest, Jump};
use super::lexer::Lexer;
use super::token::{Kind, Token};
use crate::T;
use std::convert::TryInto;
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(s).peekable(),
        };
        parser.advance();
        parser
    }

    /// Consumes and throws away tokens until the next significant line.
    fn sync(&mut self) -> Option<()> {
        while self.lexer.next()?.kind != T!['\n'] {}
        self.advance()
    }

    /// Consumes and throws away tokens until the next significant token.
    /// This function does nothing if already at a significant token.
    fn advance(&mut self) -> Option<()> {
        loop {
            if !matches!(self.lexer.peek()?.kind, T!['\n'] | T![comment(_)]) {
                // not a blank line or comment
                break;
            }
            self.lexer.next()?;
        }
        Some(())
    }

    /// Collects the significant tokens of the current line into a vector.
    fn collect_line(&mut self) -> Vec<Token<'a>> {
        let mut line = vec![];
        while self
            .lexer
            .peek()
            .map(|t| !matches!(t.kind, T!['\n'] | T![comment(_)]))
            .unwrap_or(false)
        {
            // we can unwrap here because peek is Some
            line.push(self.lexer.next().unwrap());
        }
        line
    }

    /// Parse an A instruction.
    fn a_inst(&mut self) -> Result<LineData<'a>, ParseError> {
        let mut next = || self.lexer.next().ok_or(ParseError::A);
        assert_eq!(
            next()?.kind,
            T![@],
            "programmer error: parse_at called not at @"
        );
        let dest = match next()?.kind {
            T![number(n)] => ADest::Const(n),
            T![ident(s)] => ADest::Ident(s),
            _ => return Err(ParseError::A),
        };
        Ok(LineData::AInst(dest))
    }

    /// Parse a label.
    fn label(&mut self) -> Result<&'a str, ParseError> {
        let mut next = || self.lexer.next().ok_or(ParseError::Label);
        assert_eq!(
            next()?.kind,
            T!['('],
            "programmer error: parse_label called not at label"
        );
        let label = match next()?.kind {
            T![ident(s)] => s,
            _ => return Err(ParseError::Label),
        };
        match next()?.kind {
            T![')'] => Ok(label),
            _ => Err(ParseError::Label),
        }
    }

    /// Parse a C instruction.
    fn c_inst(&mut self) -> Result<LineData<'a>, ParseError> {
        let line = self.collect_line();
        Ok(LineData::CInst(
            (&line).try_into()?,
            (&line).try_into()?,
            (&line).try_into()?,
        ))
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Line<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let line = self.lexer.peek()?.line;
        let data = match self.lexer.peek()?.kind {
            T!['('] => self.label().map(|l| LineData::Label(l)),
            T![@] => self.a_inst(),
            _ => self.c_inst(),
        };
        let mut line = Line { line, data };
        // check for trailing tokens and report as an error
        // then synchronize to prepare parsing the next line
        if self.lexer.peek().is_none() {
            return Some(line);
        }
        if !matches!(self.lexer.peek()?.kind, T![comment(_)] | T!['\n']) {
            line.data = Err(ParseError::Trailing);
            self.sync();
        }
        // advance to the next significant
        self.advance();
        Some(line)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Line<'a> {
    line: usize,
    data: Result<LineData<'a>, ParseError>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LineData<'a> {
    Label(&'a str),
    AInst(ADest<'a>),
    CInst(Dest, Comp, Jump),
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
    #[error("error parsing dest")]
    Dest,
    #[error("error parsing comp")]
    Comp,
    #[error("error parsing jump")]
    Jump,
}

#[cfg(test)]
mod test {
    use super::{ADest, Dest, Jump, Kind, Line, LineData, ParseError, Parser};
    use crate::instruction::Comp;
    use crate::{cmp, T};

    #[test]
    fn a_inst() {
        let parser = Parser::new("@ident\n@err; oops\n@100");
        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![
                Line {
                    line: 1,
                    data: Ok(LineData::AInst(ADest::Ident("ident"))),
                },
                Line {
                    line: 2,
                    data: Err(ParseError::Trailing),
                },
                Line {
                    line: 3,
                    data: Ok(LineData::AInst(ADest::Const(100))),
                },
            ]
        );
    }

    #[test]
    fn label() {
        let parser = Parser::new("(foo)\n\n@123");
        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![
                Line {
                    line: 1,
                    data: Ok(LineData::Label("foo")),
                },
                Line {
                    line: 3,
                    data: Ok(LineData::AInst(ADest::Const(123))),
                },
            ]
        );
    }

    #[test]
    #[rustfmt::skip]
    fn c_inst() {
        let parser = Parser::new("D = M + 1; JEQ");
        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Line {
                line: 1,
                data: Ok(LineData::CInst(Dest::D, cmp![M + 1], Jump::EQ)),
            }]
        );

        let parser = Parser::new("
            foo = M + 1; JEQ
            D = foo + 1; JEQ
            D = M + 1; foo
            D = M + 1; JEQ foo
            D = M;
            foo
        ");
        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![
                Line { line: 2, data: Err(ParseError::Dest) },
                Line { line: 3, data: Err(ParseError::Comp) },
                Line { line: 4, data: Err(ParseError::Jump) },
                Line { line: 5, data: Err(ParseError::Trailing) },
                Line { line: 6, data: Ok(LineData::CInst(Dest::D, cmp![M], Jump::NONE)) },
                Line { line: 7, data: Err(ParseError::Comp) },
            ]
        );
    }

    #[test]
    fn advance() {
        let mut parser = Parser::new("1\n\n\n\n// comment\n\nfoo");
        parser.advance();
        assert_eq!(parser.lexer.peek().unwrap().kind, T![number(1)]);
        parser.lexer.next();
        parser.advance();
        assert_eq!(parser.lexer.peek().unwrap().kind, T![ident("foo")]);
    }

    #[test]
    fn sync() {
        let mut parser = Parser::new("1\n\n\n\n// comment\n\nfoo");
        assert_eq!(parser.lexer.peek().unwrap().kind, T![number(1)]);
        parser.sync();
        assert_eq!(parser.lexer.peek().unwrap().kind, T![ident("foo")]);
    }
}
