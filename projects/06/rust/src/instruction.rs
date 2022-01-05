use bitflags::bitflags;

use super::parser::ParseError;
use super::token::{Kind, Token};
use crate::T;

#[derive(Debug, PartialEq, Eq)]
pub enum ADest<'a> {
    Const(u16),
    Ident(&'a str),
}

bitflags! {
    pub struct Dest: u8 {
        const NONE = 0b000;
        const M = 0b001;
        const D = 0b010;
        const A = 0b100;
    }
}

bitflags! {
    pub struct Jump: u8 {
        const GT = 0b001;
        const EQ = 0b010;
        const LT = 0b100;

        const GE = Self::GT.bits | Self::EQ.bits;
        const LE = Self::LT.bits | Self::EQ.bits;
        const NE = Self::GT.bits | Self::LT.bits;

        const NONE = 0b000;
        const ALL = Self::GT.bits | Self::EQ.bits | Self::LT.bits;
    }
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
    [A&D] => { Comp::DAndA };
    [D&M] => { Comp::DAndM };
    [M&D] => { Comp::DAndM };
    [D|A] => { Comp::DOrA };
    [A|D] => { Comp::DOrA };
    [D|M] => { Comp::DOrM };
    [M|D] => { Comp::DOrM };
}

impl<'a> TryFrom<&Vec<Token<'a>>> for Dest {
    type Error = ParseError;
    fn try_from(tokens: &Vec<Token<'a>>) -> Result<Self, Self::Error> {
        let eq = tokens.into_iter().position(|tok| tok.kind == T![=]);
        match eq {
            None => return Ok(Dest::NONE),
            Some(1) => (),
            _ => return Err(ParseError::Dest),
        }
        if let T![ident(s)] = tokens[0].kind {
            Ok(s.chars()
                .map(|c| match c {
                    'M' => Ok(Dest::M),
                    'A' => Ok(Dest::A),
                    'D' => Ok(Dest::D),
                    _ => Err(ParseError::Dest),
                })
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .fold(Dest::NONE, |acc, dest| acc | *dest))
        } else {
            Err(ParseError::Dest)
        }
    }
}

impl<'a> TryFrom<&Vec<Token<'a>>> for Comp {
    type Error = ParseError;
    fn try_from(tokens: &Vec<Token<'a>>) -> Result<Self, Self::Error> {
        // skip dest values if there are any
        let mut tokens = &tokens[..];
        if tokens.len() >= 2 && tokens[1].kind == T![=] {
            tokens = &tokens[2..];
        }
        let kinds: Vec<_> = tokens.into_iter().map(|t| t.kind).collect();
        Ok(match &kinds[..] {
            [T![number(0)], ..] => cmp![0],
            [T![number(1)], ..] => cmp![1],
            [T![-], T![number(1)], ..] => cmp![-1],
            [T![ident("D")], T![+], T![number(1)], ..] => cmp![D + 1],
            [T![ident("A")], T![+], T![number(1)], ..] => cmp![A + 1],
            [T![ident("M")], T![+], T![number(1)], ..] => cmp![M + 1],
            [T![ident("D")], T![-], T![number(1)], ..] => cmp![D - 1],
            [T![ident("A")], T![-], T![number(1)], ..] => cmp![A - 1],
            [T![ident("M")], T![-], T![number(1)], ..] => cmp![M - 1],
            [T![ident("D")], T![+], T![ident("A")], ..] => cmp![D + A],
            [T![ident("A")], T![+], T![ident("D")], ..] => cmp![A + D],
            [T![ident("D")], T![+], T![ident("M")], ..] => cmp![D + M],
            [T![ident("M")], T![+], T![ident("D")], ..] => cmp![M + D],
            [T![ident("D")], T![-], T![ident("A")], ..] => cmp![D - A],
            [T![ident("D")], T![-], T![ident("M")], ..] => cmp![D - M],
            [T![ident("A")], T![-], T![ident("D")], ..] => cmp![A - D],
            [T![ident("M")], T![-], T![ident("D")], ..] => cmp![M - D],
            [T![ident("D")], T![&], T![ident("A")], ..] => cmp![D & A],
            [T![ident("D")], T![&], T![ident("M")], ..] => cmp![D & M],
            [T![ident("A")], T![&], T![ident("D")], ..] => cmp![A & D],
            [T![ident("M")], T![&], T![ident("D")], ..] => cmp![M & D],
            [T![ident("D")], T![|], T![ident("A")], ..] => cmp![D | A],
            [T![ident("D")], T![|], T![ident("M")], ..] => cmp![D | M],
            [T![ident("A")], T![|], T![ident("D")], ..] => cmp![A | D],
            [T![ident("M")], T![|], T![ident("D")], ..] => cmp![M | D],
            [T![!], T![ident("D")], ..] => cmp![!D],
            [T![!], T![ident("A")], ..] => cmp![!A],
            [T![!], T![ident("M")], ..] => cmp![!M],
            [T![-], T![ident("D")], ..] => cmp![-D],
            [T![-], T![ident("A")], ..] => cmp![-A],
            [T![-], T![ident("M")], ..] => cmp![-M],
            [T![ident("D")], ..] => cmp![D],
            [T![ident("A")], ..] => cmp![A],
            [T![ident("M")], ..] => cmp![M],
            _ => return Err(ParseError::Comp),
        })
    }
}

impl<'a> TryFrom<&Vec<Token<'a>>> for Jump {
    type Error = ParseError;
    fn try_from(tokens: &Vec<Token<'a>>) -> Result<Self, Self::Error> {
        let semi = tokens.into_iter().position(|tok| tok.kind == T![;]);
        if semi.is_none() {
            return Ok(Jump::NONE);
        }
        let semi = semi.unwrap();
        let jmp = match tokens.get(semi + 1).map(|t| t.kind) {
            Some(T![ident("JGT")]) => Jump::GT,
            Some(T![ident("JEQ")]) => Jump::EQ,
            Some(T![ident("JGE")]) => Jump::GE,
            Some(T![ident("JLT")]) => Jump::LT,
            Some(T![ident("JNE")]) => Jump::NE,
            Some(T![ident("JLE")]) => Jump::LE,
            Some(T![ident("JMP")]) => Jump::ALL,
            None => Jump::NONE, // blank semicolon is okay
            _ => return Err(ParseError::Jump),
        };
        if tokens.get(semi + 2).is_some() {
            return Err(ParseError::Trailing);
        }
        Ok(jmp)
    }
}

#[cfg(test)]
mod test {
    use super::ParseError;
    use super::{Comp, Dest, Jump};
    use super::{Kind, Token};
    use crate::{cmp, T};

    macro_rules! tok {
        ( $($kind:tt)* ) => { Token{ line: 0, kind: T![$($kind)*] } };
    }

    #[test]
    fn jump() {
        let tokens = &vec![tok!(;), tok!(ident("JGT"))];
        assert_eq!(tokens.try_into(), Ok(Jump::GT));

        let tokens = &vec![tok!(ident("D")), tok!(;), tok!(ident("JNE"))];
        assert_eq!(tokens.try_into(), Ok(Jump::NE));

        let tokens = &vec![tok!(@), tok!(ident("LOOP"))];
        assert_eq!(tokens.try_into(), Ok(Jump::NONE));

        let tokens = &vec![tok!(;), tok!(ident("OOP"))];
        assert_eq!(TryInto::<Jump>::try_into(tokens), Err(ParseError::Jump));
    }

    #[test]
    fn dest() {
        let test = |input| {
            let tokens = &vec![tok!(ident(input)), tok!(=)];
            TryInto::<Dest>::try_into(tokens)
        };

        assert_eq!(test("D"), Ok(Dest::D));
        assert_eq!(test("MD"), Ok(Dest::D | Dest::M));
        assert_eq!(test("DM"), Ok(Dest::D | Dest::M));
        assert_eq!(test("ADM"), Ok(Dest::A | Dest::D | Dest::M));
        assert_eq!(test("ADAMDADA"), Ok(Dest::A | Dest::D | Dest::M));
        assert_eq!(test("X"), Err(ParseError::Dest));

        let tokens = &vec![tok!(@), tok!(number(100))];
        assert_eq!(tokens.try_into(), Ok(Dest::NONE));
    }

    #[test]
    fn comp() {
        let tokens = &vec![tok!(-), tok!(ident("M"))];
        assert_eq!(tokens.try_into(), Ok(cmp![-M]));

        let tokens = &vec![
            tok!(ident("M")),
            tok!(=),
            tok!(ident("D")),
            tok!(&),
            tok!(ident("A")),
        ];
        assert_eq!(tokens.try_into(), Ok(cmp![D & A]));

        let tokens = &vec![
            tok!(ident("M")),
            tok!(=),
            tok!(ident("D")),
            tok!(;),
            tok!(ident("JGT")),
        ];
        assert_eq!(tokens.try_into(), Ok(cmp![D]));
    }
}
