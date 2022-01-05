use super::token::{Kind, Token};
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

/// Iterate over ASCII characters of a source and emit Tokens.
pub struct Lexer<'a> {
    source: &'a str,
    iter: Peekable<Enumerate<Chars<'a>>>,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            source: s,
            iter: s.chars().enumerate().peekable(),
            line: 1,
        }
    }

    /// Advance the iterator while f(char) == true and return the
    /// end index (exclusive).
    fn next_while(&mut self, f: fn(char) -> bool) -> Option<usize> {
        loop {
            let next = self.iter.peek();
            match next {
                None => return Some(self.source.len()),
                Some(tok) => {
                    if !f(tok.1) {
                        return Some(tok.0);
                    }
                }
            }
            self.iter.next();
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // skip whitespace
        self.next_while(|c| c.is_whitespace() && c != '\n')?;

        let (start, c) = self.iter.next()?;
        let kind = match c {
            '/' => {
                let (_, next) = self.iter.peek()?;
                if *next != '/' {
                    Kind::Error
                } else {
                    let end = self.next_while(|c| c != '\n')?;
                    Kind::Comment(&self.source[start..end])
                }
            }
            '0'..='9' => {
                let end = self.next_while(|c| c.is_ascii_digit())?;
                match self.source[start..end].parse() {
                    Ok(n) => Kind::Number(n),
                    _ => Kind::Error,
                }
            }
            'a'..='z' | 'A'..='Z' => {
                let end =
                    self.next_while(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-'))?;
                Kind::Ident(&self.source[start..end])
            }
            '!' => Kind::Bang,
            '@' => Kind::At,
            '=' => Kind::Equal,
            '+' => Kind::Plus,
            '-' => Kind::Minus,
            '&' => Kind::And,
            '|' => Kind::Or,
            ';' => Kind::Semicolon,
            '(' => Kind::LParen,
            ')' => Kind::RParen,
            '\n' => Kind::Newline,
            _ => return None,
        };
        let tok = Token {
            kind: kind,
            line: self.line,
        };
        if kind == Kind::Newline {
            self.line += 1;
        }
        Some(tok)
    }
}

#[cfg(test)]
mod test {
    use super::Kind;
    use super::Lexer;
    use crate::T;
    // use hack_as_macros::Tpp;

    #[test]
    #[rustfmt::skip]
    fn kinds() {
        let kinds: Vec<_> = Lexer::new("// comment\n123 foo !   @=+-\t&\n\n|;\n(LOOP-1)\n")
            .map(|t| t.kind)
            .collect();
        assert_eq!(
            kinds,
            vec![
                T![comment("// comment")], T!['\n'],
                T![number(123)],
                T![ident("foo")],
                T![!], T![@], T![=], T![+], T![-], T![&],
                T!['\n'], T!['\n'],
                T![|], T![;],
                T!['\n'],
                T!['('], T![ident("LOOP-1")], T![')'],
                T!['\n'],
            ]
        );
    }

    #[test]
    fn lines() {
        let lines: Vec<_> = Lexer::new("1\n2 // 2\n3 3 3\n").map(|t| t.line).collect();
        assert_eq!(lines, vec![1, 1, 2, 2, 2, 3, 3, 3, 3,]);
    }
}
