use std::iter::Enumerate;
use std::iter::Peekable;
use std::str::Chars;

/// Iterate over ASCII characters of a source and emit (Token, &str)
/// pairs.
pub struct Lexer<'a> {
    source: &'a str,
    iter: Peekable<Enumerate<Chars<'a>>>,
    line_number: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            source: s,
            iter: s.chars().enumerate().peekable(),
            line_number: 0,
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
    type Item = (Token, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        // skip whitespace
        self.next_while(|c| c.is_whitespace() && c != '\n')?;

        let (start, c) = self.iter.next()?;
        Some(match c {
            '/' => {
                let (_, next) = self.iter.peek()?;
                if *next != '/' {
                    (Token::Error, &self.source[start..=start])
                } else {
                    let end = self.next_while(|c| c != '\n')?;
                    (Token::Comment, &self.source[start..end])
                }
            }
            '0'..='9' => {
                let end = self.next_while(|c| c.is_ascii_digit())?;
                (Token::Number, &self.source[start..end])
            }
            'a'..='z' | 'A'..='Z' => {
                let end =
                    self.next_while(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-'))?;
                (Token::Ident, &self.source[start..end])
            }
            '!' => (Token::Bang, &self.source[start..=start]),
            '@' => (Token::At, &self.source[start..=start]),
            '=' => (Token::Equal, &self.source[start..=start]),
            '+' => (Token::Plus, &self.source[start..=start]),
            '-' => (Token::Minus, &self.source[start..=start]),
            '&' => (Token::And, &self.source[start..=start]),
            '|' => (Token::Or, &self.source[start..=start]),
            ';' => (Token::Semicolon, &self.source[start..=start]),
            '(' => (Token::LParen, &self.source[start..=start]),
            ')' => (Token::RParen, &self.source[start..=start]),
            '\n' => (Token::Newline, &self.source[start..=start]),
            _ => return None,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Token {
    Error,
    Comment,
    Number,
    Ident,
    Bang,
    At,
    Equal,
    Plus,
    Minus,
    And,
    Or,
    Semicolon,
    LParen,
    RParen,
    Newline,
}

#[macro_export]
macro_rules! T {
    [error] => { Token::Error };
    [comment($s:tt)] => { (Token::Comment, $s) };
    [number($s:tt)] => { (Token::Number, $s) };
    [ident($s:tt)] => { (Token::Ident, $s) };
    [!] => { (Token::Bang, "!") };
    [@] => { (Token::At, "@") };
    [=] => { (Token::Equal, "=") };
    [+] => { (Token::Plus, "+") };
    [-] => { (Token::Minus, "-") };
    [&] => { (Token::And, "&") };
    [|] => { (Token::Or, "|") };
    [;] => { (Token::Semicolon, ";") };
    ['('] => { (Token::LParen, "(") };
    [')'] => { (Token::RParen, ")") };
    ['\n'] => { (Token::Newline, "\n") };

    [$head:tt, $($tail:tt),+] => {
        vec![
            T![$head],
            $(T![$tail]),*
        ]
    };
}

// T![comment("foo"), '\n'] ==> vec![T![comment("foo")], T!['\n']]

#[cfg(test)]
mod test {
    use super::Lexer;
    use super::Token;

    #[test]
    fn tokenizer() {
        let lexer = Lexer::new("// comment\n123 foo !   @=+-\t&\n\n|;\n(LOOP-1)\n");
        assert_eq!(
            lexer.collect::<Vec<_>>(),
            vec![
                T![comment("// comment")],
                T!['\n'],
                T![number("123")],
                T![ident("foo")],
                T![!],
                T![@],
                T![=],
                T![+],
                T![-],
                T![&],
                T!['\n'],
                T!['\n'],
                T![|],
                T![;],
                T!['\n'],
                T!['('],
                T![ident("LOOP-1")],
                T![')'],
                T!['\n'],
            ]
        );
    }
}
