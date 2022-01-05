#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: Kind<'a>,
    pub line: usize,
}

#[rustfmt::skip]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kind<'a> {
    Error,
    Comment(&'a str),
    Number(u16),
    Ident(&'a str),
    Bang, At, Equal, Plus, Minus,
    And, Or, Semicolon,
    LParen, RParen, Newline,
}

#[macro_export]
macro_rules! T {
    [error] => { Kind::Error };
    [comment($s:tt)] => { Kind::Comment($s) };
    [number($s:tt)] => { Kind::Number($s) };
    [ident($s:tt)] => { Kind::Ident($s) };
    [!] => { Kind::Bang };
    [@] => { Kind::At };
    [=] => { Kind::Equal };
    [+] => { Kind::Plus };
    [-] => { Kind::Minus };
    [&] => { Kind::And };
    [|] => { Kind::Or };
    [;] => { Kind::Semicolon };
    ['('] => { Kind::LParen };
    [')'] => { Kind::RParen };
    ['\n'] => { Kind::Newline };
}
