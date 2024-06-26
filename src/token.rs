#[warn(dead_code)]
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(isize),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Eq,
    NotEq,

    Lt,
    Gt,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    // Data
    String(String),
}

impl Token {
    pub fn string(&self) -> String {
        match self {
            Token::Assign => "=".to_string(),
            Token::Bang => "!".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Slash => "/".to_string(),
            Token::Eq => "==".to_string(),
            Token::NotEq => "!=".to_string(),
            Token::Gt => ">".to_string(),
            Token::Lt => "<".to_string(),
            Token::Lparen => "(".to_string(),
            Token::Rparen => ")".to_string(),
            Token::False => "false".to_string(),
            Token::True => "true".to_string(),
            Token::Comma => ",".to_string(),
            Token::Int(value) => value.to_string(),
            Token::Ident(value) => value.to_string(),
            t => format!("{:?}", t),
        }
    }
}
