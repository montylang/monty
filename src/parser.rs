#[derive(Debug, Clone, PartialEq)]
enum BinOp {
    Add,
    Sub,
    Div,
    Mod,
    Mul,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    ShiftRight,
    ShiftLeft,
    BitAnd,
    BitOr,
    BitXor,
}

impl BinOp {
    fn precedence(&self) -> i64 {
        match self {
            BinOp::Div | BinOp::Mod | BinOp::Mul => 10,
            BinOp::Add | BinOp::Sub => 9,
            BinOp::ShiftLeft | BinOp::ShiftRight => 8,
            BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le => 7,
            BinOp::Eq | BinOp::Ne => 6,
            BinOp::BitAnd => 5,
            BinOp::BitXor => 4,
            BinOp::BitOr => 3,
        }
    }
}

// TODO: Move elsewhere eventu&mut ally
#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Int(i64),
    Float(f64),
}

type ParseResult<T> = Result<T, String>;

pub struct ParseContext {
    content: Vec<char>,
    pos: usize,
}

impl ParseContext {
    fn new(content: &str) -> ParseContext {
        ParseContext {
            content: content.chars().collect::<Vec<_>>(),
            pos: 0,
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.content.len()
    }

    fn peek(&self) -> Option<char> {
        self.content.get(self.pos).cloned()
    }

    fn pop(&mut self) -> Option<char> {
        if self.pos < self.content.len() {
            self.pos += 1;
            Some(self.content[self.pos - 1])
        } else {
            None
        }
    }
}

fn is_digit_char(c: char) -> bool {
    c == '.' || c.is_ascii_digit()
}

fn is_symbol_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Assumes ctx is already at the start of a number
fn parse_num(ctx: &mut ParseContext) -> ParseResult<Expr> {
    let is_neg = if ctx.peek() == Some('-') {
        ctx.pos += 1;
        true
    } else {
        false
    };
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_digit_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }
    if s.is_empty() {
        // Lonely - symbol.. maybe this should return a unary `-` op instead
        Err("Invalid integer".to_owned())
    } else if let Ok(value) = s.parse::<i64>() {
        Ok(Expr::Int(if is_neg { -value } else { value }))
    } else if let Ok(value) = s.parse::<f64>() {
        Ok(Expr::Float(if is_neg { -value } else { value }))
    } else {
        Err("Invalid number".to_owned())
    }
}

/// Assumes ctx is already at the start of a symbol and first char isn't a number
fn parse_symbol(ctx: &mut ParseContext) -> String {
    let mut end_pos = ctx.pos;
    while end_pos < ctx.content.len() && is_symbol_char(ctx.content[end_pos]) {
        end_pos += 1;
    }
    let result = ctx.content[ctx.pos..end_pos].iter().collect::<String>();
    ctx.pos = end_pos;
    result
}

fn parse_char(ctx: &mut ParseContext, expected: char) -> ParseResult<()> {
    match ctx.peek() {
        Some(c) if c == expected => {
            ctx.pos += 1;
            Ok(())
        },
        Some(c) => Err(format!("Expected `{expected}`, found `{c}`")),
        _ => Err(format!("Expected `{expected}`, found EOF")),
    }
}

/// Assumes the current char is the start of a binop
fn parse_binop(ctx: &mut ParseContext) -> Option<BinOp> {
    match ctx.pop() {
        Some('+') => Some(BinOp::Add),
        Some('-') => Some(BinOp::Sub),
        Some('/') => Some(BinOp::Div),
        Some('%') => Some(BinOp::Mod),
        Some('*') => Some(BinOp::Mul),
        Some('&') => Some(BinOp::BitAnd),
        Some('|') => Some(BinOp::BitOr),
        Some('^') => Some(BinOp::BitXor),
        Some('=') if parse_char(ctx, '=').is_ok() => Some(BinOp::Eq),
        Some('!') if parse_char(ctx, '=').is_ok() => Some(BinOp::Ne),
        Some('<') => Some(match ctx.peek() {
            Some('<') => {
                ctx.pos += 1;
                BinOp::ShiftLeft
            },
            Some('=') => {
                ctx.pos += 1;
                BinOp::Le
            },
            Some('>') => {
                panic!("What the hell is the `<>` operator?");
            },
            _ => BinOp::Lt,
        }),
        Some('>') => Some(match ctx.peek() {
            Some('>') => {
                ctx.pos += 1;
                BinOp::ShiftRight
            },
            Some('=') => {
                ctx.pos += 1;
                BinOp::Ge
            },
            _ => BinOp::Gt,
        }),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_num() {
        assert_eq!(Ok(Expr::Int(1234)), parse_num(&mut ParseContext::new("1234")));
        assert_eq!(Ok(Expr::Float(3.14)), parse_num(&mut ParseContext::new("3.14")));
        assert_eq!(Ok(Expr::Int(-1234)), parse_num(&mut ParseContext::new("-1234")));
        assert_eq!(Ok(Expr::Float(-3.14)), parse_num(&mut ParseContext::new("-3.14")));
        assert_eq!(Ok(Expr::Float(0.0)), parse_num(&mut ParseContext::new("-0.0")));
        assert_eq!(Ok(Expr::Float(0.0)), parse_num(&mut ParseContext::new("0.0")));
        assert_eq!(Ok(Expr::Int(0)), parse_num(&mut ParseContext::new("0")));
        assert_eq!(Ok(Expr::Int(0)), parse_num(&mut ParseContext::new("-0")));
    }

    #[test]
    fn test_parse_symbol() {
        assert_eq!("abcd", parse_symbol(&mut ParseContext::new("abcd")));
        assert_eq!("abc123٣", parse_symbol(&mut ParseContext::new("abc123٣")));
    }

    #[test]
    fn test_parse_char() {
        assert!(parse_char(&mut ParseContext::new("a"), 'a').is_ok());
        assert!(parse_char(&mut ParseContext::new("b"), 'a').is_err());
        assert!(parse_char(&mut ParseContext::new(""), 'a').is_err());
    }

    #[test]
    fn test_parse_binop() {
        assert_eq!(Some(BinOp::Add), parse_binop(&mut ParseContext::new("+237")));
        assert_eq!(Some(BinOp::Sub), parse_binop(&mut ParseContext::new("->")));
        // Nice try
        assert_eq!(Some(BinOp::Div), parse_binop(&mut ParseContext::new("//")));
        assert_eq!(Some(BinOp::Mod), parse_binop(&mut ParseContext::new("%")));
        assert_eq!(Some(BinOp::Mul), parse_binop(&mut ParseContext::new("*")));
        assert_eq!(Some(BinOp::Eq), parse_binop(&mut ParseContext::new("==")));
        assert_eq!(None, parse_binop(&mut ParseContext::new("=")));
        assert_eq!(Some(BinOp::Ne), parse_binop(&mut ParseContext::new("!=")));
        assert_eq!(Some(BinOp::Lt), parse_binop(&mut ParseContext::new("<")));
        assert_eq!(Some(BinOp::Le), parse_binop(&mut ParseContext::new("<=")));
        assert_eq!(Some(BinOp::Gt), parse_binop(&mut ParseContext::new(">")));
        assert_eq!(Some(BinOp::Ge), parse_binop(&mut ParseContext::new(">=")));
        assert_eq!(Some(BinOp::ShiftRight), parse_binop(&mut ParseContext::new(">>")));
        assert_eq!(Some(BinOp::ShiftLeft), parse_binop(&mut ParseContext::new("<<")));
        assert_eq!(Some(BinOp::BitAnd), parse_binop(&mut ParseContext::new("&")));
        assert_eq!(Some(BinOp::BitOr), parse_binop(&mut ParseContext::new("|")));
        assert_eq!(Some(BinOp::BitXor), parse_binop(&mut ParseContext::new("^")));
    }
}
