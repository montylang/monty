#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
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
    And,
    Or,
}

impl BinOp {
    fn precedence(&self) -> u8 {
        match self {
            BinOp::And | BinOp::Or => 11,
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

// TODO: Move elsewhere eventually
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Symbol(String),
    Call(Box<Expr>, Vec<Expr>),
}

type ParseResult<T> = Result<T, String>;

pub struct ParseContext {
    content: Vec<char>,
    pos: usize,
}

impl ParseContext {
    pub fn new(content: &str) -> ParseContext {
        ParseContext {
            content: content.chars().collect::<Vec<_>>(),
            pos: 0,
        }
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

pub fn parse_expr(ctx: &mut ParseContext) -> ParseResult<Expr> {
    parse_expr_prec(ctx, 0)
}

/*
 * Tries parsing operators until the precedence value doesn't meet requirement.
 * In other words, it recurses, but doesn't consume lower priority ops.
 */
fn parse_expr_prec(ctx: &mut ParseContext, precedence: u8) -> ParseResult<Expr> {
    consume_hs(ctx);
    let unchained = parse_expr_unchained(ctx)?;
    consume_hs(ctx);
    let mut expr = parse_calls(ctx, unchained)?;
    consume_hs(ctx);
    loop {
        let start = ctx.pos;
        match parse_binop(ctx) {
            Some(binop) => {
                let next_precedence = binop.precedence();
                if next_precedence >= precedence {
                    let rhs_expr = parse_expr_prec(ctx, next_precedence)?;
                    expr = Expr::BinOp(binop, Box::new(expr), Box::new(rhs_expr))
                } else {
                    ctx.pos = start;
                    return Ok(expr);
                }
            }
            None => return Ok(expr),
        }
    }
}

fn parse_expr_unchained(ctx: &mut ParseContext) -> ParseResult<Expr> {
    match ctx.peek() {
        Some(c) if c.is_numeric() => parse_num(ctx),
        Some(c) if is_symbol_char(c) => Ok(parse_symbol(ctx)),
        Some('(') => {
            ctx.pos += 1;
            let result = parse_expr(ctx)?;
            if !parse_char(ctx, ')') {
                println!("{}", ctx.pos);
                return Err("Expected `)`".to_owned());
            }
            Ok(result)
        }
        Some(c) => Err(format!("Expected expr, found {c}")),
        None => todo!(),
    }
}

fn parse_calls(ctx: &mut ParseContext, base: Expr) -> ParseResult<Expr> {
    match ctx.peek() {
        Some('(') => {
            ctx.pos += 1;
            let args = parse_arg_list(ctx)?;
            Ok(Expr::Call(Box::new(base), args))
        }
        _ => Ok(base),
    }
}

/// Expects opening `(` to have been parsed already
fn parse_arg_list(ctx: &mut ParseContext) -> ParseResult<Vec<Expr>> {
    let mut args = vec![];
    consume_hs(ctx);
    while ctx.peek() != Some(')') {
        args.push(parse_expr(ctx)?);
        consume_hs(ctx);
        if !parse_char(ctx, ',') {
            break;
        }
        consume_hs(ctx);
    }
    if !parse_char(ctx, ')') {
        Err(format!("Expected `)`, found {:?}", ctx.peek()))
    } else {
        Ok(args)
    }
}

/// Assumes ctx is already at the start of a number
fn parse_num(ctx: &mut ParseContext) -> ParseResult<Expr> {
    let start = ctx.pos;
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_digit_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }
    if let Ok(value) = s.parse::<i64>() {
        Ok(Expr::Int(value))
    } else if let Ok(value) = s.parse::<f64>() {
        Ok(Expr::Float(value))
    } else {
        ctx.pos = start;
        Err("Invalid number".to_owned())
    }
}

/// Assumes ctx is already at the start of a symbol and first char isn't a number
fn parse_symbol(ctx: &mut ParseContext) -> Expr {
    let mut end_pos = ctx.pos;
    while end_pos < ctx.content.len() && is_symbol_char(ctx.content[end_pos]) {
        end_pos += 1;
    }
    let result = ctx.content[ctx.pos..end_pos].iter().collect::<String>();
    ctx.pos = end_pos;
    Expr::Symbol(result)
}

fn parse_char(ctx: &mut ParseContext, expected: char) -> bool {
    match ctx.peek() {
        Some(c) if c == expected => {
            ctx.pos += 1;
            true
        }
        _ => false,
    }
}

fn parse_until_symbol_boundary(ctx: &mut ParseContext, sym: &str) -> bool {
    let mut i = 0;
    for c in sym.chars() {
        // If it's legit
        if ctx.pos + i < ctx.content.len() && ctx.content[ctx.pos + i] == c {
            i += 1;
        } else {
            return false;
        }
    }
    if i == sym.len() {
        if ctx.pos + i == ctx.content.len() || !is_symbol_char(ctx.content[ctx.pos + i]) {
            // Not quite!
            ctx.pos += sym.len();
            true
        } else {
            false
        }
    } else {
        false
    }
}

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
        Some('=') if parse_char(ctx, '=') => Some(BinOp::Eq),
        Some('!') if parse_char(ctx, '=') => Some(BinOp::Ne),
        Some('<') => Some(match ctx.peek() {
            Some('<') => {
                ctx.pos += 1;
                BinOp::ShiftLeft
            }
            Some('=') => {
                ctx.pos += 1;
                BinOp::Le
            }
            Some('>') => {
                panic!("What the hell is the `<>` operator?");
            }
            _ => BinOp::Lt,
        }),
        Some('>') => Some(match ctx.peek() {
            Some('>') => {
                ctx.pos += 1;
                BinOp::ShiftRight
            }
            Some('=') => {
                ctx.pos += 1;
                BinOp::Ge
            }
            _ => BinOp::Gt,
        }),
        Some('a') if parse_until_symbol_boundary(ctx, "nd") => Some(BinOp::And),
        Some('o') if parse_until_symbol_boundary(ctx, "r") => Some(BinOp::Or),
        _ => {
            ctx.pos -= 1;
            None
        }
    }
}

/// Consumes any amount of horizontal space
fn consume_hs(ctx: &mut ParseContext) {
    loop {
        match ctx.peek() {
            Some(' ') => ctx.pos += 1,
            Some('\t') => ctx.pos += 1,
            Some('#') => {
                consume_comment(ctx);
                return;
            }
            _ => return,
        }
    }
}

/// Expects to already be at a # character
fn consume_comment(ctx: &mut ParseContext) {
    while ctx.peek() != Some('\n') {
        ctx.pos += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_num() {
        assert_eq!(
            Ok(Expr::Int(1234)),
            parse_num(&mut ParseContext::new("1234"))
        );
        assert_eq!(
            Ok(Expr::Float(3.14)),
            parse_num(&mut ParseContext::new("3.14"))
        );
        assert_eq!(
            Ok(Expr::Int(1234)),
            parse_num(&mut ParseContext::new("1234"))
        );
        assert_eq!(
            Ok(Expr::Float(3.14)),
            parse_num(&mut ParseContext::new("3.14"))
        );
        assert_eq!(
            Ok(Expr::Float(0.0)),
            parse_num(&mut ParseContext::new("0.0"))
        );
        assert_eq!(Ok(Expr::Int(0)), parse_num(&mut ParseContext::new("0")));
    }

    #[test]
    fn test_parse_symbol() {
        assert_eq!(
            Expr::Symbol("abcd".to_owned()),
            parse_symbol(&mut ParseContext::new("abcd"))
        );
        assert_eq!(
            Expr::Symbol("abc123٣".to_owned()),
            parse_symbol(&mut ParseContext::new("abc123٣"))
        );
    }

    #[test]
    fn test_parse_char() {
        assert!(parse_char(&mut ParseContext::new("a"), 'a'));
        assert!(!parse_char(&mut ParseContext::new("b"), 'a'));
        assert!(!parse_char(&mut ParseContext::new(""), 'a'));
    }

    #[test]
    fn test_parse_binop() {
        assert_eq!(
            Some(BinOp::Add),
            parse_binop(&mut ParseContext::new("+237"))
        );
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
        assert_eq!(
            Some(BinOp::ShiftRight),
            parse_binop(&mut ParseContext::new(">>"))
        );
        assert_eq!(
            Some(BinOp::ShiftLeft),
            parse_binop(&mut ParseContext::new("<<"))
        );
        assert_eq!(
            Some(BinOp::BitAnd),
            parse_binop(&mut ParseContext::new("&"))
        );
        assert_eq!(Some(BinOp::BitOr), parse_binop(&mut ParseContext::new("|")));
        assert_eq!(
            Some(BinOp::BitXor),
            parse_binop(&mut ParseContext::new("^"))
        );
        assert_eq!(Some(BinOp::And), parse_binop(&mut ParseContext::new("and")));
        assert_eq!(Some(BinOp::Or), parse_binop(&mut ParseContext::new("or")));
        assert_eq!(None, parse_binop(&mut ParseContext::new("ori")));
        assert_eq!(None, parse_binop(&mut ParseContext::new("andi")));
    }

    #[test]
    fn test_parse_precedence() {
        assert_eq!(
            Ok(Expr::BinOp(
                BinOp::Add,
                Box::new(Expr::Symbol("a".to_owned())),
                Box::new(Expr::Symbol("b".to_owned()))
            )),
            parse_expr(&mut ParseContext::new("a+b"))
        );
        assert_eq!(
            Ok(Expr::BinOp(
                BinOp::Add,
                Box::new(Expr::BinOp(
                    BinOp::Mul,
                    Box::new(Expr::Symbol("a".to_owned())),
                    Box::new(Expr::Symbol("b".to_owned()))
                )),
                Box::new(Expr::Symbol("c".to_owned()))
            )),
            parse_expr(&mut ParseContext::new("a*b+c"))
        );
        assert_eq!(
            Ok(Expr::BinOp(
                BinOp::Add,
                Box::new(Expr::Symbol("a".to_owned())),
                Box::new(Expr::BinOp(
                    BinOp::Mul,
                    Box::new(Expr::Symbol("b".to_owned())),
                    Box::new(Expr::Symbol("c".to_owned()))
                ))
            )),
            parse_expr(&mut ParseContext::new("a+b*c"))
        );
        assert_eq!(
            Ok(Expr::BinOp(
                BinOp::Mul,
                Box::new(Expr::BinOp(
                    BinOp::Add,
                    Box::new(Expr::Symbol("a".to_owned())),
                    Box::new(Expr::Symbol("b".to_owned()))
                )),
                Box::new(Expr::Symbol("c".to_owned()))
            )),
            parse_expr(&mut ParseContext::new("(a +b \t )*   c"))
        );
    }

    #[test]
    fn test_parse_call() {
        assert_eq!(
            Ok(Expr::Call(
                Box::new(Expr::Symbol("f".to_owned())),
                vec![Expr::Int(4), Expr::Symbol("henlo".to_owned()),],
            )),
            parse_expr(&mut ParseContext::new("f(4, henlo)"))
        );
        assert_eq!(
            Ok(Expr::Call(
                Box::new(Expr::BinOp(
                    BinOp::Add,
                    Box::new(Expr::Symbol("f".to_owned())),
                    Box::new(Expr::Int(3)),
                )),
                vec![Expr::Int(4), Expr::Symbol("henlo".to_owned()),],
            )),
            parse_expr(&mut ParseContext::new("(f + 3)(4, henlo)"))
        );
    }
}
