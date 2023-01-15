use std::{cmp::Ordering, io::{stdout, Write}};

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
    And,
    Or,
    Mappend,
    Cons,
}

impl BinOp {
    fn precedence(&self) -> u8 {
        match self {
            BinOp::Cons => 13,
            BinOp::Mappend => 12,
            BinOp::And | BinOp::Or => 11,
            BinOp::Div | BinOp::Mod | BinOp::Mul => 10,
            BinOp::Add | BinOp::Sub => 9,
            BinOp::ShiftLeft | BinOp::ShiftRight => 8,
            BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le => 7,
            BinOp::Eq | BinOp::Ne => 6,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    Int(i64),
    Float(f64),
    Symbol(String),
    ClassPattern(String, Vec<Arg>),
    Tuple(Vec<Arg>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Num {
    Int(i64),
    Float(f64),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeFunSig {
    name: String,
    args: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassVariant {
    name: String,
    fields: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Type {
        name: String,
        sigs: Vec<TypeFunSig>,
    },
    Class {
        name: String,
        variants: Vec<ClassVariant>,
    },
    Instance {
        class_name: String,
        type_name: String,
        implementations: Vec<Expr>,
    },
    Expr(Expr),
}

// TODO: Move elsewhere eventually
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Char(char),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Symbol(String),
    Call(Box<Expr>, Vec<Expr>),
    Lambda(Vec<Arg>, Vec<Expr>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
    Return(Box<Expr>),
    Assignment(Arg, Box<Expr>),
}

type ParseResult<T> = Result<T, String>;

pub struct ParseContext {
    content: Vec<char>,
    pos: usize,
    indent_stack: Vec<usize>,
}

impl ParseContext {
    pub fn new(content: &str) -> ParseContext {
        ParseContext {
            content: content.chars().collect::<Vec<_>>(),
            pos: 0,
            indent_stack: vec![],
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

    fn indent_len(&self) -> usize {
        self.indent_stack.iter().sum()
    }

    fn at_eof(&self) -> bool {
        self.pos >= self.content.len()
    }
}

fn stdout_formatting_bold_red() {
    stdout().write_all(&[0x1b, 0x5b, 0x31, 0x3b, 0x33, 0x31, 0x6d]).unwrap();
}

fn stdout_formatting_bold() {
    stdout().write_all(&[0x1b, 0x5b, 0x31, 0x6d]).unwrap();
}

fn stdout_formatting_clear() {
    stdout().write_all(&[0x1b, 0x5b, 0x30, 0x6d]).unwrap();
}

pub fn pretty_print_error(ctx: &ParseContext, message: &str) {
    let mut start = ctx.pos.min(ctx.content.len() - 1);
    let mut end = start;
    for i in start..ctx.content.len() {
        if ctx.content[i] == '\n' {
            break;
        }
        end = i;
    }
    for i in (0..=start).rev() {
        if ctx.content[i] == '\n' {
            break;
        }
        start = i;
    }
    let line_num = 1 + ctx.content.iter()
        .take(ctx.pos)
        .filter(|c| **c == '\n')
        .count();
    let column_num = ctx.pos - start;
    let line_num_str = format!("{line_num}: ");
    stdout_formatting_bold_red();
    println!("Parse error:\n");
    stdout_formatting_clear();
    println!("{line_num_str}{}", ctx.content[start..=end].iter().collect::<String>());
    println!("{}^", str::repeat(" ", column_num + line_num_str.len()));
    stdout_formatting_bold();
    println!("{message}");
    stdout_formatting_clear();
    println!();
}

fn is_digit_char(c: char) -> bool {
    c == '.' || c.is_ascii_digit()
}

fn is_symbol_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

pub fn parse_file_string(content: &str) -> ParseResult<Vec<Statement>> {
    let mut ctx = ParseContext::new(content);
    let res = begin_parsing(&mut ctx);
    if let Err(message) = &res {
        pretty_print_error(&ctx, message);
    }
    res
}

fn begin_parsing(ctx: &mut ParseContext) -> ParseResult<Vec<Statement>> {
    let mut statements = vec![];
    loop {
        consume_blank_lines(ctx);
        if ctx.at_eof() {
            break;
        }
        statements.push(parse_statement(ctx)?);
    }
    Ok(statements)
}

pub fn parse_expr(ctx: &mut ParseContext) -> ParseResult<Expr> {
    consume_hs(ctx);
    match ctx.pop() {
        Some('d') if parse_until_symbol_boundary(ctx, "ef") => parse_expr_def(ctx),
        Some('r') if parse_until_symbol_boundary(ctx, "eturn") => parse_expr_return(ctx),
        _ => {
            ctx.pos -= 1;
            parse_expr_prec(ctx, 0)
        }
    }
}

pub fn parse_statement(ctx: &mut ParseContext) -> ParseResult<Statement> {
    consume_blank_lines(ctx);
    match ctx.pop() {
        Some('t') if parse_until_symbol_boundary(ctx, "ype") => parse_statement_type(ctx),
        Some('c') if parse_until_symbol_boundary(ctx, "lass") => parse_statement_class(ctx),
        Some('i') if parse_until_symbol_boundary(ctx, "nstance") => parse_statement_instance(ctx),
        _ => {
            ctx.pos -= 1;
            let expr = Statement::Expr(parse_expr(ctx)?);
            consume_hs(ctx);
            if !ctx.at_eof() {
                parse_char(ctx, '\n')?;
            }
            Ok(expr)
        }
    }
}

fn parse_indented_items<T>(
    ctx: &mut ParseContext,
    item_parser: fn(&mut ParseContext) -> ParseResult<T>,
) -> ParseResult<Vec<T>> {
    // TODO: Move this into parse_indent to avoid wasting precious time
    consume_blank_lines(ctx);
    parse_new_block_indent(ctx)?;
    let mut items = vec![item_parser(ctx)?];
    let block_indent = ctx.indent_len();
    loop {
        let start_pos = ctx.pos;
        consume_blank_lines(ctx);
        match parse_indent(ctx).cmp(&block_indent) {
            Ordering::Equal => items.push(item_parser(ctx)?),
            Ordering::Greater => {
                ctx.pos = start_pos;
                return Err("Too much indentation".to_owned());
            }
            Ordering::Less => {
                ctx.pos = start_pos;
                ctx.indent_stack.pop();
                break;
            }
        }
    }
    Ok(items)
}

/// Expects `type` to have been parsed
fn parse_statement_type(ctx: &mut ParseContext) -> ParseResult<Statement> {
    consume_hs(ctx);
    let name = parse_cap_ident(ctx)?;
    consume_hs(ctx);
    parse_char(ctx, ':')?;
    consume_hs(ctx);
    parse_char(ctx, '\n')?;
    let sigs = parse_indented_items(ctx, parse_type_sig)?;
    Ok(Statement::Type { name, sigs })
}

fn parse_type_sig(ctx: &mut ParseContext) -> ParseResult<TypeFunSig> {
    parse_expected_symbol(ctx, "def")?;
    consume_hs(ctx);
    let name = parse_symbol_name(ctx);
    if name.is_empty() {
        return Err("Expected function def name".to_owned());
    }
    consume_hs(ctx);
    parse_char(ctx, '(')?;
    consume_hs(ctx);
    let args = parse_type_sig_def_args(ctx)?;
    consume_hs(ctx);
    // XXX: Ignore the `-> self` for now. We _miiiiight_ not need it
    match ctx.pop() {
        Some('-') => {
            parse_char(ctx, '>')?;
            consume_hs(ctx);
            parse_expected_symbol(ctx, "self")?;
            consume_hs(ctx);
            if !ctx.at_eof() {
                parse_char(ctx, '\n')?;
            }
        }
        Some('\n') => {}
        _ => return Err("Expected EOL or `->`".to_owned()),
    }
    Ok(TypeFunSig { name, args })
}

fn parse_type_sig_def_args(ctx: &mut ParseContext) -> ParseResult<Vec<String>> {
    parse_delimited_in_parens(ctx, ')', |ctx| Ok(parse_symbol_name(ctx)))
}

fn parse_expected_symbol(ctx: &mut ParseContext, expected: &str) -> ParseResult<()> {
    let pos = ctx.pos;
    let found = parse_symbol_name(ctx);
    if found != expected {
        ctx.pos = pos;
        Err(format!("Expected {expected}, found {found}"))
    } else {
        Ok(())
    }
}

fn parse_cap_ident(ctx: &mut ParseContext) -> ParseResult<String> {
    match ctx.peek() {
        Some(c) if c.is_ascii_uppercase() => Ok(parse_symbol_name(ctx)),
        Some(c) => Err(format!("Expected capitalized identifier, found {c}")),
        None => Err("Expected capitalized identifier, found EOF".to_owned()),
    }
}

/// Expects `class` to have been parsed
fn parse_statement_class(ctx: &mut ParseContext) -> ParseResult<Statement> {
    consume_hs(ctx);
    let name = parse_cap_ident(ctx)?;
    consume_hs(ctx);
    parse_char(ctx, ':')?;
    consume_hs(ctx);
    parse_char(ctx, '\n')?;
    let variants = parse_indented_items(ctx, parse_class_variant)?;
    Ok(Statement::Class { name, variants })
}

fn parse_class_variant(ctx: &mut ParseContext) -> ParseResult<ClassVariant> {
    let name = parse_cap_ident(ctx)?;

    consume_hs(ctx);
    let fields = if parse_char(ctx, '(').is_ok() {
        parse_delimited_in_parens(ctx, ')', |ctx| Ok(parse_symbol_name(ctx)))?
    } else {
        vec![]
    };
    Ok(ClassVariant { name, fields })
}

/// Expects `instance` to have been parsed
fn parse_statement_instance(ctx: &mut ParseContext) -> ParseResult<Statement> {
    consume_hs(ctx);
    let class_name = parse_cap_ident(ctx)?;
    consume_hs(ctx);
    parse_expected_symbol(ctx, "of")?;
    consume_hs(ctx);
    let type_name = parse_cap_ident(ctx)?;
    consume_hs(ctx);
    parse_char(ctx, ':')?;
    consume_hs(ctx);
    parse_char(ctx, '\n')?;
    let implementations = parse_indented_items(ctx, |ctx| {
        parse_expected_symbol(ctx, "def")?;
        parse_expr_def(ctx)
    })?;
    Ok(Statement::Instance {
        class_name,
        type_name,
        implementations,
    })
}

fn parse_expr_return(ctx: &mut ParseContext) -> ParseResult<Expr> {
    consume_hs(ctx);
    Ok(Expr::Return(Box::new(parse_expr(ctx)?)))
}

/// Expects `def` to have been parsed
fn parse_expr_def(ctx: &mut ParseContext) -> ParseResult<Expr> {
    consume_hs(ctx);
    let name = ctx
        .peek()
        .filter(|&c| is_symbol_char(c))
        .map(|_| parse_symbol_name(ctx));
    consume_hs(ctx);
    parse_char(ctx, '(')?;
    let args = parse_args(ctx)?;
    consume_hs(ctx);
    parse_char(ctx, ':')?;
    consume_hs(ctx);
    // We're finally here, BOYS
    parse_char(ctx, '\n')?;

    let mut body = parse_indented_items(ctx, parse_expr)?;
    match body.pop().unwrap() {
        Expr::Return(ret_val) => {
            body.push(*ret_val);
        }
        _ => return Err("The last statement in a def must be a return".to_owned()),
    };

    let lambda = Expr::Lambda(args, body);
    Ok(if let Some(name) = name {
        Expr::Assignment(Arg::Symbol(name), Box::new(lambda))
    } else {
        lambda
    })
}

fn parse_indent(ctx: &mut ParseContext) -> usize {
    assert!(ctx.pos == 0 || ctx.content[ctx.pos - 1] == '\n' || ctx.at_eof());
    let mut depth = 0;
    loop {
        match ctx.pop() {
            Some(' ') => depth += 1,
            Some('\t') => depth += 4,
            _ => {
                ctx.pos -= 1;
                break;
            }
        }
    }
    depth
}

fn parse_new_block_indent(ctx: &mut ParseContext) -> ParseResult<()> {
    let start_pos = ctx.pos;
    let indent = parse_indent(ctx);
    let previous_indent = ctx.indent_len();
    if indent <= previous_indent {
        ctx.pos = start_pos;
        return Err("Expected a start of a new block".to_owned());
    }
    ctx.indent_stack.push(indent - previous_indent);
    Ok(())
}

fn try_parse_assign_op(ctx: &mut ParseContext) -> bool {
    if ctx.pos + 2 >= ctx.content.len() {
        false
    } else if ctx.content[ctx.pos] == '=' && ctx.content[ctx.pos + 1] != '=' {
        ctx.pos += 2;
        true
    } else {
        false
    }
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
    if try_parse_assign_op(ctx) {
        let arg = expr_to_arg(expr)?;
        let expr = parse_expr(ctx)?;
        return Ok(Expr::Assignment(arg, Box::new(expr)));
    }
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
        Some(c) if c.is_numeric() => Ok(match parse_num(ctx)? {
            Num::Int(value) => Expr::Int(value),
            Num::Float(value) => Expr::Float(value),
        }),
        Some(c) if is_symbol_char(c) => Ok(parse_expr_symbol(ctx)),
        Some('(') => {
            ctx.pos += 1;
            let first_expr = parse_expr(ctx)?;
            match ctx.peek() {
                Some(',') => {
                    ctx.pos += 1;
                    let mut args = parse_expr_args(ctx, ')')?;
                    args.insert(0, first_expr);
                    match maybe_parse_lambda(ctx, args.clone()) {
                        Some(res) => res,
                        None => Ok(Expr::Tuple(args)),
                    }
                }
                Some(')') => {
                    ctx.pos += 1;
                    match maybe_parse_lambda(ctx, vec![first_expr.clone()]) {
                        Some(res) => res,
                        None => Ok(first_expr),
                    }
                }
                _ => Err("Expected `)` or `,`".to_owned()),
            }
        }
        Some('[') => {
            ctx.pos += 1;
            Ok(Expr::List(parse_expr_args(ctx, ']')?))
        }
        Some('\'') => {
            ctx.pos += 1;
            let res = parse_maybe_escaped_char(ctx, '\'')?;
            parse_char(ctx, '\'')?;
            Ok(Expr::Char(res))
        }
        Some('\"') => {
            ctx.pos += 1;
            let chars = parse_escaped_body(ctx, '\"')?
                .iter()
                .copied()
                .map(Expr::Char)
                .collect();
            Ok(Expr::List(chars))
        }
        Some(c) => Err(format!("Expected expr, found {c}")),
        None => Err("Expected expr, found EOF".to_owned()),
    }
}

/// Expects the first opening char e.g. " or ' to have been parsed and eaten
fn parse_escaped_body(ctx: &mut ParseContext, terminal_char: char) -> ParseResult<Vec<char>> {
    let mut chars = vec![];
    loop {
        match ctx.peek() {
            Some(c) if c == terminal_char => break,
            _ => chars.push(parse_maybe_escaped_char(ctx, terminal_char)?),
        }
    }
    ctx.pos += 1;
    Ok(chars)
}

fn parse_maybe_escaped_char(ctx: &mut ParseContext, terminal_char: char) -> ParseResult<char> {
    match ctx.pop() {
        Some('\\') => parse_escaped_char(ctx),
        Some(c) if c == terminal_char => Err(format!(
            "Expected a char between `{terminal_char}` boundaries"
        )),
        Some(c) => Ok(c),
        None => Err(format!("Expected closing `{terminal_char}`, found EOF")),
    }
}

/// Expects the opening `\` to have been parsed
fn parse_escaped_char(ctx: &mut ParseContext) -> ParseResult<char> {
    match ctx.pop() {
        Some('t') => Ok('\t'),
        Some('0') => Ok('\0'),
        Some('n') => Ok('\n'),
        Some('r') => Ok('\r'),
        Some('"') => Ok('"'),
        Some('\'') => Ok('\''),
        Some('\\') => Ok('\\'),
        Some('x') => {
            let res = (parse_hex_char(ctx)? << 4) | parse_hex_char(ctx)?;
            Ok(res as char)
        }
        Some(c) => Err(format!("Invalid escape sequence: `\\{c}`")),
        None => Err("Expected escape character but hit EOF".to_owned()),
    }
}

fn parse_hex_char(ctx: &mut ParseContext) -> ParseResult<u8> {
    match ctx.pop().map(|c| c.to_ascii_lowercase()) {
        Some(c) if c.is_ascii_digit() => Ok(c as u8 - b'0'),
        Some(c @ 'a'..='f') => Ok(c as u8 - b'a' + 10),
        Some(c) => Err(format!("Invalid hex digit: {c}")),
        None => Err("Expected hex digit, found EOF".to_owned()),
    }
}

fn maybe_parse_lambda(ctx: &mut ParseContext, args: Vec<Expr>) -> Option<ParseResult<Expr>> {
    consume_hs(ctx);
    match ctx.peek() {
        Some(':') => {
            ctx.pos += 1;
            Some(parse_lambda(ctx, args))
        }
        _ => None,
    }
}

fn parse_lambda(ctx: &mut ParseContext, expr_args: Vec<Expr>) -> ParseResult<Expr> {
    let body_expr = parse_expr(ctx)?;
    let args = expr_args
        .into_iter()
        .map(expr_to_arg)
        .collect::<ParseResult<Vec<_>>>()?;
    Ok(Expr::Lambda(args, vec![body_expr]))
}

fn expr_to_arg(expr: Expr) -> ParseResult<Arg> {
    match expr {
        Expr::Int(v) => Ok(Arg::Int(v)),
        Expr::Float(v) => Ok(Arg::Float(v)),
        Expr::Symbol(v) => Ok(Arg::Symbol(v)),
        Expr::Tuple(values) => {
            let tuple_args = values
                .into_iter()
                .map(expr_to_arg)
                .collect::<ParseResult<Vec<_>>>()?;
            Ok(Arg::Tuple(tuple_args))
        }
        expr => Err(format!("{expr:?} is not a valid arg format!")),
    }
}

fn parse_calls(ctx: &mut ParseContext, base: Expr) -> ParseResult<Expr> {
    let mut res = base;
    loop {
        match ctx.peek() {
            Some('(') => {
                ctx.pos += 1;
                let args = parse_expr_args(ctx, ')')?;
                res = Expr::Call(Box::new(res), args)
            }
            Some('.') => {
                ctx.pos += 1;
                let sym = parse_expr_symbol(ctx);
                consume_hs(ctx);
                parse_char(ctx, '(')?;
                let mut args = parse_expr_args(ctx, ')')?;
                args.insert(0, res);
                res = Expr::Call(Box::new(sym), args)
            }
            _ => break,
        }
    }
    Ok(res)
}

/// Expects opening `(` to have been parsed already
fn parse_expr_args(ctx: &mut ParseContext, closing_char: char) -> ParseResult<Vec<Expr>> {
    parse_delimited_in_parens(ctx, closing_char, parse_expr)
}

/// Expects opening `(` to have been parsed already
fn parse_delimited_in_parens<T>(
    ctx: &mut ParseContext,
    closing_char: char,
    item_parser: fn(&mut ParseContext) -> ParseResult<T>,
) -> ParseResult<Vec<T>> {
    let mut args = vec![];
    consume_hs(ctx);
    while ctx.peek() != Some(closing_char) {
        args.push(item_parser(ctx)?);
        consume_hs(ctx);
        if parse_char(ctx, ',').is_err() {
            break;
        }
        consume_hs(ctx);
    }
    parse_char(ctx, closing_char)?;
    Ok(args)
}

/// Expects opening `(` to have been parsed already
fn parse_args(ctx: &mut ParseContext) -> ParseResult<Vec<Arg>> {
    parse_delimited_in_parens(ctx, ')', parse_arg)
}

fn parse_arg(ctx: &mut ParseContext) -> ParseResult<Arg> {
    match ctx.peek() {
        Some(c) if c.is_ascii_digit() => Ok(match parse_num(ctx)? {
            Num::Int(value) => Arg::Int(value),
            Num::Float(value) => Arg::Float(value),
        }),
        Some(c) if c.is_lowercase() || c == '_' => Ok(Arg::Symbol(parse_symbol_name(ctx))),
        // Pattern matched class type
        Some(c) if c.is_uppercase() => {
            let name = parse_symbol_name(ctx);
            consume_hs(ctx);

            Ok(Arg::ClassPattern(
                name,
                match parse_char(ctx, '(') {
                    Ok(_) => parse_delimited_in_parens(ctx, ')', parse_arg)?,
                    _ => vec![],
                },
            ))
        }
        Some('(') => {
            ctx.pos += 1;
            Ok(Arg::Tuple(parse_delimited_in_parens(ctx, ')', parse_arg)?))
        }
        _ => Err("Invalid function argument".to_owned()),
    }
}

/// Assumes ctx is already at the start of a number
fn parse_num(ctx: &mut ParseContext) -> ParseResult<Num> {
    let start = ctx.pos;
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_digit_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }
    if let Ok(value) = s.parse::<i64>() {
        Ok(Num::Int(value))
    } else if let Ok(value) = s.parse::<f64>() {
        Ok(Num::Float(value))
    } else {
        ctx.pos = start;
        Err("Invalid number".to_owned())
    }
}

/// Assumes ctx is already at the start of a symbol and first char isn't a number
fn parse_expr_symbol(ctx: &mut ParseContext) -> Expr {
    Expr::Symbol(parse_symbol_name(ctx))
}

fn parse_symbol_name(ctx: &mut ParseContext) -> String {
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
        }
        Some(c) => Err(format!("Expected {expected:#?} found {c:#?}")),
        None => Err(format!("Expected {expected:#?} found EOF")),
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
        Some('|') => Some(BinOp::Cons),
        Some('=') if parse_char(ctx, '=').is_ok() => Some(BinOp::Eq),
        Some('!') if parse_char(ctx, '=').is_ok() => Some(BinOp::Ne),
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
                ctx.pos += 1;
                BinOp::Mappend
            }
            _ => BinOp::Lt,
        }),
        Some('>') => Some(match ctx.peek() {
            Some('>') => {
                ctx.pos += 1;
                BinOp::ShiftRight
            }
            Some('<') => {
                panic!("What the hell is the `><` operator? Is that how you feel rn?")
            }
            Some('=') => {
                ctx.pos += 1;
                BinOp::Ge
            }
            _ => BinOp::Gt,
        }),
        Some('a') if parse_until_symbol_boundary(ctx, "nd") => Some(BinOp::And),
        Some('o') if parse_until_symbol_boundary(ctx, "r") => Some(BinOp::Or),
        Some(_) => {
            ctx.pos -= 1;
            None
        },
        None => None,
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

fn consume_blank_lines(ctx: &mut ParseContext) {
    while consume_blank_line(ctx) {}
}

fn consume_blank_line(ctx: &mut ParseContext) -> bool {
    let start = ctx.pos;
    loop {
        match ctx.peek() {
            Some(' ') => ctx.pos += 1,
            Some('\t') => ctx.pos += 1,
            Some('#') => {
                consume_comment(ctx);
                break;
            }
            _ => break,
        }
    }
    if parse_char(ctx, '\n').is_ok() {
        true
    } else {
        ctx.pos = start;
        false
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

    fn code_str(s: &str) -> String {
        let mut lines = s.split("\n").filter(|l| !l.is_empty());
        let first = lines.next().unwrap();
        let preceding_whitespace = first.chars().take_while(|c| c.is_whitespace()).count();
        let trimmed = lines.map(|l| {
            if l.len() < preceding_whitespace {
                "".to_owned()
            } else {
                format!("{}\n", &l[preceding_whitespace..])
            }
        });
        first[preceding_whitespace..].to_owned() + "\n" + &trimmed.collect::<String>()
    }

    #[test]
    fn test_parse_num() {
        assert_eq!(
            Ok(Num::Int(1234)),
            parse_num(&mut ParseContext::new("1234"))
        );
        assert_eq!(
            Ok(Num::Float(3.14)),
            parse_num(&mut ParseContext::new("3.14"))
        );
        assert_eq!(
            Ok(Num::Int(1234)),
            parse_num(&mut ParseContext::new("1234"))
        );
        assert_eq!(
            Ok(Num::Float(3.14)),
            parse_num(&mut ParseContext::new("3.14"))
        );
        assert_eq!(
            Ok(Num::Float(0.0)),
            parse_num(&mut ParseContext::new("0.0"))
        );
        assert_eq!(Ok(Num::Int(0)), parse_num(&mut ParseContext::new("0")));
    }

    #[test]
    fn test_parse_expr_symbol() {
        assert_eq!(
            Expr::Symbol("abcd".to_owned()),
            parse_expr_symbol(&mut ParseContext::new("abcd"))
        );
        assert_eq!(
            Expr::Symbol("abc123٣".to_owned()),
            parse_expr_symbol(&mut ParseContext::new("abc123٣"))
        );
    }

    #[test]
    fn test_parse_char() {
        assert!(parse_char(&mut ParseContext::new("a"), 'a').is_ok());
        assert!(parse_char(&mut ParseContext::new("b"), 'a').is_err());
        assert!(parse_char(&mut ParseContext::new(""), 'a').is_err());
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
        assert_eq!(
            Ok(Expr::Call(
                Box::new(Expr::Symbol("f".to_owned())),
                vec![Expr::Int(4), Expr::Symbol("henlo".to_owned()),],
            )),
            parse_expr(&mut ParseContext::new("(4).f(henlo)"))
        );
        assert_eq!(
            Ok(Expr::Call(
                Box::new(Expr::Call(
                    Box::new(Expr::Symbol("f".to_owned())),
                    vec![Expr::Int(3)]
                )),
                vec![Expr::Int(4)]
            )),
            parse_expr(&mut ParseContext::new("f(3)(4)"))
        );
        assert_eq!(
            Ok(Expr::Call(
                Box::new(Expr::Symbol("c".to_owned())),
                vec![
                    Expr::Call(
                        Box::new(Expr::Symbol("b".to_owned())),
                        vec![Expr::Symbol("a".to_owned()), Expr::Int(4)]
                    ),
                    Expr::Int(7),
                ]
            )),
            parse_expr(&mut ParseContext::new("a.b(4).c(7)"))
        );
    }

    #[test]
    fn test_parse_tuple() {
        assert_eq!(
            Ok(Expr::Tuple(vec![Expr::Int(420), Expr::Int(96)],)),
            parse_expr(&mut ParseContext::new("(420, 96)")) // plot twist
        );
        assert_eq!(
            Ok(Expr::Tuple(vec![Expr::Int(420)],)),
            parse_expr(&mut ParseContext::new("(420,)"))
        );
        assert_eq!(
            Ok(Expr::Int(420)),
            parse_expr(&mut ParseContext::new("(420)"))
        );
    }

    #[test]
    fn test_parse_lambda() {
        assert_eq!(
            Ok(Expr::Lambda(vec![Arg::Int(1337)], vec![Expr::Int(42)])),
            parse_expr(&mut ParseContext::new("(1337): 42"))
        );
        assert_eq!(
            Ok(Expr::Lambda(
                vec![Arg::Int(1337), Arg::Symbol("b".to_owned())],
                vec![Expr::Int(42)]
            )),
            parse_expr(&mut ParseContext::new("(1337, b): 42"))
        );
    }

    #[test]
    fn test_parse_def() {
        assert_eq!(
            Ok(Expr::Lambda(
                vec![Arg::Int(1337)],
                vec![Expr::Int(42), Expr::Int(69)]
            )),
            parse_expr(&mut ParseContext::new(&code_str(
                "
                def (1337):
                  42
                  return 69
            "
            )))
        );
        assert!(parse_expr(&mut ParseContext::new(&code_str(
            "
            def (1337):
              42
              69
        "
        )))
        .is_err());
        assert_eq!(
            Ok(Expr::Assignment(
                Arg::Symbol("magic_numbers".to_owned()),
                Box::new(Expr::Lambda(
                    vec![Arg::Int(1337)],
                    vec![Expr::Int(42), Expr::Int(69)]
                ))
            )),
            parse_expr(&mut ParseContext::new(&code_str(
                "
                def magic_numbers(1337):
                  42
                  return 69
            "
            )))
        );
    }

    #[test]
    fn test_parse_list() {
        assert_eq!(
            Ok(Expr::List(vec![
                Expr::Int(1),
                Expr::Int(1),
                Expr::Int(2),
                Expr::Int(3),
                Expr::Int(5),
            ])),
            parse_expr(&mut ParseContext::new("[1, 1, 2, 3, 5]"))
        );
    }

    #[test]
    fn test_parse_expr_char() {
        assert_eq!(
            Ok(Expr::Char('a')),
            parse_expr(&mut ParseContext::new("'a'"))
        );
        assert_eq!(
            Ok(Expr::Char('\'')),
            parse_expr(&mut ParseContext::new(r#"'\''"#))
        );
        assert_eq!(
            Ok(Expr::Char('\x7a')),
            parse_expr(&mut ParseContext::new(r#"'\x7a'"#))
        );
        assert_eq!(
            Ok(Expr::Char(0xff as char)),
            parse_expr(&mut ParseContext::new(r#"'\xff'"#))
        );
        assert!(parse_expr(&mut ParseContext::new(r#"'\8'"#)).is_err());
    }

    #[test]
    fn test_parse_expr_stringo() {
        assert_eq!(
            Ok(Expr::List(vec![])),
            parse_expr(&mut ParseContext::new(r#""""#))
        );
        assert_eq!(
            Ok(Expr::List(vec![Expr::Char('\n')])),
            parse_expr(&mut ParseContext::new(r#""\n""#))
        );
        let mut ctx = ParseContext::new(r#""abcdefg""#);
        assert_eq!(
            Ok(Expr::List(vec![
                Expr::Char('a'),
                Expr::Char('b'),
                Expr::Char('c'),
                Expr::Char('d'),
                Expr::Char('e'),
                Expr::Char('f'),
                Expr::Char('g'),
            ])),
            parse_expr(&mut ctx)
        );
        assert!(ctx.at_eof());
    }

    #[test]
    fn test_parse_expr_assignment() {
        assert_eq!(
            Ok(Expr::Assignment(
                Arg::Symbol("x".to_owned()),
                Box::new(Expr::BinOp(
                    BinOp::Add,
                    Box::new(Expr::Int(3)),
                    Box::new(Expr::Int(5))
                ))
            )),
            parse_expr(&mut ParseContext::new(&code_str(
                "
                x = 3 + 5
            "
            )))
        )
    }

    #[test]
    fn test_parse_statement_type() {
        assert_eq!(
            Ok(Statement::Type {
                name: "Blitzmax".to_owned(),
                sigs: vec![
                    TypeFunSig {
                        name: "blitz".to_owned(),
                        args: vec!["a".to_owned(), "b".to_owned()]
                    },
                    TypeFunSig {
                        name: "max".to_owned(),
                        args: vec![]
                    },
                ],
            }),
            parse_statement(&mut ParseContext::new(&code_str(
                "
                type Blitzmax:
                  def blitz(a, b)
                  def max() -> self
            "
            )))
        )
    }

    #[test]
    fn test_parse_statement_class() {
        assert_eq!(
            Ok(Statement::Class {
                name: "Animal".to_owned(),
                variants: vec![
                    ClassVariant {
                        name: "Dog".to_owned(),
                        fields: vec!["breed".to_owned()]
                    },
                    ClassVariant {
                        name: "Cat".to_owned(),
                        fields: vec!["tooth_count".to_owned(), "eye_count".to_owned()]
                    },
                ],
            }),
            parse_statement(&mut ParseContext::new(&code_str(
                "
                class Animal:
                  Dog(breed)
                  Cat(tooth_count, eye_count)
            "
            )))
        )
    }

    #[test]
    fn test_parse_statement_instance() {
        assert_eq!(
            Ok(Statement::Instance {
                class_name: "JsonValue".to_owned(),
                type_name: "Show".to_owned(),
                implementations: vec![
                    Expr::Assignment(
                        Arg::Symbol("str".to_owned()),
                        Box::new(Expr::Lambda(
                            vec![Arg::ClassPattern("JsonNull".to_owned(), vec![])],
                            vec![Expr::List(vec![
                                Expr::Char('n'),
                                Expr::Char('u'),
                                Expr::Char('l'),
                                Expr::Char('l'),
                            ]),]
                        ))
                    ),
                    Expr::Assignment(
                        Arg::Symbol("str".to_owned()),
                        Box::new(Expr::Lambda(
                            vec![Arg::ClassPattern(
                                "JsonBool".to_owned(),
                                vec![Arg::ClassPattern("True".to_owned(), vec![])]
                            )],
                            vec![Expr::List(vec![
                                Expr::Char('t'),
                                Expr::Char('r'),
                                Expr::Char('u'),
                                Expr::Char('e'),
                            ]),]
                        ))
                    )
                ],
            }),
            parse_statement(&mut ParseContext::new(&code_str(
                "
                instance JsonValue of Show:
                  def str(JsonNull):
                    return \"null\"

                  def str(JsonBool(True)):
                    return \"true\"
            "
            )))
        )
    }
}
