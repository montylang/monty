mod parser;
use parser::*;

fn main() {
    parse_expr(&mut ParseContext::new("(a+b)*c")).unwrap();
    println!("Goodbye, Mars!");
}
