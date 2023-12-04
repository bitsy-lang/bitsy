use super::*;

use std::sync::Arc;

use lalrpop_util::lalrpop_mod;
use lalrpop_util::{lexer::Token, ParseError};
lalrpop_mod!(pub grammar);

#[derive(Debug)]
pub enum ModDecl {
    Component(Arc<Component>),
    Wire(Wire),
    When(When),
}

pub fn parse_top(package: &str) -> Result<Circuit, ParseError<usize, Token<'_>, &'static str>>  {
    let source_info = SourceInfo::from_string(package);
    let package: Package = grammar::PackageParser::new().parse(&source_info, package)?;
    Ok(Circuit::new(package))
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        let source_info = SourceInfo::from_string(expr);
        *grammar::ExprParser::new().parse(&source_info, expr).unwrap()
    }
}
