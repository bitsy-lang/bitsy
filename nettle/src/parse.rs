use super::*;

use lalrpop_util::lalrpop_mod;
use lalrpop_util::{lexer::Token, ParseError};
lalrpop_mod!(grammar);

#[derive(Debug)]
pub enum ModDecl {
    Component(Component),
    Wire(Wire),
    When(When),
}

pub fn parse_top(package: &str) -> Result<Circuit, ParseError<usize, Token<'_>, &'static str>>  {
    let source_info = SourceInfo::from_string(package);
    let package: Package = grammar::PackageParser::new().parse(package)?;
    Ok(Circuit::new(package, source_info))
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        *grammar::ExprParser::new().parse(expr).unwrap()
    }
}

pub fn parse_testbench(testbench: &str) -> Result<Testbench, ParseError<usize, Token<'_>, &'static str>> {
    Ok(grammar::TestbenchParser::new().parse(testbench)?)
}

pub fn parse_testbench_command(testbench_command: &str) -> Result<TestbenchCommand, ParseError<usize, Token<'_>, &'static str>> {
    grammar::TestbenchCommandParser::new().parse(testbench_command)
}
