use super::*;

use lalrpop_util::lalrpop_mod;
use lalrpop_util::{lexer::Token, ParseError};
lalrpop_mod!(grammar);

#[derive(Debug)]
pub enum ModDecl {
    Component(Component),
    Wire(Wire),
}

pub fn parse_top(circuit: &str) -> Circuit {
    let component: Component = grammar::TopParser::new().parse(circuit).unwrap();
    Circuit::new(component)
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        *grammar::ExprParser::new().parse(expr).unwrap()
    }
}

pub fn parse_testbench(testbench: &str) -> Testbench {
    grammar::TestbenchParser::new().parse(testbench).unwrap()
}

pub fn parse_testbench_command(testbench_command: &str) -> Result<TestbenchCommand, ParseError<usize, Token<'_>, &'static str>> {
    grammar::TestbenchCommandParser::new().parse(testbench_command)
}
