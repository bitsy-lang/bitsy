use super::*;

use lalrpop_util::lalrpop_mod;
use lalrpop_util::{lexer::Token, ParseError};
lalrpop_mod!(testbench_grammar);

#[derive(Debug, Clone)]
pub struct Testbench(pub(crate) Option<String>, pub(crate) Vec<TestbenchLink>, pub(crate) Vec<TestbenchCommand>);

#[derive(Debug, Clone)]
pub(crate) struct TestbenchLink(pub(crate) Path, pub(crate) String, pub(crate) Vec<(String, String)>);

#[derive(Debug, Clone)]
pub(crate) enum TestbenchCommand {
    Cd(Option<Path>),
    Watch(Watch),
    Peek(Path),
    Poke(Path, Value),
    Set(Path, Value),
    Clock,
    Reset,
    Show,
    Run,
    Debug,
    Eval(Expr),
    Assert(Expr),
}

#[derive(Debug, Clone)]
pub struct Watch {
    pub path: Path,
    pub format: WatchFormat,
}

#[derive(Debug, Clone)]
pub enum WatchFormat {
    Normal,
    Hex,
    Bin,
    Bool,
}

pub fn parse_testbench(testbench: &str) -> Result<Testbench, ParseError<usize, Token<'_>, &'static str>> {
    let source_info = SourceInfo::from_string(testbench);
    Ok(testbench_grammar::TestbenchParser::new().parse(&source_info, testbench)?)
}

pub fn parse_testbench_command(testbench_command: &str) -> Result<TestbenchCommand, ParseError<usize, Token<'_>, &'static str>> {
    let source_info = SourceInfo::from_string(testbench_command);
    testbench_grammar::TestbenchCommandParser::new().parse(&source_info, testbench_command)
}
