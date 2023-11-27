use super::*;

#[derive(Debug, Clone)]
pub struct Testbench(pub(crate) Vec<TestbenchLink>, pub(crate) Vec<TestbenchCommand>);

#[derive(Debug, Clone)]
pub(crate) struct TestbenchLink(pub(crate) Path, pub(crate) String, pub(crate) Vec<(String, String)>);

#[derive(Debug, Clone)]
pub(crate) enum TestbenchCommand {
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
