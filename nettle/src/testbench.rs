use super::*;

#[derive(Debug)]
pub struct Testbench(pub(crate) Vec<TestbenchCommand>);

#[derive(Debug)]
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

pub(crate) fn read_testbench_file(filename: &str) -> Option<Testbench> {
    if let Ok(text) = std::fs::read_to_string(filename) {
        Some(parse::parse_testbench(&text))
    } else {
        None
    }
}

pub(crate) fn testbench_for(filename: &str) -> Option<String> {
    let path = std::path::Path::new(filename);
    let parent: &std::path::Path = path.parent().unwrap();
    let file = format!("{}.tb", path.file_stem().unwrap().to_str().unwrap());
    let tb_filename = parent.join(file).into_os_string().into_string().unwrap();
    let exists = std::fs::metadata(&tb_filename).map(|metadata| metadata.is_file()).unwrap_or(false);

    if exists {
        Some(tb_filename)
    } else {
        None
    }
}
