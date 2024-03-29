use std::sync::Arc;

type Pos = usize;

#[derive(Clone, Debug)]
pub struct SourceInfo {
    source: Source,
    linelens: LineLens,
}

impl SourceInfo {
    pub fn unknown() -> SourceInfo {
        SourceInfo {
            source: Source::Unknown,
            linelens: LineLens::from(""),
        }
    }

    pub fn from_file(filepath: &std::path::Path, contents: &str) -> SourceInfo {
        SourceInfo {
            source: Source::File(Arc::new(filepath.to_owned())),
            linelens: LineLens::from(contents),
        }
    }

    pub fn from_string(contents: &str) -> SourceInfo {
        SourceInfo {
            source: Source::String(Arc::new(contents.to_owned())),
            linelens: LineLens::from(contents),
        }
    }

    pub fn start(&self, item: &dyn HasLoc) -> LineCol {
        self.linelens.linecol(item.loc().start)
    }

    pub fn end(&self, item: &dyn HasLoc) -> LineCol {
        self.linelens.linecol(item.loc().end)
    }
}

#[derive(Clone, Debug)]
pub enum Source {
    File(Arc<std::path::PathBuf>),
    String(Arc<String>),
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LineCol(usize, usize);

impl LineCol {
    pub fn line(&self) -> usize {
        self.0 + 1
    }

    pub fn col(&self) -> usize {
        self.1 + 1
    }
}

impl std::fmt::Display for LineCol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}", self.line(), self.col())
    }
}


#[derive(Clone, Debug)]
pub struct Loc {
    start: Pos,
    end: Pos,
    source_info: SourceInfo,
}

impl Loc {
    pub fn unknown() -> Loc {
        Loc {
            start: 0,
            end: 0,
            source_info: SourceInfo::unknown(),
        }
    }

    pub(crate) fn from(source_info: &SourceInfo, start: usize, end: usize) -> Loc {
        Loc {
            start,
            end,
            source_info: source_info.clone(),
        }
    }

    pub fn start(&self) -> LineCol {
        self.source_info.linelens.linecol(self.start)
    }

    pub fn end(&self) -> LineCol {
        self.source_info.linelens.linecol(self.end)
    }
}

pub trait HasLoc {
    fn loc(&self) -> Loc;
}

#[derive(Clone, Debug)]
struct LineLens(Vec<usize>);

impl LineLens {
    fn from(text: &str) -> LineLens {
        let mut lens = vec![];
        for line in text.split("\n") {
            lens.push(line.len() + 1);
        }
        LineLens(lens)
    }

    fn linecol(&self, pos: Pos) -> LineCol {
        let mut line = 0;
        let mut col = pos;
        for line_len in &self.0 {
            if col >= *line_len {
                col -= *line_len;
                line += 1;
            } else {
                break
            }
        }
        LineCol(line, col)
    }
}

#[test]
fn linelens() {
    let text = "Hello,
world!
How are you?";

    let linelens = LineLens::from(text);
    assert_eq!(linelens.linecol(0).to_string(), "1:1".to_string());
    assert_eq!(linelens.linecol(5).to_string(), "1:6".to_string());
    assert_eq!(linelens.linecol(6).to_string(), "1:7".to_string());
    assert_eq!(linelens.linecol(7).to_string(), "2:1".to_string());
    assert_eq!(linelens.linecol(7).to_string(), "2:1".to_string());
    assert_eq!(linelens.linecol(12).to_string(), "2:6".to_string());
    assert_eq!(linelens.linecol(13).to_string(), "2:7".to_string());
    assert_eq!(linelens.linecol(14).to_string(), "3:1".to_string());
}
