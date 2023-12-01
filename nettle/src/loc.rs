type Pos = usize;

#[derive(Clone, Debug)]
pub struct SourceInfo {
    source: Source,
    linelens: LineLens,
}

impl SourceInfo {
    pub fn from_file(filepath: &std::path::Path, contents: &str) -> SourceInfo {
        SourceInfo {
            source: Source::File(filepath.to_owned()),
            linelens: LineLens::from(contents),
        }
    }

    pub fn from_string(contents: &str) -> SourceInfo {
        SourceInfo {
            source: Source::String(contents.to_owned()),
            linelens: LineLens::from(contents),
        }
    }

    pub fn start(&self, item: &dyn HasLoc) -> RowCol {
        self.linelens.rowcol(item.loc().start)
    }

    pub fn end(&self, item: &dyn HasLoc) -> RowCol {
        self.linelens.rowcol(item.loc().end)
    }
}

#[derive(Clone, Debug)]
pub enum Source {
    File(std::path::PathBuf),
    String(String),
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RowCol(usize, usize);

impl RowCol {
    pub fn row(&self) -> usize {
        self.0 + 1
    }

    pub fn col(&self) -> usize {
        self.1 + 1
    }
}

impl std::fmt::Display for RowCol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}", self.row(), self.col())
    }
}


#[derive(Clone, Debug, Copy)]
pub struct Loc {
    start: Pos,
    end: Pos,
}

impl Loc {
    pub(crate) fn from(start: usize, end: usize) -> Loc {
        Loc {
            start,
            end,
        }
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

    fn rowcol(&self, pos: Pos) -> RowCol {
        let mut row = 0;
        let mut col = pos;
        for line_len in &self.0 {
            if col >= *line_len {
                col -= *line_len;
                row += 1;
            } else {
                break
            }
        }
        RowCol(row, col)
    }
}

#[test]
fn linelens() {
    let text = "Hello,
world!
How are you?";

    let linelens = LineLens::from(text);
    assert_eq!(linelens.rowcol(0).to_string(), "1:1".to_string());
    assert_eq!(linelens.rowcol(5).to_string(), "1:6".to_string());
    assert_eq!(linelens.rowcol(6).to_string(), "1:7".to_string());
    assert_eq!(linelens.rowcol(7).to_string(), "2:1".to_string());
    assert_eq!(linelens.rowcol(7).to_string(), "2:1".to_string());
    assert_eq!(linelens.rowcol(12).to_string(), "2:6".to_string());
    assert_eq!(linelens.rowcol(13).to_string(), "2:7".to_string());
    assert_eq!(linelens.rowcol(14).to_string(), "3:1".to_string());
}
