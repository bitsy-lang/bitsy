use std::sync::Arc;

type Pos = usize;

/// A [`SourceInfo`] maintains location data for parsed objects.
/// Maintains the filename (if from a file) or the originating string (if from a string).
/// Helps with the conversion from byte-position in the source to a [`LineCol`].
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

    pub fn source(&self) -> &Source {
        &self.source
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

    pub fn linecol_from(&self, pos: usize) -> LineCol {
        self.linelens.linecol(pos)
    }
}

#[derive(Clone, Debug)]
pub enum Source {
    File(Arc<std::path::PathBuf>),
    String(Arc<String>),
    Unknown,
}

/// A [`LineCol`] is a container for a line and column.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LineCol(usize, usize);

impl LineCol {
    /// The line number. Starts with line 1.
    pub fn line(&self) -> usize {
        self.0 + 1
    }

    /// The column. Starts with column 1.
    pub fn col(&self) -> usize {
        self.1 + 1
    }
}

impl std::fmt::Display for LineCol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}", self.line(), self.col())
    }
}

/// A [`Loc`] tracks the span of an object parsed from a source.
#[derive(Clone)]
pub struct Loc {
    start: Pos,
    end: Pos,
    source_info: SourceInfo,
}

impl std::fmt::Debug for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.source_info.source {
            Source::File(path) => write!(f, "[{}-{}:{:?}]", self.start(), self.end(), path),
            Source::String(s) => write!(f, "[{}-{}:{:?}]", self.start(), self.end(), String::from_utf8_lossy(&s.as_bytes()[self.start..self.end])),
            Source::Unknown => write!(f, "[{}-{}]", self.start(), self.end()),
        }
    }
}

impl Loc {
    /// When the location of something is unknown, you can use this.
    pub fn unknown() -> Loc {
        Loc {
            start: 0,
            end: 0,
            source_info: SourceInfo::unknown(),
        }
    }

    pub fn from(source_info: &SourceInfo, start: usize, end: usize) -> Loc {
        Loc {
            start,
            end,
            source_info: source_info.clone(),
        }
    }

    /// The start of the span.
    pub fn start(&self) -> LineCol {
        self.source_info.linelens.linecol(self.start)
    }

    /// The end of the span.
    pub fn end(&self) -> LineCol {
        self.source_info.linelens.linecol(self.end)
    }

    pub fn source(&self) -> &str {
        if let Source::String(source) = &self.source_info.source {
            &source[self.start..self.end]
        } else {
            ""
        }
    }
}

/// Many objects have location information.
/// [`HasLoc`] allows you to call [`HasLoc::loc`] to get the span information.
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
    // TODO Move this to tests.
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
