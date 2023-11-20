use super::*;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug)]
pub struct Path(Arc<String>);

impl Path {
    pub fn parent(&self) -> Path {
        let mut path_parts: Vec<&str> = self.split('.').collect();
        path_parts.pop();
        path_parts.join(".").into()
    }

    pub fn set(&self) -> Path {
        format!("{self}.set").into()
    }

    pub fn is_absolute(&self) -> bool {
        self.0.starts_with("top.")
    }

    pub fn join(&self, path: Path) -> Path {
        format!("{}.{}", self, path).into()
    }
}

// TODO remove this, replace with Path::parent()
pub(crate) fn parent_of(path: Path) -> Path {
    let mut path_parts: Vec<&str> = path.split('.').collect();
    path_parts.pop();
    path_parts.join(".").into()
}

impl std::ops::Deref for Path {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", &self.0)
    }
}

impl From<String> for Path {
    fn from(path: String) -> Path {
        Path(Arc::new(path))
    }
}

impl From<&str> for Path {
    fn from(path: &str) -> Path {
        Path(Arc::new(path.to_string()))
    }
}
