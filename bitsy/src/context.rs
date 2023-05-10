use core::slice::Iter;

#[derive(Clone, Debug)]
pub struct Context<T>(Vec<(String, T)>);

impl<T: Clone> Context<T> {
    pub fn empty() -> Context<T> {
        Context(vec![])
    }

    pub fn from(ctx: Vec<(String, T)>) -> Context<T> {
        Context(ctx)
    }

    pub fn lookup(&self, v: &str) -> Option<T> {
        for (v0, t) in &self.0 {
            if v0 == v {
                return Some(t.clone());
            }
        }
        None
    }

    pub fn extend(&self, v: String, t: T) -> Context<T> {
        let mut result = self.clone();
        result.0.push((v, t));
        result
    }

    pub fn into_inner(self) -> Vec<(String, T)> {
        self.0
    }
}

impl<T> std::ops::Deref for Context<T> {
    type Target = Vec<(String, T)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Context<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, (name, v)) in self.0.iter().enumerate() {
            write!(f, "{name} : {v}")?;
            if i + 1 < self.0.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}
