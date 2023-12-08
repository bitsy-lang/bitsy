/// A [`Context`] is an associative list which assigns each element with type information.
#[derive(Clone, Debug)]
pub struct Context<K, T>(Vec<(K, T)>);

impl<K: Eq + Clone, T: Clone> Context<K, T> {
    pub fn empty() -> Context<K, T> {
        Context(vec![])
    }

    pub fn from(ctx: Vec<(K, T)>) -> Context<K, T> {
        Context(ctx)
    }

    pub fn lookup(&self, v: &K) -> Option<T> {
        for (v0, t) in &self.0 {
            if v0 == v {
                return Some(t.clone());
            }
        }
        None
    }

    pub fn extend(&self, v: K, t: T) -> Context<K, T> {
        let mut result = self.clone();
        result.0.push((v, t));
        result
    }

    pub fn extend_from(&self, another: &Context<K, T>) -> Context<K, T> {
        let mut result = self.clone();
        for (v, t) in another.0.iter() {
            result.0.push((v.clone(), t.clone()));
        }
        result
    }

    pub fn into_inner(self) -> Vec<(K, T)> {
        self.0
    }
}

impl<K, T> std::ops::Deref for Context<K, T> {
    type Target = Vec<(K, T)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K: std::fmt::Display, T: std::fmt::Display> std::fmt::Display for Context<K, T> {
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
