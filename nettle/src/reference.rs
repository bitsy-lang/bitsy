use anyhow::anyhow;

use std::sync::Mutex;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Reference<T>(Arc<String>, Arc<Mutex<Option<Arc<T>>>>);

impl<T> Reference<T> {
    pub fn new<S: Into<Arc<String>>>(name: S) -> Reference<T> {
        Reference(name.into(), Arc::new(std::sync::Mutex::new(None)))
    }

    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn get(&self) -> Option<Arc<T>> {
        let lock = self.1.lock().unwrap();
        match &*lock {
            Some(t) => Some(Arc::clone(t)),
            None => None,
        }
    }

    pub fn is_resolved(&self) -> bool {
        self.1.lock().unwrap().is_some()
    }

    pub fn resolve_to(&self, t: Arc<T>) -> anyhow::Result<()> {
        let mut lock = self.1.lock().unwrap();
        match &*lock {
            Some(_t) => Err(anyhow!("Ref is already resolved.")),
            None => {
                *lock = Some(t);
                Ok(())
            },
        }
    }
}

impl<T> From<&str> for Reference<T> {
    fn from(s: &str) -> Reference<T> {
        Reference::new(s.to_string())
    }
}

impl<T> From<String> for Reference<T> {
    fn from(s: String) -> Reference<T> {
        Reference::new(s)
    }
}

impl<T> PartialEq for Reference<T> {
    fn eq(&self, other: &Reference<T>) -> bool {
        if let (Some(arc1), Some(arc2)) = (self.get(), other.get()) {
            Arc::ptr_eq(&arc1, &arc2)
        } else {
            false
        }
    }
}
