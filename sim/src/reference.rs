use super::*;
use anyhow::anyhow;

#[derive(Debug, Clone)]
pub struct Ref<T>(String, Arc<std::sync::Mutex<Option<Arc<T>>>>);

impl<T> PartialEq for Ref<T> {
    fn eq(&self, other: &Ref<T>) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Ref<T> {}

impl<T> Ref<T> {
    pub fn new(name: String) -> Ref<T> {
        Ref(name, Arc::new(std::sync::Mutex::new(None)))
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

    pub fn name(&self) -> &str {
        &self.0
    }
}
