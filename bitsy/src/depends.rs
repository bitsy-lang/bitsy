use std::collections::BTreeSet;
use log::*;

#[derive(Debug, Clone)]
pub struct Depends<T> {
    nodes: Vec<T>,
    edges: Vec<(T, T)>
}

#[derive(Debug, PartialEq, Eq)]
pub struct CycleDetected;

impl<T: Eq + Clone + Ord + std::fmt::Debug> Depends<T> {
    pub fn new() -> Depends<T> {
        Depends {
            nodes: vec![],
            edges: vec![],
        }
    }

    pub fn add(&mut self, t: T) {
        if !self.nodes.contains(&t) {
            self.nodes.push(t);
        }
    }

    pub fn add_dependency(&mut self, s: T, t: T) {
        self.add(s.clone());
        self.add(t.clone());

        let edge = (s, t);
        if !self.edges.contains(&edge) {
            self.edges.push(edge);
        }
    }

    fn index_of(&self, t: &T) -> usize {
        for (i, node) in self.nodes.iter().enumerate() {
            if node == t {
                return i;
            }
        }
        error!("Panic");
        panic!("Not found: {t:?}")
    }

    fn edge_indexes_of(&self, t: &T) -> Vec<usize> {
        let mut results = vec![];
        for (i, (sink, source)) in self.edges.iter().enumerate() {
            if sink == t {
                results.push(i);
            } else if source == t {
                results.push(i);
            }
        }
        results.sort();
        results.dedup();
        results
    }

    fn remove(&mut self, t: &T) {
        self.nodes.swap_remove(self.index_of(t));
        for i in self.edge_indexes_of(t).iter().rev() {
            self.edges.swap_remove(*i);
        }
    }

    fn roots(&self) -> Vec<T> {
        let mut roots: BTreeSet<T> = self.nodes.iter().map(|n| n.to_owned()).collect();
        for (sink, source) in &self.edges {
            roots.remove(sink);
        }
        roots.into_iter().collect()
    }

    fn dependents(&self, t: &T) -> Vec<T> {
        let mut results = vec![];
        for (sink, source) in &self.edges {
            if sink == t {
                results.push(source.clone());
            }
        }
        results
    }

    fn cycles(&self) -> Vec<Vec<T>> {
        todo!()
    }

    pub fn sort(&self) -> Result<Vec<T>, CycleDetected> {
        let mut copy = self.clone();
        let mut results = vec![];

        while !copy.nodes.is_empty() {
            let roots = copy.roots();

            // cycle detected
            if roots.is_empty() && !copy.nodes.is_empty() {
                return Err(CycleDetected);
            }

            results.extend_from_slice(roots.as_slice());
            for root in roots {
                copy.remove(&root);
            }
        }
        Ok(results)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_one() {
        let mut depends = Depends::new();
        depends.add_dependency("A", "B");
        depends.add_dependency("A", "C");

        depends.add_dependency("B", "D");
        depends.add_dependency("B", "E");

        depends.add_dependency("C", "E");

        depends.add("F");

        let mut roots = depends.roots();
        roots.sort();
        assert_eq!(roots, vec!["D", "E", "F"]);

        let sorted = depends.sort().unwrap();
        let a_idx = sorted.iter().position(|x| x == &"A").unwrap();
        let b_idx = sorted.iter().position(|x| x == &"B").unwrap();
        let c_idx = sorted.iter().position(|x| x == &"C").unwrap();
        let d_idx = sorted.iter().position(|x| x == &"D").unwrap();
        let e_idx = sorted.iter().position(|x| x == &"E").unwrap();
        let f_idx = sorted.iter().position(|x| x == &"F").unwrap();

        assert!(f_idx < a_idx);
        assert!(f_idx < b_idx);
        assert!(f_idx < c_idx);
        assert!(d_idx < a_idx);
        assert!(b_idx < a_idx);
        assert!(c_idx < a_idx);
        assert!(e_idx < c_idx);
    }

    #[test]
    fn test_two() {
        let mut depends = Depends::new();
        depends.add_dependency("A", "B");
        depends.add_dependency("B", "C");
        depends.add_dependency("C", "A");

        assert_eq!(depends.sort(), Err(CycleDetected));
    }
}
