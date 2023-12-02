use crate::path::Path;
use crate::expr::Expr;

pub type NetId = usize;
pub type CombId = usize;
pub type ExtId = usize;
pub type RegId = usize;

pub struct Circuit {
    pub nets: Vec<Net>, // indexed by NetId
    pub combs: Vec<Comb>, // indexed by NetId
    pub regs: Vec<RegInfo>, // indexed by RegId

    pub dependents: Vec<Dependents>, // indexed by NetId
}

#[derive(Clone)]
pub struct Net(pub Path, pub Vec<Path>);

#[derive(Debug, Clone)]
pub struct Comb(pub NetId, pub Expr);

#[derive(Debug)]
pub struct RegInfo {
    pub set_net_id: NetId,
    pub val_net_id: NetId,
    pub reset: Expr,
}

#[derive(Debug)]
pub struct Dependents {
    pub combs: Vec<CombId>,
}
