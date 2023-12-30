/// [`sim`] is the submodule where the naive simulator goes.
///
/// Notable are:
/// * [`sim::SimCircuit`] which is the netlist representation of a [`Circuit`],
/// * [`sim::Sim`] which is the simulator itself.
/// * [`sim::Value`] which represents all live values during simulation.
pub mod sim;

/// [`ast`] is the submodule for the AST parser.
pub mod ast;

mod types;
mod typecheck;
mod package;
mod resolve;
mod component;
mod circuit;
mod parse;
mod expr;
mod path;
mod context;
mod loc;
mod error;

#[cfg(test)]
mod tests;

pub use types::*;
pub use typecheck::*;
pub use package::*;
pub use resolve::*;
pub use component::*;
pub use circuit::*;
pub use parse::*;
pub use expr::*;
pub use path::*;
pub use context::*;
pub use loc::*;
pub use error::*;
