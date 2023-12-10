use super::*;

use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum CircuitError {
    ExtHasNonPort(Loc, Name),
    DuplicateComponent(Arc<Component>),
    MultipleDrivers(Loc, Name),
    NoDrivers(Arc<Component>),
    WrongWireType(Loc, Name, WireType),
    IncomingPortDriven(Loc, Name),
    NoSuchComponent(Loc, String),
    TypeError(TypeError),
    ParseError(Loc, String),
    Unknown(Option<Loc>, String),
}

#[derive(Clone, Debug)]
pub enum TypeError {
    UndefinedReference(Arc<Expr>),
    NotExpectedType(Arc<Type>, Arc<Type>, Arc<Expr>),
    InvalidWord(Arc<Expr>),
    CantInferType(Arc<Expr>),
    Other(Arc<Expr>, String),
}

impl HasLoc for TypeError {
    fn loc(&self) -> Loc {
        match self {
            TypeError::UndefinedReference(e) => e.loc(),
            TypeError::NotExpectedType(_type_expected, _type_actual, e) => e.loc(),
            TypeError::InvalidWord(e) => e.loc(),
            TypeError::CantInferType(e) => e.loc(),
            TypeError::Other(e, _msg) => e.loc(),
        }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            TypeError::UndefinedReference(expr) => write!(f, "Undefiend reference: {expr:?}"),
            TypeError::NotExpectedType(type_expected, type_actual, expr) => write!(f, "Not expected type: {expr:?} has type {type_actual:?} but expected {type_expected:?}."),
            TypeError::InvalidWord(expr) => write!(f, "Invalid literal: {expr:?}"),
            TypeError::CantInferType(expr) => write!(f, "Can't infer type: {expr:?}"),
            TypeError::Other(_expr, _string) => write!(f, "{self:?}"),
        }
    }
}

impl std::fmt::Display for CircuitError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CircuitError::ExtHasNonPort(_loc, name) => write!(f, "Ext declares a component other than an incoming or outgoing: {name}"),
            CircuitError::DuplicateComponent(component) => write!(f, "Duplicate component: {}", component.name()),
            CircuitError::MultipleDrivers(_loc, name) => write!(f, "Component has multiple drivers: {name}."),
            CircuitError::NoDrivers(component) => write!(f, "Component is not driven: {}", component.name()),
            CircuitError::WrongWireType(_loc, name, wire_type) => {
                let symbol = match wire_type {
                    WireType::Direct => ":=",
                    WireType::Latch => "<=",
                    WireType::Proc => "<=!",
                };
                write!(f, "Wrong wire type: {name} does not support {symbol}")
            },
            CircuitError::IncomingPortDriven(_loc, name) => write!(f, "Incoming port is being driven from inside a mod, but shouldn't be: {name}"),
            CircuitError::NoSuchComponent(_loc, s) => write!(f, "No such component: {s}"),
            CircuitError::TypeError(type_error) => write!(f, "Type Error: {type_error}"),
            CircuitError::ParseError(_loc, _error) => write!(f, "{self:?}"),
            CircuitError::Unknown(_loc, _message) => write!(f, "{self:?}"),
        }
    }
}

impl HasLoc for CircuitError {
    fn loc(&self) -> Loc {
        match self {
            CircuitError::ExtHasNonPort(loc, _name) => loc.clone(),
            CircuitError::DuplicateComponent(component) => component.loc(),
            CircuitError::MultipleDrivers(loc, _name) => loc.clone(),
            CircuitError::NoDrivers(component) => component.loc(),
            CircuitError::WrongWireType(loc, _name, _wire_type) => loc.clone(),
            CircuitError::IncomingPortDriven(loc, _name) => loc.clone(),
            CircuitError::NoSuchComponent(loc, _name) => loc.clone(),
            CircuitError::TypeError(type_error) => type_error.loc(),
            CircuitError::ParseError(loc, _error) => loc.clone(),
            CircuitError::Unknown(loc, _string) => loc.clone().unwrap_or_else(|| Loc::unknown()),
        }
    }
}
