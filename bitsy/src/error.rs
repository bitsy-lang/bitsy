use super::*;

use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum BitsyError {
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
    NotExpectedType(Type, Type, Arc<Expr>),
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
            TypeError::NotExpectedType(type_expected, type_actual, _expr) => write!(f, "Not expected type: has type {type_actual:?} but expected {type_expected:?}."),
            TypeError::InvalidWord(expr) => write!(f, "Invalid literal: {expr:?}"),
            TypeError::CantInferType(expr) => write!(f, "Can't infer type: {expr:?}"),
            TypeError::Other(_expr, message) => write!(f, "{message}"),
        }
    }
}

impl std::fmt::Display for BitsyError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BitsyError::ExtHasNonPort(_loc, name) => write!(f, "Ext declares a component other than an incoming or outgoing: {name}"),
            BitsyError::DuplicateComponent(component) => write!(f, "Duplicate component: {}", component.name()),
            BitsyError::MultipleDrivers(_loc, name) => write!(f, "Component has multiple drivers: {name}."),
            BitsyError::NoDrivers(component) => write!(f, "Component is not driven: {}", component.name()),
            BitsyError::WrongWireType(_loc, name, wire_type) => {
                let symbol = match wire_type {
                    WireType::Direct => ":=",
                    WireType::Latch => "<=",
                    WireType::Proc => "<=!",
                };
                write!(f, "Wrong wire type: {name} does not support {symbol}")
            },
            BitsyError::IncomingPortDriven(_loc, name) => write!(f, "Incoming port is being driven from inside a mod, but shouldn't be: {name}"),
            BitsyError::NoSuchComponent(_loc, s) => write!(f, "No such component: {s}"),
            BitsyError::TypeError(type_error) => write!(f, "Type Error: {type_error}"),
            BitsyError::ParseError(_loc, error) => write!(f, "{error}"),
            BitsyError::Unknown(_loc, message) => write!(f, "{message}"),
        }
    }
}

impl HasLoc for BitsyError {
    fn loc(&self) -> Loc {
        match self {
            BitsyError::ExtHasNonPort(loc, _name) => loc.clone(),
            BitsyError::DuplicateComponent(component) => component.loc(),
            BitsyError::MultipleDrivers(loc, _name) => loc.clone(),
            BitsyError::NoDrivers(component) => component.loc(),
            BitsyError::WrongWireType(loc, _name, _wire_type) => loc.clone(),
            BitsyError::IncomingPortDriven(loc, _name) => loc.clone(),
            BitsyError::NoSuchComponent(loc, _name) => loc.clone(),
            BitsyError::TypeError(type_error) => type_error.loc(),
            BitsyError::ParseError(loc, _error) => loc.clone(),
            BitsyError::Unknown(loc, _string) => loc.clone().unwrap_or_else(|| Loc::unknown()),
        }
    }
}
