use super::*;

use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum BitsyError {
    ExtHasNonPort(Span, Name),
    DuplicateComponent(Arc<Component>),
    MultipleDrivers(Span, Name),
    NoDrivers(Arc<Component>),
    NoDriversPort(Arc<Component>, Arc<Component>),
    WrongWireType(Span, Name, WireType),
    IncomingPortDriven(Span, Name),
    NoSuchComponent(Span, String),
    TypeError(TypeError),
    ParseError(Span, String),
    Unknown(Option<Span>, String),
}

#[derive(Clone, Debug)]
pub enum TypeError {
    UndefinedReference(Arc<Expr>),
    NotExpectedType(Type, Type, Arc<Expr>),
    InvalidWord(Arc<Expr>),
    CantInferType(Arc<Expr>),
    Other(Arc<Expr>, String),
}

impl HasSpan for TypeError {
    fn span(&self) -> Span {
        match self {
            TypeError::UndefinedReference(e) => e.span(),
            TypeError::NotExpectedType(_type_expected, _type_actual, e) => e.span(),
            TypeError::InvalidWord(e) => e.span(),
            TypeError::CantInferType(e) => e.span(),
            TypeError::Other(e, _msg) => e.span(),
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
            BitsyError::ExtHasNonPort(_span, name) => write!(f, "Ext declares a component other than an incoming or outgoing: {name}"),
            BitsyError::DuplicateComponent(component) => write!(f, "Duplicate component: {}", component.name()),
            BitsyError::MultipleDrivers(_span, name) => write!(f, "Component has multiple drivers: {name}."),
            BitsyError::NoDrivers(component) => write!(f, "Component is not driven: {}", component.name()),
            BitsyError::NoDriversPort(component, port) => write!(f, "Port is not driven: {}.{}", component.name(), port.name()),
            BitsyError::WrongWireType(_span, name, wire_type) => {
                let symbol = match wire_type {
                    WireType::Dom    => "$=",
                    WireType::Direct => ":=",
                    WireType::Latch  => "<=",
                    WireType::Proc   => "<=!",
                };
                write!(f, "Wrong wire type: {name} does not support {symbol}")
            },
            BitsyError::IncomingPortDriven(_span, name) => write!(f, "Incoming port is being driven from inside a mod, but shouldn't be: {name}"),
            BitsyError::NoSuchComponent(_span, s) => write!(f, "No such component: {s}"),
            BitsyError::TypeError(type_error) => write!(f, "Type Error: {type_error}"),
            BitsyError::ParseError(_span, error) => write!(f, "{error}"),
            BitsyError::Unknown(_span, message) => write!(f, "{message}"),
        }
    }
}

impl HasSpan for BitsyError {
    fn span(&self) -> Span {
        match self {
            BitsyError::ExtHasNonPort(span, _name) => span.clone(),
            BitsyError::DuplicateComponent(component) => component.span(),
            BitsyError::MultipleDrivers(span, _name) => span.clone(),
            BitsyError::NoDrivers(component) => component.span(),
            BitsyError::NoDriversPort(component, _port) => component.span(),
            BitsyError::WrongWireType(span, _name, _wire_type) => span.clone(),
            BitsyError::IncomingPortDriven(span, _name) => span.clone(),
            BitsyError::NoSuchComponent(span, _name) => span.clone(),
            BitsyError::TypeError(type_error) => type_error.span(),
            BitsyError::ParseError(span, _error) => span.clone(),
            BitsyError::Unknown(span, _string) => span.clone().unwrap_or_else(|| Span::unknown()),
        }
    }
}
