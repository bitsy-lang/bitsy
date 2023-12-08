use super::*;

impl Package {
    pub fn check(&self) -> Result<(), Vec<(Path, CircuitError)>> {
        let mut errors: Vec<(Path, CircuitError)> = vec![];
        for moddef in self.moddefs() {
            if let Err(component_errors) = self.check_component(moddef.clone()) {
                for component_error in component_errors {
                    errors.push((moddef.name().into(), component_error));
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_component(&self, component: Arc<Component>) -> Result<(), Vec<CircuitError>> {
        let mut errors = vec![];

        match &*component {
            Component::Mod(_loc, _name, _children, _wires, _whens) => {
                errors.extend(self.check_typecheck(component.clone()));
                errors.extend(self.check_wires_no_such_component(component.clone()));
                errors.extend(self.check_children_duplicate_names(component.clone()));
                errors.extend(self.check_wires_duplicate_targets(component.clone()));
                errors.extend(self.check_missing_drivers(component.clone()));
                errors.extend(self.check_wires_wiretype(component.clone()));
                errors.extend(self.check_incoming_port_driven(component.clone()));
            },
            Component::Ext(loc, _name, children) => {
                for component in children {
                    if !component.is_port() {
                        errors.push(CircuitError::ExtHasNonPort(loc.clone(), component.name().to_string()));
                    }
                }
            },
            _ => (),
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_children_duplicate_names(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut seen = BTreeSet::new();
        for child in &component.children() {
            if !seen.contains(child.name()) {
                seen.insert(child.name());
            } else {
                errors.push(CircuitError::DuplicateComponent(child.clone()));
            }
        }
        errors

    }

    fn check_wires_duplicate_targets(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut seen = BTreeSet::new();
        for Wire(loc, target, _expr, _typ) in &component.wires() {
            if !seen.contains(target) {
                seen.insert(target);
            } else {
                errors.push(CircuitError::MultipleDrivers(loc.clone(), target.to_string()));
            }
        }
        errors
    }

    fn check_typecheck(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let ctx = match self.context_for(component.clone()) {
            Ok(ctx) => ctx,
            Err(e) => return vec![CircuitError::Unknown(Some(component.loc()), format!("{e:?}"))],
        };

        let mut errors = vec![];

        for Wire(loc, target, expr, _wiretype) in &component.wires() {
            let target_typ = if let Some(typ) = ctx.lookup(target) {
                typ
            } else {
                errors.push(CircuitError::NoSuchComponent(loc.clone(), target.to_string()));
                continue;
            };

            match expr.typecheck(&target_typ, ctx.clone()) {
                Err(e) => errors.push(CircuitError::TypeError(e)),
                Ok(()) => (),
            }

        }
        errors
    }

    fn check_wires_no_such_component(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(loc, target, _expr, _wiretype) in &component.wires() {
            if self.component_from(component.clone(), target.clone()).is_none() {
                errors.push(CircuitError::NoSuchComponent(loc.clone(), target.to_string()));
            }
        }
        errors
    }

    fn check_wires_wiretype(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(loc, target, _expr, wiretype) in &component.wires() {
            if let Some(component) = self.component_from(component.clone(), target.clone()) {
                match (&*component, wiretype) {
                    (Component::Reg(_loc, name, _typ, _reset), WireType::Direct) =>
                        errors.push(CircuitError::WrongWireType(loc.clone(), name.clone(), WireType::Direct)),
                    (Component::Node(_loc, name, _typ), WireType::Latch) =>
                        errors.push(CircuitError::WrongWireType(loc.clone(), name.clone(), WireType::Latch)),
                    (Component::Outgoing(_loc, name, _typ), WireType::Latch) =>
                        errors.push(CircuitError::WrongWireType(loc.clone(), name.clone(), WireType::Latch)),
                    (_, _) => (),
                }
            }
        }
        errors
    }

    fn check_incoming_port_driven(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];

        for Wire(_loc, target, _expr, _wiretype) in &component.wires() {
            if let Some(component) = self.component_from(component.clone(), target.clone()) {
                let is_local = !target.contains(".");
                if is_local {
                    match &*component {
                        Component::Incoming(loc, name, _typ) =>
                            errors.push(CircuitError::IncomingPortDriven(loc.clone(), name.clone())),
                        _ => (),
                    }
                }
            }
        }
        errors
    }

    fn check_missing_drivers(&self, component: Arc<Component>) -> Vec<CircuitError> {
        let mut errors = vec![];
        let mut terminals_remaining: Vec<(Path, Arc<Component>)> = self.visible_paths(component.clone());

        for Wire(_loc, target, _expr, _typ) in &component.wires() {
            terminals_remaining = terminals_remaining.into_iter().filter(|(path, _component)| path != target).collect();
        }

        for (path, component) in terminals_remaining.into_iter() {
            let mut is_incoming_port = false;

            if let Component::Incoming(_loc, _name, _typ) = &*component {
                is_incoming_port = true;
            }

            let is_local = !path.contains(".");

            if !is_local && is_incoming_port {
                errors.push(CircuitError::NoDrivers(component.clone()));
            } else if is_local && !is_incoming_port {
                errors.push(CircuitError::NoDrivers(component.clone()));
            }
        }
        errors
    }
}

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