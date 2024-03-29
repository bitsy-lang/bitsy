use super::*;

impl Package {
    pub(crate) fn check(&self) -> Result<(), Vec<BitsyError>> {
        let mut errors: Vec<BitsyError> = vec![];

        for item in self.items() {
            match item {
                Item::EnumTypeDef(typedef)   => {
                    if let Err(errs) = self.check_enum_typedef(typedef.clone()) {
                        for error in errs {
                            errors.push(error)
                        }
                    }
                },
//                Item::StructTypeDef(typedef) => self.check_struct_typedef(typedef.clone()),
//                Item::AltTypeDef(typedef)    => self.check_alt_typedef(typedef.clone()),
                _ => (),
            }
        }

        for fndef in self.fndefs() {
            if let Err(fndef_errors) = self.check_typecheck_fndef(fndef.clone()) {
                for fndef_error in fndef_errors {
                    errors.push(fndef_error);
                }
            }
        }

        for moddef in self.moddefs() {
            if let Err(component_errors) = self.check_component(moddef.clone()) {
                for component_error in component_errors {
                    errors.push(component_error);
                }
            }

            for submod in moddef.submods() {
                if let Err(component_errors) = self.check_component(submod) {
                    for component_error in component_errors {
                        errors.push(component_error);
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_enum_typedef(&self, typedef: Arc<EnumTypeDef>) -> Result<(), Vec<BitsyError>> {
        // TODO maybe print the values in actual form presented in the program text
        // TODO and also show the location of the error
        let mut names = BTreeSet::new();
        let mut values = BTreeSet::new();
        let mut errors = vec![];

        for (name, value) in &typedef.values {
            if !names.insert(name.clone()) {
                errors.push(BitsyError::Unknown(Some(typedef.span.clone()), format!("Duplicate name in enum: {name}")));
            }
            if !values.insert(value.value()) {
                errors.push(BitsyError::Unknown(Some(typedef.span.clone()), format!("Duplicate value in enum: {value:?}")));
            }
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn check_component(&self, component: Arc<Component>) -> Result<(), Vec<BitsyError>> {
        let mut errors = vec![];

        match &*component {
            Component::Mod(_loc, _name, _children, _wires, _whens) => {
                errors.extend(self.check_typecheck_component(component.clone()));
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
                        errors.push(BitsyError::ExtHasNonPort(loc.clone(), component.name().to_string()));
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

    fn check_children_duplicate_names(&self, component: Arc<Component>) -> Vec<BitsyError> {
        let mut errors = vec![];
        let mut seen = BTreeSet::new();
        for child in &component.children() {
            if !seen.contains(child.name()) {
                seen.insert(child.name());
            } else {
                errors.push(BitsyError::DuplicateComponent(child.clone()));
            }
        }
        errors
    }

    fn check_wires_duplicate_targets(&self, component: Arc<Component>) -> Vec<BitsyError> {
        let mut errors = vec![];
        let mut seen = BTreeSet::new();
        for Wire(loc, target, _expr, _typ) in &component.wires() {
            if !seen.contains(target) {
                seen.insert(target);
            } else {
                errors.push(BitsyError::MultipleDrivers(loc.clone(), target.to_string()));
            }
        }
        errors
    }

    fn check_typecheck_fndef(&self, fndef: Arc<FnDef>) -> Result<(), Vec<BitsyError>> {
        let mut errors = vec![];

        match fndef.body.typecheck(fndef.ret.clone(), fndef.context()) {
            Err(e) => errors.push(BitsyError::TypeError(e)),
            Ok(()) => fndef.body.assert_has_types(),
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn check_typecheck_component(&self, component: Arc<Component>) -> Vec<BitsyError> {
        let ctx = self.context_for(component.clone());
        let mut errors = vec![];

        for Wire(loc, target, expr, _wiretype) in &component.wires() {
            let target_typ = if let Some(typ) = ctx.lookup(target) {
                typ
            } else {
                errors.push(BitsyError::NoSuchComponent(loc.clone(), target.to_string()));
                continue;
            };

            match expr.typecheck(target_typ, ctx.clone()) {
                Err(e) => errors.push(BitsyError::TypeError(e)),
                Ok(()) => expr.assert_has_types(),
            }
        }

        for When(expr, wires) in &component.whens() {
            match expr.typecheck(Type::Word(1), ctx.clone()) {
                Err(e) => errors.push(BitsyError::TypeError(e)),
                Ok(()) => expr.assert_has_types(),
            }
            for Wire(loc, target, expr, _wiretype) in wires {
                let target_typ = if let Some(typ) = ctx.lookup(target) {
                    typ
                } else {
                    errors.push(BitsyError::NoSuchComponent(loc.clone(), target.to_string()));
                    continue;
                };

                match expr.typecheck(target_typ, ctx.clone()) {
                    Err(e) => errors.push(BitsyError::TypeError(e)),
                    Ok(()) => expr.assert_has_types(),
                }
            }
        }

        for child in component.children() {
            if let Component::Reg(_loc, _name, _typ, Some(reset)) = &*child {
                // TODO This is done to turn the reference to the type into the actual type.
                let typ = self.type_of(child.clone()).unwrap();
                match reset.typecheck(typ, ctx.clone()) {
                    Err(e) => errors.push(BitsyError::TypeError(e)),
                    Ok(()) => reset.assert_has_types(),
                }
            }
        }

        errors
    }

    fn check_wires_no_such_component(&self, component: Arc<Component>) -> Vec<BitsyError> {
        let mut errors = vec![];

        for Wire(loc, target, _expr, _wiretype) in &component.wires() {
            if self.component_from(component.clone(), target.clone()).is_none() {
                errors.push(BitsyError::NoSuchComponent(loc.clone(), target.to_string()));
            }
        }
        errors
    }

    fn check_wires_wiretype(&self, component: Arc<Component>) -> Vec<BitsyError> {
        let mut errors = vec![];

        for Wire(loc, target, _expr, wiretype) in &component.wires() {
            if let Some(component) = self.component_from(component.clone(), target.clone()) {
                match (&*component, wiretype) {
                    (Component::Reg(_loc, name, _typ, _reset), WireType::Direct) => {
                        errors.push(BitsyError::WrongWireType(loc.clone(), name.clone(), WireType::Direct))
                    },
                    (Component::Node(_loc, name, _typ), WireType::Latch) => {
                        errors.push(BitsyError::WrongWireType(loc.clone(), name.clone(), WireType::Latch))
                    },
                    (Component::Outgoing(_loc, name, _typ), WireType::Latch) => {
                        errors.push(BitsyError::WrongWireType(loc.clone(), name.clone(), WireType::Latch))
                    },
                    (_, _) => (),
                }
            }
        }
        errors
    }

    fn check_incoming_port_driven(&self, component: Arc<Component>) -> Vec<BitsyError> {
        let mut errors = vec![];

        for Wire(_loc, target, _expr, _wiretype) in &component.wires() {
            if let Some(component) = self.component_from(component.clone(), target.clone()) {
                let is_local = !target.contains(".");
                if is_local {
                    match &*component {
                        Component::Incoming(loc, name, _typ) => {
                            errors.push(BitsyError::IncomingPortDriven(loc.clone(), name.clone()))
                        },
                        _ => (),
                    }
                }
            }
        }
        errors
    }

    fn check_missing_drivers(&self, component: Arc<Component>) -> Vec<BitsyError> {
        let mut errors = vec![];
        let mut terminals_remaining: Vec<(Path, Arc<Component>)> = self.visible_paths(component.clone());

        for Wire(_loc, target, _expr, _typ) in &component.wires() {
            terminals_remaining = terminals_remaining.into_iter().filter(|(path, _component)| path != target).collect();
        }

        for When(_expr, wires) in &component.whens() {
            for Wire(_loc, target, _expr, _typ) in wires {
                terminals_remaining = terminals_remaining.into_iter().filter(|(path, _component)| path != target).collect();
            }
        }

        for (path, remaining_component) in terminals_remaining.into_iter() {
            let mut is_incoming_port = false;

            if let Component::Incoming(_loc, _name, _typ) = &*remaining_component {
                is_incoming_port = true;
            }

            let is_local = !path.contains(".");

            if !is_local && is_incoming_port {
                let inst_component = self.component_from(component.clone(), path.parent()).unwrap();
                errors.push(BitsyError::NoDriversPort(inst_component.clone(), remaining_component.clone()));
            } else if is_local && !is_incoming_port {
                errors.push(BitsyError::NoDrivers(remaining_component.clone()));
            }
        }
        errors
    }
}
