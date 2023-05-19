use crate::common::*;
use crate::context::Context;

////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Namespace {
    pub decls: Vec<Decl>,
}

#[derive(Debug)]
pub enum ShapeDef {
    EnumDef(EnumDef),
    StructDef(StructDef),
}

#[derive(Debug)]
pub enum Decl {
    ModDef(ModDef),
    EnumDef(EnumDef),
    StructDef(StructDef),
    FnDef(FnDef),
}

#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: String,
    pub params: Context<ShapeRef>,
    pub result: ShapeRef,
    pub visibility: Visibility,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub params: Context<Kind>,
    pub visibility: Visibility,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField(pub FieldName, pub ShapeRef);

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub params: Context<Kind>,
    pub visibility: Visibility,
    pub alts: Vec<EnumAlt>,
}

#[derive(Debug, Clone)]
pub struct EnumAlt(pub String, pub Option<ShapeRef>);

#[derive(Debug, Clone)]
pub struct ModDef {
    pub name: String,
    pub visibility: Visibility,
    pub ports: Vec<Port>,
    pub components: Vec<Component>,
    pub wires: Vec<Wire>,
}

#[derive(Debug, Clone)]
pub struct Wire(pub Visibility, pub TerminalRef, pub Box<Expr>);

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Loc, String),
    Field(Loc, Box<Expr>, String),
    Lit(Loc, Value),
    Cast(Loc, Box<Expr>, ShapeRef),
    Let(Loc, String, Box<Expr>, Option<ShapeRef>, Box<Expr>),
    BinOp(Loc, BinOp, Box<Expr>, Box<Expr>),
    Eq(Loc, Box<Expr>, Box<Expr>),
    Neq(Loc, Box<Expr>, Box<Expr>),
    Match(Loc, Box<Expr>, Vec<MatchArm>),
    If(Loc, Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Loc, FnRef, Vec<Box<Expr>>),
    Slice(Loc, Box<Expr>, Box<Expr>),
    Tuple(Loc, Vec<Box<Expr>>),
    Struct(Loc, Vec<(FieldName, Box<Expr>)>),
    Enum(Loc, CtorName, Option<Box<Expr>>),
    Hole(Loc, String),
}

#[derive(Debug, Clone)]
pub struct MatchArm(pub Box<MatchPattern>, pub Box<Expr>);

#[derive(Debug, Clone)]
pub enum Component {
    Reg(ComponentName, Visibility, RegComponent),
    Mod(ComponentName, Visibility, ModComponent),
    Gate(ComponentName, Visibility, GateComponent),
    Const(ComponentName, Visibility, Value, ShapeRef),
}

#[derive(Debug, Clone)]
pub struct RegComponent {
    pub shape: ShapeRef,
    pub domain: DomainRef,
    pub init: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct ModComponent {
    pub mod_def_ref: ModDefRef,
}

#[derive(Debug, Clone)]
pub struct GateComponent {
    pub gate_ref: GateRef,
}

#[derive(Debug, Clone)]
pub struct Port(pub String, pub Vec<Pin>);

#[derive(Debug, Clone)]
pub struct Pin(pub String, pub Direction, pub ShapeRef, pub DomainRef);


////////////////////////////////////////////////////////////////////////////////
// Impls
////////////////////////////////////////////////////////////////////////////////

impl Namespace {
    pub fn mod_defs(&self) -> Vec<&ModDef> {
        let mut result = vec![];
        for decl in &self.decls {
            if let Decl::ModDef(mod_def) = decl {
                result.push(mod_def);
            }
        }
        result
    }

    pub fn enum_defs(&self) -> Vec<&EnumDef> {
        let mut result = vec![];
        for decl in &self.decls {
            if let Decl::EnumDef(enum_def) = decl {
                result.push(enum_def);
            }
        }
        result
    }

    pub fn mod_def(&self, name: &str) -> BitsyResult<&ModDef> {
        for mod_def in self.mod_defs() {
            if mod_def.name == name {
                return Ok(&mod_def);
            }
        }

        Err(BitsyError::Unknown(Loc::unknown(), format!("No such module found: {name}")))
    }

    pub fn enum_def(&self, name: &str) -> BitsyResult<&EnumDef> {
        for enum_def in self.enum_defs() {
            if enum_def.name == name {
                return Ok(&enum_def);
            }
        }

        Err(BitsyError::Unknown(Loc::unknown(), format!("No such enum found: {name}")))
    }

    pub fn shape_defs(&self) -> Vec<ShapeDef> {
        let mut results = vec![];
        for decl in &self.decls {
            if let Decl::EnumDef(enum_def) = decl {
                results.push(ShapeDef::EnumDef(enum_def.clone()));
            } else if let Decl::StructDef(struct_def) = decl {
                results.push(ShapeDef::StructDef(struct_def.clone()));
            }
        }
        results
    }

    pub fn shape_def(&self, name: &str) -> BitsyResult<ShapeDef> {
        for decl in &self.decls {
            if decl.name() == name {
                if let Decl::EnumDef(enum_def) = decl {
                    return Ok(ShapeDef::EnumDef(enum_def.clone()));
                } else if let Decl::StructDef(struct_def) = decl {
                    return Ok(ShapeDef::StructDef(struct_def.clone()));
                } else {
                    return Err(BitsyError::Unknown(Loc::unknown(), format!("Decl {name} is not a shape decl")));
                }
            }
        }

        return Err(BitsyError::Unknown(Loc::unknown(), format!("No such shape def found: {name}")))
    }
}

impl ShapeDef {
    pub fn shape_refs(&self) -> Vec<ShapeRef> {
        match self {
            ShapeDef::EnumDef(enum_def) => enum_def.shape_refs(),
            ShapeDef::StructDef(struct_def) => struct_def.shape_refs(),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            ShapeDef::EnumDef(enum_def) => &enum_def.name,
            ShapeDef::StructDef(struct_def) => &struct_def.name,
        }
    }

    pub fn params(&self) -> Context<Kind> {
        match self {
            ShapeDef::EnumDef(enum_def) => enum_def.params.clone(),
            ShapeDef::StructDef(struct_def) => struct_def.params.clone(),
        }
    }
}

impl Decl {
    pub fn name(&self) -> &str {
        match self {
            Decl::ModDef(mod_def) => &mod_def.name,
            Decl::EnumDef(enum_def) => &enum_def.name,
            Decl::StructDef(struct_def) => &struct_def.name,
            Decl::FnDef(fn_def) => &fn_def.name,
        }
    }
}

impl StructDef {
    pub fn shape_refs(&self) -> Vec<ShapeRef> {
        let mut results = vec![];
        for StructField(_field_name, shape_ref) in &self.fields {
            // ignore parameters
            if self.params.lookup(shape_ref.shape_family_name()).is_none() {
                results.push(shape_ref.clone());
            }
            for shape_ref0 in &shape_ref.internal_shape_refs() {
                if self.params.lookup(shape_ref0.shape_family_name()).is_none() {
                    results.push(shape_ref0.clone());
                }
            }
        }
        results
    }
}

impl EnumDef {
    pub fn shape_refs(&self) -> Vec<ShapeRef> {
        let mut results = vec![];

        for EnumAlt(_ctor_name, payload_shape_ref) in &self.alts {
            if let Some(shape_ref) = payload_shape_ref {
                if self.params.lookup(shape_ref.shape_family_name()).is_none() {
                    results.push(shape_ref.clone());
                    results.extend_from_slice(&shape_ref.internal_shape_refs());
                }
            }
        }
        results
    }
}

impl Wire {
    pub fn visibility(&self) -> Visibility {
        self.0
    }

    pub fn sink(&self) -> &TerminalRef {
        &self.1
    }
}

impl Component {
    pub fn name(&self) -> &str {
        match self {
            Component::Reg(name, _, _) => name,
            Component::Mod(name, _, _) => name,
            Component::Gate(name, _, _) => name,
            Component::Const(name, _, _, _) => name,
        }
    }

    pub fn port<T>(&self, port_name: T) -> TerminalRef
        where T: Into<PortName> {
        TerminalRef(self.name().to_string(), port_name.into())
    }
}

impl Pin {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn direction(&self) -> Direction {
        self.1
    }

    pub fn shape(&self) -> &ShapeRef {
        &self.2
    }

    pub fn domain(&self) -> &DomainRef {
        &self.3
    }
}

impl ModDef {
    pub fn depends_on(&self) -> Vec<ModDefRef> {
        let mut result = vec![];
        for component in &self.components {
            if let Component::Mod(_name, _visibility, mod_component) = component {
                result.push(mod_component.mod_def_ref.clone());
            }
        }
        result
    }
}
