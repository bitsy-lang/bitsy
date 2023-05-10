use super::*; // todo!()
use std::sync::Arc;
use lazy_static::lazy_static;

//////////////////////////
// Refs
//////////////////////////

#[derive(Debug, Clone)]
pub struct Module(Arc<ModuleDef>);

#[derive(Debug, Clone)]
pub struct Gate(Arc<GateDef>);

#[derive(Debug, Clone)]
pub struct ShapeFamily(Arc<ShapeFamilyNode>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Shape(Arc<ShapeNode>);

#[derive(Debug, Clone)]
pub struct Expr(Arc<ExprNode>);


//////////////////////////
// Defs
//////////////////////////

#[derive(Debug)]
pub struct ModuleDef {
    name: String,
    ports: Vec<Port>,
    terminals: Vec<Terminal>,
    components: Vec<Component>,
    wires: Vec<Wire>,
}

#[derive(Debug, Clone)]
pub struct GateDef {
    name: String,
    // terminals // todo!()
    // action // todo!()
}


#[derive(Debug, Clone)]
pub struct ShapeFamilyNode {
    name: String,
    params: Option<Context<Kind>>, // Option for variadic
    decl: ShapeFamilyDecl,
}

#[derive(Debug, Clone)]
pub enum ShapeFamilyDecl {
    Builtin,
    Enum(Vec<EnumAlt>),
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone)]
pub struct EnumAlt {
    ctor_name: String,
    payload: Option<Shape>,
}

#[derive(Debug, Clone)]
pub struct StructField(pub FieldName, pub Shape);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ShapeNode {
    Family(ShapeFamily, Vec<Shape>),
    Var(String),
    Nat(u64),
}

#[derive(Debug, Clone)]
pub enum ExprNode {
    Var(String),
    Lit(Value),
    Let(String, Expr, Option<Shape>, Expr),
    Add(Expr, Expr),
    Mul(Expr, Expr),
    Eq(Expr, Expr),
    Neq(Expr, Expr),
    Match(Expr, Vec<MatchArm>),
    Tuple(Vec<Expr>),
    Struct(Vec<(FieldName, Expr)>),
    Enum(CtorName, Option<Expr>),
}


//////////////////////////
// Traits
//////////////////////////

impl std::ops::Deref for Shape {
    type Target = ShapeNode;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::Deref for ShapeFamily {
    type Target = ShapeFamilyNode;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Module {}

impl PartialEq for Gate {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Gate {}

impl PartialEq for ShapeFamily {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for ShapeFamily {}


//////////////////////////
// Other
//////////////////////////

impl std::fmt::Display for Shape {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let shape_def: &ShapeNode = &self.0;
        match shape_def {
            ShapeNode::Var(x) => write!(f, "{x}"),
            ShapeNode::Nat(n) => write!(f, "{n}"),
            ShapeNode::Family(shape_family, args) => {
                write!(f, "{}", shape_family.name())?;
                if args.len() > 0 {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        write!(f, "{arg}")?;
                        if i + 1 < args.len() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
        }
    }
}

impl Module {
    pub fn new(name: &str) -> Module {
        let module_def = ModuleDef {
            name: name.to_string(),
            ports: vec![],
            terminals: vec![],
            components: vec![],
            wires: vec![],
        };
        Module(Arc::new(module_def))
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn component(&self, component_name: &str) -> Option<&Component> {
        for component in &self.0.components {
            if component.name() == component_name {
                return Some(component);
            }
        }
        None
    }

    pub fn terminal(&self, component_name: &str, port_name: &str) -> Option<&Terminal> {
        for terminal in &self.0.terminals {
            if terminal.0 == component_name && terminal.1 == port_name {
                return Some(terminal);
            }
        }
        None
    }
}

impl ShapeFamily {
    pub fn builtins() -> Vec<ShapeFamily> {
        vec![
            BIT_SHAPE_FAMILY.clone(),
            WORD_SHAPE_FAMILY.clone(),
            TUPLE_SHAPE_FAMILY.clone(),
        ]
    }

    pub fn new_enum(
        name: &str,
        args: Context<Kind>,
        alts: Vec<EnumAlt>,
    ) -> ShapeFamily {
        let shape_family_def = ShapeFamilyNode {
            name: name.to_string(),
            params: Some(args),
            decl: ShapeFamilyDecl::Enum(alts),
        };
        ShapeFamily(Arc::new(shape_family_def))
    }

    pub fn new_struct(
        name: &str,
        args: Context<Kind>,
        fields: Vec<StructField>,
    ) -> ShapeFamily {
        let shape_family_def = ShapeFamilyNode {
            name: name.to_string(),
            params: Some(args),
            decl: ShapeFamilyDecl::Struct(fields),
        };
        ShapeFamily(Arc::new(shape_family_def))
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn params(&self) -> Option<Context<Kind>> {
        self.params.clone()
    }

    pub fn decl(&self) -> &ShapeFamilyDecl {
        &self.0.decl
    }
}

lazy_static! {
    static ref BIT_SHAPE_FAMILY: ShapeFamily = {
        ShapeFamily(Arc::new(ShapeFamilyNode {
            name: "Bit".to_string(),
            params: Some(Context::empty()),
            decl: ShapeFamilyDecl::Builtin,
        }))
    };

    static ref WORD_SHAPE_FAMILY: ShapeFamily = {
        ShapeFamily(Arc::new(ShapeFamilyNode {
            name: "Word".to_string(),
            params: Some(Context::from(vec![("n".to_string(), Kind::Nat)])),
            decl: ShapeFamilyDecl::Builtin,
        }))
    };

    static ref TUPLE_SHAPE_FAMILY: ShapeFamily = {
        ShapeFamily(Arc::new(ShapeFamilyNode {
            name: "Tuple".to_string(),
            params: None, // None means variadic
            decl: ShapeFamilyDecl::Builtin,
        }))
    };
}

impl Shape {
    pub fn bit() -> Shape {
        Shape::family(BIT_SHAPE_FAMILY.clone(), vec![])
    }

    pub fn word(n: u64) -> Shape {
        Shape::family(WORD_SHAPE_FAMILY.clone(), vec![Shape::nat(n)])
    }

    pub fn tuple(args: Vec<Shape>) -> Shape {
        Shape::family(TUPLE_SHAPE_FAMILY.clone(), args)
    }

    pub fn family(shape_family: ShapeFamily, args: Vec<Shape>) -> Shape {
        Shape(Arc::new(ShapeNode::Family(shape_family, args)))
    }

    pub fn nat(n: u64) -> Shape {
        Shape(Arc::new(ShapeNode::Nat(n)))
    }

    pub fn var(x: String) -> Shape {
        Shape(Arc::new(ShapeNode::Var(x)))
    }

    pub fn enum_alts(&self) -> Option<Vec<EnumAlt>> {
        match self.as_node() {
            ShapeNode::Family(shape_family, args) => {
                match shape_family.decl() {
                    ShapeFamilyDecl::Builtin => None,
                    ShapeFamilyDecl::Enum(alts) => Some(alts.to_vec()),
                    ShapeFamilyDecl::Struct(fs) => None,
                }
            },
            _ => None,
        }
    }

    pub fn as_tuple(&self) -> Option<Vec<Shape>> {
        match self.as_node() {
            ShapeNode::Family(shape_family, args) => {
                if shape_family == &*TUPLE_SHAPE_FAMILY {
                    Some(args.clone())
                } else {
                    None
                }
            },
            _ => None,
        }
    }

    pub fn as_word(&self) -> Option<u64> {
        match self.as_node() {
            ShapeNode::Family(shape_family, args) => {
                if shape_family == &*WORD_SHAPE_FAMILY {
                    assert_eq!(args.len(), 1);
                    if let ShapeNode::Nat(n) = args[0].as_node() {
                        Some(*n)
                    } else {
                        panic!("Expected Nat-kinded argument")
                    }
                } else {
                    None
                }
            },
            _ => None,
        }
    }

    pub fn is_builtin(&self) -> bool {
        match self.as_node() {
            ShapeNode::Family(shape_family, args) => {
                match shape_family.decl() {
                    ShapeFamilyDecl::Builtin => true,
                    ShapeFamilyDecl::Enum(alts) => false,
                    ShapeFamilyDecl::Struct(fs) => false,
                }
            },
            _ => false,
        }
    }

    pub fn enum_alt(&self, ctor_name: &str) -> Option<EnumAlt> {
        for alt in &self.enum_alts()? {
            if alt.ctor_name == ctor_name {
                return Some(alt.clone());
            }
        }
        None
    }

    pub fn as_struct(&self) -> Option<Vec<StructField>> {
        match self.as_node() {
            ShapeNode::Family(shape_family, args) => {
                match shape_family.decl() {
                    ShapeFamilyDecl::Builtin => None,
                    ShapeFamilyDecl::Enum(alts) => None,
                    ShapeFamilyDecl::Struct(fs) => Some(fs.to_vec()),
                }
            },
            _ => None,
        }
    }

    pub fn as_node(&self) -> &ShapeNode {
        self.0.as_ref()
    }
}

impl EnumAlt {
    pub fn new(ctor_name: String, payload: Option<Shape>) -> EnumAlt {
        EnumAlt { ctor_name, payload }
    }

    pub fn ctor_name(&self) -> &str {
        &self.ctor_name
    }

    pub fn payload(&self) -> Option<Shape> {
        self.payload.clone()
    }
}

impl Gate {
    pub fn builtins() -> Vec<Gate> {
        vec![
            Gate(Arc::new(GateDef { name: "And".to_string() })),
            Gate(Arc::new(GateDef { name: "Or".to_string()  })),
            Gate(Arc::new(GateDef { name: "Not".to_string() })),
        ]
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}

impl From<ExprNode> for Expr {
    fn from(node: ExprNode) -> Expr {
        Expr(Arc::new(node))
    }
}

impl Expr {
    pub fn as_node(&self) -> &ExprNode {
        &self.0
    }

    pub fn var(s: String) -> Expr {
        ExprNode::Var(s).into()
    }

    pub fn lit(v: Value) -> Expr {
        ExprNode::Lit(v).into()
    }

    pub fn let_expr(x: String, def: Expr, ascription: Option<Shape>, body: Expr) -> Expr {
        ExprNode::Let(x, def, ascription, body).into()
    }

    pub fn add(a: Expr, b: Expr) -> Expr {
        ExprNode::Add(a, b).into()
    }

    pub fn eq(a: Expr, b: Expr) -> Expr {
        ExprNode::Eq(a, b).into()
    }

    pub fn neq(a: Expr, b: Expr) -> Expr {
        ExprNode::Neq(a, b).into()
    }

    pub fn match_expr(subject: Expr, arms: Vec<MatchArm>) -> Expr {
        ExprNode::Match(subject, arms).into()
    }

    pub fn tuple(exprs: Vec<Expr>) -> Expr {
        ExprNode::Tuple(exprs).into()
    }

    pub fn struct_expr(field_values: Vec<(FieldName, Expr)>) -> Expr {
        ExprNode::Struct(field_values).into()
    }

    pub fn enum_expr(ctor_name: String, payload: Option<Expr>) -> Expr {
        ExprNode::Enum(ctor_name, payload).into()
    }
}
