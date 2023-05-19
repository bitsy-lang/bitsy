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
pub struct Shape(Arc<ShapeNode>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type(Arc<TypeNode>);

#[derive(Debug, Clone)]
pub struct Expr(Arc<ExprNode>, Loc);


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
pub struct ShapeNode {
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
    payload: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct StructField(pub FieldName, pub Type);

#[derive(Debug, Clone)]
pub struct Reference(Arc<crate::Component>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeNode {
    Family(Shape, Vec<Type>),
    Ref(Reference),
    Var(String),
    Nat(u64),
}

#[derive(Debug, Clone)]
pub enum ExprNode {
    Var(String),
    Lit(Value),
    Field(Expr, String),
    Cast(Expr, Type),
    Let(String, Expr, Option<Type>, Expr),
    BinOp(BinOp, Expr, Expr),
    Eq(Expr, Expr),
    Neq(Expr, Expr),
    Call(FnRef, Vec<Expr>),
    Slice(Expr, Expr),
    SliceConst(Expr, u64),
    Match(Expr, Vec<MatchArm>),
    If(Expr, Expr, Expr),
    Tuple(Vec<Expr>),
    Struct(Vec<(FieldName, Expr)>),
    Enum(CtorName, Option<Expr>),
    Hole(String),
}


//////////////////////////
// Traits
//////////////////////////

impl std::ops::Deref for Type {
    type Target = TypeNode;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::Deref for Shape {
    type Target = ShapeNode;

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

impl PartialEq for Shape {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Shape {}

impl PartialEq for Reference {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Reference {}


//////////////////////////
// Other
//////////////////////////

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let shape_def: &TypeNode = &self.0;
        match shape_def {
            TypeNode::Var(x) => write!(f, "{x}"),
            TypeNode::Nat(n) => write!(f, "{n}"),
            TypeNode::Family(shape_family, args) => {
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
            },
            TypeNode::Ref(component) => write!(f, "Ref<{}>", component.0.name()),
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

impl Shape {
    pub fn builtins() -> Vec<Shape> {
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
    ) -> Shape {
        let shape_family_def = ShapeNode {
            name: name.to_string(),
            params: Some(args),
            decl: ShapeFamilyDecl::Enum(alts),
        };
        Shape(Arc::new(shape_family_def))
    }

    pub fn new_struct(
        name: &str,
        args: Context<Kind>,
        fields: Vec<StructField>,
    ) -> Shape {
        let shape_family_def = ShapeNode {
            name: name.to_string(),
            params: Some(args),
            decl: ShapeFamilyDecl::Struct(fields),
        };
        Shape(Arc::new(shape_family_def))
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
    static ref BIT_SHAPE_FAMILY: Shape = {
        Shape(Arc::new(ShapeNode {
            name: "Bit".to_string(),
            params: Some(Context::empty()),
            decl: ShapeFamilyDecl::Builtin,
        }))
    };

    static ref WORD_SHAPE_FAMILY: Shape = {
        Shape(Arc::new(ShapeNode {
            name: "Word".to_string(),
            params: Some(Context::from(vec![("n".to_string(), Kind::Nat)])),
            decl: ShapeFamilyDecl::Builtin,
        }))
    };

    static ref TUPLE_SHAPE_FAMILY: Shape = {
        Shape(Arc::new(ShapeNode {
            name: "Tuple".to_string(),
            params: None, // None means variadic
            decl: ShapeFamilyDecl::Builtin,
        }))
    };
}

impl Type {
    pub fn bit() -> Type {
        Type::family(BIT_SHAPE_FAMILY.clone(), vec![])
    }

    pub fn word(n: u64) -> Type {
        Type::family(WORD_SHAPE_FAMILY.clone(), vec![Type::nat(n)])
    }

    pub fn tuple(args: Vec<Type>) -> Type {
        Type::family(TUPLE_SHAPE_FAMILY.clone(), args)
    }

    pub fn family(shape_family: Shape, args: Vec<Type>) -> Type {
        Type(Arc::new(TypeNode::Family(shape_family, args)))
    }

    pub fn nat(n: u64) -> Type {
        Type(Arc::new(TypeNode::Nat(n)))
    }

    pub fn var(x: String) -> Type {
        Type(Arc::new(TypeNode::Var(x)))
    }

    pub fn ref_(component: Arc<Component>) -> Type {
        Type(Arc::new(TypeNode::Ref(Reference(component))))
    }

    pub fn enum_alts(&self) -> Option<Vec<EnumAlt>> {
        match self.as_node() {
            TypeNode::Family(shape_family, args) => {
                match shape_family.decl() {
                    ShapeFamilyDecl::Builtin => None,
                    ShapeFamilyDecl::Enum(alts) => Some(alts.to_vec()),
                    ShapeFamilyDecl::Struct(fs) => None,
                }
            },
            _ => None,
        }
    }

    pub fn as_tuple(&self) -> Option<Vec<Type>> {
        match self.as_node() {
            TypeNode::Family(shape_family, args) => {
                if shape_family == &*TUPLE_SHAPE_FAMILY {
                    Some(args.clone())
                } else {
                    None
                }
            },
            _ => None,
        }
    }

    pub fn as_word(&self, context: &Context<Type>) -> Option<u64> {
        match self.as_node() {
            TypeNode::Family(shape_family, args) => {
                if shape_family == &*WORD_SHAPE_FAMILY {
                    assert_eq!(args.len(), 1);
                    match args[0].as_node() {
                        TypeNode::Nat(n) => Some(*n),
                        TypeNode::Var(x) => {
                            if let Some(s) = context.lookup(x) {
                                if let TypeNode::Nat(n) = s.as_node() {
                                    Some(*n)
                                } else {
                                    error!("Panic");
                                    panic!("?")
                                }
                            } else {
                                todo!()
                            }
                        },
                        _ => {
                            error!("Panic");
                            panic!("Expected Nat-kinded argument")
                        },
                    }
                } else {
                    None
                }
            },
            _ => None,
        }
    }

    pub fn as_reference(&self) -> Option<&Component> {
        match self.as_node() {
            TypeNode::Ref(Reference(reference)) => {
                let component: &Component = reference.as_ref();
                Some(component)
            }
            _ => None,
        }
    }

    pub fn is_builtin(&self) -> bool {
        match self.as_node() {
            TypeNode::Family(shape_family, args) => {
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
            TypeNode::Family(shape_family, args) => {
                match shape_family.decl() {
                    ShapeFamilyDecl::Builtin => None,
                    ShapeFamilyDecl::Enum(alts) => None,
                    ShapeFamilyDecl::Struct(fs) => Some(fs.to_vec()),
                }
            },
            _ => None,
        }
    }

    pub fn params(&self) -> Option<Context<Type>> {
        match self.as_node() {
            TypeNode::Family(shape_family, args) => {
                let mut context = Context::empty();
                let params: Context<Kind> = shape_family.params().unwrap();
                for (i, (v, _kind)) in params.iter().enumerate() {
                    context = context.extend(v.to_string(), args[i].clone());
                }
                Some(context)
            },
            _ => None,
        }
    }

    pub fn subst(&self, x: &str, t: Type) -> Type {
        match self.as_node() {
            TypeNode::Family(shape, args) => self.clone(),
            TypeNode::Var(y) => {
                if x == y {
                    t
                } else {
                    self.clone()
                }
            },
            TypeNode::Nat(n) => self.clone(),
            TypeNode::Ref(r) => self.clone(),
        }
    }

    pub fn substs(&self, xts: &[(String, Type)]) -> Type {
        let mut result = self.clone();
        for (x, t) in xts {
            result = result.subst(x, t.clone());
        }
        result
    }

    pub fn as_node(&self) -> &TypeNode {
        self.0.as_ref()
    }
}

impl ExprNode {
    pub fn at(self, loc: Loc) -> Expr {
        Expr(Arc::new(self), loc)
    }
}


impl EnumAlt {
    pub fn new(ctor_name: String, payload: Option<Type>) -> EnumAlt {
        EnumAlt { ctor_name, payload }
    }

    pub fn ctor_name(&self) -> &str {
        &self.ctor_name
    }

    pub fn payload(&self) -> Option<Type> {
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

impl Expr {
    pub fn loc(&self) -> &Loc {
        &self.1
    }

    pub fn as_node(&self) -> &ExprNode {
        &self.0
    }

    pub fn var(loc: Loc, s: String) -> Expr {
        ExprNode::Var(s).at(loc)
    }

    pub fn lit(loc: Loc, v: Value) -> Expr {
        ExprNode::Lit(v).at(loc)
    }

    pub fn field(loc: Loc, subject: Expr, field: String) -> Expr {
        ExprNode::Field(subject, field).at(loc)
    }
    pub fn cast_expr(loc: Loc, e: Expr, typ: Type) -> Expr {
        ExprNode::Cast(e, typ).at(loc)
    }

    pub fn let_expr(loc: Loc, x: String, def: Expr, ascription: Option<Type>, body: Expr) -> Expr {
        ExprNode::Let(x, def, ascription, body).at(loc)
    }

    pub fn binop(loc: Loc, op: BinOp, a: Expr, b: Expr) -> Expr {
        ExprNode::BinOp(op, a, b).at(loc)
    }

    pub fn eq(loc: Loc, a: Expr, b: Expr) -> Expr {
        ExprNode::Eq(a, b).at(loc)
    }

    pub fn neq(loc: Loc, a: Expr, b: Expr) -> Expr {
        ExprNode::Neq(a, b).at(loc)
    }

    pub fn match_expr(loc: Loc, subject: Expr, arms: Vec<MatchArm>) -> Expr {
        ExprNode::Match(subject, arms).at(loc)
    }

    pub fn if_expr(loc: Loc, subject: Expr, true_branch: Expr, false_branch: Expr) -> Expr {
        ExprNode::If(subject, true_branch, false_branch).at(loc)
    }

    pub fn fn_call(loc: Loc, fn_ref: FnRef, args: Vec<Expr>) -> Expr {
        ExprNode::Call(fn_ref, args).at(loc)
    }

    pub fn tuple(loc: Loc, exprs: Vec<Expr>) -> Expr {
        ExprNode::Tuple(exprs).at(loc)
    }

    pub fn struct_expr(loc: Loc, field_values: Vec<(FieldName, Expr)>) -> Expr {
        ExprNode::Struct(field_values).at(loc)
    }

    pub fn enum_expr(loc: Loc, ctor_name: String, payload: Option<Expr>) -> Expr {
        ExprNode::Enum(ctor_name, payload).at(loc)
    }

    pub fn slice(loc: Loc, subject: Expr, index: Expr) -> Expr {
        ExprNode::Slice(subject, index).at(loc)
    }

    pub fn slice_const(loc: Loc, subject: Expr, index: u64) -> Expr {
        ExprNode::SliceConst(subject, index).at(loc)
    }

    pub fn hole_expr(loc: Loc, contents: String) -> Expr {
        ExprNode::Hole(contents).at(loc)
    }
}
