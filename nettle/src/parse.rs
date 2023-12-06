use super::*;

use std::sync::Arc;

use lalrpop_util::ParseError;
use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar);

#[derive(Debug)]
pub enum ModDecl {
    Component(Arc<Component>),
    Wire(Wire),
    When(When),
}

pub fn parse_top(package: &str, top: Option<&str>) -> Result<Circuit, Vec<CircuitError>>  {
    let source_info = SourceInfo::from_string(package);
    let package: Package = match grammar::PackageParser::new().parse(&source_info, package) {
        Ok(package) => {
            package.resolve_references()?;
            package
        },
        Err(ParseError::UnrecognizedToken { token, expected }) => {
            let start_idx = token.0;
            let end_idx = token.2;
            let loc = Loc::from(&source_info, start_idx, end_idx);

            let message = format!("Parse error: Expected one of {}", expected.join(" "));
            return Err(vec![CircuitError::ParseError(loc, message)]);
        },
        _ => {
            let message = format!("Parse error");
            return Err(vec![CircuitError::ParseError(Loc::unknown(), message)]);
        },
    };

    let moddefs = package.moddefs();
    let top = top.unwrap_or_else(|| moddefs.first().unwrap().name());
    Ok(Circuit::new(package, top))
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        let source_info = SourceInfo::from_string(expr);
        *grammar::ExprParser::new().parse(&source_info, expr).unwrap()
    }
}
