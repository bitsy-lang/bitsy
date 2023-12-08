use super::*;

use std::sync::Arc;

use lalrpop_util::ParseError;
use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar);

#[derive(Debug)]
enum ModDecl {
    Component(Arc<Component>),
    Wire(Wire),
    When(When),
}

pub fn load_package_from_file<P: AsRef<std::path::Path>>(path: P) -> Result<Package, Vec<CircuitError>> {
    let package_text = std::fs::read_to_string(path.as_ref()).unwrap();
    let source_info = SourceInfo::from_file(path.as_ref(), &package_text);
    package_from_string(source_info, &package_text)
}

pub fn load_package_from_string(package_text: &str) -> Result<Package, Vec<CircuitError>> {
    let source_info = SourceInfo::from_string(package_text);
    package_from_string(source_info, package_text)
}

fn package_from_string(source_info: SourceInfo, package_text: &str) -> Result<Package, Vec<CircuitError>> {
    let package: Package = match grammar::PackageParser::new().parse(&source_info, &package_text) {
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
    Ok(package)
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        let source_info = SourceInfo::from_string(expr);
        *grammar::ExprParser::new().parse(&source_info, expr).unwrap()
    }
}
