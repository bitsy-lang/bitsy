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

pub fn load_package_from_file<P: AsRef<std::path::Path>>(path: P) -> Result<Package, Vec<BitsyError>> {
    let package_text = std::fs::read_to_string(path.as_ref()).unwrap();
    let source_info = SourceInfo::from_file(path.as_ref(), &package_text);
    package_from_string(source_info, &package_text)
}

pub fn load_package_from_string(package_text: &str) -> Result<Package, Vec<BitsyError>> {
    let source_info = SourceInfo::from_string(package_text);
    package_from_string(source_info, package_text)
}

fn package_from_string(source_info: SourceInfo, package_text: &str) -> Result<Package, Vec<BitsyError>> {
    match grammar::PackageParser::new().parse(&source_info, &package_text) {
        Ok(decls) => {
            Package::new(decls)
        },
        Err(ParseError::UnrecognizedToken { token, expected }) => {
            let start_idx = token.0;
            let end_idx = token.2;
            let loc = Loc::from(&source_info, start_idx, end_idx);

            let message = format!("Parse error: Expected one of {}", expected.join(" "));
            return Err(vec![BitsyError::ParseError(loc, message)]);
        },
        Err(ParseError::InvalidToken { location }) => {
            let loc = Loc::from(&source_info, location, location + 1);
            let message = format!("Parse error");
            return Err(vec![BitsyError::ParseError(loc, message)]);
        },
        Err(ParseError::ExtraToken { token }) => {
            let start_idx = token.0;
            let end_idx = token.2;
            let loc = Loc::from(&source_info, start_idx, end_idx);
            let message = format!("Parse error: extra token: {token:?}");
            return Err(vec![BitsyError::ParseError(loc, message)]);
        },
        Err(ParseError::UnrecognizedEof { location, expected }) => {
            let loc = Loc::from(&source_info, location, location + 1);
            let message = format!("Parse error: Unexpected end of file: Expected {expected:?}");
            return Err(vec![BitsyError::ParseError(loc, message)]);
        },
        Err(ParseError::User { error }) => {
            let message = format!("Parse error: {error:?}");
            return Err(vec![BitsyError::ParseError(Loc::unknown(), message)]);
        },
    }
}

impl From<&str> for Expr {
    fn from(expr: &str) -> Expr {
        let source_info = SourceInfo::from_string(expr);
        Expr::clone(&grammar::ExprParser::new().parse(&source_info, expr).unwrap())
    }
}
