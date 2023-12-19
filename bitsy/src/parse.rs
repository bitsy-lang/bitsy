use super::*;

pub fn load_package_from_file<P: AsRef<std::path::Path>>(path: P) -> Result<Package, Vec<BitsyError>> {
    let package_text = std::fs::read_to_string(path.as_ref()).unwrap();
    let source_info = SourceInfo::from_file(path.as_ref(), &package_text);
    package_from_string(source_info, &package_text)
}

pub fn load_package_from_string(package_text: &str) -> Result<Package, Vec<BitsyError>> {
    let source_info = SourceInfo::from_string(package_text);
    package_from_string(source_info, package_text)
}

fn package_from_string(_source_info: SourceInfo, package_text: &str) -> Result<Package, Vec<BitsyError>> {
    // TODO source info
    let package_ast = crate::ast::parse_package_from_string(package_text)?;
    Package::from(&package_ast)
}
