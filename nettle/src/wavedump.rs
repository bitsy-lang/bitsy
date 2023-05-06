use std::sync::Arc;
use std::fmt::Write;
use std::collections::BTreeMap;

pub struct Wavedump {
    top: Arc<Mod>,
    shortnames: BTreeMap<String, String>,
    i: usize,
}

impl Wavedump {
    pub fn new(top: Arc<Mod>) -> Self {

        let mut wavedump = Wavedump {
            top: top.clone(),
            shortnames: BTreeMap::new(),
            i: 0,
        };
        wavedump.make_shortnames(top);
        wavedump
    }

    pub fn gen_shortname(&mut self) -> String {
        self.i += 1;
        let mut i = self.i;

        let mut shortname = Vec::new();

        // Allowed characters: ! to ~ (decimal 33 to 126)
        while i > 0 {
            shortname.push(((i % 94) + 33) as u8);
            i = i / 94;
        }

        String::from_utf8_lossy(shortname.as_slice()).into()
    }

    fn make_shortnames(&mut self, module: Arc<Mod>) {
        for var in &module.vars {
            let shortname = self.gen_shortname();
            self.shortnames.insert(var.name.clone(), shortname);
        }

        for submodule in &module.mods {
            self.make_shortnames(submodule.clone());
        }
    }
}

impl Wavedump {
    pub fn write_header(&self, f: &mut dyn Write) -> std::fmt::Result {
        writeln!(f, "$date")?;
        writeln!(f, "    Sat Apr 29 15:46:50 2023")?;
        writeln!(f, "$end")?;

        writeln!(f, "$version")?;
        writeln!(f, "    Nettle")?;
        writeln!(f, "$end")?;

        self.write_definitions(f, &self.top)?;

        writeln!(f, "#0")?;
        writeln!(f, "1\"")?;
        writeln!(f, "#10")?;
        writeln!(f, "0\"")?;
        writeln!(f, "#20")?;
        writeln!(f, "1\"")?;
        writeln!(f, "#30")?;

        Ok(())
    }

    fn write_definitions(&self, f: &mut dyn Write, module: &Mod) -> std::fmt::Result {
        self.write_module_definitions(f, module, &"")?;

        writeln!(f, "$enddefinitions $end")?;
        writeln!(f, "$dumpsvars")?;

        for (_name, shortname) in &self.shortnames {
            writeln!(f, "0{shortname}")?;
        }
        writeln!(f, "$end")?;

        Ok(())
    }

    fn write_module_definitions(&self, f : &mut dyn Write, module: &Mod, indent: &str) -> std::fmt::Result {
        writeln!(f, "{indent}$scope module {} $end", module.name)?;

        for var in &module.vars {
            let shortname = &self.shortnames[&var.name];
            writeln!(f, "{indent}    $var {} {} {} {} {} $end", var.typ, var.size, shortname, var.name, var.indexing)?;
        }

        let mut new_indent = String::from(indent);
        new_indent.push_str(&"    ");
        for submodule in &module.mods {
            self.write_module_definitions(f, submodule, &new_indent)?;
        }

        writeln!(f, "$upscope $end")?;
        Ok(())
    }
}

pub struct Var {
    pub typ: String,
    pub size: usize,
    pub name: String,
    pub indexing: String,
}

pub struct Mod {
    pub name: String,
    pub vars: Vec<Var>,
    pub mods: Vec<Arc<Mod>>,
}



/*
pub fn write_change(f : &mut dyn Write) -> std::fmt::Result {
    writeln!(f, "$var reg 8 ! data [7:0] $end")?;

    Ok(())
}
*/
