use std::fmt::Write;

use super::ast::*;

pub fn pretty_print(circuit: &Nettle) -> String {
    let mut out = String::new();
    writeln!(out, "domains {{").unwrap();
    for domain in &circuit.domains {
        writeln!(out, "    {} {{}}", domain.name()).unwrap();
    }
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();

    writeln!(out, "signals {{").unwrap();
    for (i, signal) in circuit.signals.iter().enumerate() {
        if i > 0 {
            writeln!(out).unwrap();
        }

        writeln!(out, "    {} : {}@{} {{", signal.path, signal.shape, signal.domain.name()).unwrap();
        for attr in &signal.attrs {
            writeln!(out, "        {attr};").unwrap();
        }
        writeln!(out, "    }}").unwrap();
    }
    writeln!(out, "}}").unwrap();
    out
}

