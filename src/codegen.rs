use crate::parser::*;

pub trait CodeGen {
    fn codegen(&self);
}

impl CodeGen for Program {
    fn codegen(&self) {

    }
}