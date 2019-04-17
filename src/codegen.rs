extern crate llvm_sys as llvm;

use crate::parser::*;
use self::llvm::{LLVMContext, LLVMModule};

pub trait IrGen {
    fn build(&self, ctx: *mut LLVMContext, module: *mut LLVMModule);
}

impl IrGen for FunDefinition {
    fn build(&self, ctx: *mut LLVMContext, module: *mut LLVMModule) {
        unsafe {
            let builder = llvm::core::LLVMCreateBuilderInContext(ctx);
            let int = llvm::core::LLVMInt64TypeInContext(ctx);

            let mut params = vec![int; self.args.len()];
            let function_type = llvm::core::LLVMFunctionType(int, params.as_mut_ptr(), 0, 0);
            let function =
                llvm::core::LLVMAddFunction(module, format!("{}\0", self.name).as_ptr() as *const _, function_type);

            let block = llvm::core::LLVMAppendBasicBlockInContext(ctx,
                                                               function,
                                                               b"entry\0".as_ptr() as *const _);
            llvm::core::LLVMPositionBuilderAtEnd(builder, block);

            llvm::core::LLVMBuildRetVoid(builder);
            llvm::core::LLVMDisposeBuilder(builder);
        }
    }
}

impl IrGen for Statement {
    fn build(&self, ctx: *mut LLVMContext, module: *mut LLVMModule) {
        match self {
            Statement::FunDefinition(fun_def) => fun_def.build(ctx, module)
        }
    }
}

pub trait Compile {
    fn compile(&self, name: &str);
}

impl Compile for Program {
    fn compile(&self, name: &str) {
        unsafe {
            let ctx = llvm::core::LLVMContextCreate();
            let module = llvm::core::LLVMModuleCreateWithName(format!("{}\0", name).as_ptr() as *const _);

            self.get_statements().iter().for_each(|item| {
                item.build(ctx, module);
            });

            llvm::core::LLVMDumpModule(module);
            llvm::core::LLVMDisposeModule(module);
            llvm::core::LLVMContextDispose(ctx);
        }
    }
}