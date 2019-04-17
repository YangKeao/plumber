extern crate llvm_sys as llvm;

use crate::parser::*;

pub trait CodeGen {
    fn codegen(&self, name: &[u8]);
}

impl CodeGen for Program {
    fn codegen(&self, name: &[u8]) {
        unsafe {
            let context = llvm::core::LLVMContextCreate();
            let module = llvm::core::LLVMModuleCreateWithName(name.as_ptr() as *const _);

            let builder = llvm::core::LLVMCreateBuilderInContext(context);
            // Get the type signature for void nop(void);
            // Then create it in our module.
            let void = llvm::core::LLVMVoidTypeInContext(context);
            let function_type = llvm::core::LLVMFunctionType(void, std::ptr::null_mut(), 0, 0);
            let function =
                llvm::core::LLVMAddFunction(module, b"nop\0".as_ptr() as *const _, function_type);

            // Create a basic block in the function and set our builder to generate
            // code in it.
            let bb = llvm::core::LLVMAppendBasicBlockInContext(context,
                                                               function,
                                                               b"entry\0".as_ptr() as *const _);
            llvm::core::LLVMPositionBuilderAtEnd(builder, bb);

            // Emit a `ret void` into the function
            llvm::core::LLVMBuildRetVoid(builder);

            // Dump the module as IR to stdout.
            llvm::core::LLVMDumpModule(module);

            // Clean up. Values created in the context mostly get cleaned up there.
            llvm::core::LLVMDisposeBuilder(builder);
            llvm::core::LLVMDisposeModule(module);
            llvm::core::LLVMContextDispose(context);
        }
    }
}