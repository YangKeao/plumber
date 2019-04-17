extern crate llvm_sys as llvm;

use self::llvm::core::{LLVMAddTargetDependentFunctionAttr, LLVMBuildAdd, LLVMBuildCall, LLVMBuildMul, LLVMBuildSub, LLVMConstInt, LLVMGetNamedFunction, LLVMGetParam, LLVMBuildSDiv};
use self::llvm::execution_engine::LLVMRecompileAndRelinkFunction;
use self::llvm::prelude::LLVMValueRef;
use self::llvm::{LLVMBuilder, LLVMContext, LLVMModule, LLVMValue};
use crate::parser::*;
use std::collections::HashMap;

pub trait IrGen {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &HashMap<&str, LLVMValueRef>,
    ) -> Option<LLVMValueRef>;
}

impl IrGen for MonadicExpression {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &HashMap<&str, LLVMValueRef>,
    ) -> Option<LLVMValueRef> {
        let int = unsafe { llvm::core::LLVMInt64TypeInContext(ctx) };
        match self {
            MonadicExpression::FunctionCall(fun_call) => {
                let function = unsafe {
                    LLVMGetNamedFunction(
                        module,
                        format!("{}\0", fun_call.function_name.get_name()).as_ptr() as *const _,
                    )
                };
                let mut args: Vec<_> = fun_call
                    .args
                    .iter()
                    .map(|item| item.build(ctx, module, builder, value_map).unwrap())
                    .collect();
                Some(unsafe {
                    // TODO: set LLVMBuildCall Name
                    LLVMBuildCall(
                        builder,
                        function,
                        args.as_mut_ptr(),
                        args.len() as u32,
                        format!("{}\0", fun_call.function_name.get_name()).as_ptr() as *const _,
                    )
                })
            }
            MonadicExpression::Variable(var) => Some(*value_map.get(var.get_name()).unwrap()),
            MonadicExpression::Const(num) => Some(unsafe { LLVMConstInt(int, *num as u64, 0) }),
        }
    }
}

impl IrGen for BinaryExpression {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &HashMap<&str, LLVMValueRef>,
    ) -> Option<LLVMValueRef> {
        let left = self.left.build(ctx, module, builder, value_map).unwrap();
        let right = self.right.build(ctx, module, builder, value_map).unwrap();
        match self.op {
            BinaryOperation::Plus => Some(unsafe {
                LLVMBuildAdd(builder, left, right, b"addtmp\0".as_ptr() as *const _)
            }),
            BinaryOperation::Minus => Some(unsafe {
                LLVMBuildSub(builder, left, right, b"subtmp\0".as_ptr() as *const _)
            }),
            BinaryOperation::Mul => Some(unsafe {
                LLVMBuildMul(builder, left, right, b"multmp\0".as_ptr() as *const _)
            }),
            BinaryOperation::Div => Some(unsafe {
                LLVMBuildSDiv(builder, left, right, b"multmp\0".as_ptr() as *const _)
            })
        }
    }
}

impl IrGen for Expression {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &HashMap<&str, LLVMValueRef>,
    ) -> Option<LLVMValueRef> {
        match self {
            Expression::BinaryExpression(binary_expr) => {
                binary_expr.build(ctx, module, builder, value_map)
            }
            Expression::MonadicExpression(monadic_expr) => {
                monadic_expr.build(ctx, module, builder, value_map)
            }
        }
    }
}

impl IrGen for FunDefinition {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &HashMap<&str, LLVMValueRef>,
    ) -> Option<LLVMValueRef> {
        unsafe {
            let int = llvm::core::LLVMInt64TypeInContext(ctx);

            let mut params = vec![int; self.args.len()];
            let function_type =
                llvm::core::LLVMFunctionType(int, params.as_mut_ptr(), self.args.len() as u32, 0);
            let function = llvm::core::LLVMAddFunction(
                module,
                format!("{}\0", self.name).as_ptr() as *const _,
                function_type,
            );

            let block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx,
                function,
                b"entry\0".as_ptr() as *const _,
            );
            llvm::core::LLVMPositionBuilderAtEnd(builder, block);

            let mut bind_map = HashMap::new(); // TODO: Join the HashMap in args
            for (index, var) in self.args.iter().enumerate() {
                bind_map.insert(var.get_name(), LLVMGetParam(function, index as u32));
            }
            self.bindings.iter().for_each(|bind| {
                let name = bind.var.get_name();
                let value = bind.value.build(ctx, module, builder, &bind_map).unwrap();
                bind_map.insert(name, value);
            });
            llvm::core::LLVMBuildRet(
                builder,
                self.expr.build(ctx, module, builder, &bind_map).unwrap(),
            );
        }
        None
    }
}

impl IrGen for Statement {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &HashMap<&str, LLVMValueRef>,
    ) -> Option<LLVMValueRef> {
        match self {
            Statement::FunDefinition(fun_def) => fun_def.build(ctx, module, builder, value_map),
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
            let builder = llvm::core::LLVMCreateBuilderInContext(ctx);
            let module =
                llvm::core::LLVMModuleCreateWithName(format!("{}\0", name).as_ptr() as *const _);

            self.get_statements().iter().for_each(|item| {
                item.build(ctx, module, builder, &HashMap::new());
            });

            llvm::core::LLVMDisposeBuilder(builder);
            llvm::core::LLVMDumpModule(module);
            llvm::core::LLVMDisposeModule(module);
            llvm::core::LLVMContextDispose(ctx);
        }
    }
}
