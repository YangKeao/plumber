extern crate llvm_sys as llvm;

use self::llvm::core::*;
use self::llvm::execution_engine::*;
use self::llvm::prelude::*;
use self::llvm::target::*;

use self::llvm::transforms::scalar::*;
use self::llvm::{LLVMBuilder, LLVMContext, LLVMModule, LLVMPassManager};

use crate::parser::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

macro_rules! into_raw_str {
    ($x: expr) => (
        format!("{}\0", $x).as_ptr() as *const _
    )
}

#[derive(Clone)]
pub struct CompileContext {
    pub llvm_ctx: *mut LLVMContext,
    pub module: *mut LLVMModule,
    pub fpm: *mut LLVMPassManager,
    pub builder: *mut LLVMBuilder,
    pub value_map: Arc<RefCell<HashMap<String, LLVMValueRef>>>,
    pub typ_map: Arc<RefCell<HashMap<String, LLVMTypeRef>>>,
}

pub trait IrGen {
    fn build(&self, _ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        unimplemented!()
    }
}

impl IrGen for StructDefinition {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        let mut fields: Vec<LLVMTypeRef> = self
            .fields
            .iter()
            .map(|field| field.build(ctx).unwrap().1)
            .collect();
        let typ = unsafe {
            LLVMStructTypeInContext(ctx.llvm_ctx, fields.as_mut_ptr(), fields.len() as u32, 0)
        };
        ctx.typ_map
            .borrow_mut()
            .insert(self.name.get_name().to_string(), typ);
        Some((std::ptr::null_mut(), typ))
    }
}

impl IrGen for Typ {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        match self {
            Typ::I8 => Some((std::ptr::null_mut(), unsafe {
                LLVMInt8TypeInContext(ctx.llvm_ctx)
            })),
            Typ::I16 => Some((std::ptr::null_mut(), unsafe {
                LLVMInt16TypeInContext(ctx.llvm_ctx)
            })),
            Typ::I32 => Some((std::ptr::null_mut(), unsafe {
                LLVMInt32TypeInContext(ctx.llvm_ctx)
            })),
            Typ::I64 => Some((std::ptr::null_mut(), unsafe {
                LLVMInt64TypeInContext(ctx.llvm_ctx)
            })),
            Typ::U8 => Some((std::ptr::null_mut(), unsafe {
                LLVMInt8TypeInContext(ctx.llvm_ctx)
            })),
            Typ::U16 => Some((std::ptr::null_mut(), unsafe {
                LLVMInt16TypeInContext(ctx.llvm_ctx)
            })),
            Typ::U32 => Some((std::ptr::null_mut(), unsafe {
                LLVMInt32TypeInContext(ctx.llvm_ctx)
            })),
            Typ::U64 => Some((std::ptr::null_mut(), unsafe {
                LLVMInt64TypeInContext(ctx.llvm_ctx)
            })),
            Typ::Struct(st) => Some((std::ptr::null_mut(),
                *ctx.typ_map
                    .borrow()
                    .get(&st.get_name().to_string())
                    .unwrap()
            )),
        }
    }
}

impl IrGen for MonadicExpression {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        let int = unsafe { llvm::core::LLVMInt64TypeInContext(ctx.llvm_ctx) };
        match self {
            MonadicExpression::FunctionCall(fun_call) => {
                let function = unsafe {
                    LLVMGetNamedFunction(
                        ctx.module,
                        into_raw_str!(fun_call.function_name.get_name()),
                    )
                };
                let function_typ = *ctx
                    .typ_map
                    .borrow()
                    .get(&fun_call.function_name.get_name().to_string())
                    .unwrap();

                let params_size = unsafe { LLVMCountParamTypes(function_typ) as usize };
                let mut params = vec![std::ptr::null_mut(); params_size];
                unsafe { LLVMGetParamTypes(function_typ, params.as_mut_ptr()) }

                let mut args: Vec<_> = fun_call
                    .args
                    .iter()
                    .enumerate()
                    .map(|(index, item)| {
                        let expr = item.build(ctx).unwrap().0;
                        unsafe {
                            LLVMBuildIntCast(
                                ctx.builder,
                                expr,
                                params[index],
                                into_raw_str!("tmpcast"),
                            )
                        }
                    })
                    .collect();
                Some((
                    unsafe {
                        // TODO: set LLVMBuildCall Name
                        LLVMBuildCall(
                            ctx.builder,
                            function,
                            args.as_mut_ptr(),
                            args.len() as u32,
                            into_raw_str!(fun_call.function_name.get_name()),
                        )
                    },
                    unsafe {
                        LLVMGetReturnType(
                            *ctx.typ_map
                                .borrow()
                                .get(fun_call.function_name.get_name())
                                .unwrap(),
                        )
                    },
                ))
            }
            MonadicExpression::Variable(var) => Some((
                *ctx.value_map.borrow().get(var.get_name()).unwrap(),
                std::ptr::null_mut(),
            )), // TODO: Add Type for value
            MonadicExpression::Const(num) => Some((
                unsafe { LLVMConstInt(int, *num as u64, 0) },
                Typ::I64.build(ctx).unwrap().1,
            )),
            MonadicExpression::TypeCast(type_cast) => {
                let value = type_cast.inner.build(ctx).unwrap().0;
                let typ = type_cast.typ.build(ctx).unwrap().1;
                Some((
                    unsafe {
                        LLVMBuildIntCast(ctx.builder, value, typ, into_raw_str!("tmpcast"))
                    },
                    typ,
                ))
            }
        }
    }
}

impl IrGen for BinaryExpression {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        let left = self.left.build(ctx).unwrap().0;
        let right = self.right.build(ctx).unwrap().0; // TODO: cast type for binary operator
        match self.op {
            BinaryOperation::Plus => Some((
                unsafe { LLVMBuildAdd(ctx.builder, left, right, into_raw_str!("addtmp")) },
                std::ptr::null_mut(),
            )),
            BinaryOperation::Minus => Some((
                unsafe { LLVMBuildSub(ctx.builder, left, right, into_raw_str!("minustmp")) },
                std::ptr::null_mut(),
            )),
            BinaryOperation::Mul => Some((
                unsafe { LLVMBuildMul(ctx.builder, left, right, into_raw_str!("multmp")) },
                std::ptr::null_mut(),
            )),
            BinaryOperation::Div => Some((
                unsafe {
                    LLVMBuildSDiv(ctx.builder, left, right, into_raw_str!("divtmp"))
                },
                std::ptr::null_mut(),
            )),
        }
    }
}

impl IrGen for Expression {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        match self {
            Expression::BinaryExpression(binary_expr) => binary_expr.build(ctx),
            Expression::MonadicExpression(monadic_expr) => monadic_expr.build(ctx),
        }
    }
}

impl IrGen for FunDefinition {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        unsafe {
            let ret_typ = self.typ.build(ctx).unwrap().1;

            let mut params: Vec<_> = self
                .args
                .iter()
                .map(|arg| arg.typ.build(ctx).unwrap().1)
                .collect();
            let function_type = llvm::core::LLVMFunctionType(
                ret_typ,
                params.as_mut_ptr(),
                self.args.len() as u32,
                0,
            );

            ctx.typ_map
                .borrow_mut()
                .insert(self.name.to_string(), function_type);

            let function = llvm::core::LLVMAddFunction(
                ctx.module,
                into_raw_str!(self.name),
                function_type,
            );

            let block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.llvm_ctx,
                function,
                into_raw_str!("entry"),
            );
            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, block);

            let mut bind_map = ctx.value_map.borrow_mut().clone();
            for (index, var) in self.args.iter().enumerate() {
                bind_map.insert(var.name.clone(), LLVMGetParam(function, index as u32));
            }

            let mut ctx = ctx.clone();
            ctx.value_map = Arc::new(RefCell::new(bind_map));
            self.bindings.iter().for_each(|bind| {
                let value = bind.value.build(&ctx).unwrap().0;
                let value = LLVMBuildIntCast(
                    ctx.builder,
                    value,
                    bind.var.typ.build(&ctx).unwrap().1,
                    into_raw_str!("tmpcast"),
                );
                ctx.value_map
                    .borrow_mut()
                    .insert(bind.var.name.clone(), value);
            });
            let ret_value = self.expr.build(&ctx).unwrap().0;
            let ret_value = LLVMBuildIntCast(
                ctx.builder,
                ret_value,
                ret_typ,
                into_raw_str!("tmpcast"),
            );
            llvm::core::LLVMBuildRet(ctx.builder, ret_value);

            LLVMRunFunctionPassManager(ctx.fpm, function);
            Some((function, function_type))
        }
    }
}

impl IrGen for Statement {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        match self {
            Statement::FunDefinition(fun_def) => {
                fun_def.build(ctx)
            }
            Statement::StructDefinition(struct_def) => {
                struct_def.build(ctx)
            }
        }
    }
}

pub trait Compile {
    fn compile(&self, name: &str);
}

impl Compile for Program {
    fn compile(&self, name: &str) {
        unsafe {
            let ctx = LLVMContextCreate();
            let builder = LLVMCreateBuilderInContext(ctx);
            let module =
                LLVMModuleCreateWithNameInContext(into_raw_str!(name), ctx);

            let fpm = LLVMCreateFunctionPassManagerForModule(module);
            LLVMAddInstructionCombiningPass(fpm);
            LLVMAddReassociatePass(fpm);
            LLVMAddGVNPass(fpm);
            LLVMAddCFGSimplificationPass(fpm);
            LLVMInitializeFunctionPassManager(fpm); // TODO: Add more OPT pass

            let typ_map = Arc::new(RefCell::new(HashMap::new()));
            self.get_statements().iter().for_each(|item| {
                item.build(&CompileContext {
                    llvm_ctx: ctx,
                    module,
                    fpm,
                    builder,
                    value_map: Arc::new(RefCell::new(HashMap::new())),
                    typ_map: typ_map.clone(),
                });
            });

            LLVMDisposeBuilder(builder);
            LLVMDumpModule(module);

            // Finish generate Codes

            LLVMLinkInMCJIT();
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            let mut execution_engine = std::mem::uninitialized();
            let mut out = std::mem::zeroed();
            LLVMCreateExecutionEngineForModule(&mut execution_engine, module, &mut out);

            let addr = LLVMGetFunctionAddress(execution_engine, into_raw_str!("Brighter"));

            let f: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(addr);

            let x = 1;
            let y = 1;
            let res = f(x, y);

            println!("{} {} {}", x, y, res);

            LLVMDisposeExecutionEngine(execution_engine);
            LLVMContextDispose(ctx);
        }
    }
}
