extern crate llvm_sys as llvm;

use self::llvm::core::*;
use self::llvm::execution_engine::*;
use self::llvm::prelude::*;
use self::llvm::target::*;

use self::llvm::transforms::scalar::*;
use self::llvm::{LLVMBuilder, LLVMContext, LLVMModule, LLVMPassManager};

use crate::llvm_utils::*;
use crate::parser::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

#[derive(Hash, Eq, PartialEq, Clone)]
pub struct Ident {
    name: String,
    typ: IdentType
}

#[derive(Hash, Eq, PartialEq, Clone)]
pub enum IdentType {
    Function,
    Variable,
    Struct
}

impl Ident {
    fn new(name: String, typ: IdentType) -> Ident {
        Ident {
            name,
            typ,
        }
    }
}

#[derive(Clone)]
pub struct CompileContext {
    pub llvm_ctx: *mut LLVMContext,
    pub module: *mut LLVMModule,
    pub fpm: *mut LLVMPassManager,
    pub builder: *mut LLVMBuilder,
    pub value_map: Arc<RefCell<HashMap<Ident, LLVMValueRef>>>,
    pub typ_map: Arc<RefCell<HashMap<Ident, LLVMTypeRef>>>,
}

impl CompileContext {
    fn new(name: &str) -> CompileContext {
        unsafe {
            let ctx = LLVMContextCreate();
            let builder = LLVMCreateBuilderInContext(ctx);
            let module = LLVMModuleCreateWithNameInContext(into_raw_str!(name), ctx);

            let fpm = LLVMCreateFunctionPassManagerForModule(module);
            LLVMAddInstructionCombiningPass(fpm);
            LLVMAddReassociatePass(fpm);
            LLVMAddGVNPass(fpm);
            LLVMAddCFGSimplificationPass(fpm);
            LLVMInitializeFunctionPassManager(fpm); // TODO: Add more OPT pass

            let typ_map = Arc::new(RefCell::new(HashMap::new()));
            CompileContext {
                llvm_ctx: ctx,
                module,
                fpm,
                builder,
                value_map: Arc::new(RefCell::new(HashMap::new())),
                typ_map: typ_map.clone(),
            }
        }
    }
    fn print(&self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }
    fn drop(self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMContextDispose(self.llvm_ctx);
        }
    }
}

pub struct JITEngine<'a> {
    ctx: &'a CompileContext,
    engine: LLVMExecutionEngineRef,
}

impl<'a> JITEngine<'a> {
    fn new(ctx: &CompileContext) -> JITEngine {
        let engine = unsafe {
            LLVMLinkInMCJIT();
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            let mut execution_engine = std::mem::uninitialized();
            let mut out = std::mem::zeroed();
            LLVMCreateExecutionEngineForModule(&mut execution_engine, ctx.module, &mut out);
            execution_engine
        };
        JITEngine { ctx, engine }
    }
    fn get_func(&self, name: &str) -> u64 {
        unsafe { LLVMGetFunctionAddress(self.engine, into_raw_str!(name)) }
    }
    fn drop(self) {
        unsafe {
            LLVMDisposeExecutionEngine(self.engine);
        }
    }
}

pub trait IrGen {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)>;
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

        let function_type = unsafe {llvm::core::LLVMFunctionType(
            typ,
            fields.as_mut_ptr(),
            self.fields.len() as u32,
            0,
        )};
        let function = unsafe {
            llvm::core::LLVMAddFunction(ctx.module, into_raw_str!(self.name.get_name()), function_type)
        };
        let block = unsafe {llvm::core::LLVMAppendBasicBlockInContext(
            ctx.llvm_ctx,
            function,
            into_raw_str!("entry"),
        )};
        unsafe {llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, block)};
        let ptr = unsafe {LLVMBuildAlloca(ctx.builder, typ, into_raw_str!("tmpalloca"))};
        for i in 0..fields.len() {
            unsafe {
                let field = LLVMBuildStructGEP(ctx.builder, ptr, i as u32, into_raw_str!("tmpgep"));
                LLVMBuildStore(ctx.builder, LLVMGetParam(function, i as u32), field);
            }
        }
        unsafe {LLVMBuildRet(ctx.builder, LLVMBuildLoad(ctx.builder, ptr, into_raw_str!("tmpload")))};

        ctx.typ_map
            .borrow_mut()
            .insert(Ident::new(self.name.get_name().to_string(), IdentType::Struct), typ);
        Some((std::ptr::null_mut(), typ))
    }
}

impl IrGen for Typ {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        match self {
            Typ::I8 => Some((std::ptr::null_mut(), llvm_i8!(ctx))),
            Typ::I16 => Some((std::ptr::null_mut(), llvm_i16!(ctx))),
            Typ::I32 => Some((std::ptr::null_mut(), llvm_i32!(ctx))),
            Typ::I64 => Some((std::ptr::null_mut(), llvm_i64!(ctx))),
            Typ::U8 => Some((std::ptr::null_mut(), llvm_i8!(ctx))),
            Typ::U16 => Some((std::ptr::null_mut(), llvm_i16!(ctx))),
            Typ::U32 => Some((std::ptr::null_mut(), llvm_i32!(ctx))),
            Typ::U64 => Some((std::ptr::null_mut(), llvm_i64!(ctx))),
            Typ::Struct(st) => Some((
                std::ptr::null_mut(),
                *ctx.typ_map
                    .borrow_mut()
                    .get(&Ident::new(st.get_name().to_string(), IdentType::Struct))
                    .unwrap(),
            )),
        }
    }
}

impl IrGen for FunctionCall {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        let function = unsafe {
            LLVMGetNamedFunction(
                ctx.module,
                into_raw_str!(self.function_name.get_name()),
            )
        };
        let function_typ = *ctx
            .typ_map
            .borrow_mut()
            .get(&Ident::new(self.function_name.get_name().to_string(), IdentType::Function))
            .unwrap();

        let params_size = unsafe { LLVMCountParamTypes(function_typ) as usize };
        let mut params = vec![std::ptr::null_mut(); params_size];
        unsafe { LLVMGetParamTypes(function_typ, params.as_mut_ptr()) }

        let mut args: Vec<_> = self
            .args
            .iter()
            .enumerate()
            .map(|(index, item)| {
                evaluate_value::<Expression, _>(ctx, item)
//                llvm_cast_int(ctx, expr, params[index])
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
                    into_raw_str!("tmpcall"),
                )
            },
            unsafe {
                LLVMGetReturnType(
                    *ctx.typ_map
                        .borrow_mut()
                        .get(&Ident::new(self.function_name.get_name().to_string(), IdentType::Function))
                        .unwrap(),
                )
            },
        ))
    }
}

impl IrGen for MonadicExpression {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        match self {
            MonadicExpression::FunctionCall(fun_call) => {
                fun_call.build(ctx)
            }
            MonadicExpression::Variable(var) => Some((
                *ctx.value_map.borrow_mut().get(&Ident::new(var.get_name().to_string(), IdentType::Variable)).unwrap(),
                std::ptr::null_mut(),
            )), // TODO: Add Type for value
            MonadicExpression::Const(num) => Some((
                unsafe { LLVMConstInt(llvm_i64!(ctx), *num as u64, 0) },
                Typ::I64.build(ctx).unwrap().1,
            )),
            MonadicExpression::TypeCast(type_cast) => {
                let value = evaluate_value(ctx, &type_cast.inner);
                let typ = evaluate_type(ctx, &type_cast.typ);
                Some((llvm_cast_int(ctx, value, typ), typ))
            }
        }
    }
}

impl IrGen for BinaryExpression {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        let left = evaluate_value(ctx, &self.left);
        let right = evaluate_value(ctx, &self.right); // TODO: cast type for binary operator
        match self.op {
            BinaryOperation::Plus => Some((llvm_add(ctx, left, right), std::ptr::null_mut())),
            BinaryOperation::Minus => Some((llvm_sub(ctx, left, right), std::ptr::null_mut())),
            BinaryOperation::Mul => Some((llvm_mul(ctx, left, right), std::ptr::null_mut())),
            BinaryOperation::Div => Some((llvm_div(ctx, left, right), std::ptr::null_mut())),
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
                .insert(Ident::new(self.name.to_string(), IdentType::Function), function_type);

            let function =
                llvm::core::LLVMAddFunction(ctx.module, into_raw_str!(self.name), function_type);

            let block = llvm::core::LLVMAppendBasicBlockInContext(
                ctx.llvm_ctx,
                function,
                into_raw_str!("entry"),
            );
            llvm::core::LLVMPositionBuilderAtEnd(ctx.builder, block);

            let mut bind_map = ctx.value_map.borrow_mut().clone();
            for (index, var) in self.args.iter().enumerate() {
                bind_map.insert(Ident::new(var.name.clone(), IdentType::Variable), LLVMGetParam(function, index as u32));
            }

            let mut ctx = ctx.clone();
            ctx.value_map = Arc::new(RefCell::new(bind_map));
            self.bindings.iter().for_each(|bind| {
                let value = bind.value.build(&ctx).unwrap().0;
//                let value = llvm_cast_int(&ctx, value, bind.var.typ.build(&ctx).unwrap().1); // TODO: Cast not only Int
                ctx.value_map
                    .borrow_mut()
                    .insert(Ident::new(bind.var.name.clone(), IdentType::Variable), value);
            });
            let ret_value = self.expr.build(&ctx).unwrap().0;
//            let ret_value = llvm_cast_int(&ctx, ret_value, ret_typ); // TODO: Cast not only Int
            LLVMBuildRet(ctx.builder, ret_value);

            LLVMRunFunctionPassManager(ctx.fpm, function);
            Some((function, function_type))
        }
    }
}

impl IrGen for Statement {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        match self {
            Statement::FunDefinition(fun_def) => fun_def.build(ctx),
            Statement::StructDefinition(struct_def) => struct_def.build(ctx),
        }
    }
}

pub trait Compile {
    fn compile(&self, name: &str);
}

impl Compile for Program {
    fn compile(&self, name: &str) {
        unsafe {
            let ctx = CompileContext::new(name);
            self.get_statements().iter().for_each(|item| {
                item.build(&ctx);
            });

            ctx.print();

            let engine = JITEngine::new(&ctx);

            let addr = engine.get_func("Brighter");
            let f: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(addr);

            let x = 1;
            let y = 1;
            let res = f(x, y);
            println!("{} {} {}", x, y, res);
            engine.drop();
            ctx.drop();
        }
    }
}

impl<T: IrGen> IrGen for Box<T> {
    fn build(&self, ctx: &CompileContext) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        self.deref().build(ctx)
    }
}
