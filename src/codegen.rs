extern crate llvm_sys as llvm;

use self::llvm::core::*;
use self::llvm::execution_engine::*;
use self::llvm::prelude::{LLVMValueRef, LLVMTypeRef};
use self::llvm::{LLVMBuilder, LLVMContext, LLVMModule, LLVMValue};
use crate::parser::*;
use std::collections::HashMap;
use self::llvm::target::{LLVM_InitializeNativeTarget, LLVM_InitializeNativeAsmPrinter};
use self::llvm::analysis::LLVMVerifyFunction;

pub trait IrGen {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &mut HashMap<String, LLVMValueRef>,
        typ_map: &mut HashMap<String, LLVMTypeRef>,
    ) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        unimplemented!()
    }

    fn build_raw(&self,
                 ctx: *mut LLVMContext,
                 module: *mut LLVMModule,
                 builder: *mut LLVMBuilder,
                 value_map: &mut HashMap<String, LLVMValueRef>,
                 typ_map: &mut HashMap<String, LLVMTypeRef>,) {
        unimplemented!()
    }
}

impl IrGen for StructDefinition {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &mut HashMap<String, LLVMValueRef>,
        typ_map: &mut HashMap<String, LLVMTypeRef>,
    ) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        let mut fields: Vec<LLVMTypeRef> = self.fields.iter().map(|field| {
            field.build(ctx, module, builder, value_map, typ_map).unwrap().1
        }).collect();
        let typ =unsafe {LLVMStructTypeInContext(ctx, fields.as_mut_ptr(), fields.len() as u32, 0)};
        typ_map.insert(self.name.get_name().to_string(), typ);
        Some((std::ptr::null_mut(), typ))
    }
}

impl IrGen for Typ {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &mut HashMap<String, LLVMValueRef>,
        typ_map: &mut HashMap<String, LLVMTypeRef>,
    )  -> Option<(LLVMValueRef, LLVMTypeRef)> {
        match self {
            Typ::I8 => Some((std::ptr::null_mut(), unsafe {LLVMInt8TypeInContext(ctx)})),
            Typ::I16 => Some((std::ptr::null_mut(), unsafe {LLVMInt16TypeInContext(ctx)})),
            Typ::I32 => Some((std::ptr::null_mut(), unsafe {LLVMInt32TypeInContext(ctx)})),
            Typ::I64 => Some((std::ptr::null_mut(), unsafe {LLVMInt64TypeInContext(ctx)})),
            Typ::U8 => Some((std::ptr::null_mut(), unsafe {LLVMInt8TypeInContext(ctx)})),
            Typ::U16 => Some((std::ptr::null_mut(), unsafe {LLVMInt16TypeInContext(ctx)})),
            Typ::U32 => Some((std::ptr::null_mut(), unsafe {LLVMInt32TypeInContext(ctx)})),
            Typ::U64 => Some((std::ptr::null_mut(), unsafe {LLVMInt64TypeInContext(ctx)})),
            Typ::Struct(st) => Some((std::ptr::null_mut(), unsafe {*typ_map.get(&st.get_name().to_string()).unwrap()}))
        }
    }
}

impl IrGen for MonadicExpression {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &mut HashMap<String, LLVMValueRef>,
        typ_map: &mut HashMap<String, LLVMTypeRef>,
    ) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        let int = unsafe { llvm::core::LLVMInt64TypeInContext(ctx) };
        match self {
            MonadicExpression::FunctionCall(fun_call) => {
                let function = unsafe {
                    LLVMGetNamedFunction(
                        module,
                        format!("{}\0", fun_call.function_name.get_name()).as_ptr() as *const _,
                    )
                };
                let function_typ = *typ_map.get( &fun_call.function_name.get_name().to_string()).unwrap();

                let params_size = unsafe {LLVMCountParamTypes(function_typ) as usize};
                let mut params = vec![std::ptr::null_mut(); params_size];
                unsafe {
                    LLVMGetParamTypes(function_typ, params.as_mut_ptr())
                }

                let mut args: Vec<_> = fun_call
                    .args
                    .iter().enumerate()
                    .map(|(index, item)| {
                        let expr = item.build(ctx, module, builder, value_map, typ_map).unwrap().0;
                        unsafe {LLVMBuildIntCast(builder, expr, params[index], b"tmpcast\0".as_ptr() as *const _)}
                    })
                    .collect();
                Some((unsafe {
                    // TODO: set LLVMBuildCall Name
                    LLVMBuildCall(
                        builder,
                        function,
                        args.as_mut_ptr(),
                        args.len() as u32,
                        format!("{}\0", fun_call.function_name.get_name()).as_ptr() as *const _,
                    )
                }, unsafe {
                    LLVMGetReturnType(*typ_map.get(fun_call.function_name.get_name()).unwrap())
                }))
            }
            MonadicExpression::Variable(var) => Some((*value_map.get(var.get_name()).unwrap(), std::ptr::null_mut())), // TODO: Add Type for value
            MonadicExpression::Const(num) => Some((unsafe { LLVMConstInt(int, *num as u64, 0) }, Typ::I64.build(ctx, module, builder, value_map, typ_map).unwrap().1)),
        }
    }
}

impl IrGen for BinaryExpression {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &mut HashMap<String, LLVMValueRef>,
        typ_map: &mut HashMap<String, LLVMTypeRef>,
    ) -> Option<(LLVMValueRef, LLVMTypeRef)> {
        let left = self.left.build(ctx, module, builder, value_map, typ_map).unwrap().0;
        let right = self.right.build(ctx, module, builder, value_map, typ_map).unwrap().0; // TODO: cast type for binary operator
        match self.op {
            BinaryOperation::Plus => Some((unsafe {
                LLVMBuildAdd(builder, left, right, b"addtmp\0".as_ptr() as *const _)
            }, std::ptr::null_mut())),
            BinaryOperation::Minus => Some((unsafe {
                LLVMBuildSub(builder, left, right, b"subtmp\0".as_ptr() as *const _)
            }, std::ptr::null_mut())),
            BinaryOperation::Mul => Some((unsafe {
                LLVMBuildMul(builder, left, right, b"multmp\0".as_ptr() as *const _)
            }, std::ptr::null_mut())),
            BinaryOperation::Div => Some((unsafe {
                LLVMBuildSDiv(builder, left, right, b"multmp\0".as_ptr() as *const _)
            }, std::ptr::null_mut()))
        }
    }
}

impl IrGen for Expression {
    fn build(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &mut HashMap<String, LLVMValueRef>,
        typ_map: &mut HashMap<String, LLVMTypeRef>,
    ) -> Option<(LLVMValueRef, LLVMTypeRef)>  {
        match self {
            Expression::BinaryExpression(binary_expr) => {
                binary_expr.build(ctx, module, builder, value_map, typ_map)
            }
            Expression::MonadicExpression(monadic_expr) => {
                monadic_expr.build(ctx, module, builder, value_map, typ_map)
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
        value_map: &mut HashMap<String, LLVMValueRef>,
        typ_map: &mut HashMap<String, LLVMTypeRef>,
    ) -> Option<(LLVMValueRef, LLVMTypeRef)>  {
        unsafe {
            let ret_typ = self.typ.build(ctx, module, builder, value_map, typ_map).unwrap().1;

            let mut params: Vec<_> = self.args.iter().map(|arg| {
                arg.typ.build(ctx, module, builder, value_map, typ_map).unwrap().1
            }).collect();
            let function_type =
                llvm::core::LLVMFunctionType(ret_typ, params.as_mut_ptr(), self.args.len() as u32, 0);

            typ_map.insert(self.name.to_string(), function_type);

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

            let mut bind_map = value_map.clone(); // TODO: Join the HashMap in args
            for (index, var) in self.args.iter().enumerate() {
                bind_map.insert(var.name.clone(), LLVMGetParam(function, index as u32));
            }
            self.bindings.iter().for_each(|bind| {
                let value = bind.value.build(ctx, module, builder, &mut bind_map, typ_map).unwrap().0;
                let value = LLVMBuildIntCast(builder, value, bind.var.typ.build(ctx, module, builder, value_map, typ_map).unwrap().1, b"tmpcast\0".as_ptr() as *const _);
                bind_map.insert(bind.var.name.clone(), value);
            });
            let ret_value = self.expr.build(ctx, module, builder, &mut bind_map, typ_map).unwrap().0;
            let ret_value = LLVMBuildIntCast(builder, ret_value, ret_typ, b"tmpcast\0".as_ptr() as *const _);
            llvm::core::LLVMBuildRet(
                builder,
                ret_value,
            );
            Some((function, function_type))
        }
    }
}

impl IrGen for Statement {
    fn build_raw(
        &self,
        ctx: *mut LLVMContext,
        module: *mut LLVMModule,
        builder: *mut LLVMBuilder,
        value_map: &mut HashMap<String, LLVMValueRef>,
        typ_map: &mut HashMap<String, LLVMTypeRef>,
    ) {
        match self {
            Statement::FunDefinition(fun_def) => {fun_def.build(ctx, module, builder, value_map, typ_map);},
            Statement::StructDefinition(struct_def) => {struct_def.build(ctx, module, builder, value_map, typ_map);},
        };
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
                LLVMModuleCreateWithNameInContext(format!("{}\0", name).as_ptr() as *const _, ctx);

            let mut typ_map = HashMap::new();
            self.get_statements().iter().for_each(|item| {
                item.build_raw(ctx, module, builder, &mut HashMap::new(), &mut typ_map);
            });

            LLVMDisposeBuilder(builder);
            LLVMDumpModule(module);

            LLVMLinkInMCJIT();
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            let mut execution_engine = std::mem::uninitialized();
            let mut out = std::mem::zeroed();
            LLVMCreateExecutionEngineForModule(&mut execution_engine, module, &mut out);

            let addr = LLVMGetFunctionAddress(execution_engine, b"Brighter\0".as_ptr() as *const _);

            let f: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(addr);

            let x = 1;
            let y = 1;
            let res = f(x, y, );

            println!("{} {} {}", x, y, res);

            LLVMDisposeExecutionEngine(execution_engine);
            LLVMContextDispose(ctx);
        }
    }
}
