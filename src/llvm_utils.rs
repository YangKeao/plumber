use crate::codegen::{CompileContext, IrGen};
use llvm_sys::core::{LLVMBuildAdd, LLVMBuildIntCast, LLVMBuildMul, LLVMBuildSDiv, LLVMBuildSub};
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use std::ops::Deref;
macro_rules! into_raw_str {
    ($x: expr) => {
        format!("{}\0", $x).as_ptr() as *const _
    };
}

pub fn llvm_add(ctx: &CompileContext, left: LLVMValueRef, right: LLVMValueRef) -> LLVMValueRef {
    unsafe { LLVMBuildAdd(ctx.builder, left, right, into_raw_str!("addtmp")) }
}
pub fn llvm_sub(ctx: &CompileContext, left: LLVMValueRef, right: LLVMValueRef) -> LLVMValueRef {
    unsafe { LLVMBuildSub(ctx.builder, left, right, into_raw_str!("subtmp")) }
}
pub fn llvm_mul(ctx: &CompileContext, left: LLVMValueRef, right: LLVMValueRef) -> LLVMValueRef {
    unsafe { LLVMBuildMul(ctx.builder, left, right, into_raw_str!("multmp")) }
}
pub fn llvm_div(ctx: &CompileContext, left: LLVMValueRef, right: LLVMValueRef) -> LLVMValueRef {
    unsafe { LLVMBuildSDiv(ctx.builder, left, right, into_raw_str!("divtmp")) } // TODO: support Unsigned Div
}

macro_rules! llvm_i8 {
    ($ctx: expr) => {
        unsafe { LLVMInt8TypeInContext($ctx.llvm_ctx) }
    };
}
macro_rules! llvm_i16 {
    ($ctx: expr) => {
        unsafe { LLVMInt16TypeInContext($ctx.llvm_ctx) }
    };
}
macro_rules! llvm_i32 {
    ($ctx: expr) => {
        unsafe { LLVMInt32TypeInContext($ctx.llvm_ctx) }
    };
}
macro_rules! llvm_i64 {
    ($ctx: expr) => {
        unsafe { LLVMInt64TypeInContext($ctx.llvm_ctx) }
    };
}

pub fn llvm_cast_int(ctx: &CompileContext, val: LLVMValueRef, dest: LLVMTypeRef) -> LLVMValueRef {
    unsafe { LLVMBuildIntCast(ctx.builder, val, dest, into_raw_str!("tmpcast")) }
}

pub fn evaluate_value<T: IrGen, S: Deref<Target = T>>(
    ctx: &CompileContext,
    item: S,
) -> LLVMValueRef {
    item.build(ctx).unwrap().0
}

pub fn evaluate_type<T: IrGen, S: Deref<Target = T>>(ctx: &CompileContext, item: S) -> LLVMTypeRef {
    item.build(ctx).unwrap().1
}
