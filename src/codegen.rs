use std::path::Path;

use inkwell::execution_engine::ExecutionEngine;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::OptimizationLevel;
use inkwell::{context::Context, memory_buffer::MemoryBuffer, values::BasicValueEnum};

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
}

pub fn codegen() {
    let context = Context::create();
    let module = context.create_module("pickles");
    let bob = context.create_builder();

    let rust_llvm_ir = MemoryBuffer::create_from_file(Path::new("runtime.ll")).unwrap();
    let rust_module = context.create_module_from_ir(rust_llvm_ir).unwrap();
    module.link_in_module(rust_module).unwrap();

    let add_fun = module.get_function("add").unwrap();

    let i32_type = context.i32_type();
    let i64_type = context.i64_type();
    let f64_type = context.f64_type();

    let so_called_value = module.get_struct_type("Value").unwrap();

    let fn_type = so_called_value.fn_type(&[], false);
    let fn_value = module.add_function("my_main", fn_type, None);
    let entry = context.append_basic_block(fn_value, "entry");
    bob.position_at_end(entry);

    let so_called_float = module.get_struct_type("Value::Float").unwrap();
    let float_val1 = so_called_float.const_named_struct(&[f64_type.const_float(3.14).into()]);
    let float_val2 = so_called_float.const_named_struct(&[f64_type.const_float(1.0).into()]);

    let so_called_int = module.get_struct_type("Value::Int").unwrap();
    let int_val1 = so_called_int.const_named_struct(&[i64_type.const_int(1, false).into()]);
    dbg!(float_val1);
    dbg!(float_val2);
    dbg!(int_val1);

    let return_value = bob
        .build_call(add_fun, &[float_val1.into(), float_val2.into()], "call")
        .try_as_basic_value()
        .unwrap_left();

    bob.build_return(Some(&return_value));

    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");

    let triple = TargetMachine::get_default_triple();
    let cpu = TargetMachine::get_host_cpu_name().to_string();
    let features = TargetMachine::get_host_cpu_features().to_string();

    let target = Target::from_triple(&triple).unwrap();
    let machine = target
        .create_target_machine(
            &triple,
            &cpu,
            &features,
            OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();

    // create a module and do JIT stuff

    machine
        .write_to_file(&module, FileType::Assembly, "out.asm".as_ref())
        .unwrap();

    let engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let maybe_fn = unsafe { engine.get_function::<unsafe extern "C" fn() -> Value>("my_main") };
    let compiled_fn = match maybe_fn {
        Ok(f) => f,
        Err(err) => {
            panic!("!> Error during execution: {:?}", err);
        }
    };
    dbg!(&compiled_fn);
    println!("Result: {:?}", unsafe { compiled_fn.call() });
}
