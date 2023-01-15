use std::path::Path;

use inkwell::OptimizationLevel;
use inkwell::targets::{Target, InitializationConfig, TargetMachine, RelocMode, CodeModel, FileType};
use inkwell::{context::Context, memory_buffer::MemoryBuffer, values::BasicValueEnum};
use inkwell::execution_engine::ExecutionEngine;

pub fn codegen() {
    let context = Context::create();
    let module = context.create_module("pickles");
    let bob = context.create_builder();

    let rust_llvm_ir = MemoryBuffer::create_from_file(Path::new("very_complex.ll")).unwrap();
    let rust_module = context.create_module_from_ir(rust_llvm_ir).unwrap();
    module.link_in_module(rust_module).unwrap();

    let inc_fun = module.get_function("inc").unwrap();

    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let fn_value = module.add_function("my_main", fn_type, None);
    let entry = context.append_basic_block(fn_value, "entry");
    bob.position_at_end(entry);

    let param = context.i32_type().const_int(69, false);
    let return_value = bob.build_call(inc_fun, &[param.into()], "call").try_as_basic_value().unwrap_left();

    bob.build_return(Some(&return_value));


    Target::initialize_native(&InitializationConfig::default()).expect("Failed to initialize native target");

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

    machine.write_to_file(&module, FileType::Assembly, "out.asm".as_ref()).unwrap();

    let engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();

    let maybe_fn = unsafe { engine.get_function::<unsafe extern "C" fn() -> i32>("my_main") };
    let compiled_fn = match maybe_fn {
        Ok(f) => f,
        Err(err) => {
            panic!("!> Error during execution: {:?}", err);
        },
    };
    dbg!(&compiled_fn);
    println!("Result: {}", unsafe { compiled_fn.call() });
}
