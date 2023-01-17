pub struct ClassType {
    name: String,
    instance_type_ids: Vec<usize>,
}

pub struct InstanceType {
    name: String,
    class_type_id: usize,
}

pub struct Object {
    values: Vec<Value>,
    instance_type_id: Option<usize>,
}

pub enum Value {
    Int(i64),
    Float(f64),
    Object(*mut Object),
}

#[no_mangle]
pub fn add(v1: Value, v2: Value) -> Value {
    match (v1, v2) {
        (Value::Int(lhs), Value::Int(rhs))     => Value::Int(lhs + rhs),
        (Value::Int(lhs), Value::Float(rhs))   => Value::Float(lhs as f64 + rhs),
        (Value::Float(lhs), Value::Int(rhs))   => Value::Float(lhs + rhs as f64),
        (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
        _ => todo!()
    }
}

fn _foo() {
    let _class_types: Vec<ClassType> = vec![];
    let _instance_types: Vec<InstanceType> = vec![];
}
