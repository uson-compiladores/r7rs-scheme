#![allow(dead_code)]

use std::env;

struct Environment {
    parent_environment:Option<Box<Environment>>,
    variables:Vec<String>,
    values:Vec<SchemeObject>
}
impl Environment {
    fn new(parent_environment:Option<Box<Environment>>) -> Environment {
        Environment {
            parent_environment:parent_environment,
            variables:vec!(),
            values:vec!()
        }
    }
}

struct CallFrame {
    next_expression:Box<ByteCode>,
    environment:Environment,
    values:Vec<SchemeObject>,
    caller_frame:Option<Box<CallFrame>>
}
impl CallFrame {
    fn new(next_expression:Box<ByteCode>,
           environment:Environment,
           values:Vec<SchemeObject>,
           caller_frame:Option<Box<CallFrame>>) -> CallFrame {
        CallFrame {
            next_expression:next_expression,
            environment:environment,
            values:values,
            caller_frame:caller_frame
        }
    }
}

struct Closure {
    body:Box<ByteCode>,
    environment:Environment,
    variables:Vec<String>
}
impl Closure {
    fn new(body:Box<ByteCode>, environment:Environment, variables:Vec<String>) -> Closure {
        Closure {
            body:body,
            environment:environment,
            variables:variables
        }
    }
}

struct ComputationState {
    accumulator:SchemeObject,
    next_expression:Box<ByteCode>,
    environment:Environment,
    values:Vec<SchemeObject>,
    stack:CallFrame
}
impl ComputationState {
    fn new(accumulator:SchemeObject,
           next_expression:Box<ByteCode>,
           environment:Environment,
           values:Vec<SchemeObject>,
           stack:CallFrame) -> ComputationState {
        ComputationState {
            accumulator:accumulator,
            next_expression:next_expression,
            environment:environment,
            values:values,
            stack:stack
        }
    }
}
    
enum ByteCode {
    HALT,
    REFER    { variable:String,              next_expression:Box<ByteCode>                                },
    CONSTANT { object:SchemeObject,          next_expression:Box<ByteCode>                                },
    CLOSE    { variables:Vec<String>,        body:Box<ByteCode>,            next_expression:Box<ByteCode> },
    TEST     { then_clause:Box<ByteCode>,    else_clause:Box<ByteCode>                                    },
    ASSIGN   { variable:String,              next_expression:Box<ByteCode>                                },
    CONTI    { next_expression:Box<ByteCode>                                                              },
    NUATE    { stack:CallFrame,              variable:String                                             },
    FRAME    { next_return:Box<ByteCode>,    next:Box<ByteCode>                                          },
    ARGUMENT { next:Box<ByteCode>                                                                        },
    APPLY,
    RETURN
}

enum SchemeObject {
    SchemeNil,
    SchemeBool(bool),
    SchemeInteger(i32),
    SchemeFloat(f32),
    SchemeString(String),
    SchemeClosure(Closure)
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() > 1 {
        println!("The first argument is {}", args[1]);
    }
}
