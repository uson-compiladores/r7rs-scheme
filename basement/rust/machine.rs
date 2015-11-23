#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]

#[derive(Clone)]
struct Environment {
    next:EnvironmentPtr,
    vars:Objects,
    vals:Vec<Object>
}

type EnvironmentPtr = Option<Box<Environment>>;

impl Environment {
    fn new(next:EnvironmentPtr) -> Environment {
        Environment { next:next, vars:vec!(), vals:vec!() }
    }
}

#[derive(Clone)]
struct CallFrame {
    ip:ByteCodePtr,
    env:Environment,
    args:Vec<Object>,
    next:CallFramePtr
}

type CallFramePtr = Option<Box<CallFrame>>;

impl CallFrame {
    fn new(ip:ByteCodePtr, env:Environment, args:Vec<Object>, next:CallFramePtr) -> CallFrame {
        CallFrame { ip:ip, env:env, args:args, next:next }
    }
}

#[derive(Clone)]
struct Closure {
    env:Environment,
    vars:Objects,
    body:ByteCodePtr
}

impl Closure {
    fn new(env:Environment, vars:Objects, body: ByteCodePtr) -> Closure{
        Closure { env:env, vars:vars, body:body }
    }
}

#[derive(Clone)]
enum Object {
    NIL,
    BOOL(bool),
    INTEGER(i64),
    IDENTIFIER(String),
    CLOSURE(Closure)
}

type Objects = Vec<Object>;

#[derive(Clone)]
enum ByteCode {
    HALT,
    REFER     { var:Object,         ip:ByteCodePtr                      },
    CONSTANT  { obj:Object,         ip:ByteCodePtr                      },
    CLOSE     { vars:Objects,       body:ByteCodePtr,    ip:ByteCodePtr },
    TEST      { thenc:ByteCodePtr,  elsec:ByteCodePtr                   },
    ASSIGN    { var:Object,         ip:ByteCodePtr                      },
    CONTI     { ip:ByteCodePtr                                          },
    NUATE     { frame:CallFrame,    var:Object                          },
    FRAME     { ip:ByteCodePtr,     ret:ByteCodePtr                     },
    ARGUMENT  { ip:ByteCodePtr                                          },
    APPLY,
    RETURN
}

type ByteCodePtr = Box<ByteCode>;

struct Registers {
    acc:Object,
    ip:ByteCodePtr,
    env:Environment,
    args:Objects,
    stack:CallFrame
}

impl Registers {
    fn new(acc:Object, ip:ByteCodePtr, env:Environment, args:Objects, stack:CallFrame) -> Registers {
        Registers { acc:acc, ip:ip, env:env, args:args, stack:stack }
    }
    fn acc(&self) -> &Object { &self.acc }
    fn ip(&self) -> &ByteCodePtr { &self.ip }
    fn env(&self) -> &Environment { &self.env }
    fn args(&self) -> &Objects { &self.args }
    fn stack(&self) -> &CallFrame { &self.stack }
}

struct Machine {
    state:Registers
}

impl Machine {
    fn init(state:Registers) -> Machine {
        Machine { state:state }
    }

    fn step(&mut self) -> Option<Object> {
        let (mut acc, ip, mut env, mut args, mut stack) = (
            self.state.acc.clone(),
            self.state.ip.clone(),
            self.state.env.clone(),
            self.state.args.clone(),
            self.state.stack.clone()
                );

        let x = match *ip {
            ByteCode::HALT => {
                
            },
            ByteCode::REFER { var:ref var, ip:ref ip } => {
                
            },
            ByteCode::CONSTANT { obj:ref obj, ip:ref ip } => {

            },
            ByteCode::CLOSE { vars:ref vars, body:ref body, ip:ref ip } => {

            },
            ByteCode::TEST { thenc:ref thenc, elsec:ref elsec } => {
                
            },
            ByteCode::ASSIGN { var:ref var, ip:ref ip } => {

            },
            ByteCode::CONTI { ip:ref ip } => {
                
            },
            ByteCode::NUATE { frame:ref frame, var:ref var } => {

            },
            ByteCode::FRAME { ip:ref ip, ret:ref ret } => {

            },
            ByteCode::ARGUMENT { ip:ref ip } => {

            },
            ByteCode::APPLY => {

            },
            ByteCode::RETURN => {

            }
        };

        let return_value = acc.clone();
        self.state = Registers { acc:acc, ip:ip, env:env, args:args, stack:stack };

        Some(return_value)
    }
}

fn main() {
    let program:ByteCodePtr = Box::new(
        ByteCode::CONSTANT {
            obj:Object::INTEGER(2),
            ip:Box::new(
                ByteCode::HALT
                    )
        }
        );
}
// (:frame:
//  (:halt:)
//  (:constant:
//   2
//   (:argument:
//    (:constant:
//     1
//     (:argument:
//      (:close:
//       (x y)
//       (:refer:
//        x
//        (:return:))
//       (:apply:)))))))
