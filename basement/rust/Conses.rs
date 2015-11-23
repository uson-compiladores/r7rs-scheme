#![allow(dead_code)]
#![allow(unused_variables)]

struct Pair {
    car:Box<Object>,
    cdr:Box<Object>
}

enum Object {
    Integer(i64),
    Pair(Pair),
    Nil
}

macro_rules! list[
    () => (Object::Nil);
    ($x:expr) => (Pair{car: Box::new($x), cdr: Box::new(Object::Nil)});
    ($x:expr, $($xs:expr), +) => (Pair{car: Box::new($x), cdr: Box::new(list!($($xs),+))});
    ];

fn main() {
    let lst1 = list![Object::Integer(1)];
    let lst2 = list![Object::Nil];
    let lst3 = list![Object::Pair(list![])];
    match *lst1.car {
        Object::Integer(i) => println!("{}",i),
        Object::Pair(p) => println!("Es un cons"),
        Object::Nil => println!("()")
    }
    match *lst2.car {
        Object::Integer(i) => println!("{}",i),
        Object::Pair(p) => println!("Es un cons"),
        Object::Nil => println!("()")
    }
    match *lst3.car {
        Object::Integer(i) => println!("{}",i),
        Object::Pair(p) => println!("Es un cons"),
        Object::Nil => println!("()")
    }
}
