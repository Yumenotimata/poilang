use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Kind {
    Star,
    Kfun(Box<Kind>, Box<Kind>)
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    Var(TyVar),
    Con(String, Kind),
    App(Box<Type>, Box<Type>),
}


#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TyVar {
    id: String,
    kind: Kind
}

pub trait HasKind {
    fn kind(&self) -> &Kind;
}

impl HasKind for Type {
    fn kind(&self) -> &Kind {
        match self {
            Type::Var(tyvar) => &tyvar.kind,
            Type::Con(_, kind) => kind,
            Type::App(l, _) => {
                match l.kind() {
                    Kind::Kfun(_, k) => k,
                    _ => unreachable!(),
                }
            }
        }
    }
}

type Subst = HashMap<TyVar, Type>;

pub trait Types {
    // fn apply(self, subst: &Subst) -> Self;
    fn apply(&mut self, subst: &Subst);
}

impl Types for Type {
    fn apply(&mut self, subst: &Subst) {
        match self {
            Type::Var(tyvar) => match subst.get(tyvar) {
                Some(u) => *self = u.clone(),
                None => {},
            },
            Type::App(l, r) => {
                l.apply(subst);
                r.apply(subst);
            },
            _ => {},
        }
    }
    // fn apply(mut self, subst: &Subst) -> Self {
    //     fn apply_in_place(this: &mut Type, subst: &Subst) {
    //         match this {
    //             Type::Var(tyvar) => match subst.get(tyvar) {
    //                 Some(u) => *this = u.clone(),
    //                 None => {},
    //             },
    //             Type::App(l, r) => {
    //                 apply_in_place(l, subst);
    //                 apply_in_place(r, subst);
    //             },
    //             _ => {},
    //         }
    //     }

    //     apply_in_place(&mut self, subst);
        
    //     self
    // }
}

fn m2o<T, F: FnOnce(&mut T)>(mut this: T, f: F) -> T {
    f(&mut this);
    this
}