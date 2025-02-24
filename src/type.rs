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
    fn apply(self, subst: &Subst) -> Self;
}

impl Types for Type {
    fn apply(mut self, subst: &Subst) -> Self {
        match &mut self {
            Type::Var(tyvar) => { 
                subst
                    .get(tyvar)
                    .map(|u| self = u.clone()); 
            },
            _ => todo!(),
        }
        self
    }
}