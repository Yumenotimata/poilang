use std::collections::{HashMap, HashSet};
use thiserror::Error;

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

#[macro_export]
macro_rules! var {
    ($id:expr, $kind:expr) => {
        TyVar {
            id: $id.to_string(),
            kind: $kind
        }
    };
}

#[macro_export]
macro_rules! tvar {
    ($id:expr, $kind:expr) => {
        Type::Var(
            var!($id, $kind)
        )
    };
}

#[macro_export]
macro_rules! tcon {
    ($id:expr, $kind:expr) => {
        Type::Con(
            $id.to_string(),
            $kind
        )
    };
}

#[macro_export]
macro_rules! tapp {
    ($l:expr, $r:expr) => {
        Type::App(
            Box::new($l),
            Box::new($r)
        )
    };
}

#[macro_export]
macro_rules! subst {
    ($($key:expr => $value:expr),*) => {
        {
            let mut m = HashMap::<TyVar, Type>::new();
            $(
                m.insert($key, $value);
            )*
            m
        }
    };
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

pub type Subst = HashMap<TyVar, Type>;

pub fn compose(mut s1: Subst, s2: Subst) -> Subst {
    let applyed_s2_by_s1: Subst = 
        s2
            .into_iter()
            .map(|(u, v) | (u, v.apply(&s1)))
            .collect();
    s1.extend(applyed_s2_by_s1);
    s1
}

pub fn merge(mut s1: Subst, s2: Subst) -> Result<Subst, TypeError> {
    let s1_keys: HashSet<&TyVar> = s1.keys().collect();
    let s2_keys: HashSet<&TyVar> = s2.keys().collect();
    let s_intersection: HashSet<&TyVar> = s1_keys.intersection(&s2_keys).cloned().collect();
    let conflicting: HashSet<TyVar> = s_intersection.into_iter().filter(|k| s1.get(k) != s2.get(k)).cloned().collect();

    if conflicting.is_empty() {
        s1.extend(s2);
        Ok(s1)
    } else {
        Err(TypeError::MergeError(conflicting))
    }
}

pub trait Types {
    fn apply(self, subst: &Subst) -> Self;
    fn apply_mut(&mut self, subst: &Subst);
}

impl Types for Type {
    fn apply(mut self, subst: &Subst) -> Self {
        Types::apply_mut(&mut self, subst);
        self
    }

    fn apply_mut(&mut self, subst: &Subst) {
        match self {
            Type::Var(tyvar) => match subst.get(tyvar) {
                Some(u) => *self = u.clone(),
                None => {},
            },
            Type::App(l, r) => {
                l.apply_mut(subst);
                r.apply_mut(subst);
            },
            _ => {},
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Error)]
pub enum TypeError {
    #[error("Kind mismatch")]
    KindMismatch,
    #[error("Merge error: {0:?}")]
    MergeError(HashSet<TyVar>),
    #[error("Unify error")]
    UnifyError,
}

pub fn mgu(t1: &Type, t2: &Type) -> Result<Subst, TypeError> {
    fn mgu_var(v1: &TyVar, t: &Type) -> Result<Subst, TypeError> {
        match t {
            Type::Var(v2) => {
                if v1 == v2 {
                    Ok(HashMap::new())
                } else {
                    Ok(subst!(v1.clone() => t.clone()))
                }
            },
            _ => {
                if t.kind() == &v1.kind {
                    Ok(subst!(v1.clone() => t.clone()))
                } else {
                    Err(TypeError::KindMismatch)
                }
            }
        }
    }

    match (t1, t2) {
        (Type::Var(v), t) => mgu_var(v, t),
        (t, Type::Var(v)) => mgu_var(v, t),
        (Type::Con(c1, _), Type::Con(c2, _)) if c1 == c2 => Ok(HashMap::new()),
        (Type::App(l1, r1), Type::App(l2, r2)) => {
            let s1 = mgu(l1, l2)?;
            let r1 = r1.clone().apply(&s1);
            let r2 = r2.clone().apply(&s1);
            let s2 = mgu(&r1, &r2)?;
            Ok(compose(s2, s1))
        },
        _ => Err(TypeError::UnifyError),
    }
}

fn r#match(t1: &Type, t2: &Type) -> Result<Subst, TypeError> {
    match (t1, t2) {
        (Type::Var(v), t) => Ok(subst!(v.clone() => t.clone())),
        (t, Type::Var(v)) => Ok(subst!(v.clone() => t.clone())),
        (Type::Con(c1, _), Type::Con(c2, _)) if c1 == c2 => Ok(HashMap::new()),
        (Type::App(l1, r1), Type::App(l2, r2)) => {
            let s1 = r#match(l1, l2)?;
            let r1 = r1.clone().apply(&s1);
            let r2 = r2.clone().apply(&s1);
            let s2 = r#match(&r1, &r2)?;
            merge(s1, s2)
        },
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_apply() {
        let subst = subst! {
            var!("a", Kind::Star) => tcon!("Int", Kind::Star),
            var!("b", Kind::Star) => tcon!("Bool", Kind::Star),
            var!("c", Kind::Star) => tvar!("a", Kind::Star)
        };

        {
            let ty = tapp!(tvar!("a", Kind::Star), tvar!("b", Kind::Star));
            let ty = ty.apply(&subst);

            assert_eq!(ty, tapp!(tcon!("Int", Kind::Star), tcon!("Bool", Kind::Star)));
        }
        {
            let ty = tapp!(tvar!("a", Kind::Star), tvar!("c", Kind::Star));
            let ty = ty.apply(&subst);

            assert_eq!(ty, tapp!(tcon!("Int", Kind::Star), tvar!("a", Kind::Star)));
        }
    }

    #[test]
    fn test_compose() {
        let s2 = subst! {
            var!("a", Kind::Star) => tvar!("b", Kind::Star)
        };
        let s1 = subst! {
            var!("b", Kind::Star) => tcon!("Int", Kind::Star)
        };

        let s = compose(s1, s2);
        
        assert_eq!(s, subst! {
            var!("a", Kind::Star) => tcon!("Int", Kind::Star),
            var!("b", Kind::Star) => tcon!("Int", Kind::Star)
        });
    }

    #[test]
    fn test_merge() {
        {
            let s2 = subst! {
                var!("a", Kind::Star) => tvar!("b", Kind::Star)
            };
            let s1 = subst! {
                var!("b", Kind::Star) => tcon!("Int", Kind::Star)
            };

            let s = merge(s1, s2).unwrap();
            
            assert_eq!(s, subst! {
                var!("a", Kind::Star) => tvar!("b", Kind::Star),
                var!("b", Kind::Star) => tcon!("Int", Kind::Star)
            });
        }
        {
            let s2 = subst! {
                var!("a", Kind::Star) => tvar!("Int", Kind::Star)
            };
            let s1 = subst! {
                var!("a", Kind::Star) => tcon!("Bool", Kind::Star)
            };

            let e = merge(s1, s2).unwrap_err();
            assert_eq!(e, TypeError::MergeError(HashSet::from_iter(vec![var!("a", Kind::Star)])));
        }
    }

    #[test]
    fn test_mgu() {
        {
            let t1 = tapp!(tvar!("a", Kind::Star), tvar!("b", Kind::Star));
            let t2 = tapp!(tcon!("Int", Kind::Star), tcon!("Bool", Kind::Star));
            let subst = mgu(&t1, &t2).unwrap();

            assert_eq!(subst, subst! {
                var!("a", Kind::Star) => tcon!("Int", Kind::Star),
                var!("b", Kind::Star) => tcon!("Bool", Kind::Star)
            });
        }
        {
            let t1 = tapp!(tvar!("a", Kind::Star), tvar!("b", Kind::Star));
            let t2 = tapp!(tcon!("Int", Kind::Star), tvar!("a", Kind::Star));
            let subst = mgu(&t1, &t2).unwrap();

            assert_eq!(subst, subst! {
                var!("a", Kind::Star) => tcon!("Int", Kind::Star),
                var!("b", Kind::Star) => tcon!("Int", Kind::Star)
            });
        }
        {
            let t1 = tapp!(tvar!("a", Kind::Star), tvar!("b", Kind::Star));
            let t2 = tapp!(tvar!("b", Kind::Star), tcon!("Bool", Kind::Star));
            let subst = mgu(&t1, &t2).unwrap();

            assert_eq!(subst, subst! {
                var!("a", Kind::Star) => tcon!("Bool", Kind::Star),
                var!("b", Kind::Star) => tcon!("Bool", Kind::Star)
            });
        }
    }

    #[test]
    fn test_match() {
        {
            let t1 = tapp!(tvar!("a", Kind::Star), tvar!("b", Kind::Star));
            let t2 = tapp!(tcon!("Int", Kind::Star), tcon!("Bool", Kind::Star));
            let subst = r#match(&t1, &t2).unwrap();

            assert_eq!(subst, subst! {
                var!("a", Kind::Star) => tcon!("Int", Kind::Star),
                var!("b", Kind::Star) => tcon!("Bool", Kind::Star)
            });
        }
        {
            let t1 = tapp!(tvar!("a", Kind::Star), tvar!("b", Kind::Star));
            let t2 = tapp!(tcon!("Int", Kind::Star), tvar!("a", Kind::Star));
            let subst = r#match(&t1, &t2).unwrap();

            assert_eq!(subst, subst! {
                var!("a", Kind::Star) => tcon!("Int", Kind::Star),
                var!("b", Kind::Star) => tcon!("Int", Kind::Star)
            });
        }
        {
            let t1 = tapp!(tvar!("a", Kind::Star), tvar!("b", Kind::Star));
            let t2 = tapp!(tvar!("b", Kind::Star), tcon!("Bool", Kind::Star));
            let subst = r#match(&t1, &t2).unwrap();

            assert_eq!(subst, subst! {
                var!("a", Kind::Star) => tvar!("b", Kind::Star),
                var!("b", Kind::Star) => tcon!("Bool", Kind::Star)
            });
        }
    }
}