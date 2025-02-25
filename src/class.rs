use std::collections::HashMap;
use thiserror::Error;

use crate::r#type::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pred {
    id: String,
    ty: Type
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Qual<T> {
    ps: Vec<Pred>,
    t: T
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    id: String,
    supers: Vec<String>,
    insts: Vec<Qual<Pred>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassEnv {
    classes: HashMap<String, Class>,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ClassError {
    #[error("Class {0} already exists")]
    AlreadyExists(String),
    // TODO: Add Info for Instance
    #[error("No super class for instance {0}")]
    NoSuperClassForInst(String),
    #[error("Predicate {0:?} does not match {1:?}")]
    PredMismatch(Pred, Pred),
    #[error(transparent)]
    TypeError(#[from] TypeError),
    #[error("Overlapping instances for {0:?} and {1:?}")]
    OverlapInstance(Pred, Pred)
}

fn mguPred(p1: &Pred, p2: &Pred) -> Result<Subst, ClassError> {
    if p1.id != p2.id {
        return Err(ClassError::PredMismatch(p1.clone(), p2.clone()));
    }

    Ok(mgu(&p1.ty, &p2.ty)?)
}

impl ClassEnv {
    pub fn new() -> Self {
        ClassEnv {
            classes: HashMap::new()
        }
    }
    
    pub fn add_class(&mut self, id: String, supers: Vec<String>) -> Result<(), ClassError> {
        let class = Class {
            id: id.clone(),
            supers,
            insts: vec![]
        };

        if let Some(_) = self.classes.insert(id.clone(), class) {
            Err(ClassError::AlreadyExists(id))
        } else {
            Ok(())
        }
    }

    pub fn insts(&self, id: &String) -> Option<&Vec<Qual<Pred>>> {
        self.classes.get(id).map(|c| &c.insts)
    }

    pub fn add_inst(&mut self, p: Pred, ps: Vec<Pred>) -> Result<(), ClassError> {
        fn overlap(p1: &Pred, p2: &Pred) -> bool {
            mguPred(p1, p2).is_ok()
        }
        
        if self.classes.get(&p.id).is_none() {
            return Err(ClassError::NoSuperClassForInst(p.id));
        } else {
            let qs = 
                self.insts(&p.id)
                    .map(|qs| qs.iter().cloned().map(|q| q.t).collect::<Vec<_>>())
                    .unwrap_or_default();

            for q in qs.iter() {
                if overlap(&q, &p) {
                    return Err(ClassError::OverlapInstance(q.clone(), p.clone()));
                }
            }

            if let Some(c) = self.classes.get_mut(&p.id) {
                c.insts.push(Qual { ps, t: p.clone() });
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_class() {
        let mut env = ClassEnv::new();
        assert_eq!(env.add_class("Eq".to_string(), vec![]), Ok(()));
        assert_eq!(env.add_class("Ord".to_string(), vec!["Eq".to_string()]), Ok(()));
        assert_eq!(env.add_class("Eq".to_string(), vec![]), Err(ClassError::AlreadyExists("Eq".to_string())));
        assert_eq!(env.classes, {
            let mut map = HashMap::new();
            map.insert("Eq".to_string(), Class {
                id: "Eq".to_string(),
                supers: vec![],
                insts: vec![]
            });
            map.insert("Ord".to_string(), Class {
                id: "Ord".to_string(),
                supers: vec!["Eq".to_string()],
                insts: vec![]
            });
            map
        });
    }

    #[test]
    fn test_add_inst() {
        {
            let mut env = ClassEnv::new();
            assert_eq!(env.add_inst(Pred { id: "Eq".to_string(), ty: tcon!("Int", Kind::Star) }, vec![]), Err(ClassError::NoSuperClassForInst("Eq".to_string())));
        }
        {
            let mut env = ClassEnv::new();
            env.add_class("Eq".to_string(), vec![]).unwrap();
            env.add_inst(Pred { id: "Eq".to_string(), ty: tcon!("Int", Kind::Star) }, vec![]).unwrap();
            assert_eq!(env.add_inst(Pred { id: "Eq".to_string(), ty: tcon!("Int", Kind::Star) }, vec![]), Err(ClassError::OverlapInstance(
                Pred { id: "Eq".to_string(), ty: tcon!("Int", Kind::Star) },
                Pred { id: "Eq".to_string(), ty: tcon!("Int", Kind::Star) }
            )));
        }
        {
            let mut env = ClassEnv::new();
            env.add_class("Eq".to_string(), vec![]).unwrap();
            env.add_inst(Pred { id: "Eq".to_string(), ty: tcon!("Int", Kind::Star) }, vec![]).unwrap();
            env.add_inst(Pred { id: "Eq".to_string(), ty: tcon!("Bool", Kind::Star) }, vec![]).unwrap();
            assert_eq!(env.insts(&"Eq".to_string()), Some(&vec![
                Qual {
                    ps: vec![],
                    t: Pred { id: "Eq".to_string(), ty: tcon!("Int", Kind::Star) }
                },
                Qual {
                    ps: vec![],
                    t: Pred { id: "Eq".to_string(), ty: tcon!("Bool", Kind::Star) }
                }
            ]));
        }
    }
}