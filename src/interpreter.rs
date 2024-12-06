use crate::syntax;

#[derive(Debug)]
pub enum Error {
    NameNotBound(Var),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Var {
    Regular(syntax::Var),
}

impl Var {
    pub fn pretty(&self) -> String {
        match self {
            Self::Regular(v) => v.name.clone(),
        }
    }
}

#[derive(Clone)]
pub struct Scope {
    vars: std::collections::HashMap<Var, Value>,
}

impl Scope {
    pub fn pretty(&self) -> String {
        let mut ret = "[".to_owned();
        for (k, v) in self.vars.iter() {
            ret.push_str(&format!("({}, {})", k.pretty(), v.pretty()));
        }
        ret.push_str("]");
        ret
    }
    pub fn new() -> Self {
        Self {
            vars: std::collections::HashMap::new(),
        }
    }
    pub fn bind(&mut self, nm: Var, v: Value) {
        self.vars.insert(nm, v);
    }
    pub fn bind_unary<F>(&mut self, nm: &str, f: F)
        where F: Fn(Value) -> Result<Value, Error> + 'static
    {
        let nm = Var::Regular(syntax::Var { name: nm.to_owned() });
        self.bind(nm.clone(), Value::Builtin(Builtin { nm: Some(nm), f: std::rc::Rc::new(f) }));
    }
    pub fn bind_binary<F>(&mut self, nm: &str, f: F)
        where F: Fn(Value, Value) -> Result<Value, Error> + 'static
    {
        let nm = Var::Regular(syntax::Var { name: nm.to_owned() });
        let rf = std::rc::Rc::new(f);
        self.bind(nm.clone(), Value::Builtin(Builtin {
            nm: Some(nm),
            f: std::rc::Rc::new(move |x| {
                let rff = rf.clone();
                Ok(Value::Builtin(Builtin {
                    nm: None,
                    f: std::rc::Rc::new(move |y| {
                        rff(x.clone(), y)
                    }),
                }))
            }),
        }));
    }
    pub fn collapse(&mut self, other: Self) {
        self.vars.extend(other.vars)
    }
    pub fn lookup(&self, nm: &Var) -> Option<&Value> {
        self.vars.get(nm)
    }
}

#[derive(Clone)]
pub enum ClosureBody {
    Expr(syntax::Expr),
    Closure(Box<Closure>),
}

impl ClosureBody {
    pub fn pretty(&self) -> String {
        match self {
            Self::Expr(e) => format!("{:?}", e),
            Self::Closure(c) => c.pretty(),
        }
    }
}

#[derive(Clone)]
pub struct Closure {
    arg: Var,
    scope: Scope, // bindings for free (non-arg) variables in body
    body: ClosureBody,
}

impl Closure {
    pub fn pretty(&self) -> String {
        format!("({} -> {})", self.arg.pretty(), self.body.pretty())
    }
}

#[derive(Clone)]
pub struct Builtin {
    nm: Option<Var>,
    f: std::rc::Rc<dyn Fn(Value) -> Result<Value, Error>>,
}

impl Builtin {
    pub fn pretty(&self) -> String {
        if let Some(nm) = &self.nm {
            format!("<builtin: {}>", nm.pretty())
        } else {
            "<builtin>".to_owned()
        }
    }
    pub fn apply(&self, v: Value) -> Result<Value, Error> {
        (self.f)(v)
    }
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Closure(Closure),
    Composition(Vec<Value>),
    Builtin(Builtin),
}

impl Value {
    pub fn pretty(&self) -> String {
        match self {
            Self::Nil => "nil".to_owned(),
            Self::Integer(x) => format!("{}", x),
            Self::Float(x) => format!("{}", x),
            Self::String(x) => format!("{}", x),
            Self::Char(x) => format!("{}", x),
            Self::Closure(cl) => cl.pretty(),
            Self::Composition(fs) =>
                format!("[{}]", fs.iter().map(|f| f.pretty()).collect::<Vec<_>>().join(" . ")),
            Self::Builtin(_) => "<builtin>".to_owned(),
        }
    }
    pub fn apply(self, i: &mut Interpreter, v: Value) -> Result<Value, Error> {
        match self {
            Self::Closure(Closure { arg, mut scope, body }) => {
                match body {
                    ClosureBody::Expr(e) => {
                        scope.bind(arg, v);
                        i.eval_in(&scope, e)
                    },
                    ClosureBody::Closure(mut c) => {
                        c.scope.collapse(scope);
                        c.scope.bind(arg, v);
                        Ok(Value::Closure(*c))
                    }
                }
            },
            Self::Composition(fs) => {
                let mut res = v;
                for f in fs.into_iter().rev() {
                    res = f.apply(i, res)?;
                }
                Ok(res)
            },
            Self::Builtin(b) => b.apply(v),
            _ => Ok(self),
        }
    }
}

pub struct Interpreter {
    globals: Scope,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Scope::new();
        globals.bind_binary("+", |vx, vy| {
            if let (Value::Integer(x), Value::Integer(y)) = (vx, vy) {
                Ok(Value::Integer(x + y))
            } else {
                Ok(Value::Nil)
            }
        });
        globals.bind_binary("*", |vx, vy| {
            if let (Value::Integer(x), Value::Integer(y)) = (vx, vy) {
                Ok(Value::Integer(x * y))
            } else {
                Ok(Value::Nil)
            }
        });
        Self {
            globals,
        }
    }
    fn capture_fresh(&self, s: &mut Scope, locals: &Scope, skip: &Vec<Var>, e: &syntax::UExpr) -> Result<(), Error> {
        log::info!("checking: {:?} (skip is {:?})", e, skip);
        match e {
            syntax::UExpr::Atom(syntax::Atom::Var(vnm)) => {
                let nm = Var::Regular(vnm.clone());
                if skip.contains(&nm) { return Ok(()); }
                if self.globals.lookup(&nm).is_some() { return Ok(()); }
                if let Some(v) = locals.lookup(&nm) {
                    s.bind(nm, v.clone());
                    Ok(())
                } else {
                    log::info!("name not bound while capturing: {}", nm.pretty());
                    Err(Error::NameNotBound(nm))
                }
            },
            syntax::UExpr::Apply(f, xs) => {
                self.capture_fresh(s, locals, skip, &f.u)?;
                for x in xs {
                    self.capture_fresh(s, locals, skip, &x.u)?;
                }
                Ok(())
            },
            syntax::UExpr::Compose(fs) => {
                for f in fs {
                    self.capture_fresh(s, locals, skip, &f.u)?;
                }
                Ok(())
            },
            syntax::UExpr::AbstractCompose(vs, fs) => {
                let mut newskip = skip.clone();
                for v in vs {
                    newskip.push(Var::Regular(v.clone()));
                }
                for f in fs {
                    self.capture_fresh(s, locals, &newskip, &f.u)?;
                }
                Ok(())
            },
            _ => { Ok(()) },
        }
    }
    fn eval_atom(&mut self, ctx: syntax::Span, locals: &Scope, a: syntax::Atom) -> Result<Value, Error> {
        match a {
            syntax::Atom::Var(nm) => {
                let var = Var::Regular(nm);
                if let Some(v) = locals.lookup(&var).or_else(|| self.globals.lookup(&var)) {
                    log::info!("read var {} = {}", var.pretty(), v.pretty());
                    Ok(v.clone())
                } else {
                    Err(Error::NameNotBound(var))
                }
            }
            syntax::Atom::Integer(x) => Ok(Value::Integer(x)),
            syntax::Atom::Float(x) => Ok(Value::Float(x)),
            syntax::Atom::String(x) => Ok(Value::String(x)),
        }
    }
    pub fn eval_in(&mut self, locals: &Scope, e: syntax::Expr) -> Result<Value, Error> {
        log::info!("eval {:?} in context {:?}", e, locals.pretty());
        match e.u {
            syntax::UExpr::Atom(a) => self.eval_atom(e.span, locals, a),
            syntax::UExpr::Apply(bf, xs) => {
                log::info!("evaluating function");
                let mut res = self.eval_in(locals, *bf)?;
                log::info!("evaluating args");
                for x in xs {
                    let vx = self.eval_in(locals, x)?;
                    let y = vx.apply(self, Value::Nil)?;
                    res = res.apply(self, y)?;
                }
                Ok(res)
            },
            syntax::UExpr::Compose(fs) => {
                let mut vs = Vec::new();
                for f in fs {
                    let v = self.eval_in(locals, f)?;
                    vs.push(v);
                }
                Ok(Value::Composition(vs))
            },
            syntax::UExpr::AbstractCompose(vars, fs) => {
                let body = syntax::Tagged {
                    u: syntax::UExpr::Compose(fs),
                    span: e.span,
                };
                let skip = vars.iter().map(|v| Var::Regular(v.clone())).collect();
                let mut itr = vars.into_iter().rev();
                if let Some(fv) = itr.next() {
                    log::info!("wrapping");
                    let mut scope = Scope::new();
                    log::info!("collecting fresh variables... (skipping: {:?})", skip);
                    self.capture_fresh(&mut scope, locals, &skip, &body.u)?;
                    log::info!("collected fresh vars! {}", scope.pretty());
                    let mut cl = Closure {
                        arg: Var::Regular(fv),
                        scope,
                        body: ClosureBody::Expr(body),
                    };
                    for v in itr {
                        cl = Closure {
                            arg: Var::Regular(v),
                            scope: Scope::new(),
                            body: ClosureBody::Closure(Box::new(cl)),
                        }
                    }
                    Ok(Value::Closure(cl))
                } else {
                    self.eval_in(locals, body)
                }
            },
        }
    }
    pub fn eval(&mut self, e: syntax::Expr) -> Result<Value, Error> {
        self.eval_in(&Scope::new(), e)
    }
}
