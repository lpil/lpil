// TODO: how to we load the database from disk at boot?

use std::sync::Arc;

type ModuleName = Arc<Vec<String>>;
type CompilerResult<T> = Result<Arc<T>, Arc<Error>>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Error {
    Cycle { modules: Vec<String> },
    Parse,
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Yup,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AST<Type> {
    Node(Type),
}

#[salsa::query_group(GleamStorage)]
trait Gleam: salsa::Database {
    #[salsa::input]
    fn input_source(&self, key: ModuleName) -> String;

    fn parse(&self, key: ModuleName) -> CompilerResult<AST<()>>;

    #[salsa::cycle(recover_type_check_cycle)]
    fn type_check(&self, key: ModuleName) -> CompilerResult<AST<Type>>;

    fn codegen(&self, key: ModuleName) -> CompilerResult<String>;
}

fn parse(db: &impl Gleam, name: ModuleName) -> CompilerResult<AST<()>> {
    // Get the source code for the module
    let _source = db.input_source(name.clone());

    // Parse the source code
    println!("parsing {:?}", name);
    if name.len() == 4 {
        Err(Arc::new(Error::Parse))
    } else {
        Ok(Arc::new(AST::Node(())))
    }
}

fn type_check(db: &impl Gleam, name: ModuleName) -> CompilerResult<AST<Type>> {
    // Parse the module
    let _ast = db.parse(name.clone())?;

    // Type check some modules that this module pretends to depend upon
    let deps = if name.as_slice() == &["main"] {
        // NOTE: Uncomment this to create an import cycle
        vec![vec!["test".to_string()]]
    // vec![vec!["lib".to_string()]]
    } else if name.as_slice() == &["test"] {
        vec![vec!["lib".to_string()], vec!["main".to_string()]]
    } else {
        vec![]
    };
    for dep in deps {
        db.type_check(Arc::new(dep))?;
    }

    // Type check the AST
    println!("type checking {:?}", name);
    if name.len() == 5 {
        Err(Arc::new(Error::Type))
    } else {
        Ok(Arc::new(AST::Node(Type::Yup)))
    }
}

fn codegen(db: &impl Gleam, name: ModuleName) -> CompilerResult<String> {
    // Get the typed AST
    let _typed_ast = db.type_check(name.clone())?;
    println!("codegening {:?}", name);
    // Generate code for the AST
    Ok(Arc::new("-module(app).".to_string()))
}

fn recover_type_check_cycle(
    _db: &impl Gleam,
    x: &Vec<String>,
    cycle: &ModuleName,
) -> CompilerResult<AST<Type>> {
    println!("x {:#?}", x);
    println!("cycle {:#?}", cycle);
    Err(Arc::new(Error::Cycle {
        modules: cycle.iter().map(|s| s.to_string()).collect(),
    }))
}

#[salsa::database(GleamStorage)]
#[derive(Default)]
struct Database {
    runtime: salsa::Runtime<Database>,
}

// Tell salsa where to find the runtime in your context.
impl salsa::Database for Database {
    fn salsa_runtime(&self) -> &salsa::Runtime<Self> {
        &self.runtime
    }

    fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<Self> {
        &mut self.runtime
    }
}

pub fn main() {
    let mut database = Database::default();
    let test = Arc::new(vec!["test".to_string()]);
    let lib = Arc::new(vec!["lib".to_string()]);
    let main = Arc::new(vec!["main".to_string()]);

    database.set_input_source(main, "".to_string());
    database.set_input_source(lib, "".to_string());
    database.set_input_source(test.clone(), "".to_string());

    println!("compiling {:?}\n", database.codegen(test.clone()));
    println!("compiling again {:?}\n", database.codegen(test));
}
