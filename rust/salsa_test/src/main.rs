// TODO: how to we load the database from disk at boot?
// TODO: can we avoid cloning the module name repeatedly?

type ModuleName = Vec<String>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Error {
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

    fn parse(&self, key: ModuleName) -> Result<AST<()>, Error>;

    fn type_check(&self, key: ModuleName) -> Result<AST<Type>, Error>;

    fn codegen(&self, key: ModuleName) -> Result<String, Error>;
}

fn parse(db: &impl Gleam, name: ModuleName) -> Result<AST<()>, Error> {
    // Get the source code for the module
    let _source = db.input_source(name.clone());

    // Parse the source code
    println!("parsing {:?}", name);
    if name.len() == 4 {
        Err(Error::Parse)
    } else {
        Ok(AST::Node(()))
    }
}

fn type_check(db: &impl Gleam, name: ModuleName) -> Result<AST<Type>, Error> {
    // Parse the module
    let _ast = db.parse(name.clone())?;

    // Type check some modules that this module pretends to depend upon
    let deps = if name.as_slice() == &["main"] {
        vec![vec!["lib".to_string()]]
    } else if name.as_slice() == &["test"] {
        vec![vec!["lib".to_string()], vec!["main".to_string()]]
    } else {
        vec![]
    };
    for dep in deps {
        db.type_check(dep)?;
    }

    // Type check the AST
    println!("type checking {:?}", name);
    if name.len() == 5 {
        Err(Error::Type)
    } else {
        Ok(AST::Node(Type::Yup))
    }
}

fn codegen(db: &impl Gleam, name: ModuleName) -> Result<String, Error> {
    // Get the typed AST
    let _typed_ast = db.type_check(name.clone())?;
    println!("codegening {:?}", name);
    // Generate code for the AST
    Ok("-module(app).".to_string())
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

    database.set_input_source(vec!["main".to_string()], "".to_string());
    database.set_input_source(vec!["lib".to_string()], "".to_string());
    database.set_input_source(vec!["test".to_string()], "".to_string());

    println!("{:?}", database.codegen(vec!["test".to_string()]));
}
