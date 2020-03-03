// TODO: parse
// TODO: compile deps
// TODO: how to we load the database from disk at boot?

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
    let _source = db.input_source(name.clone());
    if name.len() == 4 {
        Err(Error::Parse)
    } else {
        Ok(AST::Node(()))
    }
}

fn type_check(db: &impl Gleam, name: ModuleName) -> Result<AST<Type>, Error> {
    let _ast = db.parse(name.clone())?;
    if name.len() == 5 {
        Err(Error::Type)
    } else {
        Ok(AST::Node(Type::Yup))
    }
}

fn codegen(db: &impl Gleam, name: ModuleName) -> Result<String, Error> {
    let _typed_ast = db.type_check(name.clone())?;
    Ok("-module(app).".to_string())
}

#[salsa::database(GleamStorage)]
#[derive(Default)]
struct DatabaseStruct {
    runtime: salsa::Runtime<DatabaseStruct>,
}

// Tell salsa where to find the runtime in your context.
impl salsa::Database for DatabaseStruct {
    fn salsa_runtime(&self) -> &salsa::Runtime<Self> {
        &self.runtime
    }

    fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<Self> {
        &mut self.runtime
    }
}
