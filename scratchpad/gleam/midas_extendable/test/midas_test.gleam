import midas.{type Effect}

// User defined collection of data and effects they want
pub type Context {
  Context(
    log: fn(String) -> Effect(Context, Nil),
    log2: fn(String) -> Effect(Context, Nil),
  )
}

pub fn main() -> Nil {
  let ctx = Context(log: todo, log2: todo)

  midas.run(ctx, {
    // This function can be used to get the context within an effect,
    // regardless of how deep within the programmer is.
    // Not sure why I need a type annotation here.
    use ctx: Context <- midas.context
    use _ <- midas.do(ctx.log("Hello!"))
    midas.done(Nil)
  })

  Nil
}
