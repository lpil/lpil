// Wait, I think I'm just making a stack of the reader, future, and result monads.

import snag.{type Snag}

//
// A future type, to help with async.
//

pub opaque type Future(t) {
  Future(data: t)
}

fn await(future: Future(t1), next: fn(t1) -> Future(t2)) -> Future(t2) {
  // Pretend this is async
  next(future.data)
}

fn resolve(data: t1) -> Future(t1) {
  Future(data)
}

//
// The Effect type.
//
// The difference here is that it is parameterised with the user's context.
//

pub opaque type Effect(ctx, t) {
  Effect(run: fn(ctx) -> Future(Result(t, Snag)))
}

pub fn done(data: t) -> Effect(ctx, t) {
  Effect(fn(_) { resolve(Ok(data)) })
}

pub fn fail(error: Snag) -> Effect(ctx, t) {
  Effect(fn(_) { resolve(Error(error)) })
}

pub fn context(task: fn(ctx) -> Effect(ctx, t)) -> Effect(ctx, t) {
  Effect(fn(ctx) { task(ctx).run(ctx) })
}

pub fn run(ctx: ctx, task: Effect(ctx, t1)) -> Future(Result(t1, Snag)) {
  task.run(ctx)
}

pub fn do(
  first: Effect(ctx, t1),
  second: fn(t1) -> Effect(ctx, t2),
) -> Effect(ctx, t2) {
  Effect(fn(ctx) {
    use t1 <- await(first.run(ctx))
    case t1 {
      Ok(t1) -> second(t1).run(ctx)
      Error(e) -> resolve(Error(e))
    }
  })
}

pub fn side_effect(
  function: fn(fn(a) -> Future(a)) -> Future(Result(t1, Snag)),
) -> Effect(ctx, t1) {
  Effect(fn(_effects) { function(resolve) })
}
