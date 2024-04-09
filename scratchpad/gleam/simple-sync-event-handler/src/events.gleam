import gleam/list

pub type Event {
  One(some: Int, data: String)
  Two(other: Float, data: String)
}

pub opaque type Eventer(state) {
  Eventer(state: state, handlers: List(fn(state, Event) -> state))
}

pub fn new_eventer(state: t) -> Eventer(t) {
  Eventer(state, [])
}

pub fn add_handler(
  eventer: Eventer(t),
  handler: fn(t, Event) -> state,
) -> Eventer(t) {
  Eventer(..eventer, handlers: [handler, ..eventer.handlers])
}

pub fn dispatch(eventer: Eventer(t), event: Event) -> Eventer(t) {
  let state =
    list.fold(eventer.handlers, eventer.state, fn(state, handler) {
      handler(state, event)
    })
  Eventer(..eventer, state: state)
}
