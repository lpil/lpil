import gleam/dynamic as dyn
import gleam/dynamic_extra.{decoder, run, with}
import gleam/function
import gleeunit/should

type Box {
  Box(one: Int, two: Float, three: String, four: Bool)
}

pub fn pipeline_ok_test() {
  let data = dyn.from(#(1, 2.0, "three", True))

  Box
  |> function.curry4
  |> decoder
  |> with(dyn.element(0, dyn.int))
  |> with(dyn.element(1, dyn.float))
  |> with(dyn.element(2, dyn.string))
  |> with(dyn.element(3, dyn.bool))
  |> run(data)
  |> should.equal(Ok(Box(1, 2.0, "three", True)))
}

pub fn pipeline_first_fails_test() {
  let data = dyn.from(#(1.0, 2.0, "three", True))

  Box
  |> function.curry4
  |> decoder
  |> with(dyn.element(0, dyn.int))
  |> with(dyn.element(1, dyn.float))
  |> with(dyn.element(2, dyn.string))
  |> with(dyn.element(3, dyn.bool))
  |> run(data)
  |> should.equal(Error([dyn.DecodeError("Int", "Float", ["0"])]))
}

pub fn pipeline_last_fails_test() {
  let data = dyn.from(#(1, 2.0, "three", Nil))

  Box
  |> function.curry4
  |> decoder
  |> with(dyn.element(0, dyn.int))
  |> with(dyn.element(1, dyn.float))
  |> with(dyn.element(2, dyn.string))
  |> with(dyn.element(3, dyn.bool))
  |> run(data)
  |> should.equal(Error([dyn.DecodeError("Bool", "Nil", ["3"])]))
}

pub fn pipeline_all_fails_test() {
  let data = dyn.from(#(Nil, Nil, Nil, Nil))

  Box
  |> function.curry4
  |> decoder
  |> with(dyn.element(0, dyn.int))
  |> with(dyn.element(1, dyn.float))
  |> with(dyn.element(2, dyn.string))
  |> with(dyn.element(3, dyn.bool))
  |> run(data)
  |> should.equal(Error([
    dyn.DecodeError("Int", "Nil", ["0"]),
    dyn.DecodeError("Float", "Nil", ["1"]),
    dyn.DecodeError("String", "Nil", ["2"]),
    dyn.DecodeError("Bool", "Nil", ["3"]),
  ]))
}
