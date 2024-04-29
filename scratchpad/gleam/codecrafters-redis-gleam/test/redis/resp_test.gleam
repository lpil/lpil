import gleam/string
import gleeunit/should
import redis/resp.{Parsed}

pub fn parse_null_test() {
  <<"_\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(data: resp.Null, remaining_input: <<>>))
}

pub fn parse_null_string_test() {
  <<"$-1\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(data: resp.NullString, remaining_input: <<>>))
}

pub fn parse_null_array_test() {
  <<"*-1\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(data: resp.NullArray, remaining_input: <<>>))
}

pub fn parse_simple_string_pong_test() {
  <<"+PONG\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(data: resp.String("PONG"), remaining_input: <<>>))
}

pub fn parse_simple_string_pong_with_extra_test() {
  <<"+PONG\r\n+PONG\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(
    Parsed(data: resp.String("PONG"), remaining_input: <<"+PONG\r\n":utf8>>),
  )
}

pub fn parse_simple_string_invalid_unicode_test() {
  <<"+POG":utf8, 1_232_837:size(128), "\r\n":utf8>>
  |> resp.parse
  |> should.be_error
  |> should.equal(resp.InvalidUnicode)
}

pub fn parse_simple_string_not_enough_input_test() {
  <<"+POG":utf8>>
  |> resp.parse
  |> should.be_error
  |> should.equal(resp.NotEnoughInput)
}

pub fn parse_bulk_string_pong_test() {
  <<"$4\r\nPONG\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(data: resp.String("PONG"), remaining_input: <<>>))
}

pub fn parse_bulk_string_10_test() {
  <<"$10\r\n0123456789\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(
    Parsed(data: resp.String("0123456789"), remaining_input: <<>>),
  )
}

pub fn parse_bulk_string_202_test() {
  let string = string.repeat("0123456789", 20) <> "ab"

  <<"$202\r\n":utf8, string:utf8, "\r\n":utf8, "extra!!":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(
    Parsed(data: resp.String(string), remaining_input: <<"extra!!":utf8>>),
  )
}

pub fn parse_bulk_string_not_enough_input_test() {
  <<"$10\r\n0123456789\r":utf8>>
  |> resp.parse
  |> should.be_error
  |> should.equal(resp.NotEnoughInput)
}

pub fn parse_bulk_string_not_enough_input_1_test() {
  <<"$10\r\n0123456789":utf8>>
  |> resp.parse
  |> should.be_error
  |> should.equal(resp.NotEnoughInput)
}

pub fn parse_bulk_string_invalid_input_test() {
  <<"$4\r\n":utf8, 255, 255, 255, 255, "\r\n":utf8>>
  |> resp.parse
  |> should.be_error
  |> should.equal(resp.InvalidUnicode)
}

pub fn parse_array_empty_test() {
  <<"*0\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(resp.Array([]), <<>>))
}

pub fn parse_array_hello_world_test() {
  <<"*2\r\n$5\r\nhello\r\n$5\r\nworld\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(
    Parsed(resp.Array([resp.String("hello"), resp.String("world")]), <<>>),
  )
}

pub fn parse_array_gleam_is_really_cool_test() {
  <<"*4\r\n$5\r\nGleam\r\n$2\r\nis\r\n$6\r\nreally\r\n$4\r\ncool\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(
    Parsed(
      resp.Array([
        resp.String("Gleam"),
        resp.String("is"),
        resp.String("really"),
        resp.String("cool"),
      ]),
      <<>>,
    ),
  )
}

pub fn parse_int_0_test() {
  <<":0\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(resp.Int(0), <<>>))
}

pub fn parse_int_1000_test() {
  <<":1000\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(resp.Int(1000), <<>>))
}

pub fn parse_int_positive_1000_test() {
  <<":+1000\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(resp.Int(1000), <<>>))
}

pub fn parse_int_negative_1000_test() {
  <<":-1000\r\n":utf8>>
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(resp.Int(-1000), <<>>))
}

pub fn roundtrip_string_test() {
  let input = resp.String("Gleam")
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}

pub fn roundtrip_array_empty_test() {
  let input = resp.Array([])
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}

pub fn roundtrip_array_non_empty_test() {
  let input = resp.Array([resp.String("Hello"), resp.String("Joe")])
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}

pub fn roundtrip_array_nested_test() {
  let input =
    resp.Array([
      resp.Array([resp.String("Hello"), resp.String("Joe")]),
      resp.Array([resp.String("Hello"), resp.String("Joe")]),
    ])
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}

pub fn roundtrip_null_test() {
  let input = resp.Null
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}

pub fn roundtrip_int_negative_test() {
  let input = resp.Int(-123)
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}

pub fn roundtrip_int_positive_test() {
  let input = resp.Int(123)
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}

pub fn roundtrip_nullstring_test() {
  let input = resp.NullString
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}

pub fn roundtrip_nullarray_test() {
  let input = resp.NullArray
  input
  |> resp.encode
  |> resp.parse
  |> should.be_ok
  |> should.equal(Parsed(input, <<>>))
}
