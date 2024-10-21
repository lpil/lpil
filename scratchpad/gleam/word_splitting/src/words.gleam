import gleam/bit_array
import gleam/list
import gleam/result
import gleam/string

pub fn split(text: String) -> List(String) {
  loop(text, <<text:utf8>>, 0, 0, [])
}

fn loop(
  rest: String,
  input: BitArray,
  start: Int,
  length: Int,
  words: List(String),
) -> List(String) {
  case string.pop_grapheme(rest) {
    Ok(#(c, rest)) -> {
      let size = string.byte_size(c)
      let is_letter =
        c
        |> string.to_utf_codepoints
        |> list.first
        |> result.map(string.utf_codepoint_to_int)
        |> result.unwrap(0)
        |> is_letter_codepoint_int

      case is_letter {
        True -> {
          loop(rest, input, start, length + size, words)
        }
        False if length == 0 -> {
          loop(rest, input, start + size, 0, words)
        }
        False -> {
          let assert Ok(word) = bit_array.slice(input, start, length)
          let assert Ok(word) = bit_array.to_string(word)
          loop(rest, input, start + length + size, 0, [word, ..words])
        }
      }
    }
    Error(_) if length == 0 -> list.reverse(words)
    Error(_) -> {
      let assert Ok(word) = bit_array.slice(input, start, length)
      let assert Ok(word) = bit_array.to_string(word)
      list.reverse([word, ..words])
    }
  }
}

fn is_letter_codepoint_int(codepoint: Int) -> Bool {
  // Latin A-Z
  codepoint >= 65
  && codepoint <= 90
  // Latin a-z
  || codepoint >= 97
  && codepoint <= 122
  // Latin Extended 1 (À-Ö)
  || codepoint >= 192
  && codepoint <= 214
  // Latin Extended 2 (Ø-ö)
  || codepoint >= 216
  && codepoint <= 246
  // Other Latin Extended
  || codepoint >= 248
  && codepoint <= 696
  // Greek
  || codepoint >= 880
  && codepoint <= 1023
  // Cyrillic
  || codepoint >= 1040
  && codepoint <= 1103
  // Armenian
  || codepoint >= 1329
  && codepoint <= 1366
  // Hebrew (א - ת)
  || codepoint >= 1488
  && codepoint <= 1514
  // CJK Unified Ideographs (Chinese, Kanji)
  || codepoint >= 19_968
  && codepoint <= 40_959
  // Hiragana (Japanese)
  || codepoint >= 12_352
  && codepoint <= 12_447
  // Katakana (Japanese)
  || codepoint >= 12_448
  && codepoint <= 12_543
}
