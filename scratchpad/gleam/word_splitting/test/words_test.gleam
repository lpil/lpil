import gleeunit
import gleeunit/should
import words

pub fn main() {
  gleeunit.main()
}

pub fn english_test() {
  "Hello, world! Café \"de Paris\" 123 Apples."
  |> words.split
  |> should.equal(["Hello", "world", "Café", "de", "Paris", "Apples"])
}

pub fn greek_test() {
  "Γειά σου, κόσμε!"
  |> words.split
  |> should.equal(["Γειά", "σου", "κόσμε"])
}

pub fn russian_test() {
  "Привет, мир! How are you?"
  |> words.split
  |> should.equal(["Привет", "мир", "How", "are", "you"])
}

pub fn hebrew_test() {
  "שלום לכולם!"
  |> words.split
  |> should.equal(["שלום", "לכולם"])
}

pub fn chinese_test() {
  "你好，世界！"
  |> words.split
  |> should.equal(["你好", "世界"])
}

pub fn japanese_test() {
  "こんにちは、世界！"
  |> words.split
  |> should.equal(["こんにちは", "世界"])
}

pub fn emoji_test() {
  "🙂🚀 ok ya?"
  |> words.split
  |> should.equal(["ok", "ya"])
}

pub fn accented_latin_test() {
  "Àlô, ça va?"
  |> words.split
  |> should.equal(["Àlô", "ça", "va"])
}

pub fn vietnamese_test() {
  "Xin chào"
  |> words.split
  |> should.equal(["Xin", "chào"])
}
