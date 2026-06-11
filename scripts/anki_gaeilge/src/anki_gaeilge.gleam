import abair
import ankiconnect
import argv
import gleam/bool
import gleam/dict
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import input
import shellout
import simplifile

// const deck_to_add_to = deck_test
const deck_to_add_to = deck_module_3

const deck_module_1 = "0 Irish::lpil::CityLit Irish::Beginners module 1"

const deck_module_2 = "0 Irish::lpil::CityLit Irish::Beginners module 2"

const deck_module_3 = "0 Irish::lpil::CityLit Irish::Beginners module 3"

const deck_misc_irish = "0 Irish::lpil::Misc Irish"

const all_decks = [
  deck_module_1,
  deck_module_2,
  deck_module_3,
  deck_test,
  deck_misc_irish,
]

const deck_test = "test-deck-plz-ignore"

pub fn main() {
  case argv.load().arguments {
    ["add-audio"] -> list.each(all_decks, add_audio)
    ["add-page", path] -> add_page(path)
    _ -> panic as "unexpected CLI argument"
  }
}

fn add_page(path: String) -> Nil {
  let assert Ok(text) = simplifile.read(path) as "reading file"
  io.println("Adding to " <> deck_to_add_to)
  text
  |> string.trim
  |> string.split_once("---")
  |> result.map(pair.first)
  |> result.unwrap(text)
  |> string.split("\n\n")
  |> list.filter(fn(item) { item != "" })
  |> list.each(add_item)
}

fn add_item(text: String) -> Nil {
  let assert [gaeilge, ..berla] = string.split(text, "\n")
  let berla = berla |> list.map(string.trim) |> string.join("\n<br>\n")
  io.println(
    "\nGaeilge: "
    <> string.inspect(gaeilge)
    <> "\nBérla:   "
    <> string.inspect(berla),
  )

  let assert Ok(answer) = input.input("Add note? [Yn] ")
  let skip = answer != "" && answer != "y"
  use <- bool.guard(skip, Nil)

  let id =
    ankiconnect.NewNote(
      deck_name: deck_to_add_to,
      model_name: "Basic (and reversed card)",
      fields: dict.from_list([
        #("Front", gaeilge),
        #("Back", berla),
      ]),
      tags: [],
      audio: [],
      video: [],
      picture: [],
    )
    |> ankiconnect.add_note_request
    |> assert_send(ankiconnect.add_note_response)

  io.println("Added #" <> int.to_string(id))
  Nil
}

fn add_audio(deck: String) -> Nil {
  let all_notes =
    ankiconnect.notes_info_query_request(
      "deck:\"" <> deck <> "\" -[sound -tag:no-audio",
    )
    |> assert_send(ankiconnect.notes_info_response)

  list.each(all_notes, process_note)
}

fn process_note(note: ankiconnect.NoteInfo) {
  let id = int.to_string(note.note_id)
  let wav = id <> ".wav"
  let mp3 = id <> ".mp3"
  let assert Ok(front) = dict.get(note.fields, "Front")
  let original = front.value
  let normalised = normalise(original)

  io.println("\n" <> original)
  case original != normalised {
    True -> io.println("  edited: " <> normalised)
    False -> Nil
  }
  let assert Ok(answer) = input.input("add audio? [Yn] ")
  let skip = answer != "" && answer != "y"
  use <- bool.guard(skip, Nil)

  io.println("  Synthesizing")
  let synthesised =
    normalised
    |> abair.synthesis_request
    |> assert_send(abair.synthesis_response)
  let assert Ok(_) = simplifile.write_bits(wav, synthesised.audio)
  io.println("  Compressing")
  let assert Ok(_) =
    shellout.command(
      "ffmpeg",
      ["-i", wav, "-c:a", "libmp3lame", "-q:a", "4", mp3],
      ".",
      [],
    )

  io.println("  Storing media")
  ankiconnect.store_media_file_request(
    mp3,
    ankiconnect.Path(mp3),
    delete_existing: True,
  )
  |> assert_send(ankiconnect.store_media_file_response)

  io.println("  Updating note")
  dict.map_values(note.fields, fn(_, v) { v.value })
  |> dict.insert("Front", normalised <> "<br>[sound:" <> mp3 <> "]")
  |> ankiconnect.update_note_fields_request(note.note_id, _)
  |> assert_send(ankiconnect.update_note_fields_response)

  let assert Ok(_) = simplifile.delete(mp3)
  let assert Ok(_) = simplifile.delete(wav)
  Nil
}

fn normalise(text: String) -> String {
  let text = string.replace(text, "&nbsp;", " ")

  let wrapped_in_div =
    string.starts_with(text, "<div>") && string.ends_with(text, "</div>")
  let text = case wrapped_in_div {
    True -> text |> string.drop_start(5) |> string.drop_end(6)
    False -> text
  }

  let text = case string.ends_with(text, "<br>") {
    True -> text |> string.drop_end(4)
    False -> text
  }

  let text = case string.split_once(text, "(") {
    Ok(#(before, _after)) -> before
    Error(_) -> text
  }

  string.trim(text)
}

fn assert_send(
  request: Request(String),
  parser: fn(Response(String)) -> Result(a, b),
) -> a {
  let assert Ok(response) = httpc.send(request)
  let assert Ok(data) = parser(response)
  data
}
