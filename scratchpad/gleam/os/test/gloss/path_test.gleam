import gleam/list
import gleam/option
import gleam/string
import gloss/path

const unix_paths = [
  "", ".", "..", "/", "//", "one", "two", "one/two", "/one", "/one/two", "one/",
  "./one", "one/.", "one/..", "../one", "\\one", "C:/one", "one//two",
]

const windows_paths = [
  "", ".", "..", "C:", "C:\\", "C:one", "C:two", "C:\\one", "C:\\two", "\\", "/",
  "\\one", "\\two", "one", "two", "one\\two", "one/two", "\\\\one\\two",
  "//one/two", "\\\\server\\share\\file", "./one", "one\\.", "one\\..", "1:one",
  "one\\D:two",
]

pub fn join_unix_prefix_is_always_none_test() {
  use path <- list.each(unix_paths)
  assert path.parts_unix(path).prefix == option.None
    as { path <> " should not have prefix" }
}

pub fn join_unix_kind_is_always_preserved_test() {
  use left <- list.each(unix_paths)
  use right <- list.each(unix_paths)
  assert path.kind_unix(path.join_unix(left, right)) == path.kind_unix(left)
    as { "joining " <> left <> " to " <> right <> " should not change kind" }
}

pub fn join_unix_root_is_always_preserved_test() {
  use left <- list.each(unix_paths)
  use right <- list.each(unix_paths)
  assert path.parts_unix(path.join_unix(left, right)).rooted
    == path.parts_unix(left).rooted
    as { "joining " <> left <> " to " <> right <> " should not change rooted" }
}

pub fn join_windows_prefix_is_always_preserved_test() {
  use left <- list.each(windows_paths)
  use right <- list.each(windows_paths)
  assert path.parts_windows(path.join_windows(left, right)).prefix
    == path.parts_windows(left).prefix
    as { "joining " <> left <> " to " <> right <> " should not change prefix" }
}

pub fn join_windows_root_is_always_preserved_test() {
  use left <- list.each(windows_paths)
  use right <- list.each(windows_paths)
  assert path.parts_windows(path.join_windows(left, right)).rooted
    == path.parts_windows(left).rooted
    as { "joining " <> left <> " to " <> right <> " should not change rooted" }
}

pub fn join_windows_kind_is_always_preserved_test() {
  use left <- list.each(windows_paths)
  use right <- list.each(windows_paths)
  assert path.kind_windows(path.join_windows(left, right))
    == path.kind_windows(left)
    as { "joining " <> left <> " to " <> right <> " should not change kind" }
}

pub fn kind_unix_agrees_with_parts_unix_test() {
  use path <- list.each(unix_paths)
  let expected = case path.parts_unix(path).rooted {
    True -> path.Absolute
    False -> path.Relative
  }
  assert path.kind_unix(path) == expected
    as { "kind and parts disagree about " <> path }
}

pub fn kind_windows_agrees_with_parts_windows_test() {
  use path <- list.each(windows_paths)
  let parts = path.parts_windows(path)
  let expected = case parts.prefix, parts.rooted {
    option.Some(_), True -> path.Absolute
    option.Some(_), False -> path.DriveRelative
    option.None, True -> path.RootRelative
    option.None, False -> path.Relative
  }
  assert path.kind_windows(path) == expected
    as { "kind and parts disagree about " <> path }
}

pub fn file_name_unix_is_the_last_component_test() {
  use path <- list.each(unix_paths)
  let expected = case list.last(path.parts_unix(path).components) {
    Ok("..") -> Error(Nil)
    other -> other
  }
  assert path.file_name_unix(path) == expected
    as { "file name and parts disagree about " <> path }
}

pub fn file_name_windows_is_the_last_component_test() {
  use path <- list.each(windows_paths)
  let expected = case list.last(path.parts_windows(path).components) {
    Ok("..") -> Error(Nil)
    other -> other
  }
  assert path.file_name_windows(path) == expected
    as { "file name and parts disagree about " <> path }
}

pub fn join_unix_empty_right_is_identity_test() {
  use left <- list.each(unix_paths)
  assert path.join_unix(left, "") == left
    as { "joining nothing to " <> left <> " should return it unchanged" }
}

pub fn join_windows_empty_right_is_identity_test() {
  use left <- list.each(windows_paths)
  assert path.join_windows(left, "") == left
    as { "joining nothing to " <> left <> " should return it unchanged" }
}

pub fn parts_windows_ignores_separator_spelling_test() {
  use path <- list.each(windows_paths)
  assert path.parts_windows(path)
    == path.parts_windows(string.replace(path, "\\", "/"))
    as { "separator spelling should not affect the parts of " <> path }
}

pub fn kind_windows_1_test() {
  assert path.kind_windows("") == path.Relative
}

pub fn kind_windows_2_test() {
  assert path.kind_windows("foo.txt") == path.Relative
}

pub fn kind_windows_3_test() {
  assert path.kind_windows("one\\two") == path.Relative
}

pub fn kind_windows_4_test() {
  assert path.kind_windows("one/two") == path.Relative
}

pub fn kind_windows_5_test() {
  assert path.kind_windows(".\\src") == path.Relative
}

pub fn kind_windows_6_test() {
  assert path.kind_windows("..\\..\\elsewhere") == path.Relative
}

pub fn kind_windows_7_test() {
  assert path.kind_windows("C:") == path.DriveRelative
}

pub fn kind_windows_8_test() {
  assert path.kind_windows("c:") == path.DriveRelative
}

pub fn kind_windows_9_test() {
  assert path.kind_windows("D:some\\file") == path.DriveRelative
}

pub fn kind_windows_10_test() {
  assert path.kind_windows("d:some/file") == path.DriveRelative
}

pub fn kind_windows_11_test() {
  assert path.kind_windows("A:foo.txt") == path.DriveRelative
}

pub fn kind_windows_12_test() {
  assert path.kind_windows("Z:foo.txt") == path.DriveRelative
}

pub fn kind_windows_13_test() {
  assert path.kind_windows("a:foo.txt") == path.DriveRelative
}

pub fn kind_windows_14_test() {
  assert path.kind_windows("z:foo.txt") == path.DriveRelative
}

pub fn kind_windows_15_test() {
  assert path.kind_windows("\\") == path.RootRelative
}

pub fn kind_windows_16_test() {
  assert path.kind_windows("/") == path.RootRelative
}

pub fn kind_windows_17_test() {
  assert path.kind_windows("\\usr\\local\\bin\\gleam") == path.RootRelative
}

pub fn kind_windows_18_test() {
  assert path.kind_windows("/usr/local/bin/gleam") == path.RootRelative
}

pub fn kind_windows_19_test() {
  assert path.kind_windows("C:\\") == path.Absolute
}

pub fn kind_windows_20_test() {
  assert path.kind_windows("C:/") == path.Absolute
}

pub fn kind_windows_21_test() {
  assert path.kind_windows("C:\\usr\\local\\bin\\gleam") == path.Absolute
}

pub fn kind_windows_22_test() {
  assert path.kind_windows("C:/usr/local/bin/gleam") == path.Absolute
}

pub fn kind_windows_23_test() {
  assert path.kind_windows("c:\\usr\\local") == path.Absolute
}

pub fn kind_windows_24_test() {
  assert path.kind_windows("C:\\usr/local") == path.Absolute
}

pub fn kind_windows_25_test() {
  assert path.kind_windows("\\\\server\\share\\file") == path.Absolute
}

pub fn kind_windows_26_test() {
  assert path.kind_windows("//server/share/file") == path.Absolute
}

pub fn kind_windows_27_test() {
  assert path.kind_windows("\\/server/share") == path.Absolute
}

pub fn kind_windows_28_test() {
  assert path.kind_windows("/\\server/share") == path.Absolute
}

pub fn kind_windows_29_test() {
  assert path.kind_windows("\\\\server\\share") == path.Absolute
}

pub fn kind_windows_30_test() {
  assert path.kind_windows("\\\\?\\C:\\file") == path.Absolute
}

pub fn kind_windows_31_test() {
  assert path.kind_windows("\\\\.\\PIPE\\name") == path.Absolute
}

pub fn kind_windows_32_test() {
  assert path.kind_windows(
      "\\\\?\\Volume{b75e2c83-0000-0000-0000-602f00000000}\\file",
    )
    == path.Absolute
}

pub fn kind_windows_33_test() {
  assert path.kind_windows("1:foo.txt") == path.Relative
}

pub fn kind_windows_34_test() {
  assert path.kind_windows("::foo.txt") == path.Relative
}

pub fn kind_windows_35_test() {
  assert path.kind_windows("CC:foo.txt") == path.Relative
}

pub fn kind_windows_36_test() {
  // é is 2 bytes, not ascii.
  assert path.kind_windows("é:foo.txt") == path.Relative
}

pub fn kind_windows_37_test() {
  assert path.kind_windows("C") == path.Relative
}

pub fn kind_windows_38_test() {
  assert path.kind_windows("foo:bar") == path.Relative
}

pub fn kind_windows_39_test() {
  assert path.kind_windows(" C:\\wibble") == path.Relative
}

pub fn kind_unix_1_test() {
  assert path.kind_unix("") == path.Relative
}

pub fn kind_unix_2_test() {
  assert path.kind_unix("gleam.toml") == path.Relative
}

pub fn kind_unix_3_test() {
  assert path.kind_unix("src/path.gleam") == path.Relative
}

pub fn kind_unix_4_test() {
  assert path.kind_unix("./src") == path.Relative
}

pub fn kind_unix_5_test() {
  assert path.kind_unix("../../elsewhere/secrets") == path.Relative
}

pub fn kind_unix_6_test() {
  assert path.kind_unix(".") == path.Relative
}

pub fn kind_unix_7_test() {
  assert path.kind_unix("..") == path.Relative
}

pub fn kind_unix_8_test() {
  assert path.kind_unix("/") == path.Absolute
}

pub fn kind_unix_9_test() {
  assert path.kind_unix("/usr/local/bin/gleam") == path.Absolute
}

pub fn kind_unix_10_test() {
  assert path.kind_unix("/usr/local/") == path.Absolute
}

pub fn kind_unix_11_test() {
  assert path.kind_unix("/.") == path.Absolute
}

pub fn kind_unix_12_test() {
  assert path.kind_unix("/../elsewhere") == path.Absolute
}

pub fn kind_unix_13_test() {
  assert path.kind_unix("//server/share") == path.Absolute
}

pub fn kind_unix_14_test() {
  assert path.kind_unix("///usr/local") == path.Absolute
}

pub fn kind_unix_15_test() {
  assert path.kind_unix("\\") == path.Relative
}

pub fn kind_unix_16_test() {
  assert path.kind_unix("\\usr\\local") == path.Relative
}

pub fn kind_unix_17_test() {
  assert path.kind_unix("one\\two") == path.Relative
}

pub fn kind_unix_18_test() {
  assert path.kind_unix("/usr\\local") == path.Absolute
}

pub fn kind_unix_19_test() {
  assert path.kind_unix("C:\\usr\\local") == path.Relative
}

pub fn kind_unix_20_test() {
  assert path.kind_unix("C:/usr/local") == path.Relative
}

pub fn kind_unix_21_test() {
  assert path.kind_unix("C:") == path.Relative
}

pub fn kind_unix_22_test() {
  assert path.kind_unix("foo:bar") == path.Relative
}

pub fn kind_unix_23_test() {
  assert path.kind_unix(" /usr/local") == path.Relative
}

pub fn kind_unix_24_test() {
  assert path.kind_unix("é/foo") == path.Relative
}

pub fn join_unix_1_test() {
  assert path.join_unix("one", "two") == "one/two"
}

pub fn join_unix_2_test() {
  assert path.join_unix("one/two", "three") == "one/two/three"
}

pub fn join_unix_3_test() {
  assert path.join_unix("one", "two/three") == "one/two/three"
}

pub fn join_unix_4_test() {
  assert path.join_unix("/usr/local", "bin/gleam") == "/usr/local/bin/gleam"
}

pub fn join_unix_5_test() {
  assert path.join_unix("one/", "two") == "one/two"
}

pub fn join_unix_6_test() {
  assert path.join_unix("/", "usr") == "/usr"
}

pub fn join_unix_7_test() {
  assert path.join_unix("/usr/", "local") == "/usr/local"
}

pub fn join_unix_8_test() {
  assert path.join_unix("", "") == ""
}

pub fn join_unix_9_test() {
  assert path.join_unix("", "one") == "one"
}

pub fn join_unix_10_test() {
  assert path.join_unix("", "/one") == "one"
}

pub fn join_unix_11_test() {
  assert path.join_unix("one", "") == "one"
}

pub fn join_unix_12_test() {
  assert path.join_unix("/", "") == "/"
}

pub fn join_unix_13_test() {
  assert path.join_unix("one/two", "/three") == "one/two/three"
}

pub fn join_unix_14_test() {
  assert path.join_unix("/usr/local", "/etc") == "/usr/local/etc"
}

pub fn join_unix_15_test() {
  assert path.join_unix("", "/one") == "one"
  assert path.join_unix("one", "/") == "one"
}

pub fn join_unix_16_test() {
  assert path.join_unix("one", "..") == "one/.."
}

pub fn join_unix_17_test() {
  assert path.join_unix("one/two", "../three") == "one/two/../three"
}

pub fn join_unix_18_test() {
  assert path.join_unix("one", "./two") == "one/./two"
}

pub fn join_unix_19_test() {
  assert path.join_unix(".", "one") == "./one"
}

pub fn join_unix_20_test() {
  assert path.join_unix("one\\two", "three") == "one\\two/three"
}

pub fn join_unix_21_test() {
  assert path.join_unix("one", "\\two") == "one/\\two"
}

pub fn join_unix_22_test() {
  assert path.join_unix("C:\\one", "two") == "C:\\one/two"
}

pub fn join_unix_23_test() {
  assert path.join_unix("one//two", "three") == "one//two/three"
}

pub fn join_unix_24_test() {
  assert path.join_unix("one", "two//three") == "one/two//three"
}

pub fn join_unix_25_test() {
  assert path.join_unix("one/", "/two") == "one/two"
}

pub fn join_unix_26_test() {
  assert path.join_unix("/", "/") == "/"
}

pub fn join_unix_27_test() {
  assert path.join_unix("one", "//two") == "one/two"
}

pub fn join_unix_31_test() {
  assert path.join_unix("one", "two/") == "one/two/"
}

pub fn join_unix_32_test() {
  assert path.join_unix("one", "/two/") == "one/two/"
}

pub fn join_unix_33_test() {
  assert path.join_unix("one/", "") == "one/"
}

pub fn join_unix_34_test() {
  assert path.join_unix("one", "///two") == "one/two"
}

pub fn join_unix_35_test() {
  assert path.join_unix("one//", "/two") == "one//two"
}

pub fn join_unix_36_test() {
  assert path.join_unix("", "///") == ""
}

pub fn join_unix_37_test() {
  assert path.join_unix("one", "/two//three") == "one/two//three"
}

pub fn join_unix_38_test() {
  assert path.join_unix("/", "one") == "/one"
}

pub fn join_unix_39_test() {
  assert path.join_unix("/", "//one") == "/one"
}

pub fn join_unix_40_test() {
  assert path.join_unix("..", "/one") == "../one"
}

pub fn join_unix_41_test() {
  assert path.join_unix("one", "//") == "one"
}

pub fn join_windows_1_test() {
  assert path.join_windows("one", "two") == "one\\two"
}

pub fn join_windows_2_test() {
  assert path.join_windows("one\\two", "three") == "one\\two\\three"
}

pub fn join_windows_3_test() {
  assert path.join_windows("C:\\usr", "local\\bin") == "C:\\usr\\local\\bin"
}

pub fn join_windows_4_test() {
  assert path.join_windows("one", "..") == "one\\.."
}

pub fn join_windows_5_test() {
  assert path.join_windows("one\\", "two") == "one\\two"
}

pub fn join_windows_6_test() {
  assert path.join_windows("one/", "two") == "one/two"
}

pub fn join_windows_7_test() {
  assert path.join_windows("one\\", "/two") == "one\\two"
}

pub fn join_windows_8_test() {
  assert path.join_windows("one/", "\\two") == "one/two"
}

pub fn join_windows_9_test() {
  assert path.join_windows("one", "\\/\\two") == "one\\two"
}

pub fn join_windows_10_test() {
  assert path.join_windows("one/two", "three") == "one/two\\three"
}

pub fn join_windows_11_test() {
  assert path.join_windows("", "") == ""
}

pub fn join_windows_12_test() {
  assert path.join_windows("one", "") == "one"
}

pub fn join_windows_13_test() {
  assert path.join_windows("one\\", "") == "one\\"
}

pub fn join_windows_14_test() {
  assert path.join_windows("", "one") == "one"
}

pub fn join_windows_15_test() {
  assert path.join_windows("", "\\") == ""
}

pub fn join_windows_16_test() {
  assert path.join_windows("one", "\\") == "one"
}

pub fn join_windows_17_test() {
  assert path.join_windows("one", "\\two") == "one\\two"
}

pub fn join_windows_18_test() {
  assert path.join_windows("", "\\one") == "one"
}

pub fn join_windows_19_test() {
  assert path.join_windows("", "//one") == "one"
}

pub fn join_windows_20_test() {
  assert path.join_windows("..", "\\one") == "..\\one"
}

pub fn join_windows_21_test() {
  assert path.join_windows("one", "\\\\server\\share") == "one\\server\\share"
}

pub fn join_windows_22_test() {
  assert path.join_windows("", "\\\\server\\share") == "server\\share"
}

pub fn join_windows_23_test() {
  assert path.join_windows("C:", "two") == "C:two"
}

pub fn join_windows_24_test() {
  assert path.join_windows("C:", "\\two") == "C:two"
}

pub fn join_windows_25_test() {
  assert path.join_windows("c:", "two") == "c:two"
}

pub fn join_windows_26_test() {
  assert path.join_windows("C:", "") == "C:"
}

pub fn join_windows_27_test() {
  assert path.join_windows("C:", "/") == "C:"
}

pub fn join_windows_28_test() {
  assert path.join_windows("C:one", "\\two") == "C:one\\two"
}

pub fn join_windows_29_test() {
  assert path.join_windows("C:\\", "\\two") == "C:\\two"
}

pub fn join_windows_30_test() {
  assert path.join_windows("C:\\one", "\\two") == "C:\\one\\two"
}

pub fn join_windows_31_test() {
  assert path.join_windows("\\one", "\\two") == "\\one\\two"
}

pub fn join_windows_32_test() {
  assert path.join_windows("\\\\server\\share", "\\file")
    == "\\\\server\\share\\file"
}

pub fn join_windows_33_test() {
  assert path.join_windows("CC:", "two") == "CC:\\two"
}

pub fn join_windows_34_test() {
  assert path.join_windows("1:", "two") == "1:\\two"
}

pub fn join_windows_35_test() {
  assert path.join_windows("one:", "two") == "one:\\two"
}

pub fn join_windows_36_test() {
  assert path.join_windows("one", "D:two") == "one\\D:two"
}

pub fn join_windows_37_test() {
  assert path.join_windows("one", "two\\") == "one\\two\\"
}

pub fn join_windows_38_test() {
  assert path.join_windows("one", "\\two/") == "one\\two/"
}

pub fn join_windows_39_test() {
  assert path.join_windows("C:one", "two") == "C:one\\two"
}

pub fn join_windows_40_test() {
  assert path.join_windows("C:one\\two", "three") == "C:one\\two\\three"
}

pub fn join_windows_41_test() {
  assert path.join_windows("C:\\one", "two") == "C:\\one\\two"
}

pub fn join_windows_42_test() {
  assert path.join_windows("C:\\", "two") == "C:\\two"
}

pub fn join_windows_43_test() {
  assert path.join_windows("one", "///two") == "one\\two"
}

pub fn join_windows_44_test() {
  assert path.join_windows("one", "\\\\\\two") == "one\\two"
}

pub fn join_windows_45_test() {
  assert path.join_windows("one\\\\", "\\two") == "one\\\\two"
}

pub fn join_windows_46_test() {
  assert path.join_windows("", "\\/\\/") == ""
}

pub fn join_windows_47_test() {
  assert path.join_windows("one", "\\two//three\\\\four")
    == "one\\two//three\\\\four"
}

pub fn join_windows_48_test() {
  assert path.join_windows("\\\\server", "share") == "\\\\server\\share"
}

pub fn join_windows_49_test() {
  assert path.join_windows("one", "C:\\two") == "one\\C:\\two"
}

pub fn join_windows_50_test() {
  assert path.join_windows("C:one", "D:two") == "C:one\\D:two"
}

pub fn join_windows_51_test() {
  assert path.join_windows("", "C:\\one") == ".\\C:\\one"
}

pub fn join_windows_52_test() {
  assert path.join_windows("", "C:one") == ".\\C:one"
}

pub fn join_windows_53_test() {
  assert path.join_windows("", "C:") == ".\\C:"
}

pub fn join_windows_54_test() {
  assert path.join_windows("", "1:one") == "1:one"
}

pub fn join_windows_55_test() {
  assert path.join_windows("", "one") == "one"
}

pub fn file_name_unix_1_test() {
  assert path.file_name_unix("one/two/three.txt") == Ok("three.txt")
}

pub fn file_name_unix_2_test() {
  assert path.file_name_unix("three.txt") == Ok("three.txt")
}

pub fn file_name_unix_3_test() {
  assert path.file_name_unix("/usr/local/bin/gleam") == Ok("gleam")
}

pub fn file_name_unix_4_test() {
  assert path.file_name_unix("/gleam") == Ok("gleam")
}

pub fn file_name_unix_5_test() {
  assert path.file_name_unix("one/.hidden") == Ok(".hidden")
}

pub fn file_name_unix_6_test() {
  assert path.file_name_unix("archive.tar.gz") == Ok("archive.tar.gz")
}

pub fn file_name_unix_7_test() {
  assert path.file_name_unix("one/...") == Ok("...")
}

pub fn file_name_unix_8_test() {
  assert path.file_name_unix("") == Error(Nil)
}

pub fn file_name_unix_9_test() {
  assert path.file_name_unix("/") == Error(Nil)
}

pub fn file_name_unix_10_test() {
  assert path.file_name_unix("//") == Error(Nil)
}

pub fn file_name_unix_11_test() {
  assert path.file_name_unix(".") == Error(Nil)
}

pub fn file_name_unix_12_test() {
  assert path.file_name_unix("./.") == Error(Nil)
}

pub fn file_name_unix_13_test() {
  assert path.file_name_unix("/.") == Error(Nil)
}

pub fn file_name_unix_14_test() {
  assert path.file_name_unix("one/.") == Ok("one")
}

pub fn file_name_unix_15_test() {
  assert path.file_name_unix("one/two/.") == Ok("two")
}

pub fn file_name_unix_16_test() {
  assert path.file_name_unix("one/./.") == Ok("one")
}

pub fn file_name_unix_17_test() {
  assert path.file_name_unix("./one") == Ok("one")
}

pub fn file_name_unix_18_test() {
  assert path.file_name_unix("one/./two") == Ok("two")
}

pub fn file_name_unix_19_test() {
  assert path.file_name_unix("..") == Error(Nil)
}

pub fn file_name_unix_20_test() {
  assert path.file_name_unix("one/..") == Error(Nil)
}

pub fn file_name_unix_21_test() {
  assert path.file_name_unix("../..") == Error(Nil)
}

pub fn file_name_unix_22_test() {
  assert path.file_name_unix("one/../two") == Ok("two")
}

pub fn file_name_unix_23_test() {
  assert path.file_name_unix("../one") == Ok("one")
}

pub fn file_name_unix_24_test() {
  assert path.file_name_unix("one/two/") == Ok("two")
}

pub fn file_name_unix_25_test() {
  assert path.file_name_unix("one/two//") == Ok("two")
}

pub fn file_name_unix_26_test() {
  assert path.file_name_unix("/one/") == Ok("one")
}

pub fn file_name_unix_27_test() {
  assert path.file_name_unix("one//two") == Ok("two")
}

pub fn file_name_unix_28_test() {
  assert path.file_name_unix("one/./") == Ok("one")
}

pub fn file_name_unix_29_test() {
  assert path.file_name_unix("one\\two") == Ok("one\\two")
}

pub fn file_name_unix_30_test() {
  assert path.file_name_unix("one/two\\three") == Ok("two\\three")
}

pub fn file_name_unix_31_test() {
  assert path.file_name_unix("C:\\one") == Ok("C:\\one")
}

pub fn file_name_unix_32_test() {
  assert path.file_name_unix("one/\\") == Ok("\\")
}

pub fn file_name_unix_33_test() {
  assert path.file_name_unix("one/.two.") == Ok(".two.")
}

pub fn file_name_unix_34_test() {
  assert path.file_name_unix("one/..two") == Ok("..two")
}

pub fn file_name_unix_35_test() {
  assert path.file_name_unix("one/two..") == Ok("two..")
}

pub fn file_name_unix_36_test() {
  assert path.file_name_unix("one/two//") == Ok("two")
}

pub fn file_name_windows_1_test() {
  assert path.file_name_windows("one\\two\\three.txt") == Ok("three.txt")
}

pub fn file_name_windows_2_test() {
  assert path.file_name_windows("one/two/three.txt") == Ok("three.txt")
}

pub fn file_name_windows_3_test() {
  assert path.file_name_windows("three.txt") == Ok("three.txt")
}

pub fn file_name_windows_4_test() {
  assert path.file_name_windows("C:\\usr\\local\\gleam") == Ok("gleam")
}

pub fn file_name_windows_5_test() {
  assert path.file_name_windows("C:\\gleam") == Ok("gleam")
}

pub fn file_name_windows_6_test() {
  assert path.file_name_windows("one\\.hidden") == Ok(".hidden")
}

pub fn file_name_windows_7_test() {
  assert path.file_name_windows("archive.tar.gz") == Ok("archive.tar.gz")
}

pub fn file_name_windows_8_test() {
  assert path.file_name_windows("one\\...") == Ok("...")
}

pub fn file_name_windows_9_test() {
  assert path.file_name_windows("one/two\\three") == Ok("three")
}

pub fn file_name_windows_10_test() {
  assert path.file_name_windows("one\\two/three") == Ok("three")
}

pub fn file_name_windows_11_test() {
  assert path.file_name_windows("") == Error(Nil)
}

pub fn file_name_windows_12_test() {
  assert path.file_name_windows("\\") == Error(Nil)
}

pub fn file_name_windows_13_test() {
  assert path.file_name_windows("/") == Error(Nil)
}

pub fn file_name_windows_14_test() {
  assert path.file_name_windows("\\\\") == Error(Nil)
}

pub fn file_name_windows_15_test() {
  assert path.file_name_windows(".") == Error(Nil)
}

pub fn file_name_windows_16_test() {
  assert path.file_name_windows(".\\.") == Error(Nil)
}

pub fn file_name_windows_17_test() {
  assert path.file_name_windows("\\.") == Error(Nil)
}

pub fn file_name_windows_18_test() {
  assert path.file_name_windows("one\\.") == Ok("one")
}

pub fn file_name_windows_19_test() {
  assert path.file_name_windows("one\\two\\.") == Ok("two")
}

pub fn file_name_windows_20_test() {
  assert path.file_name_windows("one\\.\\.") == Ok("one")
}

pub fn file_name_windows_21_test() {
  assert path.file_name_windows(".\\one") == Ok("one")
}

pub fn file_name_windows_22_test() {
  assert path.file_name_windows("one\\.\\two") == Ok("two")
}

pub fn file_name_windows_23_test() {
  assert path.file_name_windows("one/./two") == Ok("two")
}

pub fn file_name_windows_24_test() {
  assert path.file_name_windows("..") == Error(Nil)
}

pub fn file_name_windows_25_test() {
  assert path.file_name_windows("one\\..") == Error(Nil)
}

pub fn file_name_windows_26_test() {
  assert path.file_name_windows("..\\..") == Error(Nil)
}

pub fn file_name_windows_27_test() {
  assert path.file_name_windows("one\\..\\two") == Ok("two")
}

pub fn file_name_windows_28_test() {
  assert path.file_name_windows("..\\one") == Ok("one")
}

pub fn file_name_windows_29_test() {
  assert path.file_name_windows("one\\two\\") == Ok("two")
}

pub fn file_name_windows_30_test() {
  assert path.file_name_windows("one\\two\\\\") == Ok("two")
}

pub fn file_name_windows_31_test() {
  assert path.file_name_windows("one/two/") == Ok("two")
}

pub fn file_name_windows_32_test() {
  assert path.file_name_windows("\\one\\") == Ok("one")
}

pub fn file_name_windows_33_test() {
  assert path.file_name_windows("one\\\\two") == Ok("two")
}

pub fn file_name_windows_34_test() {
  assert path.file_name_windows("one\\.\\") == Ok("one")
}

pub fn file_name_windows_35_test() {
  assert path.file_name_windows("C:") == Error(Nil)
}

pub fn file_name_windows_36_test() {
  assert path.file_name_windows("C:\\") == Error(Nil)
}

pub fn file_name_windows_37_test() {
  assert path.file_name_windows("C:/") == Error(Nil)
}

pub fn file_name_windows_38_test() {
  assert path.file_name_windows("C:one") == Ok("one")
}

pub fn file_name_windows_39_test() {
  assert path.file_name_windows("C:one\\two") == Ok("two")
}

pub fn file_name_windows_40_test() {
  assert path.file_name_windows("C:\\one") == Ok("one")
}

pub fn file_name_windows_41_test() {
  assert path.file_name_windows("C:.") == Error(Nil)
}

pub fn file_name_windows_42_test() {
  assert path.file_name_windows("1:one") == Ok("1:one")
}

pub fn file_name_windows_43_test() {
  assert path.file_name_windows("one\\D:two") == Ok("D:two")
}

pub fn file_name_windows_44_test() {
  assert path.file_name_windows("foo:bar") == Ok("foo:bar")
}

pub fn file_name_windows_45_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("\\\\server\\share") == Error(Nil)
}

pub fn file_name_windows_46_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("\\\\server\\share\\") == Error(Nil)
}

pub fn file_name_windows_47_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("\\\\server\\share\\file.txt") == Ok("file.txt")
}

pub fn file_name_windows_48_test() {
  assert path.file_name_windows("//server/share/file.txt") == Ok("file.txt")
}

pub fn file_name_windows_49_test() {
  assert path.file_name_windows("one\\.two.") == Ok(".two.")
}

pub fn file_name_windows_50_test() {
  assert path.file_name_windows("one\\..two") == Ok("..two")
}

pub fn file_name_windows_51_test() {
  assert path.file_name_windows("one\\two..") == Ok("two..")
}

pub fn file_name_windows_52_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("//server/share") == Error(Nil)
}

pub fn file_name_windows_53_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("\\/server\\share") == Error(Nil)
}

pub fn file_name_windows_54_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("/\\server/share") == Error(Nil)
}

pub fn file_name_windows_55_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("//server/share/") == Error(Nil)
}

pub fn file_name_windows_56_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("\\/server/share/file.txt") == Ok("file.txt")
}

pub fn file_name_windows_57_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("//server/share/one/two") == Ok("two")
}

pub fn file_name_windows_58_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("\\\\server") == Error(Nil)
}

pub fn file_name_windows_59_test() {
  // UNC paths require a server and a share, neither are file names.
  assert path.file_name_windows("//server") == Error(Nil)
}

pub fn parts_unix_1_test() {
  assert path.parts_unix("")
    == path.Parts(prefix: option.None, rooted: False, components: [])
}

pub fn parts_unix_2_test() {
  assert path.parts_unix("one")
    == path.Parts(prefix: option.None, rooted: False, components: ["one"])
}

pub fn parts_unix_3_test() {
  assert path.parts_unix("one/two/three")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "two",
      "three",
    ])
}

pub fn parts_unix_4_test() {
  assert path.parts_unix("one.txt")
    == path.Parts(prefix: option.None, rooted: False, components: ["one.txt"])
}

pub fn parts_unix_5_test() {
  assert path.parts_unix("/")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_unix_6_test() {
  assert path.parts_unix("/one")
    == path.Parts(prefix: option.None, rooted: True, components: ["one"])
}

pub fn parts_unix_7_test() {
  assert path.parts_unix("/usr/local/bin")
    == path.Parts(prefix: option.None, rooted: True, components: [
      "usr",
      "local",
      "bin",
    ])
}

pub fn parts_unix_8_test() {
  assert path.parts_unix("one//two")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "two"])
}

pub fn parts_unix_9_test() {
  assert path.parts_unix("one/two/")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "two"])
}

pub fn parts_unix_10_test() {
  assert path.parts_unix("one/two//")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "two"])
}

pub fn parts_unix_11_test() {
  assert path.parts_unix("//")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_unix_12_test() {
  assert path.parts_unix("//one")
    == path.Parts(prefix: option.None, rooted: True, components: ["one"])
}

pub fn parts_unix_13_test() {
  assert path.parts_unix(".")
    == path.Parts(prefix: option.None, rooted: False, components: [])
}

pub fn parts_unix_14_test() {
  assert path.parts_unix("./one")
    == path.Parts(prefix: option.None, rooted: False, components: ["one"])
}

pub fn parts_unix_15_test() {
  assert path.parts_unix("one/./two")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "two"])
}

pub fn parts_unix_16_test() {
  assert path.parts_unix("one/.")
    == path.Parts(prefix: option.None, rooted: False, components: ["one"])
}

pub fn parts_unix_17_test() {
  assert path.parts_unix("/.")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_unix_18_test() {
  assert path.parts_unix("..")
    == path.Parts(prefix: option.None, rooted: False, components: [".."])
}

pub fn parts_unix_19_test() {
  assert path.parts_unix("../../one")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "..",
      "..",
      "one",
    ])
}

pub fn parts_unix_20_test() {
  assert path.parts_unix("one/../two")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "..",
      "two",
    ])
}

pub fn parts_unix_21_test() {
  assert path.parts_unix("/..")
    == path.Parts(prefix: option.None, rooted: True, components: [".."])
}

pub fn parts_unix_22_test() {
  assert path.parts_unix("one/...")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "..."])
}

pub fn parts_unix_23_test() {
  assert path.parts_unix("one/.hidden")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      ".hidden",
    ])
}

pub fn parts_unix_24_test() {
  assert path.parts_unix("one/..two")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "..two",
    ])
}

pub fn parts_unix_25_test() {
  assert path.parts_unix("one\\two")
    == path.Parts(prefix: option.None, rooted: False, components: ["one\\two"])
}

pub fn parts_unix_26_test() {
  assert path.parts_unix("\\one")
    == path.Parts(prefix: option.None, rooted: False, components: ["\\one"])
}

pub fn parts_unix_27_test() {
  assert path.parts_unix("C:/one")
    == path.Parts(prefix: option.None, rooted: False, components: ["C:", "one"])
}

pub fn parts_windows_1_test() {
  assert path.parts_windows("")
    == path.Parts(prefix: option.None, rooted: False, components: [])
}

pub fn parts_windows_2_test() {
  assert path.parts_windows("one")
    == path.Parts(prefix: option.None, rooted: False, components: ["one"])
}

pub fn parts_windows_3_test() {
  assert path.parts_windows("one\\two\\three")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "two",
      "three",
    ])
}

pub fn parts_windows_4_test() {
  assert path.parts_windows("one/two/three")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "two",
      "three",
    ])
}

pub fn parts_windows_5_test() {
  assert path.parts_windows("one/two\\three")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "two",
      "three",
    ])
}

pub fn parts_windows_6_test() {
  assert path.parts_windows("\\")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_windows_7_test() {
  assert path.parts_windows("/")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_windows_8_test() {
  assert path.parts_windows("\\one\\two")
    == path.Parts(prefix: option.None, rooted: True, components: ["one", "two"])
}

pub fn parts_windows_9_test() {
  assert path.parts_windows("/usr/local")
    == path.Parts(prefix: option.None, rooted: True, components: [
      "usr",
      "local",
    ])
}

pub fn parts_windows_10_test() {
  assert path.parts_windows("C:")
    == path.Parts(prefix: option.Some("C:"), rooted: False, components: [])
}

pub fn parts_windows_11_test() {
  assert path.parts_windows("C:one")
    == path.Parts(prefix: option.Some("C:"), rooted: False, components: ["one"])
}

pub fn parts_windows_12_test() {
  assert path.parts_windows("d:one/two")
    == path.Parts(prefix: option.Some("d:"), rooted: False, components: [
      "one",
      "two",
    ])
}

pub fn parts_windows_13_test() {
  assert path.parts_windows("C:\\")
    == path.Parts(prefix: option.Some("C:"), rooted: True, components: [])
}

pub fn parts_windows_14_test() {
  assert path.parts_windows("C:/")
    == path.Parts(prefix: option.Some("C:"), rooted: True, components: [])
}

pub fn parts_windows_15_test() {
  assert path.parts_windows("C:\\one\\two")
    == path.Parts(prefix: option.Some("C:"), rooted: True, components: [
      "one",
      "two",
    ])
}

pub fn parts_windows_16_test() {
  assert path.parts_windows("C:/one/two")
    == path.Parts(prefix: option.Some("C:"), rooted: True, components: [
      "one",
      "two",
    ])
}

pub fn parts_windows_17_test() {
  assert path.parts_windows("c:\\one")
    == path.Parts(prefix: option.Some("c:"), rooted: True, components: ["one"])
}

pub fn parts_windows_18_test() {
  assert path.parts_windows("z:one")
    == path.Parts(prefix: option.Some("z:"), rooted: False, components: ["one"])
}

pub fn parts_windows_19_test() {
  assert path.parts_windows("\\\\server\\share")
    == path.Parts(
      prefix: option.Some("\\\\server\\share"),
      rooted: True,
      components: [],
    )
}

pub fn parts_windows_20_test() {
  assert path.parts_windows("\\\\server\\share\\one")
    == path.Parts(
      prefix: option.Some("\\\\server\\share"),
      rooted: True,
      components: ["one"],
    )
}

pub fn parts_windows_21_test() {
  assert path.parts_windows("//server/share/one/two")
    == path.Parts(
      prefix: option.Some("\\\\server\\share"),
      rooted: True,
      components: ["one", "two"],
    )
}

pub fn parts_windows_22_test() {
  assert path.parts_windows("\\/server/share")
    == path.Parts(
      prefix: option.Some("\\\\server\\share"),
      rooted: True,
      components: [],
    )
}

pub fn parts_windows_23_test() {
  assert path.parts_windows("/\\server/share")
    == path.Parts(
      prefix: option.Some("\\\\server\\share"),
      rooted: True,
      components: [],
    )
}

pub fn parts_windows_24_test() {
  assert path.parts_windows("\\\\server")
    == path.Parts(
      prefix: option.Some("\\\\server"),
      rooted: True,
      components: [],
    )
}

pub fn parts_windows_25_test() {
  assert path.parts_windows("\\\\SERVER\\Share\\one")
    == path.Parts(
      prefix: option.Some("\\\\SERVER\\Share"),
      rooted: True,
      components: ["one"],
    )
}

pub fn parts_windows_26_test() {
  assert path.parts_windows("//server/share")
    == path.parts_windows("\\\\server\\share")
}

pub fn parts_windows_27_test() {
  assert path.parts_windows("C:/one") == path.parts_windows("C:\\one")
}

pub fn parts_windows_28_test() {
  assert path.parts_windows("\\\\?\\C:\\one")
    == path.Parts(prefix: option.Some("\\\\?\\C:"), rooted: True, components: [
      "one",
    ])
}

pub fn parts_windows_29_test() {
  assert path.parts_windows("\\\\.\\PIPE\\name")
    == path.Parts(prefix: option.Some("\\\\.\\PIPE"), rooted: True, components: [
      "name",
    ])
}

pub fn parts_windows_30_test() {
  assert path.parts_windows("one\\\\two")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "two"])
}

pub fn parts_windows_31_test() {
  assert path.parts_windows("one\\two\\")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "two"])
}

pub fn parts_windows_32_test() {
  assert path.parts_windows("one/two\\")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "two"])
}

pub fn parts_windows_33_test() {
  assert path.parts_windows("C:\\\\one")
    == path.Parts(prefix: option.Some("C:"), rooted: True, components: ["one"])
}

pub fn parts_windows_34_test() {
  assert path.parts_windows("\\\\server\\share\\\\one\\")
    == path.Parts(
      prefix: option.Some("\\\\server\\share"),
      rooted: True,
      components: ["one"],
    )
}

pub fn parts_windows_35_test() {
  assert path.parts_windows(".")
    == path.Parts(prefix: option.None, rooted: False, components: [])
}

pub fn parts_windows_36_test() {
  assert path.parts_windows(".\\one")
    == path.Parts(prefix: option.None, rooted: False, components: ["one"])
}

pub fn parts_windows_37_test() {
  assert path.parts_windows("one\\.\\two")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "two"])
}

pub fn parts_windows_38_test() {
  assert path.parts_windows("one\\.")
    == path.Parts(prefix: option.None, rooted: False, components: ["one"])
}

pub fn parts_windows_39_test() {
  assert path.parts_windows("\\.")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_windows_40_test() {
  assert path.parts_windows("C:.")
    == path.Parts(prefix: option.Some("C:"), rooted: False, components: [])
}

pub fn parts_windows_41_test() {
  assert path.parts_windows("C:\\.")
    == path.Parts(prefix: option.Some("C:"), rooted: True, components: [])
}

pub fn parts_windows_42_test() {
  assert path.parts_windows("..")
    == path.Parts(prefix: option.None, rooted: False, components: [".."])
}

pub fn parts_windows_43_test() {
  assert path.parts_windows("..\\..\\one")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "..",
      "..",
      "one",
    ])
}

pub fn parts_windows_44_test() {
  assert path.parts_windows("one\\..\\two")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "..",
      "two",
    ])
}

pub fn parts_windows_45_test() {
  assert path.parts_windows("C:\\..")
    == path.Parts(prefix: option.Some("C:"), rooted: True, components: [".."])
}

pub fn parts_windows_46_test() {
  assert path.parts_windows("\\\\server\\share\\..")
    == path.Parts(
      prefix: option.Some("\\\\server\\share"),
      rooted: True,
      components: [".."],
    )
}

pub fn parts_windows_47_test() {
  assert path.parts_windows("one\\...")
    == path.Parts(prefix: option.None, rooted: False, components: ["one", "..."])
}

pub fn parts_windows_48_test() {
  assert path.parts_windows("one\\.hidden")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      ".hidden",
    ])
}

pub fn parts_windows_49_test() {
  assert path.parts_windows("one\\..two")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "..two",
    ])
}

pub fn parts_windows_50_test() {
  assert path.parts_windows("1:one")
    == path.Parts(prefix: option.None, rooted: False, components: ["1:one"])
}

pub fn parts_windows_51_test() {
  assert path.parts_windows("foo:bar")
    == path.Parts(prefix: option.None, rooted: False, components: ["foo:bar"])
}

pub fn parts_windows_52_test() {
  assert path.parts_windows("one\\D:two")
    == path.Parts(prefix: option.None, rooted: False, components: [
      "one",
      "D:two",
    ])
}

pub fn parts_windows_53_test() {
  assert path.parts_windows("\\\\")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_windows_54_test() {
  assert path.parts_windows("//")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_windows_55_test() {
  assert path.parts_windows("\\/")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_windows_56_test() {
  assert path.parts_windows("/\\")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_windows_57_test() {
  assert path.parts_windows("\\\\\\")
    == path.Parts(prefix: option.None, rooted: True, components: [])
}

pub fn parts_windows_58_test() {
  assert path.parts_windows("\\\\\\one")
    == path.Parts(prefix: option.None, rooted: True, components: ["one"])
}

pub fn parent_unix_1_test() {
  assert path.parent_unix("one/two/three") == Ok("one/two")
}

pub fn parent_unix_2_test() {
  assert path.parent_unix("one/two") == Ok("one")
}

pub fn parent_unix_3_test() {
  assert path.parent_unix("/usr/local/bin") == Ok("/usr/local")
}

pub fn parent_unix_4_test() {
  assert path.parent_unix("one") == Ok("")
}

pub fn parent_unix_5_test() {
  assert path.parent_unix("one.txt") == Ok("")
}

pub fn parent_unix_6_test() {
  assert path.parent_unix("/one") == Ok("/")
}

pub fn parent_unix_7_test() {
  assert path.parent_unix("/") == Error(Nil)
}

pub fn parent_unix_8_test() {
  assert path.parent_unix("") == Error(Nil)
}

pub fn parent_unix_9_test() {
  assert path.parent_unix("//") == Error(Nil)
}

pub fn parent_unix_10_test() {
  assert path.parent_unix("one/") == Ok("")
}

pub fn parent_unix_11_test() {
  assert path.parent_unix("one///") == Ok("")
}

pub fn parent_unix_12_test() {
  assert path.parent_unix("one/.") == Ok("")
}

pub fn parent_unix_13_test() {
  assert path.parent_unix("one/./././.") == Ok("")
}

pub fn parent_unix_14_test() {
  assert path.parent_unix("one/./") == Ok("")
}

pub fn parent_unix_15_test() {
  assert path.parent_unix("one/two/") == Ok("one")
}

pub fn parent_unix_16_test() {
  assert path.parent_unix("one/two/.") == Ok("one")
}

pub fn parent_unix_17_test() {
  assert path.parent_unix("/one/") == Ok("/")
}

pub fn parent_unix_18_test() {
  assert path.parent_unix("/one/.") == Ok("/")
}

pub fn parent_unix_19_test() {
  assert path.parent_unix(".") == Error(Nil)
}

pub fn parent_unix_20_test() {
  assert path.parent_unix("./") == Error(Nil)
}

pub fn parent_unix_21_test() {
  assert path.parent_unix("././.") == Error(Nil)
}

pub fn parent_unix_22_test() {
  assert path.parent_unix("/.") == Error(Nil)
}

pub fn parent_unix_23_test() {
  assert path.parent_unix("/./././") == Error(Nil)
}

pub fn parent_unix_24_test() {
  assert path.parent_unix("one//two/three") == Ok("one//two")
}

pub fn parent_unix_25_test() {
  assert path.parent_unix("one///two") == Ok("one")
}

pub fn parent_unix_26_test() {
  assert path.parent_unix("one/./two") == Ok("one")
}

pub fn parent_unix_27_test() {
  assert path.parent_unix("./one/two") == Ok("./one")
}

pub fn parent_unix_28_test() {
  assert path.parent_unix("./one") == Ok("")
}

pub fn parent_unix_29_test() {
  assert path.parent_unix("//one/two") == Ok("//one")
}

pub fn parent_unix_30_test() {
  assert path.parent_unix("/./one") == Ok("/")
}

pub fn parent_unix_31_test() {
  assert path.parent_unix("..") == Ok("")
}

pub fn parent_unix_32_test() {
  assert path.parent_unix("one/..") == Ok("one")
}

pub fn parent_unix_33_test() {
  assert path.parent_unix("../..") == Ok("..")
}

pub fn parent_unix_34_test() {
  assert path.parent_unix("../one") == Ok("..")
}

pub fn parent_unix_35_test() {
  assert path.parent_unix("/..") == Ok("/")
}

pub fn parent_unix_36_test() {
  assert path.parent_unix("one/../") == Ok("one")
}

pub fn parent_unix_37_test() {
  assert path.parent_unix("one\\two") == Ok("")
}

pub fn parent_unix_38_test() {
  assert path.parent_unix("one/two\\three") == Ok("one")
}

pub fn parent_unix_39_test() {
  assert path.parent_unix("one/...") == Ok("one")
}

pub fn parent_unix_40_test() {
  assert path.parent_unix("one/.hidden") == Ok("one")
}

pub fn parent_unix_41_test() {
  assert path.parent_unix("one/..two/") == Ok("one")
}

pub fn parent_windows_1_test() {
  assert path.parent_windows("one\\two\\three") == Ok("one\\two")
}

pub fn parent_windows_2_test() {
  assert path.parent_windows("one/two/three") == Ok("one/two")
}

pub fn parent_windows_3_test() {
  assert path.parent_windows("one/two\\three") == Ok("one/two")
}

pub fn parent_windows_4_test() {
  assert path.parent_windows("C:\\usr\\local\\bin") == Ok("C:\\usr\\local")
}

pub fn parent_windows_5_test() {
  assert path.parent_windows("one") == Ok("")
}

pub fn parent_windows_6_test() {
  assert path.parent_windows("one.txt") == Ok("")
}

pub fn parent_windows_7_test() {
  assert path.parent_windows("\\one") == Ok("\\")
}

pub fn parent_windows_8_test() {
  assert path.parent_windows("/one") == Ok("/")
}

pub fn parent_windows_9_test() {
  assert path.parent_windows("\\") == Error(Nil)
}

pub fn parent_windows_10_test() {
  assert path.parent_windows("/") == Error(Nil)
}

pub fn parent_windows_11_test() {
  assert path.parent_windows("") == Error(Nil)
}

pub fn parent_windows_12_test() {
  assert path.parent_windows("\\\\") == Error(Nil)
}

pub fn parent_windows_13_test() {
  assert path.parent_windows("C:\\one") == Ok("C:\\")
}

pub fn parent_windows_14_test() {
  assert path.parent_windows("C:/one") == Ok("C:/")
}

pub fn parent_windows_15_test() {
  assert path.parent_windows("C:one") == Ok("C:")
}

pub fn parent_windows_16_test() {
  assert path.parent_windows("C:one\\two") == Ok("C:one")
}

pub fn parent_windows_17_test() {
  assert path.parent_windows("C:\\") == Error(Nil)
}

pub fn parent_windows_18_test() {
  assert path.parent_windows("C:/") == Error(Nil)
}

pub fn parent_windows_19_test() {
  assert path.parent_windows("C:") == Error(Nil)
}

pub fn parent_windows_20_test() {
  assert path.parent_windows("\\\\server\\share\\one")
    == Ok("\\\\server\\share\\")
}

pub fn parent_windows_21_test() {
  assert path.parent_windows("\\\\server\\share\\one\\two")
    == Ok("\\\\server\\share\\one")
}

pub fn parent_windows_22_test() {
  assert path.parent_windows("\\\\server\\share") == Error(Nil)
}

pub fn parent_windows_23_test() {
  assert path.parent_windows("\\\\server\\share\\") == Error(Nil)
}

pub fn parent_windows_24_test() {
  assert path.parent_windows("//server/share/one") == Ok("//server/share/")
}

pub fn parent_windows_25_test() {
  assert path.parent_windows("//server/share") == Error(Nil)
}

pub fn parent_windows_26_test() {
  assert path.parent_windows("\\\\server") == Error(Nil)
}

pub fn parent_windows_27_test() {
  assert path.parent_windows("one\\") == Ok("")
}

pub fn parent_windows_28_test() {
  assert path.parent_windows("one\\\\\\") == Ok("")
}

pub fn parent_windows_29_test() {
  assert path.parent_windows("one\\.") == Ok("")
}

pub fn parent_windows_30_test() {
  assert path.parent_windows("one\\.\\.\\.") == Ok("")
}

pub fn parent_windows_31_test() {
  assert path.parent_windows("one\\two\\") == Ok("one")
}

pub fn parent_windows_32_test() {
  assert path.parent_windows("one\\two\\.") == Ok("one")
}

pub fn parent_windows_33_test() {
  assert path.parent_windows("one/two/") == Ok("one")
}

pub fn parent_windows_34_test() {
  assert path.parent_windows("\\one\\") == Ok("\\")
}

pub fn parent_windows_35_test() {
  assert path.parent_windows("C:\\one\\.") == Ok("C:\\")
}

pub fn parent_windows_36_test() {
  assert path.parent_windows(".") == Error(Nil)
}

pub fn parent_windows_37_test() {
  assert path.parent_windows(".\\") == Error(Nil)
}

pub fn parent_windows_38_test() {
  assert path.parent_windows(".\\.\\.") == Error(Nil)
}

pub fn parent_windows_39_test() {
  assert path.parent_windows("\\.") == Error(Nil)
}

pub fn parent_windows_40_test() {
  assert path.parent_windows("C:\\.") == Error(Nil)
}

pub fn parent_windows_41_test() {
  assert path.parent_windows("C:.") == Error(Nil)
}

pub fn parent_windows_42_test() {
  assert path.parent_windows(".\\one") == Ok("")
}

pub fn parent_windows_43_test() {
  assert path.parent_windows("one\\.\\two") == Ok("one")
}

pub fn parent_windows_44_test() {
  assert path.parent_windows("\\.\\one") == Ok("\\")
}

pub fn parent_windows_45_test() {
  assert path.parent_windows("C:\\.\\one") == Ok("C:\\")
}

pub fn parent_windows_46_test() {
  assert path.parent_windows(".\\one\\two") == Ok(".\\one")
}

pub fn parent_windows_47_test() {
  assert path.parent_windows("one\\\\two\\three") == Ok("one\\\\two")
}

pub fn parent_windows_48_test() {
  assert path.parent_windows("one\\\\\\two") == Ok("one")
}

pub fn parent_windows_49_test() {
  assert path.parent_windows("one/two\\\\three") == Ok("one/two")
}

pub fn parent_windows_50_test() {
  assert path.parent_windows("..") == Ok("")
}

pub fn parent_windows_51_test() {
  assert path.parent_windows("one\\..") == Ok("one")
}

pub fn parent_windows_52_test() {
  assert path.parent_windows("..\\..") == Ok("..")
}

pub fn parent_windows_53_test() {
  assert path.parent_windows("..\\one") == Ok("..")
}

pub fn parent_windows_54_test() {
  assert path.parent_windows("\\..") == Ok("\\")
}

pub fn parent_windows_55_test() {
  assert path.parent_windows("C:\\..") == Ok("C:\\")
}

pub fn parent_windows_56_test() {
  assert path.parent_windows("one\\...") == Ok("one")
}

pub fn parent_windows_57_test() {
  assert path.parent_windows("one\\.hidden") == Ok("one")
}

pub fn parent_windows_58_test() {
  assert path.parent_windows("one\\..two\\") == Ok("one")
}

pub fn parent_windows_59_test() {
  assert path.parent_windows("1:one") == Ok("")
}

pub fn parent_windows_60_test() {
  assert path.parent_windows("one\\D:two") == Ok("one")
}

pub fn starts_with_unix_1_test() {
  assert path.starts_with_unix("one", "one/two")
}

pub fn starts_with_unix_2_test() {
  assert path.starts_with_unix("one", "one/two/three")
}

pub fn starts_with_unix_3_test() {
  assert path.starts_with_unix("one/two", "one/two/three")
}

pub fn starts_with_unix_4_test() {
  assert path.starts_with_unix("/one", "/one/two")
}

pub fn starts_with_unix_5_test() {
  assert path.starts_with_unix("/", "/one")
}

pub fn starts_with_unix_6_test() {
  assert path.starts_with_unix("/", "/")
}

pub fn starts_with_unix_7_test() {
  assert path.starts_with_unix("one", "one")
}

pub fn starts_with_unix_8_test() {
  assert path.starts_with_unix("one/two", "one/two")
}

pub fn starts_with_unix_9_test() {
  assert path.starts_with_unix("", "")
}

pub fn starts_with_unix_10_test() {
  assert path.starts_with_unix("", "one")
}

pub fn starts_with_unix_11_test() {
  assert path.starts_with_unix("", "one/two")
}

pub fn starts_with_unix_12_test() {
  assert !path.starts_with_unix("one", "onetwo")
}

pub fn starts_with_unix_13_test() {
  assert !path.starts_with_unix("one/tw", "one/two")
}

pub fn starts_with_unix_14_test() {
  assert !path.starts_with_unix("one.txt", "one.txt.bak")
}

pub fn starts_with_unix_15_test() {
  assert !path.starts_with_unix("one/two", "one")
}

pub fn starts_with_unix_16_test() {
  assert !path.starts_with_unix("/one/two", "/one")
}

pub fn starts_with_unix_17_test() {
  assert !path.starts_with_unix("one", "two/one")
}

pub fn starts_with_unix_18_test() {
  assert !path.starts_with_unix("one/two", "one/three")
}

pub fn starts_with_unix_19_test() {
  assert !path.starts_with_unix("/one", "one")
}

pub fn starts_with_unix_20_test() {
  assert !path.starts_with_unix("one", "/one")
}

pub fn starts_with_unix_21_test() {
  assert !path.starts_with_unix("", "/one")
}

pub fn starts_with_unix_22_test() {
  assert !path.starts_with_unix("/", "one")
}

pub fn starts_with_unix_23_test() {
  assert path.starts_with_unix("one/", "one/two")
}

pub fn starts_with_unix_24_test() {
  assert path.starts_with_unix("one", "one/two/")
}

pub fn starts_with_unix_25_test() {
  assert path.starts_with_unix("one//two", "one/two/three")
}

pub fn starts_with_unix_26_test() {
  assert path.starts_with_unix("//one", "/one/two")
}

pub fn starts_with_unix_27_test() {
  assert path.starts_with_unix("./one", "one/two")
}

pub fn starts_with_unix_28_test() {
  assert path.starts_with_unix("one", "./one/two")
}

pub fn starts_with_unix_29_test() {
  assert path.starts_with_unix("one/.", "one")
}

pub fn starts_with_unix_30_test() {
  assert path.starts_with_unix(".", "one")
}

pub fn starts_with_unix_31_test() {
  assert path.starts_with_unix("one/./two", "one/two/three")
}

pub fn starts_with_unix_32_test() {
  assert path.starts_with_unix("one", "one/..")
}

pub fn starts_with_unix_33_test() {
  assert path.starts_with_unix("..", "../one")
}

pub fn starts_with_unix_34_test() {
  assert !path.starts_with_unix("..", "one")
}

pub fn starts_with_unix_35_test() {
  assert !path.starts_with_unix("one/..", "one")
}

pub fn starts_with_unix_36_test() {
  assert path.starts_with_unix("one\\two", "one\\two/three")
}

pub fn starts_with_unix_37_test() {
  assert !path.starts_with_unix("one", "one\\two")
}

pub fn starts_with_windows_1_test() {
  assert path.starts_with_windows("one", "one\\two")
}

pub fn starts_with_windows_2_test() {
  assert path.starts_with_windows("one\\two", "one\\two\\three")
}

pub fn starts_with_windows_3_test() {
  assert path.starts_with_windows("\\one", "\\one\\two")
}

pub fn starts_with_windows_4_test() {
  assert path.starts_with_windows("\\", "\\one")
}

pub fn starts_with_windows_5_test() {
  assert path.starts_with_windows("C:\\one", "C:\\one\\two")
}

pub fn starts_with_windows_6_test() {
  assert path.starts_with_windows("C:\\", "C:\\one")
}

pub fn starts_with_windows_7_test() {
  assert path.starts_with_windows("C:one", "C:one\\two")
}

pub fn starts_with_windows_8_test() {
  assert path.starts_with_windows("C:", "C:one")
}

pub fn starts_with_windows_9_test() {
  assert path.starts_with_windows("one", "one")
}

pub fn starts_with_windows_10_test() {
  assert path.starts_with_windows("C:\\one", "C:\\one")
}

pub fn starts_with_windows_11_test() {
  assert path.starts_with_windows("C:\\", "C:\\")
}

pub fn starts_with_windows_12_test() {
  assert path.starts_with_windows("", "")
}

pub fn starts_with_windows_13_test() {
  assert path.starts_with_windows("\\\\server\\share", "\\\\server\\share")
}

pub fn starts_with_windows_14_test() {
  assert path.starts_with_windows("", "one")
}

pub fn starts_with_windows_15_test() {
  assert path.starts_with_windows("", "one\\two")
}

pub fn starts_with_windows_16_test() {
  assert !path.starts_with_windows("one", "onetwo")
}

pub fn starts_with_windows_17_test() {
  assert !path.starts_with_windows("one\\tw", "one\\two")
}

pub fn starts_with_windows_18_test() {
  assert !path.starts_with_windows("C:\\on", "C:\\one")
}

pub fn starts_with_windows_19_test() {
  assert !path.starts_with_windows("one\\two", "one")
}

pub fn starts_with_windows_20_test() {
  assert !path.starts_with_windows("C:\\one\\two", "C:\\one")
}

pub fn starts_with_windows_21_test() {
  assert !path.starts_with_windows("one", "two\\one")
}

pub fn starts_with_windows_22_test() {
  assert !path.starts_with_windows("C:\\one", "C:\\two")
}

pub fn starts_with_windows_23_test() {
  assert !path.starts_with_windows("\\one", "one")
}

pub fn starts_with_windows_24_test() {
  assert !path.starts_with_windows("one", "\\one")
}

pub fn starts_with_windows_25_test() {
  assert !path.starts_with_windows("", "\\one")
}

pub fn starts_with_windows_26_test() {
  assert !path.starts_with_windows("C:\\one", "one")
}

pub fn starts_with_windows_27_test() {
  assert !path.starts_with_windows("\\one", "C:\\one\\two")
}

pub fn starts_with_windows_28_test() {
  assert !path.starts_with_windows("", "C:one")
}

pub fn starts_with_windows_29_test() {
  assert !path.starts_with_windows("C:", "one")
}

pub fn starts_with_windows_30_test() {
  assert !path.starts_with_windows("C:one", "C:\\one\\two")
}

pub fn starts_with_windows_31_test() {
  assert !path.starts_with_windows("C:\\", "C:one")
}

pub fn starts_with_windows_32_test() {
  assert !path.starts_with_windows("C:\\one", "D:\\one\\two")
}

pub fn starts_with_windows_33_test() {
  assert !path.starts_with_windows("C:one", "D:one\\two")
}

pub fn starts_with_windows_34_test() {
  assert !path.starts_with_windows("\\\\a\\share", "\\\\b\\share\\one")
}

pub fn starts_with_windows_35_test() {
  assert !path.starts_with_windows("\\\\server\\one", "\\\\server\\two\\file")
}

pub fn starts_with_windows_36_test() {
  assert path.starts_with_windows("\\\\server\\share", "\\\\server\\share\\one")
}

pub fn starts_with_windows_37_test() {
  assert path.starts_with_windows(
    "\\\\server\\share\\one",
    "\\\\server\\share\\one\\two",
  )
}

pub fn starts_with_windows_38_test() {
  assert !path.starts_with_windows("\\\\server\\share", "\\one")
}

pub fn starts_with_windows_39_test() {
  assert !path.starts_with_windows("\\", "\\\\server\\share")
}

pub fn starts_with_windows_40_test() {
  assert path.starts_with_windows("one/two", "one\\two\\three")
}

pub fn starts_with_windows_41_test() {
  assert path.starts_with_windows("C:/one", "C:\\one\\two")
}

pub fn starts_with_windows_42_test() {
  assert path.starts_with_windows("//server/share", "\\\\server\\share\\one")
}

pub fn starts_with_windows_43_test() {
  assert path.starts_with_windows("one\\", "one\\two")
}

pub fn starts_with_windows_44_test() {
  assert path.starts_with_windows("one\\\\two", "one\\two\\three")
}

pub fn starts_with_windows_45_test() {
  assert path.starts_with_windows(".\\one", "one\\two")
}

pub fn starts_with_windows_46_test() {
  assert path.starts_with_windows("one", ".\\one\\two")
}

pub fn starts_with_windows_47_test() {
  assert path.starts_with_windows(".", "one")
}

pub fn starts_with_windows_48_test() {
  assert path.starts_with_windows("C:\\.\\one", "C:\\one\\two")
}

pub fn starts_with_windows_49_test() {
  assert path.starts_with_windows("C:.", "C:one")
}

pub fn starts_with_windows_50_test() {
  assert path.starts_with_windows("one", "one\\..")
}

pub fn starts_with_windows_51_test() {
  assert path.starts_with_windows("..", "..\\one")
}

pub fn starts_with_windows_52_test() {
  assert !path.starts_with_windows("..", "one")
}

pub fn starts_with_windows_53_test() {
  assert !path.starts_with_windows("one\\..", "one")
}

pub fn starts_with_windows_54_test() {
  assert path.starts_with_windows("one", "one\\D:two")
}

pub fn starts_with_windows_55_test() {
  assert path.starts_with_windows("1:one", "1:one\\two")
}

pub fn starts_with_windows_58_test() {
  assert !path.starts_with_windows("C:\\One", "C:\\one\\two")
}

pub fn starts_with_windows_59_test() {
  assert path.starts_with_windows("c:one", "C:one\\two")
}

pub fn starts_with_windows_60_test() {
  assert path.starts_with_windows("//SERVER/share", "\\\\server\\SHARE\\one")
}

pub fn starts_with_windows_61_test() {
  assert !path.starts_with_windows("1:One", "1:one\\two")
}
