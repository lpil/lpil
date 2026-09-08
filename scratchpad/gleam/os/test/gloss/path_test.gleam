import gleam/list
import gloss/path

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

pub fn join_windows_kind_is_always_preserved_test() {
  let paths = [
    "", "C:", "C:\\one", "C:\\two", "C:one", "C:two", "\\", "\\\\one\\two",
    "\\one", "\\two", "one", "two",
  ]

  use left <- list.each(paths)
  use right <- list.each(paths)
  assert path.kind_windows(path.join_windows(left, right))
    == path.kind_windows(left)
    as { "joining " <> left <> " to " <> right <> " should not change kind" }
}
