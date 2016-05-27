import Markdown
import Html
import List

main =
  markdown
  |> Markdown.toHtml []
  |> List.repeat 100
  |> List.intersperse divider
  |> Html.div []

divider =
  Html.hr [] []

markdown = """
# Markdown!

Markdown is a ***super easy markup language***.

* Easy for humans to read
* Easy for humans to write
* Easy for robots to read too!

Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod
tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At
vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd
gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
"""
