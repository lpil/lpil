import lustre
import lustre/attribute
import lustre/element/html

pub fn main() {
  let app =
    lustre.element(
      html.div([], [
        html.p([], [html.text("P100 CEEFAX 1 100       Fri 24 Sep 16:19")]),

        html.header([], [
          html.h1([], [
            html.text("    "),
            html.span([], [html.text("LPIL")]),
            html.text(" CEEFAX"),
          ]),
        ]),

        html.p([], [html.text("London Extra")]),
        html.h2([], [html.text("News of regional TV and local radio")]),

        html.hr([]),

        html.ol([], [
          html.li([], [
            html.text("Photo libary         "),
            html.a([attribute.href("https://immich.slowbro.lpil.uk")], [
              html.text("Immich"),
            ]),
          ]),
          html.li([], [
            html.text("Data replication     "),
            html.a([attribute.href("https://syncthing.slowbro.lpil.uk")], [
              html.text("Syncthing"),
            ]),
          ]),
          html.li([], [
            html.text("Home sensors         "),
            html.a([attribute.href("https://zigbee2mqtt.slowbro.lpil.uk")], [
              html.text("Zigbee2mqtt"),
            ]),
          ]),
          html.li([], [
            html.text("mpd music stream     "),
            html.a([attribute.href("https://mpd.slowbro.lpil.uk")], [
              html.text("url"),
            ]),
            html.text(" or "),
            html.a([attribute.href("/slowbro.m3u")], [html.text("m3u")]),
          ]),
        ]),

        html.hr([]),

        html.p([], [
          html.text(
            "Á Gráinne Mhaol ag teacht thar sáile,
Óglaigh armtha léi mar gharda,
Gaeil iad féin is ní Gaill ná Spáinnigh,
Is cuirfidh siad ruaig ar Ghallaibh.",
          ),
        ]),

        html.br([]),
        html.footer([], [
          html.p([], [html.text("Ceefax: the world at your fingertips")]),
        ]),
      ]),
    )
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
