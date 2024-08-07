import gleam/list
import gleam/result
import gleam/string
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html.{text}

pub type NavItem {
  NavItem(name: String, path: String, selected: Bool)
}

pub fn desktop_sidenav(
  pages pages: List(NavItem),
  resources resources: List(NavItem),
) -> Element(a) {
  let li_style = fn(selected) {
    case selected {
      True ->
        "group flex gap-x-3 rounded-md bg-gray-50 p-2 text-sm font-semibold leading-6 text-indigo-600"
      False ->
        "group flex gap-x-3 rounded-md p-2 text-sm font-semibold leading-6 text-gray-700 hover:bg-gray-50 hover:text-indigo-600"
    }
  }

  let page_list =
    html.ul(
      [attribute.class("-mx-2 space-y-1"), attribute.role("list")],
      list.map(pages, fn(item) {
        let classes = li_style(item.selected)
        html.li([], [
          html.a([attribute.class(classes), attribute.href(item.path)], [
            text(item.name),
          ]),
        ])
      }),
    )

  let resources_list = [
    html.div(
      [attribute.class("text-xs font-semibold leading-6 text-gray-400")],
      [text("Resources")],
    ),
    html.ul(
      [attribute.class("-mx-2 mt-2 space-y-1"), attribute.role("list")],
      list.map(resources, fn(item) {
        let letter =
          item.name |> string.first |> result.unwrap("") |> string.uppercase
        let button_style =
          "flex h-6 w-6 shrink-0 items-center justify-center rounded-lg border border-gray-200 bg-white text-[0.625rem] font-medium text-gray-400 group-hover:border-indigo-600 group-hover:text-indigo-600"
        html.li([], [
          html.a(
            [
              attribute.class(li_style(item.selected)),
              attribute.href(item.path),
            ],
            [
              html.span([attribute.class(button_style)], [text(letter)]),
              html.span([attribute.class("truncate")], [text(item.name)]),
            ],
          ),
        ])
      }),
    ),
  ]

  let bottom_section =
    html.a(
      [
        attribute.class(
          "flex items-center gap-x-4 px-6 py-3 text-sm font-semibold leading-6 text-gray-900 hover:bg-gray-50",
        ),
        attribute.href("#"),
      ],
      [
        html.img([
          attribute.alt(""),
          attribute.src(
            "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80",
          ),
          attribute.class("h-8 w-8 rounded-full bg-gray-50"),
        ]),
        html.span([attribute.class("sr-only")], [text("Your profile")]),
        html.span([attribute("aria-hidden", "true")], [text("Tom Cook")]),
      ],
    )

  html.div(
    [
      attribute.class(
        "hidden lg:fixed lg:inset-y-0 lg:z-50 lg:flex lg:w-72 lg:flex-col",
      ),
    ],
    [
      html.div(
        [
          attribute.class(
            "flex grow flex-col gap-y-5 overflow-y-auto border-r border-gray-200 bg-white px-6",
          ),
        ],
        [
          html.div([attribute.class("flex h-16 shrink-0 items-center")], [
            html.img([
              attribute.alt("Your Company"),
              attribute.src(
                "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600",
              ),
              attribute.class("h-8 w-auto"),
            ]),
          ]),
          html.nav([attribute.class("flex flex-1 flex-col")], [
            html.ul(
              [
                attribute.class("flex flex-1 flex-col gap-y-7"),
                attribute.role("list"),
              ],
              [
                html.li([], [page_list]),
                html.li([], resources_list),
                html.li([attribute.class("-mx-6 mt-auto")], [bottom_section]),
              ],
            ),
          ]),
        ],
      ),
    ],
  )
}
