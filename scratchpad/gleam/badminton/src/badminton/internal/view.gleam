import gleam/list
import gleam/result
import gleam/string
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html.{text}
import lustre/element/svg

pub type NavItem {
  NavItem(name: String, path: String, selected: Bool)
}

pub fn mobile_sidenav(
  pages pages: List(NavItem),
  resources resources: List(NavItem),
) -> Element(a) {
  // Off-canvas menu backdrop, show/hide based on off-canvas menu state.
  // Entering: "transition-opacity ease-linear duration-300"
  //   From: "opacity-0"
  //   To: "opacity-100"
  // Leaving: "transition-opacity ease-linear duration-300"
  //   From: "opacity-100"
  //   To: "opacity-0"
  let backdrop =
    html.div(
      [
        attribute("aria-hidden", "true"),
        attribute.class("fixed inset-0 bg-gray-900/80"),
      ],
      [],
    )

  // Close button, show/hide based on off-canvas menu state.
  // Entering: "ease-in-out duration-300"
  //   From: "opacity-0"
  //   To: "opacity-100"
  // Leaving: "ease-in-out duration-300"
  //   From: "opacity-100"
  //   To: "opacity-0"
  let close_button =
    html.div(
      [
        attribute.class(
          "absolute left-full top-0 flex w-16 justify-center pt-5",
        ),
      ],
      [
        html.button(
          [attribute.class("-m-2.5 p-2.5"), attribute.type_("button")],
          [
            html.span([attribute.class("sr-only")], [text("Close sidebar")]),
            svg.svg(
              [
                attribute("aria-hidden", "true"),
                attribute("stroke", "currentColor"),
                attribute("stroke-width", "1.5"),
                attribute("viewBox", "0 0 24 24"),
                attribute("fill", "none"),
                attribute.class("h-6 w-6 text-white"),
              ],
              [
                svg.path([
                  attribute("d", "M6 18L18 6M6 6l12 12"),
                  attribute("stroke-linejoin", "round"),
                  attribute("stroke-linecap", "round"),
                ]),
              ],
            ),
          ],
        ),
      ],
    )

  // Off-canvas menu, show/hide based on off-canvas menu state.
  // Entering: "transition ease-in-out duration-300 transform"
  //   From: "-translate-x-full"
  //   To: "translate-x-0"
  // Leaving: "transition ease-in-out duration-300 transform"
  //   From: "translate-x-0"
  //   To: "-translate-x-full"
  let mobile_nav_menu =
    html.div([attribute.class("relative mr-16 flex w-full max-w-xs flex-1")], [
      close_button,
      html.div(
        [
          attribute.class(
            "flex grow flex-col gap-y-5 overflow-y-auto bg-white px-6",
          ),
        ],
        navbar_content(pages: pages, resources: resources),
      ),
    ])

  html.div(
    [
      attribute("aria-modal", "true"),
      attribute.role("dialog"),
      attribute.class("relative z-50 lg:hidden"),
    ],
    [
      backdrop,
      html.div([attribute.class("fixed inset-0 flex")], [mobile_nav_menu]),
    ],
  )
}

fn navbar_content(
  pages pages: List(NavItem),
  resources resources: List(NavItem),
) -> List(Element(a)) {
  [
    nav_logo(),
    html.nav([attribute.class("flex flex-1 flex-col")], [
      html.ul(
        [
          attribute.class("flex flex-1 flex-col gap-y-7"),
          attribute.role("list"),
        ],
        [
          html.li([], [pages_list(pages)]),
          html.li([], [resources_list(resources)]),
          html.li([attribute.class("-mx-6 mt-auto")], [nav_bottom()]),
        ],
      ),
    ]),
  ]
}

fn resources_list(resources: List(NavItem)) -> Element(a) {
  element.fragment([
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
              attribute.class(nav_li_style(item.selected)),
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
  ])
}

fn nav_li_style(selected: Bool) -> String {
  case selected {
    True ->
      "group flex gap-x-3 rounded-md bg-gray-50 p-2 text-sm font-semibold leading-6 text-indigo-600"
    False ->
      "group flex gap-x-3 rounded-md p-2 text-sm font-semibold leading-6 text-gray-700 hover:bg-gray-50 hover:text-indigo-600"
  }
}

fn nav_bottom() -> Element(a) {
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
}

pub fn desktop_sidenav(
  pages pages: List(NavItem),
  resources resources: List(NavItem),
) -> Element(a) {
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
        navbar_content(pages: pages, resources: resources),
      ),
    ],
  )
}

fn nav_logo() -> Element(a) {
  html.div([attribute.class("flex h-16 shrink-0 items-center")], [
    html.img([
      attribute.alt("Your Company"),
      attribute.src(
        "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600",
      ),
      attribute.class("h-8 w-auto"),
    ]),
  ])
}

fn pages_list(pages: List(NavItem)) -> Element(a) {
  html.ul(
    [attribute.class("-mx-2 space-y-1"), attribute.role("list")],
    list.map(pages, fn(item) {
      let classes = nav_li_style(item.selected)
      html.li([], [
        html.a([attribute.class(classes), attribute.href(item.path)], [
          text(item.name),
        ]),
      ])
    }),
  )
}
