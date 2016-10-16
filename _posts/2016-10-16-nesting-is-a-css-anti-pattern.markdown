---
title: Nesting is a CSS Anti-Pattern
tags:
  - CSS
  - Code quality
---

The CSS preprocessors such as SCSS, LESS and cssnext make it easy and natural
to create complex selectors through nesting. After working with this style for
a few years I've come to think that it in fact an anti-pattern as it makes the
CSS harder to work with over time.

Here's some example SCSS from an open source project.

```scss
.results {
  .display {
    .gem-element {
      // styles...

      .gem-name {
        h2, span {
          // styles...
        }
      }

      .gem-main {
        .gem-tags {
          h4 {
            // styles...
          }

          a {
            // styles...
          }
        }
      }
      .gem-numbers {
        .gem-statistic {
          // styles...
        }
      }
    }
  }
}
```

It works, and the page looks good, but I think it can be altered to improve
maintainability.

The nesting of the styles directly mimics the nesting of the HTML elements,
as a result the styles are tightly coupled to the HTML and are more brittle.

If the `.gem-element` HTML is reused elsewhere on the site, outside of the
`.display` element, it won't inherit any of the styles as we've stated in CSS
that it has to be inside the `.display` element.

Is this right? Should a `.gem-element` look the same if used elsewhere? I
would argue that it should, and if we want it to look different it is not in
fact a `.gem-element`. This means the CSS is more specific than it needs to
be. Rather than a selector specifying the `.gem-element`s inside `.display`
inside `.results` we actually just want a selector for `.gem-elements`.

If we rewrite this SCSS as regular CSS it becomes clearer how specific we're
being.

```scss
.results .display .gem-element {
  // styles
}

.results .display .gem-element .gem-name h2,
.results .display .gem-element .gem-name span {
  // styles
}

.results .display .gem-element .gem-main .gem-tags h4 {
  // styles
}

.results .display .gem-element .gem-main .gem-tags a {
  // styles
}

.results .display .gem-element .gem-numbers .gem-statistic {
  // styles
}
```

How about we remove the qualifying classes, then it would look like this.

```scss
.gem-element {
  // styles
}

.gem-name h2,
.gem-name span {
  // styles
}

.gem-tags h4 {
  // styles
}

.gem-tags a {
  // styles
}

.gem-statistic {
  // styles
}
```

Now our selectors are easier to read, and our selectors are more general and
less coupled to the HTML. What's more, the
[specificity](https://developer.mozilla.org/en/docs/Web/CSS/Specificity) of
the selectors has been greatly reduced, so we're less likely to have problems
overriding styles later.

My preference is to go even further and to never use more than a single class
in a selector, which results in styles like this:


```scss
.gem-element {
  // styles
}

.gem-name-title {
  // styles
}

.gem-tags-title {
  // styles
}

.gem-tag-link {
  // styles
}

.gem-statistic {
  // styles
}
```

This often requires a few more classes in the HTML, but in my experience it
really pays off in the long run as your CSS is more flexible and easier to
maintain.
