---
title: Testing CSS Computed Styles with Capybara
date: 2015-05-07 12:58 UTC
---

This week I found myself with the rather strange task of testing that styles
had been correctly applied to the DOM. There was a good reason for it, honest.

Sadly with otherwise excellent [Capybara][capybara],
[Poltergeist][poltergeist], and [PhantomJS][phantomjs] computed styles are not
the most readily avalible information, so I wound up using a little Javascript
to get what we need.

```javascript
var el     = document.querySelector('.my-ele'),
    styles = window.getComputedStyle(el);
styles.color;
// => "rgb(243, 75, 125)"
```

As you can see, with `window.getcomputedstyle` we can (gasp) get the computed
styles for a given element. Property names on the object are the same as in
CSS, though `camelCase` rather than `lisp-case`.

In Capybara tests we can run arbitrary Javascript with a Javascript capable
driver (such as PhantomJS) and the method `page.evaluate_script`. My tests
wound up looking something like this.

```ruby
def computed_style(selector, prop)
  page.evaluate_script(
    "window.getComputedStyle(document.querySelector('#{selector}')).#{prop}"
  )
end

describe 'Users page' do
  it 'should have the correct title colour' do
    style = computed_style '.title', 'color'
    expect(style).to eq 'rgb(243, 75, 125)'
  end
end
```

Yup. Weird, isn't it?

[capybara]: https://github.com/jnicklas/capybara
[poltergeist]: https://github.com/teampoltergeist/poltergeist
[phantomjs]: https://github.com/ariya/phantomjs
