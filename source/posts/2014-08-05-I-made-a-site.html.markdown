---
title:  "I made a site"
categories:
  - lpil.uk
  - Web dev
---

**Update:** This post is no longer an accurate description of this site. Since
making it I've migrated to the [middleman](middleman) static site compiler,
switched from coffeescript to vanilla Javascript, and completely dropped any
CSS framework.

[middlema]: https://github.com/middleman/middleman

Why? I found that middleman is much more powerful and flexible than Jekyll, I
didn't feel that coffeescript provides any advantage over Javascript
(especially ES6 Javascript), and I felt that my site was too simple to really
warrant a CSS framework.

But still, this is how I did it first time round- enjoy.

---

It struck me the other day that a web developer should probably have a personal
website. I feel it doesn't inspire much confidence if a creator doesn't use
what they produce. Plus, if I made a site, I'd get to play with some new toys.
Awesome.

## Toys used

At the heart of the site is [Jekyll][jekyll], a static site generator from the
Github team. Static sites are great! They're *really fast*, there's no database
to fiddle about with, and deploying the site is as easy as `git pull; jekyll
build`. Jekyll also has support for [SCSS][scss] and
[Coffeescript][coffeescript], compiling them to CSS and Javascript when it
compiles the site. What's not to like? 

The guys at [thoughtbot][thoughtbot] have made a ton of awesome open source web
dev toys, and this time round I've been using [Bourbon][bourbon], a SCSS mixin
library, and [Neat][neat], a lightweight SCSS grid framework. This is the first
time I've used them, but so far I'm a fan. Rather than spraying additional
classes all over your HTML, you specify your columns using SCSS mixins. This is
pretty cool, because rather than having this...

```scss
<section>
  <aside class="col-xs-6" >First column</aside>
  <article class="col-xs-6" >Second Column</article>
</section>
```
```scss
section { }
aside   { }
article { }
```

We get this...

```html
<section>
  <aside>First column</aside>
  <article>Second Column</article>
</section>
```
```scss
section { @include outer-container; }
aside   { @include span-columns(6); }
article { @include span-columns(6); }
```

Yup- now we get to keep our styling in the stylesheets!

Next I'm using [Guard::LiveReload][guard-livereload] to refresh my web browser
when I save a file in development- no more manually refreshing the page to see
my changes! I run `jekyll serve --watch` to start a webserver that'll serve the
compiled site, and recompile said site each time I make a change. I then start
Guard with Guard::LiveReload, which monitors my file system, and uses
Javascript voodoo to refresh my browser each time Jekyll recompiles the site. 

On the LiveReload website they recommend various web browser plugins, but I
didn't have much luck with them. Instead, I opted to just have Jekyll [include
the livereload.js][livereload-include] when in development. Much simpler! If
you're working on a Rack application (such as a Rails app), check out the
[Rack::LiveReload][rack-livereload] Rack middleware.

I'm also using [Font Awesome][font-awesome] for pretty icons, and
[Gravatar][gravatar] for my mugshot on the
['About' page]({{ '/about' | prepend: site.url }}).

And that's about it. If you're interested in having a look at how it all fits
together, the source for this site can be found in this
[Github repo][site-repo]. (Drop me a pull request when you find a typo.)

Cheers,  
Louis

[jekyll]: http://jekyllrb.com
[scss]: http://sass-lang.com/
[coffeescript]: http://coffeescript.org/
[thoughtbot]: http://thoughtbot.com/
[bourbon]: http://bourbon.io/
[neat]: http://neat.bourbon.io/
[guard-livereload]: https://github.com/guard/guard-livereload
[livereload-include]: https://github.com/lpil/lpil.uk/blob/7e0026d2f42c89dae369bb94bb48f1cfb31da3ce/_includes/head.html#L2
[rack-livereload]: https://github.com/johnbintz/rack-livereload
[font-awesome]: http://fortawesome.github.io/Font-Awesome/
[gravatar]: https://en.gravatar.com/
[site-repo]: https://github.com/lpil/lpil.uk
