---
title: Using SASS, ES6 Javascript, and Node modules with Brunch
hidden: true
date: 2015-08-31 21:37 UTC
tags:
  - Frontend
  - SASS
  - Javascript
  - Brunch
---

[Brunch][brunch] has recently become my favourite front end build tool, to the
point where I don't want to use Grunt or the Rails Asset Pipeline ever again.
Why is this? Two reasons...

**It's fast** Really fast. It doesn't use any temporary intermediary files,
it's builds are incremental, and it makes clever use of caching to avoid doing
work that it doesn't have to. As a result it's *much* faster than Grunt and
Gulp.

**It's simple.** Remember those horrible several hundred line Grunt configs
manipulating a `tmp/` directory? Or those Gulp configs full of pipeline logic?
Well a Brunch config that does the same probably looks something like this:

```javascript
exports.config = {
  files: {
    javascripts: { joinTo: 'main.js'  },
    templates:   { joinTo: 'main.js'  },
    stylesheets: { joinTo: 'main.css' },
  },
};
```

Yes, really.

This is because it's a specialised build tool, not a task runner like Grunt or
Rake. It has one specific set of tasks, and it can use conventions to make
this cleaner and easier.

Additionally, if I want to use a plugin that adds additional functionality
(for example, CSS minification), it's just a matter of installing the plugin
module with `npm`, and Brunch will pick it up and plug it in for us.


## The Brunch problem

It's not all perfect though. Because all the logic happens behind the scenes,
it's possible to get into a situation where something does not work, and it's
difficult to say what you've done wrong. This can become even more frustrating
when coupled with the fact that the documentation on the various conventions
and plugins often leaves a lot to be desired.

Because of this, I thought I should write a little something about setting
Brunch up to do what I generally want from my front end build tool- [ES6
Javascript][es6] using the [Babel][babel] transpiler, [SCSS][sass] instead of
CSS, [jshint][jshint] linting my Javascript, minification for both CSS and
Javascript, and the ability to import Javascript libraries from `npm`.


## The conventions

By convention Brunch will look for source files in `app/`, and output
generated files to `public/`, so put your Javascript files in `app/`. A small
project of mine look something like this:

```
app/
├── assets
│   └── index.html
├── controllers
│   └── users.js
├── main.js
├── models
│   └── user.js
├── stylesheets
│   ├── components
│   │   ├── _button.scss
│   │   └── _search.scss
│   ├── layouts
│   │   ├── _footer.scss
│   │   └── _header.scss
│   └── main.scss
└── views
    └── users.jade
```

And after Brunch has done its thing the output directory looks like this:

```
public/
├── index.html
├── main.css
└── main.js
```

Things to note:

* Files in the `app/assets/` directory are copied directly into the `public/`
  directory.
* All our Javascript has been compiled into one file, one that we have
  specified the name and location of.
* All of our SCSS has been compiled into one file, one we have also specified
  the name and location of.
* Our SCSS, Javascript, and template files (`.jade` in this case) all live
  within `app/`.

## ES6 setup

First, lets get ES6 Javascript compilation working, complete with ES6 modules,
etc.

Install `brunch`, the mandatory `javascript-brunch` plugin that handles
Javascript compilation, and the `babel-brunch` plugin, which gives us ES6
Javascript support. We'll want `brunch` installed globally so we'll have the
`brunch` command in our `$PATH`.

```
npm install -g brunch
npm install --save brunch javascript-brunch babel-brunch
```

Great! Now let's make our Brunch config file, `brunch-config.js`

```javascript
// brunch-config.js
exports.config = {
  files: {
    javascripts: { joinTo: 'main.js' },
    templates:   { joinTo: 'main.js' },
  },
};
```

We're not actually setting up any template systems, such as the excellent
[jade-brunch][jade-brunch], so we could drop the `templates` line, but
there's no harm in keeping it in the config.

And that's it. Run `brunch watch` to start Brunch, and create some test ES6
Javascript files.

```javascript
// app/main.js
import test from 'test';
test();

// app/test.js
export default function test() {
  console.log('Hello, world!');
}
```

Boom. We have a generated file at `public/main.js` that's ready to be loaded
into the browser. Once you've done that, exec `require('main');`, and your
Javascript application will run.

That's it. We've set up ES6 Javascript with Brunch.


## SCSS setup

Onto the equally difficult process of adding SCSS compilation.

```
npm install --save css-brunch sass-brunch
```

Add a stylesheets line to your brunch config:

```javascript
// brunch-config.js
exports.config = {
  files: {
    javascripts: { joinTo: 'main.js'  },
    templates:   { joinTo: 'main.js'  },
    stylesheets: { joinTo: 'main.css' },
  },
};
```

And then create some SCSS files under `app/`, and use the usual SCSS import
statements. I like to keep them in `app/stylesheets/`.

Done that? Then you should have a compiled CSS file at `public/main.css`.
Super easy.


## Using libraries from npm

There's lots of handy libraries on npm that we might want to use in our
Javascript app. [lodash][lodash], for example. Install it, as per usual:

```
npm install --save lodash
```

Enable npm support in your Brunch config:

```javascript
// brunch-config.js
exports.config = {
  files: {
    javascripts: { joinTo: 'main.js'  },
    templates:   { joinTo: 'main.js'  },
    stylesheets: { joinTo: 'main.css' },

    npm: { enabled: true },
  },
};
```

And then import it in your Javascript like a regular module.

```javascript
// app/main.js
import _    from 'lodash';
import test from 'test';
test();
```

Boom. It works.


## Javascript linting with jshint

Add your config as a `.jshintrc` file in the project directory, and then
install the plugin.

```
npm install --save jshint-brunch
```

## Minification

Lastly, CSS and Javascript minification.

```
npm install --save uglify-js-brunch clean-css-brunch
```

Run `brunch build --production` to see it in action.

So that's it. We've set up everything with Brunch. Told you it was easy. :)


[babel]: https://babeljs.io/
[brunch]: http://brunch.io/
[es6]: https://babeljs.io/docs/learn-es2015/
[jade-brunch]: https://github.com/brunch/jade-brunch
[jshint]: http://jshint.com/
[libsass]: https://github.com/sass/libsass
[lodash]: https://lodash.com
[sass]: http://sass-lang.com/
