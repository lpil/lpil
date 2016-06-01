---
title: Integrating webpack with Phoenix
tags:
  - Elixir
  - Phoenix
  - webpack
---


[Phoenix][phoenix] is an awesome web framework, and [webpack][webpack] is an
awesome frontend build tool. There's lots of posts online about why they're
awesome, so lets talk about how to mash them together instead.

[phoenix]: https://github.com/phoenixframework/phoenix
[webpack]: https://github.com/webpack/webpack

We'll start with a new Phoenix project with the latest version, which is v1.1
at the time of writing. Let's skip the automatic dependency installation when
prompted as we don't want the NPM deps. We do want the Elixir ones though.

```
mix archive.install https://github.com/phoenixframework/archives/raw/master/phoenix_new.ez
mix phoenix.new my_app
  ...
Fetch and install dependencies? [Yn]
n
  ...
cd my_app
mix deps.get
```

By default Phoenix comes with another front end build tool called
[Brunch][brunch]. It's great, but we want webpack, so open up the
`package.json` file and remove all the brunch deps so that the file looks like
this:

[brunch]: https://github.com/brunch/brunch

```json
{
  "dependencies": {
    "phoenix": "file:deps/phoenix",
    "phoenix_html": "file:deps/phoenix_html"
  }
}
```

Now let's add webpack and [Babel][babel] to our project, babel being a
compiler that will allow us to use all the shiny next-gen Javascript features
in our code.

[babel]: https://github.com/babel/babel

```
npm install --save babel babel-core babel-loader webpack
```

Now delete the `brunch-config.js` create a `webpack.config.babel.js` file its
place.

```javascript
"use strict"

const config = {
  entry: "./web/static/js/app.js",
  output: {
    path: "./priv/static",
    filename: "js/app.js",
  },

  module: {
    loaders: [
      {
        test: /\.js$/,
        loader: "babel-loader",
        query: { presets: ["es2015"], },
      },
    ],
  },
};

export default config;
```

Here we've set the entry point and output path and filename to be the same as
Phoenix uses by default, so we don't need to change our layout template for
Javascript.

And that's it. We've got Javascript compiling again. Run webpack with
`./node_modules/webpack/bin/webpack.js`, fire up the Phoenix
server (ignoring the brunch related error message), and check it out.


## SASS and CSS

Next up is getting getting the app stylesheets compiling again. We want to use
SASS, so let's rename `web/static/css/app.css` to `web/static/css/app.scss`.

In order for webpack to compile a Javascript file it needs to be added to the
dependency through an `import` statement. This is also the case for CSS and
other non-Javascript files, so we'll import the root CSS file from
`web/static/js/app.js`.

```javascript
// Require Javascript modules
import "phoenix_html";
// import socket from "./socket"

// Require stylesheets
import "../css/app.scss";
```

If we run webpack now we'll get a nasty error as it doesn't know how to handle
SCSS files, so let's teach it how to do that by adding an appropriate loader.

```
npm install --save node-sass sass-loader css-loader
```

```javascript
"use strict"

const config = {
  entry: "./web/static/js/app.js",
  output: {
    path: "./priv/static",
    filename: "js/app.js",
  },

  module: {
    loaders: [
      {
        test: /\.js$/,
        loader: "babel-loader",
        query: { presets: ["es2015"], },
      },
      // Run .scss files through the SASS and CSS loaders
      {
        test: /\.scss$/,
        loader: "css!sass",
      },
    ],
  },
};

export default config;
```

Run webpack again and now it fails trying to handle the dependencies of the
`app.scss` file, namely the fonts.

The first problem is that Phoenix doesn't actually come with the bootstrap
fonts, so either delete the compiled bootstrap CSS from `app.scss` if you
don't want to use bootstrap, or grab the fonts and put them in
`web/static/fonts`. I'll get the fonts this time, and I'll download them using
`curl`. You could manually grab them from the bootstrap website if you prefer.

```sh
mkdir web/static/fonts
cd web/static/fonts
curl -O 'https://raw.githubusercontent.com/twbs/bootstrap/master/fonts/glyphicons-halflings-regular.eot' \
     -O 'https://raw.githubusercontent.com/twbs/bootstrap/master/fonts/glyphicons-halflings-regular.svg' \
     -O 'https://raw.githubusercontent.com/twbs/bootstrap/master/fonts/glyphicons-halflings-regular.ttf' \
     -O 'https://raw.githubusercontent.com/twbs/bootstrap/master/fonts/glyphicons-halflings-regular.woff' \
     -O 'https://raw.githubusercontent.com/twbs/bootstrap/master/fonts/glyphicons-halflings-regular.woff2'
cd -
```

And now add a new loader to our webpack config so it knows how to handle
fonts.

```
npm install --save file-loader
```

```javascript
"use strict"

const config = {
  entry: "./web/static/js/app.js",
  output: {
    path: "./priv/static",
    filename: "js/app.js",
  },

  module: {
    loaders: [
      {
        test: /\.js$/,
        loader: "babel-loader",
        query: { presets: ["es2015"], },
      },
      {
        test: /\.scss$/,
        loader: "css!sass",
      },
      {
        test: /\.(ttf|eot|svg|woff2?)$/,
        loader : "file-loader?name=fonts/[name].[ext]",
      },
    ],
  },
};

export default config;
```

If we run webpack now it'll run successfully without any errors or warnings,
but out `priv/static/` directory will look like this.

```
priv/static/
├── fonts
│   ├── glyphicons-halflings-regular-448c34.woff2
│   ├── glyphicons-halflings-regular-898896.svg
│   ├── glyphicons-halflings-regular-e18bbf.ttf
│   ├── glyphicons-halflings-regular-f4769f.eot
│   └── glyphicons-halflings-regular-fa2772.woff
└── js
    └── app.js
```

We've got fonts and Javascript, but no CSS file. If we open up
`priv/static/js/app.js` we'll find that it contains our Javascript, but also
out compiled stylesheets as a Javascript string, and this string never makes
it into a `<style>` tag in the browser.

We have two options here. Either we have the Javascript inject the stylesheet
into the page using the [style-loader][style-loader], or we can do it the old
fashioned way and extract the stylesheets into a `.css` file. I'm going to do
the latter as it's a bit more complex.

[style-loader]: https://github.com/webpack/style-loader

To do this we'll need a webpack plugin called [extract text
plugin][extract-text-plugin].

[extract-text-plugin]: https://github.com/webpack/extract-text-webpack-plugin

```
npm install --save extract-text-webpack-plugin
```

```javascript
"use strict"

// Import the plugin
import ExtractText from "extract-text-webpack-plugin";

const config = {
  entry: "./web/static/js/app.js",
  output: {
    path: "./priv/static",
    filename: "js/app.js",
  },

  module: {
    loaders: [
      {
        test: /\.js$/,
        loader: "babel-loader",
        query: { presets: ["es2015"], },
      },
      {
        test: /\.scss$/,
        loader: ExtractText.extract("style", "css!sass"), // Extract CSS
      },
      {
        test: /\.(ttf|eot|svg|woff2?)$/,
        loader : "file-loader?name=fonts/[name].[ext]",
      },
    ],
  },

  // Set output location
  plugins: [
    new ExtractText("css/app.css", {
      allChunks: true
    })
  ],
};

export default config;
```

Now we have our CSS file in the correct location to be served by Phoenix, and
picked up in our templates.

This is all we have to do to the webpack configuration to get Phoenix's default
frontend assets, but if you use images in your CSS you'll probably want to add
a handler for image formats that uses the `file-loader` or
[`url-loader`][url-loader], similar to the fonts handler above.

[url-loader]: https://github.com/webpack/url-loader


## Template assets

Fonts, Javascript, and SCSS are not the only files we find in `web/static/` in
a new Phoenix project, we also find files that are used in the templates,
which live in `web/static/assets/`. Brunch kindly copied these across for us,
but webpack is only responsible for bundling modules together so it does not.

We could construct some trickery to get webpack to do this, but I think it
makes more sense to instead keep these static assets in `priv/static/`.

```
mv web/static/assets/* priv/static/
```

We'll also need to tweak our `.gitignore` file so that these are now included
in the repository.

```sh
/_build
/db
/deps
/*.ez
erl_crash.dump
/node_modules

# Ignore compiled frontend assets
/priv/static/js
/priv/static/css
/priv/static/fonts

# The config/prod.secret.exs file by default contains sensitive
# data and you should not commit it into version control.
#
# Alternatively, you may comment the line below and commit the
# secrets file as long as you replace its contents by environment
# variables.
/config/prod.secret.exs
```


## Phoenix watchers

By default Phoenix will run a Brunch watcher in a process for us so we don't
need to worry about starting it ourselves in another terminal. We can easily
set this up for webpack, or any other file watching program we want for that
matter. (I like to run an [automatic test runner][mix-test-watch] this way too).

[mix-test-watch]: https://github.com/lpil/mix-test.watch

Open up `config/dev.exs`, look for the `watchers:` config, and change it to
this.

```elixir
config :my_app, MyApp.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  code_reloader: true,
  check_origin: false,
  watchers: [
    # Run a webpack watcher to compile the frontend
    node: [
      "node_modules/webpack/bin/webpack.js",
      "--watch",
      "--progress",
      "--colors",
    ],
  ]
```

And we're done. This example project can be found [here][example]. Happy
hacking :)

[example]: https://github.com/lpil/phoenix-webpack-example
