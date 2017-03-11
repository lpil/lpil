# Training

Tracking conferences and other training fun :)

## About

A single page app written in Elm and a little Javascript. The app uses the Elm
architecture and largely uses Kris Jenkin's Elm app [pattern][pattern].

[Auth0][auth0] provides user authentication service.

[GraphCool][graphcool] provides a GraphQL based backend service.

[pattern]: http://blog.jenkster.com/2016/04/how-i-structure-elm-apps.html
[auth0]: https://auth0.com/
[graphcool]: https://www.graph.cool


## Usage

```sh
# Install deps
npm install --global yarn
make install

# Run the dev server
make start

# Run the tests
make test
make test-watch
```
