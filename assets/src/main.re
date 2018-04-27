[@bs.val] external requireAsset : string => unit = "require";

let () = {
  requireAsset("phoenix_html");
  /* We could possibly only import the CSS that we're interested in, using SCSS */
  requireAsset("bootstrap/dist/css/bootstrap.min.css");
  requireAsset("bootstrap-material-design/dist/css/bootstrap-material-design.min.css");
  requireAsset("./main.css");
};

/* ReactDOMRe.renderToElementWithId(<Page message="Hi from Reason!" />, "app"); */
