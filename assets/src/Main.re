[@bs.val] external requireAsset : string => unit = "require";

let () = {
  requireAsset("phoenix_html");
  requireAsset("./style/main.css");
};

/* ReactDOMRe.renderToElementWithId(<Page message="Hi from Reason!" />, "app"); */
