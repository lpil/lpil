[@bs.val] external requireAsset : string => unit = "require";

let () = {
  requireAsset("phoenix_html");
  requireAsset("material-design-lite/material.min.js");
  requireAsset("./style/main.css");
};
/* ReactDOMRe.renderToElementWithId(<Page message="Hi from Reason!" />, "app"); */
