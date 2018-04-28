[@bs.val] external requireAsset : string => unit = "require";

let () = {
  requireAsset("phoenix_html");
  requireAsset("./main.css");
  requireAsset("material-design-lite/material.min.js");
};
/* ReactDOMRe.renderToElementWithId(<Page message="Hi from Reason!" />, "app"); */
