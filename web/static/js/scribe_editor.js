import Scribe        from "scribe-editor";
import toolbarPlugin from "scribe-plugin-toolbar";

const editorNode  = document.querySelector("#js-scribe-editor");
const toolbarNode = document.querySelector("#js-scribe-toolbar");
const outputNode  = document.querySelector("#js-scribe-output");

if (editorNode && toolbarNode && outputNode) {
  const editor  = new Scribe(editorNode);
  const toolbar = toolbarPlugin(toolbarNode);

  editor.use(toolbar);
  editor.on("content-changed", () => {
    outputNode.value = editor.getHTML();
  });

  editor.setHTML(outputNode.value);
}
