#!/usr/bin/osascript
// This JS dialect is called JXA (JavaScript for Automation)

const exportPath = "/Users/louis/Sync/data/notes";

const app = Application.currentApplication();
app.includeStandardAdditions = true;

const notesApp = Application("Notes");
notesApp.includeStandardAdditions = true;
const notes = notesApp.notes;
const firstNote = notes[0];

const count = notes.length;
const countWidth = count.toString().length;

for (let i = 0; i < notes.length; i++) {
  const note = notes[i];

  const folder = note.container().name();
  const title = note.name();
  const body = note.body();

  const padded = ("000000" + (i + 1)).slice(countWidth * -1);
  console.log(`[${padded}/${count}] ${folder}\t${title}`);

  const folderPath = exportPath + "/" + folder;
  app.doShellScript("mkdir -p '" + folderPath + "'");

  const path = folderPath + "/" + title + ".html";
  const pathObject = Path(path);
  const file = app.openForAccess(pathObject, { writePermission: true });

  app.setEof(file, { to: 0 });
  app.write(body, { to: file });
  app.closeAccess(file);
}
