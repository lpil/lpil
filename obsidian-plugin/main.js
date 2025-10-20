const { Plugin } = require("obsidian");

class LpilPlugin extends Plugin {
  async onload() {
    this.registerEvent(
      this.app.metadataCache.on("changed", (file) => this.onChanged(file)),
    );
  }

  async onunload() {}

  async onChanged(file) {
    await this.dataNote(file, "exercise/log/sessions/", (fm) => {
      fm.start ||= localTimestamp(new Date());
      const filename = `${readableTimestamp(fm.start)} ${fm.location}`;
      return { filename: fm.location && filename };
    });

    await this.dataNote(file, "exercise/log/circuits/", (fm) => {
      fm.time ||= localTimestamp(new Date());
      fm.sets ||= 1;
      const filename = `${readableTimestamp(fm.time)} ${fm.exercise}`;
      return { filename: fm.exercise && filename };
    });

    await this.dataNote(file, "exercise/log/bike rides/", (fm) => {
      fm.start ||= localTimestamp(new Date());
      const filename = `${readableTimestamp(fm.start)} ${fm.from} to ${fm.to}`;
      return { filename: fm.from && fm.to && filename };
    });

    await this.dataNote(file, "home/meter readings/", (fm) => {
      fm.date ||= readableDate(new Date());
      return { filename: `${readableDate(fm.date)}` };
    });

    await this.dataNote(file, "todo/tasks/", (fm) => {
      return {
        filename: fm.due && fm.title && `${readableDate(fm.due)} ${fm.title}`,
      };
    });
  }

  async dataNote(file, path, callback) {
    if (!path.endsWith("/")) {
      path = path + "/";
    }

    if (!file.path.startsWith(path)) {
      return;
    }

    let data;
    await this.app.fileManager.processFrontMatter(file, (frontmatter) => {
      data = callback(frontmatter) || {};
    });

    if (data.filename) {
      const sanitisedFilename = data.filename.replace(/[\/\\:]+/, "-");
      const newPath = path + sanitisedFilename + ".md";
      if (newPath !== file.path) {
        await this.app.fileManager.renameFile(file, newPath);
      }
    }
  }
}

function localTimestamp(date) {
  if (typeof date === "string") {
    date = new Date(date);
  }
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  const hour = String(date.getHours()).padStart(2, "0");
  const minute = String(date.getMinutes()).padStart(2, "0");
  const second = String(date.getSeconds()).padStart(2, "0");
  return `${year}-${month}-${day}T${hour}:${minute}:${second}`;
}

function readableTimestamp(date) {
  if (typeof date === "string") {
    date = new Date(date);
  }
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  const hour = String(date.getHours()).padStart(2, "0");
  const minute = String(date.getMinutes()).padStart(2, "0");
  return `${year}-${month}-${day} ${hour}-${minute}`;
}

function readableDate(date) {
  if (typeof date === "string") {
    date = new Date(date);
  }
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  return `${year}-${month}-${day}`;
}

module.exports = LpilPlugin;
