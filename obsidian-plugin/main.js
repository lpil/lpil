const { Plugin } = require("obsidian");

class LpilPlugin extends Plugin {
  async onload() {
    this.registerEvent(
      this.app.metadataCache.on("changed", (file) => this.onChanged(file)),
    );
  }

  async onunload() {}

  async onChanged(file) {
    if (file.path.startsWith("exercise/log/sessions/"))
      return this.onSessionChanged(file);

    if (file.path.startsWith("exercise/log/circuits/"))
      return this.onCircuitChanged(file);
  }

  async onSessionChanged(file) {
    let start;
    let location;

    // Set default frontmatter
    await this.app.fileManager.processFrontMatter(file, (frontmatter) => {
      frontmatter.start ||= localTimestamp(new Date());
      start = new Date(frontmatter.start);
      location = frontmatter.location;
    });

    // Set default filename, so long as start and location have been set
    if (location) {
      const path = `exercise/log/sessions/${readableTimestamp(start)} ${location}.md`;
      if (file.path !== path) {
        await this.app.fileManager.renameFile(file, path);
      }
    }
  }

  async onCircuitChanged(file) {
    let start;
    let exercise;

    // Set default frontmatter
    await this.app.fileManager.processFrontMatter(file, (frontmatter) => {
      frontmatter.start ||= localTimestamp(new Date());
      start = new Date(frontmatter.start);
      exercise = frontmatter.exercise;
    });

    // Set default filename, so long as start and exercise have been set
    if (location) {
      const path = `exercise/log/circuits/${readableTimestamp(start)} ${exercise}.md`;
      if (file.path !== path) {
        await this.app.fileManager.renameFile(file, path);
      }
    }
  }
}

function localTimestamp(date) {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  const hour = String(date.getHours()).padStart(2, "0");
  const minute = String(date.getMinutes()).padStart(2, "0");
  const second = String(date.getSeconds()).padStart(2, "0");
  return `${year}-${month}-${day}T${hour}:${minute}:${second}`;
}

function readableTimestamp(date) {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  const hour = String(date.getHours()).padStart(2, "0");
  const minute = String(date.getMinutes()).padStart(2, "0");
  return `${year}-${month}-${day} ${hour}-${minute}`;
}

module.exports = LpilPlugin;
