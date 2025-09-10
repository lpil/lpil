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

    if (file.path.startsWith("exercise/log/bike rides/"))
      return this.onBikeRideChanged(file);
  }

  async onSessionChanged(file) {
    let start;
    let location;

    // Set defaults frontmatter
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
    let time;
    let exercise;

    // Set defaults frontmatter
    await this.app.fileManager.processFrontMatter(file, (frontmatter) => {
      frontmatter.time ||= localTimestamp(new Date());
      frontmatter.sets ||= 3;
      time = new Date(frontmatter.time);
      exercise = frontmatter.exercise;
    });

    // Set default filename, so long as time and exercise have been set
    if (exercise) {
      const path = `exercise/log/circuits/${readableTimestamp(time)} ${exercise}.md`;
      if (file.path !== path) {
        await this.app.fileManager.renameFile(file, path);
      }
    }
  }

  async onBikeRideChanged(file) {
    let start;
    let from;
    let to;

    // Set defaults frontmatter
    await this.app.fileManager.processFrontMatter(file, (frontmatter) => {
      frontmatter.start ||= localTimestamp(new Date());
      start = new Date(frontmatter.start);
      from = frontmatter.from;
      to = frontmatter.to;
    });

    // Set default filename, so long as time and exercise have been set
    if (from && to) {
      const path = `exercise/log/bike rides/${readableTimestamp(start)} ${from} to ${to}.md`;
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
