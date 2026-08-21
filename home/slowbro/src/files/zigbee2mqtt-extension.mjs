import { appendFileSync, mkdirSync, existsSync } from 'node:fs';
import { join as joinPath } from 'node:path';

const recordDirectory = "/record"

export default class LpilEventRecordingExtension {
  constructor(
    zigbee,
    mqtt,
    state,
    publishEntityState,
    eventBus,
    enableDisableExtension,
    restartCallback,
    addExtension,
    settings,
    logger,
  ) {
    this.zigbee = zigbee;
    this.mqtt = mqtt;
    this.state = state;
    this.publishEntityState = publishEntityState;
    this.eventBus = eventBus;
    this.enableDisableExtension = enableDisableExtension;
    this.restartCallback = restartCallback;
    this.addExtension = addExtension;
    this.settings = settings;
    this.logger = logger;
  }

  async start() {
    ensureDirectory(recordDirectory);
    this.eventBus.onMQTTMessagePublished(this, data => {
      this.onMQTTMessagePublished(data)
    });
  }

  async stop() {
    this.eventBus.removeListeners(this);
  }

  onMQTTMessagePublished(data) {
    // Do not record messages from zigbee2mqtt itself
    if (data.topic.startsWith("zigbee2mqtt/bridge/")) return;

    // Remove unwanted fields
    const fullPayload = JSON.parse(data.payload);
    delete fullPayload.elapsed;
    delete fullPayload.last_seen;

    // Do not record empty payloads
    if (!fullPayload) {
      return;
    }

    const topic = data.topic
      // Remove prefix
      .replace(/^zigbee2mqtt\//, "")
      // Remove tabs that would break the TSV format
      .replaceAll("\t", " ");

    const now = new Date();
    const [year, month, day] = new Date()
      .toISOString()
      .slice(0, "2026-08-12".length)
      .split("-");
    const yearDirectory = joinPath(recordDirectory, year);
    ensureDirectory(yearDirectory);
    const monthDirectory = joinPath(yearDirectory, month);
    ensureDirectory(monthDirectory);

    const unixMs = now.getTime();
    const payload = JSON.stringify(fullPayload);
    const line = `${unixMs}\t${topic}\t${payload}\n`;
    const path = joinPath(monthDirectory, day + ".tsv");

    appendFileSync(path, line);
  }
}

function ensureDirectory(path) {
  if (!existsSync(path)) {
    mkdirSync(path);
  }
}
