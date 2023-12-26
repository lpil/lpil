#!/usr/bin/env node

// Configuration. Edit as required.
const productCode = "AGILE-23-12-06";
const tarrifCode = "E-1R-AGILE-23-12-06-A";
const desiredHours = 4;
const dayStartHour = 16; // Data in Octopus is updated at 4pm-ish.

// Here we go!
const now = new Date();
const desiredMs = desiredHours * 60 * 60 * 1000;

class Period {
  constructor(data) {
    this.start = new Date(data.valid_from);
    this.end = new Date(data.valid_to);
    this.rate = data.value_inc_vat;
  }

  durationMs() {
    return this.end - this.start;
  }

  startOfDayPeriod() {
    const offset = dayStartHour * 60 * 60 * 1000;
    const offsetted = new Date(this.start.getTime() - offset);
    return new Date(
      offsetted.getFullYear(),
      offsetted.getMonth(),
      offsetted.getDate(),
      dayStartHour,
      0,
      0
    );
  }

  toJSON() {
    return {
      start: this.start,
      end: this.end,
      rate: this.rate,
    };
  }
}

class Day {
  constructor(date) {
    this.date = date;
    this.periods = [];
  }

  add(period) {
    this.periods.push(period);
  }

  cheapest(desiredMs) {
    // Sort by price ascending
    this.periods.sort((a, b) => a.rate - b.rate);

    // Find the cheapest periods until we have enough
    const cheapest = [];
    for (const period of this.periods) {
      if (totalDurationMs(cheapest) >= desiredMs) {
        break; // Found enough periods, we're done.
      }
      cheapest.push(period);
    }

    // Sort by start time
    cheapest.sort((a, b) => a.start - b.start);
    return cheapest;
  }
}

class Calendar {
  constructor() {
    this.days = {};
  }

  addPeriod(period) {
    this.getDay(period).add(period);
  }

  cheapest(desiredMs) {
    return Object.entries(this.days)
      .map(([date, day]) => [
        date,
        day.cheapest(desiredMs).map((p) => p.toJSON()),
      ])
      .sort((a, b) => a[0].start - b[0].start);
  }

  getDay(period) {
    const start = period.startOfDayPeriod();
    const s = start.toISOString();
    const day = this.days[s] || new Day(start);
    this.days[s] = day;
    return day;
  }
}

function totalDurationMs(periods) {
  return periods.reduce((acc, p) => acc + p.durationMs(), 0);
}

// Fetch current rates from the Octopus API

const url = `https://api.octopus.energy/v1/products/${productCode}/electricity-tariffs/${tarrifCode}/standard-unit-rates/`;
const response = await fetch(url);

if (!response.ok) {
  const body = await response.text();
  throw new Error(`${response.status} ${response.statusText} ${body}`);
}

const json = await response.json();

// Calculate the cheapest periods

const calendar = new Calendar();

for (const data of json.results) {
  calendar.addPeriod(new Period(data));
}
const cheapest = calendar.cheapest(desiredMs);

console.log(Object.fromEntries(cheapest));
