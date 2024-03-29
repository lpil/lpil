#!/usr/bin/env deno run --allow-read=/usr/share/dict/words,. --allow-write=./ --allow-net=api.gandi.net

const token = await Deno.readTextFile("gandi_api_key.txt");

const words = Deno.readTextFileSync("common_words.txt").split("\n");

let latest = "";
try {
  latest = Deno.readTextFileSync("latest.txt").trim();
} catch (_) {
  // ignore
}

async function query(word: string) {
  const domain = `${word}.pink`;
  const url = `https://api.gandi.net/v5/domain/check?name=${domain}&extension=.pink`;
  const res = await fetch(url, {
    headers: {
      Authorization: `Apikey ${token.trim()}`,
    },
  });

  if (res.status !== 200) {
    const body = res.text();
    throw new Error(`Unexpected status code: ${res.status} ${body}`);
  }

  const data = await res.json();
  const status = data.products[0].status;
  const price = data.products[0].prices?.[0].price_after_taxes || "";
  console.log([domain, status, price].join("\t"));

  if (status === "available") {
    Deno.writeTextFileSync("available.tsv", `${domain}\t${price}\n`, {
      append: true,
    });
  }

  Deno.writeTextFileSync("latest.txt", word);
  await sleep(0.2);
}

for (const word of words) {
  if (word <= latest) continue;

  while (true) {
    try {
      await query(word);
      break;
    } catch (e) {
      console.error(e);
      await sleep(10);
    }
  }
}

const available = Deno.readTextFileSync("available.tsv")
  .split("\n")
  .filter((x) => x)
  .sort((a, b) => a.indexOf("\t") - b.indexOf("\t"));
Deno.writeTextFileSync("available_sorted.tsv", available.join("\n") + "\n");

function sleep(seconds: number) {
  return new Promise((resolve) => setTimeout(resolve, seconds * 1000));
}
