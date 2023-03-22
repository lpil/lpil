import { serve } from "std/http/server.ts";
import { handleRequest } from "src/web.ts";

export async function main() {
  console.log(`HTTP webserver running on http://localhost:${port}/`);
  await serve(handleRequest, { port });
}

const port = 8000;
