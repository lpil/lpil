import { assertEquals } from "std/testing/asserts.ts";
import { handleRequest } from "src/web.ts";
import { newRequest, effects } from "./helpers.ts";

Deno.test("GET /", async () => {
  const request = newRequest("/");
  const response = await handleRequest(request, effects);
  assertEquals(response.status, 200);
});

Deno.test("GET /unknown", async () => {
  const request = newRequest("/unknown");
  const response = await handleRequest(request, effects);
  assertEquals(response.status, 404);
});
