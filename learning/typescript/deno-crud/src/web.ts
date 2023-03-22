const routes: Array<[URLPattern, Handler]> = Object.entries({
  "/": home,
  "/greet/:name": greet,
}).map(([pathname, f]) => [new URLPattern({ pathname }), f]);

export type Context = {
  request: Request;
  params: Params;
};

export type Params = { [key: string]: string };

export type Handler = (context: Context) => Promise<Response> | Response;

export async function handleRequest(request: Request): Promise<Response> {
  const context = { request, params: {} };

  for (const [urlPattern, handler] of routes) {
    const params = urlPattern.exec(request.url)?.pathname.groups;
    if (params) {
      context.params = params;
      return await handler(context);
    }
  }

  return not_found(context);
}

function home(_context: Context): Response {
  return new Response("Welcome home.", { status: 200 });
}
function greet(context: Context): Response {
  return new Response(`Hello, ${context.params.name}!`, { status: 404 });
}

export function not_found(_context: Context): Response {
  return new Response("There's nothing here...", { status: 404 });
}
