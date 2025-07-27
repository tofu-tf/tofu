import apiHtml from "../api/index.html";

export async function GET({ params, request }) {
  const response = apiHtml({ slots: { default: "<p>Hello</p>" } });

  return new Response(response, {
    headers: { "Content-Type": "text/html" },
  });
}
