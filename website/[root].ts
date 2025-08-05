import fs from 'fs';
import path from 'path';

export const prerender = false;

export async function GET({ params, request }) {
  
  const elem = params.root;
  const filePath = path.resolve('./src/api/' + elem)

  let contentType = ""
  if (elem.endsWith("html")) {
    contentType = "text/html"
  } else if (elem.endsWith("js")) {
    contentType =  "text/javascript"
  }

  const content = fs.readFileSync(filePath, 'utf-8');
  return new Response(content, {
    headers: { "Content-Type": contentType },
  })
}
