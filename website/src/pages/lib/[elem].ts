import fs from 'fs';
import path from 'path';

export const prerender = false;

export async function GET({ params, request }) {
  
  const elem = params.elem;
  const filePath = path.resolve('./src/api/lib/' + elem)

  let contentType = ""
  if (elem.endsWith("css"))
    contentType = "text/css"
  else if (elem.endsWith("js"))
    contentType =  "text/javascript"
  else if (elem.endsWith("svg")) 
    contentType = "image/svg+xml"
  else if (elem.endsWith("woff"))
    contentType = "font/woff"
  else if (elem.endsWith("ttf"))
    contentType = "font/ttf"

  const content = fs.readFileSync(filePath, 'utf-8');
  return new Response(content, {
    headers: { "Content-Type": contentType },
  })
}
