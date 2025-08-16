import fs from 'fs';
import path from 'path';

export function serveStaticFrom(relativeDir: string, filename: string): Response {
  const filePath = path.resolve(relativeDir, filename);
  const content = fs.readFileSync(filePath, 'utf-8');

  let contentType = ""
  if (filename.endsWith("css"))
    contentType = "text/css"
  else if (filename.endsWith("js"))
    contentType =  "text/javascript"
  else if (filename.endsWith("svg")) 
    contentType = "image/svg+xml"
  else if (filename.endsWith("woff"))
    contentType = "font/woff"
  else if (filename.endsWith("ttf"))
    contentType = "font/ttf"
  else if (filename.endsWith("html"))
    contentType = "text/html"

  return new Response(content, {
    headers: { 'Content-Type': contentType },
  });
}