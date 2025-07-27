import fs from 'fs';
import path from 'path';

export async function GET({ params, request }) {

  const filePath = path.resolve('./src/api/index.js')
  const content = fs.readFileSync(filePath, 'utf-8');

  return new Response(content, {
    headers: { "Content-Type": "text/javascript" },
  });
}
