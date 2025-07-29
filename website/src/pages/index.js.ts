import { serveStaticFrom } from '../utils/serveStaticFrom.ts';

export async function GET({ params, request }) {
  return serveStaticFrom('./src/api', 'index.js');
}