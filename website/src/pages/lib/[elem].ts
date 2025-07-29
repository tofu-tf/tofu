import { serveStaticFrom } from '../../utils/serveStaticFrom.ts';
export const prerender = false;

export async function GET({ params }) {
  return serveStaticFrom('./src/api/lib/', params.elem);
}
