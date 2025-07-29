import { serveStaticFrom } from '../../utils/serveStaticFrom.ts';
export const prerender = false;

export async function GET({ params }) {
 return serveHtmlFrom('./src/api/zio', params.path);
}