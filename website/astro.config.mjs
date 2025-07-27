// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightLinksValidator from 'starlight-links-validator'
import starlightBlog from 'starlight-blog'
import node from '@astrojs/node';

// https://astro.build/config
export default defineConfig({
	adapter: node({
    	mode: 'standalone',
  	}),
	image: {
    	domains: ["avatars3.githubusercontent.com"],
  	},
	site: 'https://tofu-tf.github.io',
  	base: '/tofu',
	integrations: [
		starlight({
			customCss: [
        	// Relative path to your custom CSS file
        		'./src/styles/theme.css',
      		],
			favicon: 'favicon.ico',
			editLink: {
        		baseUrl: 'https://tofu-tf.github.io/tofu/website/',
      		},
			plugins: [
				starlightLinksValidator({
					exclude: ['/blog', '/blog/'],
				}),
				starlightBlog({
					metrics: {
						readingTime: true
					},
					authors: {
						odomontois: {
							name: 'Oleg Nizhnik',
							title: 'Starlight Aficionado',
							picture: 'https://avatars3.githubusercontent.com/u/247558?s=460&v=4',
							url: 'https://github.com/Odomontois',
						},
						
					},
				})
			],
			title: 'Tofu',
			social: [
				{ icon: 'github', label: 'GitHub', href: 'https://github.com/tofu-tf/tofu' },
				{ icon: 'telegram', label: 'Telegram', href: 'https://t.me/scala_any/452/' },
				{ icon: 'seti:scala', label: 'API', href: '/api'}
			],
			sidebar: [
				{ slug: 'docs/getting-started' },
				{
					label: 'Core',
					autogenerate: { directory: 'docs/core' }
				},
				{
					label: 'Logging',
					autogenerate: { directory: 'docs/logging' },
					collapsed: true
				},
				{
					label: 'Contextual',
					autogenerate: { directory: 'docs/contextual' },
					collapsed: true
				},
				{
					label: 'Concurrent',
					autogenerate: { directory: 'docs/concurrent' },
					collapsed: true
				},
				{
					label: 'Higher-Kind',
					autogenerate: { directory: 'docs/higher-kind' },
					collapsed: true
				},
				{
					label: 'Optics',
					autogenerate: { directory: 'docs/optics' },
					collapsed: true
				},
				{
					label: 'Utilities',
					autogenerate: { directory: 'docs/utilities' },
					collapsed: true
				},
				{
					label: 'Streams',
					autogenerate: { directory: 'docs/streams' },
					collapsed: true
				}
			]
		}),
	],
});
