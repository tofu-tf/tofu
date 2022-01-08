module.exports = {
  title: "Tofu",
  tagline: "Functional programming toolbox",
  url: "https://docs.tofu.tf",
  baseUrl: "/",
  organizationName: "tofu-tf",
  projectName: "tofu",
  scripts: [
    "https://buttons.github.io/buttons.js"
  ],
  favicon: "img/tofu_logo_turquose_square.svg",
  onBrokenLinks: "log",
  onBrokenMarkdownLinks: "log",
  customFields: {
    users: [
      {
        caption: "Tinkoff bank",
        image: "/img/tinkoff-bank-general-logo-1.png",
        infoLink: "https://www.tinkoff.ru",
        pinned: true
      }
    ] 
  },
  presets: [
    [
      "@docusaurus/preset-classic",
      {
        docs: {
          path: "../docs",
          showLastUpdateAuthor: true,
          showLastUpdateTime: true,
          sidebarPath: "../website/sidebars.json"
        },
        blog: {
          path: "blog"
        },
        theme: {
          customCss: "../src/css/customTheme.css"
        }
      }
    ]
  ],
  plugins: [
    [
      '@docusaurus/plugin-ideal-image',
      {
        quality: 90,
        max: 1030, // max resized image's size.
        min: 640, // min resized image's size. if original is lower, use that size.
        steps: 4, // the max number of images generated between min and max (inclusive)
      },
    ],
  ],
  themeConfig: {
    announcementBar: {
      id: 'announcementBar-1', // Increment on change
      content:
        '⭐️ If you like Tofu, give it a star on <a target="_blank" rel="noopener noreferrer" href="https://github.com/tofu-tf/tofu">GitHub</a>! ⭐',
    },
    prism: {
      additionalLanguages: ['java', 'scala'],
      theme: require('prism-react-renderer/themes/vsDark'),
    },
    navbar: {
      // title: "Tofu",
      "logo": {
        "src": "img/tofu_logo_turquose_square.svg"
      },
      items: [
        {
          to: "docs/",
          label: "Docs",
          position: "right"
        },
        {
          href: "/api/index.html",
          label: "API",
          position: "right"
        }
      ]
    },
    footer: {
      style: "dark",
      links: [
        {
        title: 'Docs',
        items: [
          {
            label: 'Getting started',
            to: 'docs',
          },
        ],
      },
      {
        title: 'Community',
        items: [
          {
            label: 'User showcase',
            to: 'users',
          },
          {
            label: 'Stack Overflow',
            href: 'https://stackoverflow.com/questions/tagged/tofu',
          },
          {
            label: 'Telegram Group (EN|RU)',
            href: 'https://t.me/tofu_scala',
          },
          {
            label: 'Gitter Group (EN)',
            href: 'https://gitter.im/tinkoff-tofu/community',
          },
        ],
      },
      {
        title: 'More',
        items: [
          {
            label: 'Blog',
            to: 'blog',
          },
          {
            label: 'Github',
            href: 'https://github.com/tofu-tf/tofu',
          },
        ],
      },
    ],
      copyright: `Copyright © ${new Date().getFullYear()} Tinkoff.ru`,
      // logo: {
        // src: "img/tofu_logo_turquose_square.png"
      // }
    },
    colorMode: {
      switchConfig: {
        darkIcon: '🌙',
        darkIconStyle: {
          marginLeft: '2px',
        },
        lightIcon: '🔆'
      },
      respectPrefersColorScheme: true
    }
  }
}
