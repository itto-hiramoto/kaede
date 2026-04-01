// @ts-check

const autumnFavicon =
  'data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text x=%2250%%22 y=%2250%%22 style=%22dominant-baseline:central;text-anchor:middle;font-size:90px;%22>🍁</text></svg>';

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Kaede',
  tagline: 'A statically typed compiled language for concise servers.',
  url: 'https://itto-hiramoto.github.io',
  baseUrl: '/kaede/',
  onBrokenLinks: 'throw',
  organizationName: 'itto-hiramoto',
  projectName: 'kaede',
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },
  markdown: {
    hooks: {
      onBrokenMarkdownLinks: 'warn',
    },
  },
  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
        },
        blog: false,
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
  headTags: [
    {
      tagName: 'link',
      attributes: {
        rel: 'icon',
        href: autumnFavicon,
      },
    },
  ],
  themeConfig: /** @type {import('@docusaurus/preset-classic').ThemeConfig} */ ({
    announcementBar: {
      id: 'pre-release-banner',
      content: 'Kaede is pre-release. Language details may change before 1.0.',
      backgroundColor: '#4f2b18',
      textColor: '#fff6ef',
      isCloseable: false,
    },
    colorMode: {
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'Kaede',
      logo: {
        alt: 'Kaede logo',
        src: 'img/kaede-mark.svg',
        width: 32,
        height: 32,
      },
      items: [
        {
          type: 'doc',
          docId: 'intro',
          label: 'Docs',
          position: 'left',
        },
        {
          href: 'https://github.com/itto-hiramoto/kaede',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Get Started',
              to: '/docs/getting-started/installation',
            },
            {
              label: 'Examples',
              to: '/docs/examples',
            },
          ],
        },
        {
          title: 'Project',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/itto-hiramoto/kaede',
            },
            {
              label: 'VSCode extension',
              href: 'https://github.com/itto-hiramoto/kaede/tree/main/editors/vscode',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Kaede contributors.`,
    },
    prism: {
      additionalLanguages: ['rust', 'toml', 'bash'],
    },
  }),
};

module.exports = config;
