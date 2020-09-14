module.exports = {
  title: 'Monty',
  tagline: 'Function programming for python developers',
  url: 'https://mulan-szechuan-sauce.github.io',
  baseUrl: '/monty/',
  onBrokenLinks: 'throw',
  favicon: 'img/favicon.ico',
  organizationName: 'mulan-szechuan-sauce', // Usually your GitHub org/user name.
  projectName: 'monty', // Usually your repo name.
  themeConfig: {
    navbar: {
      title: 'Monty',
      logo: {
        alt: 'My Site Logo',
        src: 'img/logo.svg',
      },
      items: [
        {
          to: 'docs/',
          activeBasePath: 'docs',
          label: 'Docs',
          position: 'left',
        },
        //{to: 'blog', label: 'Blog', position: 'left'},
        {
          href: 'https://github.com/Mulan-Szechuan-Sauce/monty',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      //style: 'dark',
      //links: [
      //  {
      //    title: 'Docs',
      //    items: [
      //      {
      //        label: 'Style Guide',
      //        to: 'docs/',
      //      },
      //      {
      //        label: 'Second Doc',
      //        to: 'docs/doc2/',
      //      },
      //    ],
      //  },
      //  {
      //    title: 'Community',
      //    items: [
      //      {
      //        label: 'Stack Overflow',
      //        href: 'https://stackoverflow.com/questions/tagged/docusaurus',
      //      },
      //      {
      //        label: 'Discord',
      //        href: 'https://discordapp.com/invite/docusaurus',
      //      },
      //      {
      //        label: 'Twitter',
      //        href: 'https://twitter.com/docusaurus',
      //      },
      //    ],
      //  },
      //  {
      //    title: 'More',
      //    items: [
      //      {
      //        label: 'Blog',
      //        to: 'blog',
      //      },
      //      {
      //        label: 'GitHub',
      //        href: 'https://github.com/facebook/docusaurus',
      //      },
      //    ],
      //  },
      //],
      copyright: `Copyright © ${new Date().getFullYear()} Monty Lang`,
    },
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          editUrl:
            'https://github.com/Mulan-Szechuan-Sauce/monty/tree/master/',
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          editUrl:
            'https://github.com/Mulan-Szechuan-Sauce/monty/tree/master/',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      },
    ],
  ],
};
