const math = require("remark-math");
const katex = require("rehype-katex");

// With JSDoc @type annotations, IDEs can provide config autocompletion
/** @type {import('@docusaurus/types').DocusaurusConfig} */
(
  module.exports = {
    title: "Gospel",
    tagline: "A tool-agnostic formal specification language for OCaml.",
    url: "https://ocaml-gospel.github.io",
    baseUrl: "/gospel/",
    organizationName: "ocaml-gospel", // Usually your GitHub org/user name.
    projectName: "gospel", // Usually your repo name.
    trailingSlash: false,

    presets: [
      [
        "@docusaurus/preset-classic",
        /** @type {import('@docusaurus/preset-classic').Options} */
        ({
          docs: {
            routeBasePath: "/",
            path: "docs",
            sidebarPath: require.resolve("./sidebars.js"),
            remarkPlugins: [math],
            rehypePlugins: [katex],
          },
          theme: {
            customCss: require.resolve("./src/css/custom.css"),
          },
          sitemap: {
            changefreq: "weekly",
            priority: 0.5,
          },
        }),
      ],
    ],

    stylesheets: [
      {
        href: "https://cdn.jsdelivr.net/npm/katex@0.13.11/dist/katex.min.css",
        integrity:
          "sha384-Um5gpz1odJg5Z4HAmzPtgZKdTBHZdw8S29IecapCSB31ligYPhHQZMIlWLYQGVoc",
        crossorigin: "anonymous",
      },
    ],

    themeConfig:
      /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
      ({
        colorMode: {
          respectPrefersColorScheme: true,
        },
        navbar: {
          title: "Gospel",
          items: [
            {
              type: "doc",
              docId: "welcome",
              position: "left",
              label: "Docs",
            },
            {
              href: "https://github.com/ocaml-gospel/gospel",
              className: "header-github-link",
              "aria-label": "GitHub repository",
              position: "right",
            },
          ],
        },
        footer: {
          style: "dark",
          copyright: `Copyright © 2020—${new Date().getFullYear()} Gospel maintainers.`,
        },
        prism: {
          defaultLanguage: "ocaml",
          additionalLanguages: ["ocaml", "ebnf"],
          theme: require("prism-react-renderer/themes/nightOwlLight"),
          darkTheme: require("prism-react-renderer/themes/nightOwl"),
        },
      }),
  }
);
