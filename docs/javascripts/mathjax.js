// MathJax configuration for pymdownx.arithmatex (generic mode), see
// https://squidfunk.github.io/mkdocs-material/reference/math/#mathjax
window.MathJax = {
  tex: {
    inline: [["\\(", "\\)"]],
    display: [["\\[", "\\]"]],
    processEscapes: true,
    processEnvironments: true,
  },
  options: {
    ignoreHtmlClass: ".*|",
    processHtmlClass: "arithmatex",
  },
};

document$.subscribe(() => {
  MathJax.startup.output.clearCache();
  MathJax.typesetClear();
  MathJax.texReset();
  MathJax.typesetPromise();
});
