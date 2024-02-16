// This is a workaround to get the online documentation to produce the correct GitHub
// link to the source file in question. pkgdown assumes the package is hosted
// under the root repo, but policytree is hosted under /r-package.
// This fix is from: https://github.com/r-lib/pkgdown/issues/1152
$(function() {
    if(window.location.pathname.toLocaleLowerCase().indexOf('/reference') != -1) {
        /* Replace '/R/' with '/r-package/policytree/R/' in all external links to .R files of the GitHub repo */
        $('a[href^="https://github.com/grf-labs/policytree/blob/master/R"][href*=".R"]').attr('href', (i, val) => { return val.replace('/R/', '/r-package/policytree/R/'); });
    }
    if(window.location.pathname.toLocaleLowerCase().indexOf('/articles') != -1) {
        /* Replace '/vignettes/' with '/r-package/policytree/vignettes/' in all external links to .Rmd files of grf GitHub repo */
        $('a[href^="https://github.com/grf-labs/policytree/blob/master/vignettes"][href*=".Rmd"]').attr('href', (i, val) => { return val.replace('/vignettes/', '/r-package/policytree/vignettes/'); });
    }
});
