### Contributing

Contributions are welcome. This repository follows the standard open source protocol and setup with git where there is an abundance of existing resources to get up to speed. Condensed greatly, the workflow is to fork this repository, check out a branch, commit your changes (forming an ideally legible commit history), then submitting a pull request explaining your contribution, ideally referring to the issue you created, or the issue you chose to work on.

Building and compiling the package can be done conveniently with RStudio. A C++ compiler, together with the R packages _Rcpp_ and _BH_ is required. For documentation _roxygen2_ and _pkgdown_ is needed as well. To compile and load the package after making changes run _Build -> Install and Restart_. To test the package run _Build -> Test Package_. R Markdown tutorials are stored in `vignettes\` and these can be built by running _pkgdown::build_articles()_.


For more general details on developing R packages linking to C++ code, see the guide in our parent package: https://grf-labs.github.io/grf/DEVELOPING.html
