### Preparing a new CRAN release

0. Open a "Prepare the v.x release PR" and update CHANGELOG

1. Make the tarball

  Add

  ```
  ^tests/testthat/test_((?!cran).).*
  ```

  to `.Rbuildignore` (do not commit the change)

  then run

  ```
  R CMD build .
  ```

2. Test the package

  ```
  R CMD check --as-cran --run-donttest policytree_*.tar.gz
  ```

3. Pre-check then submit

  https://win-builder.r-project.org/upload.aspx

  https://builder.r-hub.io/ (to check on all builds, including Solaris)

  https://cran.r-project.org/submit.html

4. Merge the "Prepare the vx release" and tag it

  ```
  git tag -a v1.1.0 "release-commit-hash"
  git push --tags
  ```
