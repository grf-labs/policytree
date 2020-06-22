# Changelog
All notable changes to policytree will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [1.0] - 2020-06-22

Release 1.0 has been reviewed by the Journal of Open Source Software. See https://github.com/openjournals/joss-reviews/issues/2232 for improvements.

### Fixed
- Fix treatment vector categories bug. [#22](https://github.com/grf-labs/policytree/pull/22)

## [0.9.2] - 2020-03-21

### Fixed
- Minor package test modification to retain CRAN compatibility.

## [0.9.1] - 2020-03-20

### Added
- Export a convenience function `conditional_means`
- Runtime for `tree_search` is clarified. [#6](https://github.com/grf-labs/policytree/pull/6)
- Add an optional approximation parameter to `tree_search` that allow for skipping points. [#5](https://github.com/grf-labs/policytree/pull/5)

### Fixed
- Fix `predict` with `multi_causal_forest` forest when `newdata` is supplied. [#2](https://github.com/grf-labs/policytree/pull/2)
- Fix a depth 0 base case in `tree_search`. [#4](https://github.com/grf-labs/policytree/pull/4)
- Fix pruning in the recursive case. Note that this code branch was wrong, but it would never result in the wrong tree being returned. [#9](https://github.com/grf-labs/policytree/pull/9)

## [0.9.0] - 2020-01-11
First CRAN release.

## [0.1.0] - 2019-12-17
First beta release.
