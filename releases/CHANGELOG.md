# Changelog
All notable changes to policytree will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [1.2.3] - 2024-06-13

### Fixed
- Change package license to less restrictive MIT license. [#172](https://github.com/grf-labs/policytree/pull/172)

## [1.2.2] - 2023-06-23

### Fixed
- Drop C++11 compiler flag in src/Makevars per latest CRAN guidelines. [#168](https://github.com/grf-labs/policytree/pull/168)

## [1.2.1] - 2022-11-20

### Fixed
- Make the print()'d leaf labels in `hybrid_policy_tree` consistent with `policy_tree` (level first). [#156](https://github.com/grf-labs/policytree/pull/156)

## [1.2.0] - 2022-03-18

### Added
- Add `hybrid_policy_tree` for building deeper trees by using `policy_tree` to look ahead `search.depth` (typically equal to 2) splits. [#118](https://github.com/grf-labs/policytree/pull/118)
- Add some prescriptive warning messages to `policy_tree` in case the input type (`Gamma, X`) is beyond what is computationally feasible with exact tree search. [#116](https://github.com/grf-labs/policytree/pull/116), [#129](https://github.com/grf-labs/policytree/pull/129)
- Add `double_robust_scores` for GRF's `causal_survival_forest`. [#126](https://github.com/grf-labs/policytree/pull/126)

### Fixed
- Raise error if causal forest or instrumental forest with non-binary treatment is passed to `double_robust_scores`. [#125](https://github.com/grf-labs/policytree/pull/125)

## [1.1.1] - 2021-07-07

### Fixed

- Compatibility release for CRAN Solaris. [#104](https://github.com/grf-labs/policytree/pull/104)

## [1.1.0] - 2021-06-24

### Changed (breaking)
**IMPORTANT** Some of these changes might cause small differences in results compared to previous releases, even if the same random seed is used.
- Update `policytree` to use GRF version 2.0.0. The "one vs all" `multi_causal_forest` is deprecated and we instead utilize the new GRF estimator `multi_arm_causal_forest` which supports multiple treatment arms natively. `multi_causal_forest` will continue to work until the next release, but dispatches to `multi_arm_causal_forest` and emits a warning. Note that this allows for a drop-in replacement in workflows that rely on calls to `double_robust_scores` for policy learning, but not for workflows involving point predictions (`predict(forest)`) as the new GRF estimator will for K treatment arms `predict` a K-1 contrast matrix. [#67](https://github.com/grf-labs/policytree/issues/67)

### Added
- Add some improved documentation around runtime and the limitations of exact tree search. [#95](https://github.com/grf-labs/policytree/issues/95)

## [1.0.4] - 2021-03-10

### Added
- Add optional `leaf.label` argument to `plot.policy_tree` allowing custom treatment names. [#60](https://github.com/grf-labs/policytree/pull/60)
- Add optional `"type = node.id"` argument to `predict.policy_tree` returning the leaf node the test sample falls into. [#78](https://github.com/grf-labs/policytree/pull/78).
- Add optional `min.node.size` to `policy_tree` specifying the smallest permissible node size. [#77](https://github.com/grf-labs/policytree/pull/77)

### Fixed
- Fix `split.step` approximation for discrete covariates by redefining to skip observations instead of unique values. Note: this may cause small differences in results compared to previous releases when using the `split.step` approximation parameter. [#73](https://github.com/grf-labs/policytree/pull/73)

## [1.0.3] - 2020-09-13

### Fixed
- Minor modification to C++ `pow` call to retain CRAN compatibility. [#54](https://github.com/grf-labs/policytree/pull/54)

## [1.0.2] - 2020-09-09

### Fixed
- Speed up prediction on very large test sets. [#51](https://github.com/grf-labs/policytree/pull/51)

## [1.0.1] - 2020-07-13

### Added
- Add support for `help(<package-name>)` in the R package. [#41](https://github.com/grf-labs/policytree/pull/41)

## [1.0] - 2020-06-22

### Added
- Release 1.0 has been reviewed by the Journal of Open Source Software. [review](https://github.com/openjournals/joss-reviews/issues/2232), [paper](https://joss.theoj.org/papers/10.21105/joss.02232).

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
