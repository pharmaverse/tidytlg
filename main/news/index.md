# Changelog

## tidytlg 0.10.0.9000

- Fixed RTF landscape tag
  ([\#54](https://github.com/pharmaverse/tidytlg/issues/54)).

## tidytlg 0.10.0

CRAN release: 2025-07-02

- Fixed rounding bug
  ([\#44](https://github.com/pharmaverse/tidytlg/issues/44)).

## tidytlg 0.1.6

CRAN release: 2025-05-02

- Fixed incorrect page size in landscape orientation
  ([\#47](https://github.com/pharmaverse/tidytlg/issues/47)).

## tidytlg 0.1.5

CRAN release: 2024-06-14

- Updated [`gentlg()`](../reference/gentlg.md) to allow for passing
  multiple of either plots or tables to `huxme`. The function supports
  printing multiple of these object to `.rtf` files.
- Updated [`gentlg()`](../reference/gentlg.md) to support refined
  control over the appearance of bottom borders in tables via new
  arguments `bottom_borders` and `border_fns`. See documentation for
  these arguments for more information.
- Adjusted the text accompanying the page numbers to correctly reflect
  the contents of the printed documents produced by
  [`gentlg()`](../reference/gentlg.md).

## tidytlg 0.1.4

CRAN release: 2023-10-18

- Update `getFileName.R` to use the `logrx` package over the timber
  package.

## tidytlg 0.1.3

CRAN release: 2023-08-31

- Updated `tidytlg-package` overall documentation to resolve a CRAN
  issue. ([\#21](https://github.com/pharmaverse/tidytlg/issues/21))

## tidytlg 0.1.2

CRAN release: 2023-06-23

- Fixed bug where the use of the `wcol` argument in
  [`gentlg()`](../reference/gentlg.md) would error when no formatting
  variables were present
  ([\#14](https://github.com/pharmaverse/tidytlg/issues/14)).
- [`gentlg()`](../reference/gentlg.md) now supports multiple plots
  passed in the `plotnames` argument
  ([\#13](https://github.com/pharmaverse/tidytlg/issues/13)).
- Fixed `check_args()` function to remove some tests that were too
  strict ([\#12](https://github.com/pharmaverse/tidytlg/issues/12)).

## tidytlg 0.1.1

CRAN release: 2023-05-09

- Updated `tidytlg.Rmd` vignette to write to temp directory instead of
  user home directory.

## tidytlg 0.1.0

CRAN release: 2023-05-05

- Added a `NEWS.md` file to track changes to the package.
- Initial release of `tidytlg`.
