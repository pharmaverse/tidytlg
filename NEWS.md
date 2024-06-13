# tidytlg 0.1.5

* Updated `gentlg()` to allow for passing multiple of either plots or tables to
`huxme`. The function supports printing multiple of these object to `.rtf` files.
* Updated `gentlg()` to support refined control over the appearance of bottom borders
in tables via new arguments `bottom_borders` and `border_fns`. See documentation
for these arguments for more information.
* Adjusted the text acompanying the page numbers to correctly reflect the contents
of the printed documents produced by `gentlg()`.

# tidytlg 0.1.4

* Update getFileName.R to use the logrx package over the timber package

# tidytlg 0.1.3

* Updated tidytlg-package overall documentation to resolve a CRAN issue. (#21)

# tidytlg 0.1.2

* Fixed bug where the use of the 'wcol' argument in `gentlg()` would error when no formatting variables were present (#14)
* `gentlg()` now supports multiple plots passed in the 'plotnames' argument (#13)
* Fixed `check_args()` function to remove some tests that were too strict (#12)

# tidytlg 0.1.1

* Updated tidytlg.Rmd vignette to write to temp directory instead of user home directory

# tidytlg 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Initial release of tidytlg
