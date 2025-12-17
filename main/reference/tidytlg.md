# `tidytlg`: A package for producing tables, listings, and graphs (`TLGs`) using `tidyverse` packages.

The `tidytlg` package provide a set of function allowing you to produce
TLGs using table metadata.

## `tidytlg` Options

- `tidytlg.row_type.levels` - The values of row_type and their
  respective levels. Default: c("TABLE_BY_HEADER", "BY_HEADER1",
  "BY_HEADER2", "BY_HEADER3", "BY_HEADER4", "BY_HEADER5", "HEADER", "N",
  "VALUE", "NESTED")

- `tidytlg.fontsize.title` - Font size for titles in points. Default: 10

- `tidytlg.fontsize.table.footnote` - Font size for footnotes in points.
  For tables. Default: 9

- `tidytlg.fontsize.listing.footnote` - Font size for footnotes in
  points. For listings. Default: 8

- `tidytlg.fontsize.graph.footnote` - Font size for footnotes in points.
  For graphs. Default: 8

- `tidytlg.fontsize.table` - Font size for tables in points. Default: 9

- `tidytlg.fontsize.graph` - Font size for graphs in points. Default: 10

- `tidytlg.fontsize.listing` - Font size for listings in points.
  Default: 8

- `tidytlg.right.padding` - Amount of right padding for cells in points.
  Default: 1

- `tidytlg.left.padding` - Amount of left padding for cells in points.
  Default: 1

- `tidytlg.stat_labels` - Labels to be used for each summary statistic
  in the output table. Will need to have all available statistics if
  this option is changed!

- `tidytlg.precision.extra` - Value to be added on for each summary
  statistic when calculating precision. This will be used to make
  different statistics have different precision as needed

- `tidytlg.nested_freq.statlist.default` - Default `statlist` object for
  nested_freq tables. Default: `statlist("n (x.x)")`

- `tidytlg.univar.statlist.default` - Default `statlist` object for
  `univar` tables. Default:
  `statlist(c("N", "MEANSD", "MEDIAN", "RANGE", "IQRANGE"))`

- `tidytlg.freq.statlist.default` - Default `statlist` object for
  frequency tables. Default: `statlist("n (x.x)")`

- `tidytlg.stat_labels` - A `data.frame` controlling how the stats are
  labeled in a `univar` table. See 'Default Stat Labels' section for
  defaults.

- `tidytlg.denoms.message` - A `boolean`, should a message print
  detailing what the denominators are?

## Default Stat Labels

|              |                           |
|--------------|---------------------------|
| **stat**     | **label**                 |
| `N`          | N                         |
| `SUM`        | Sum                       |
| `MEAN`       | Mean                      |
| `GeoMEAN`    | Geometric Mean            |
| `SD`         | Std. Deviation            |
| `SE`         | Std. Error                |
| `GSD`        | Geometric Std. Deviation  |
| `GSE`        | Geometric Std. Error      |
| `CV`         | CV                        |
| `MEANSD`     | Mean (SD)                 |
| `MEANSE`     | Mean (SE)                 |
| `MEDIAN`     | Median                    |
| `MIN`        | Minimum                   |
| `MAX`        | Maximum                   |
| `RANGE`      | Range                     |
| `Q1`         | First quartile            |
| `Q3`         | Third quartile            |
| `IQRANGE`    | IQ range                  |
| `MEDRANGE`   | Median (Range)            |
| `MEDIQRANGE` | Median (Q1; Q3)           |
| `MEAN_CI`    | Mean (95% C.I.)           |
| `GeoMEAN_CI` | Geometric Mean (95% C.I.) |

## Default Precision Extra

|               |           |
|---------------|-----------|
| **stat**      | **extra** |
| `N`           | 0         |
| `SUM`         | 0         |
| `MEAN`        | 1         |
| `GeoMEAN`     | 1         |
| `SD`          | 2         |
| `SE`          | 2         |
| `GSD`         | 2         |
| `GSE`         | 2         |
| `CV`          | 1         |
| `MEDIAN`      | 1         |
| `MIN`         | 0         |
| `MAX`         | 0         |
| `Q1`          | 1         |
| `Q3`          | 1         |
| `LCL_MEAN`    | 2         |
| `UCL_MEAN`    | 2         |
| `LCL_GeoMEAN` | 2         |
| `UCL_GeoMEAN` | 2         |

## Updating Options

- For a single session, an option can be changed by
  `option(<optionToChange> = <NewValue>)`

- To change an option for a single projects across sessions in that
  projects, place the options update in the `.Rprofile` in that project
  directory.

- To change an option for a user across all sessions, place the options
  update in the `.Rprofile` file in the users home directory.

- To change an option for all users in an R environment, place the
  options update in the `.Rprofile.site` file in the R home directory.

## See also

Useful links:

- <https://pharmaverse.github.io/tidytlg/main/>

- <https://github.com/pharmaverse/tidytlg>

- Report bugs at <https://github.com/pharmaverse/tidytlg/issues>

## Author

**Maintainer**: Konrad Pagacz <kpagacz@its.jnj.com>

Authors:

- Nicholas Masel <nmasel@its.jnj.com>

- Steven Haesendonckx <shaesen2@its.jnj.com>

- Pelagia Alexandra Papadopoulou <ppapadop@its.jnj.com>

- Sheng-Wei Wang <swang69@its.jnj.com>

- Eli Miller <eli.miller@atorusresearch.com>
  ([ORCID](https://orcid.org/0000-0002-2127-9456))

- Nathan Kosiba <nkosiba@its.jnj.com>
  ([ORCID](https://orcid.org/0000-0001-5359-4234))

- Aidan Ceney <aceney@its.jnj.com>
  ([ORCID](https://orcid.org/0000-0001-8313-487X))

Other contributors:

- Janssen R&D \[copyright holder, funder\]

- David Hugh-Jones (Author of included 'huxtable' library) \[copyright
  holder\]
