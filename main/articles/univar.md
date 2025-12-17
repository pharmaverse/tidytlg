# Univariate Statistics Analysis

## Introduction

The `univar` function is called to produce univariate-type summary
statistics for numeric variables. A typical example of using the
`univar` function is to create a `tbl` chunk as shown below for
summarizing `N`, `MEAN (SD)`, `MEDIAN`, `RANGE`, `IQ Range` for the
`Age` variable in `ADSL`.

``` r
tbl <- cdisc_adsl |>
  univar(
    colvar = "TRT01PN",
    rowvar = "AGE",
    statlist = statlist(c("N", "MEANSD", "MEDIAN", "RANGE", "IQRANGE")),
    decimal = 0,
    row_header = "Age (Years)"
  )

knitr::kable(tbl)
```

| label       | 0            | 54           | 81           | row_type | group_level |
|:------------|:-------------|:-------------|:-------------|:---------|------------:|
| Age (Years) |              |              |              | HEADER   |           0 |
| N           | 5            | 5            | 5            | N        |           0 |
| Mean (SD)   | 69.6 (14.40) | 75.6 (6.73)  | 72.2 (9.23)  | VALUE    |           0 |
| Median      | 64.0         | 74.0         | 75.0         | VALUE    |           0 |
| Range       | (52; 85)     | (68; 84)     | (57; 81)     | VALUE    |           0 |
| IQ range    | (63.0; 84.0) | (71.0; 81.0) | (71.0; 77.0) | VALUE    |           0 |

## Customizing Univariate Statistics

Besides the 5 standard univariate statistics shown above that are often
required in the demographic tables, you can pick any univariate
statistics from the table below and arrange them in a character vector
for passing to the `statlist` argument.

| `Statlist`   |          Description          |
|--------------|:-----------------------------:|
| `N`          | number of non-missing values  |
| `SUM`        |              sum              |
| `MEAN`       |             mean              |
| `GeoMEAN`    |        geometric mean         |
| `SD`         |      standard deviation       |
| `SE`         |        standard error         |
| `CV`         |   coefficient of variation    |
| `GSD`        | geometric standard deviation  |
| `GSE`        |   geometric standard error    |
| `MEANSD`     |   mean (standard deviation)   |
| `MEANSE`     |     mean (standard error)     |
| `MEDIAN`     |            median             |
| `MIN`        |            minimum            |
| `MAX`        |            maximum            |
| `RANGE`      |             range             |
| `Q1`         |        first quartile         |
| `Q3`         |        third quartile         |
| `IQRANGE`    |     inter-quartile range      |
| `MEDRANGE`   |        median (range)         |
| `MEDIQRANGE` | median (inter-quartile range) |
| `MEAN_CI`    |        mean (95% C.I.)        |
| `GeoMEAN_CI` |   geometric mean (95% C.I.)   |

A customized example is shown below for displaying `N`,
`Mean (95% C.I.)`, and `Geometric Mean (95% C.I.)` for the `Age`
variable in `ADSL`.

``` r
tbl <- cdisc_adsl |>
  univar(
    colvar = "TRT01PN",
    rowvar = "AGE",
    statlist = statlist(c("N", "MEAN_CI", "GeoMEAN_CI")),
    decimal = 0,
    row_header = "Age (Years)"
  )

knitr::kable(tbl)
```

| label | 0 | 54 | 81 | row_type | group_level |
|:---|:---|:---|:---|:---|---:|
| Age (Years) |  |  |  | HEADER | 0 |
| N | 5 | 5 | 5 | N | 0 |
| Mean (95% C.I.) | 69.6 (51.72; 87.48) | 75.6 (67.24; 83.96) | 72.2 (60.74; 83.66) | VALUE | 0 |
| Geometric Mean (95% C.I.) | 68.4 (52.72; 88.73) | 75.4 (67.51; 84.13) | 71.7 (60.50; 84.94) | VALUE | 0 |

## Decimal Precision

The decimal precision to be used in display of univariate statistics is
comprised of two pieces. The base decimal precision is what controls the
base number of decimals to be used, this can be set using the `decimal`
argument. The precision extra is what controls the difference between
the precision used for different statistics, this is controlled using
the option `tidytlg.precision.extra`. The precision extra is the amount
precision will need to be adjusted from the base precision for each
different statistic. The default of the precision extra is set by
following our table and listing conventions: Range has a precision extra
of 0, Mean and Median have a precision extra of 1, `SD` has a precision
extra of 2. To see a full list of precision extra defaults, please type
`options("tidytlg.precision.extra")` in your console. An example
function call of `univar` is shown below for presenting the data using a
base decimal value of 2.

``` r
tbl <- cdisc_adsl |>
  univar(
    colvar = "TRT01PN",
    rowvar = "BMIBL",
    decimal = 2,
    row_header = "Age (Years)"
  )

knitr::kable(tbl)
```

| label | 0 | 54 | 81 | row_type | group_level |
|:---|:---|:---|:---|:---|---:|
| Age (Years) |  |  |  | HEADER | 0 |
| N | 5 | 5 | 5 | N | 0 |
| Mean (SD) | 27.080 (3.6424) | 27.180 (3.4419) | 27.760 (2.4795) | VALUE | 0 |
| Median | 27.600 | 27.300 | 28.100 | VALUE | 0 |
| Range | (21.90; 30.40) | (23.90; 32.00) | (24.90; 31.40) | VALUE | 0 |
| IQ range | (25.100; 30.400) | (23.900; 28.800) | (26.100; 28.300) | VALUE | 0 |

## Data Driven Precision

While static precision is useful in some cases, data driven precision is
also available. This is controlled using the `precisionby`,
`precisionon`, and `decimal` arguments. `precisionby` tells the function
the variable(s) the user would like to compute the precision using. This
could be variables such as `PARAMCD` if the precision is to be varied
between parameter. `precisionon` is the variable that should be used
when calculating how many base decimal places are present in the data.
The last piece to data drive precision is the `decimal` argument which
gives us a cap for base precision values. This can be used to help avoid
unnecessarily long decimals in your final output.

A customized example is shown below for presenting the univariate
summary of vital signs data using `PARAMCD` as the by variable. In
addition, we would like the precision to be data driven and varied by
parameter, which can be achieved by setting `precisionby = "PARAMCD"`.

``` r
tbl <- cdisc_advs |>
  univar(
    colvar = "TRTAN",
    rowvar = "AVAL",
    rowbyvar = "PARAMCD",
    precisionby = "PARAMCD",
    decimal = 4
  )

knitr::kable(tbl)
```

| PARAMCD | label | 0 | 54 | 81 | row_type | group_level |
|:---|:---|:---|:---|:---|:---|---:|
| DIABP | DIABP |  |  |  | BY_HEADER1 | 0 |
| DIABP | N | 186 | 147 | 204 | N | 0 |
| DIABP | Mean (SD) | 71.9 (9.75) | 71.6 (7.12) | 68.8 (10.34) | VALUE | 0 |
| DIABP | Median | 71.5 | 72.0 | 69.0 | VALUE | 0 |
| DIABP | Range | (50; 92) | (50; 87) | (43; 101) | VALUE | 0 |
| DIABP | IQ range | (65.0; 78.0) | (68.0; 77.0) | (60.0; 76.0) | VALUE | 0 |
| HEIGHT | HEIGHT |  |  |  | BY_HEADER1 | 0 |
| HEIGHT | N | 5 | 5 | 5 | N | 0 |
| HEIGHT | Mean (SD) | 161.696 (14.0567) | 172.364 (9.1494) | 163.576 (13.0260) | VALUE | 0 |
| HEIGHT | Median | 162.560 | 175.260 | 162.560 | VALUE | 0 |
| HEIGHT | Range | (147.32; 180.34) | (158.24; 181.61) | (147.32; 177.80) | VALUE | 0 |
| HEIGHT | IQ range | (148.590; 169.670) | (168.910; 177.800) | (154.940; 175.260) | VALUE | 0 |
| PULSE | PULSE |  |  |  | BY_HEADER1 | 0 |
| PULSE | N | 186 | 147 | 204 | N | 0 |
| PULSE | Mean (SD) | 69.4 (9.15) | 64.9 (10.18) | 70.5 (9.87) | VALUE | 0 |
| PULSE | Median | 70.0 | 64.0 | 70.0 | VALUE | 0 |
| PULSE | Range | (52; 94) | (50; 98) | (50; 97) | VALUE | 0 |
| PULSE | IQ range | (61.0; 76.0) | (58.0; 70.0) | (62.0; 76.5) | VALUE | 0 |
| SYSBP | SYSBP |  |  |  | BY_HEADER1 | 0 |
| SYSBP | N | 186 | 147 | 204 | N | 0 |
| SYSBP | Mean (SD) | 132.0 (12.02) | 127.5 (12.58) | 135.8 (23.65) | VALUE | 0 |
| SYSBP | Median | 131.0 | 130.0 | 132.5 | VALUE | 0 |
| SYSBP | Range | (100; 167) | (95; 151) | (95; 198) | VALUE | 0 |
| SYSBP | IQ range | (123.0; 138.0) | (122.0; 137.0) | (116.0; 150.5) | VALUE | 0 |
| TEMP | TEMP |  |  |  | BY_HEADER1 | 0 |
| TEMP | N | 61 | 49 | 68 | N | 0 |
| TEMP | Mean (SD) | 36.481 (0.3491) | 36.537 (0.4374) | 36.660 (0.2837) | VALUE | 0 |
| TEMP | Median | 36.440 | 36.560 | 36.585 | VALUE | 0 |
| TEMP | Range | (35.61; 37.67) | (34.28; 37.17) | (35.89; 37.33) | VALUE | 0 |
| TEMP | IQ range | (36.220; 36.720) | (36.390; 36.720) | (36.470; 36.915) | VALUE | 0 |
| WEIGHT | WEIGHT |  |  |  | BY_HEADER1 | 0 |
| WEIGHT | N | 47 | 33 | 54 | N | 0 |
| WEIGHT | Mean (SD) | 69.170 (10.1753) | 82.795 (11.2792) | 78.472 (15.5886) | VALUE | 0 |
| WEIGHT | Median | 71.670 | 79.380 | 75.070 | VALUE | 0 |
| WEIGHT | Range | (53.07; 80.74) | (59.88; 102.51) | (53.75; 99.79) | VALUE | 0 |
| WEIGHT | IQ range | (54.430; 78.470) | (78.470; 88.450) | (63.960; 90.720) | VALUE | 0 |

While data driven precision is usually done with a by variable it
doesn’t always have to. The `precisionon` argument can be used to
calculate data driven precision on a single variable. This might be
useful if a table template is going to be used multiple times or if
multiple parts of the table are using a similar call but need to have
different data driven precision. The following example uses the variable
CHG to calculate precision, similar to the above example we still use
`decimal = 4` to cap our decimal spaces at 4.

``` r
tbl <- cdisc_advs |>
  filter(PARAMCD == "SYSBP") |>
  univar(
    colvar = "TRTAN",
    rowvar = "CHG",
    precisionon = "CHG",
    decimal = 4
  )

knitr::kable(tbl)
```

| label     | 0           | 54          | 81           | row_type | group_level |
|:----------|:------------|:------------|:-------------|:---------|------------:|
| N         | 186         | 147         | 204          | N        |           0 |
| Mean (SD) | 0.3 (12.49) | 1.5 (9.87)  | -5.5 (11.86) | VALUE    |           0 |
| Median    | 0.0         | 1.0         | -7.0         | VALUE    |           0 |
| Range     | (-44; 33)   | (-31; 24)   | (-32; 30)    | VALUE    |           0 |
| IQ range  | (-7.0; 8.0) | (-6.0; 7.0) | (-14.0; 0.0) | VALUE    |           0 |

Another use case for the `precisionon` argument could be if you need to
calculate the summary on one variable but use another for precision for
table output formatting. The following example uses both `precisionby`
and `precisionon` to show how they can be used together to make special
tables. For this table, we are creating an element of the table that
summarizes `AVAL` but uses `CHG` to calculate precision. This allows us
to have consistent formatting throughout the table even though the two
variables may have different precision. We also calculate precision by
`PARAMCD` since the output table will be presented using that as a by
variable.

``` r
tbl <- cdisc_advs |>
  filter(PARAMCD == "SYSBP") |>
  univar(
    colvar = "TRTAN",
    rowvar = "AVAL",
    rowbyvar = "PARAMCD",
    precisionby = "PARAMCD",
    precisionon = "CHG",
    decimal = 4
  )

knitr::kable(tbl)
```

| PARAMCD | label | 0 | 54 | 81 | row_type | group_level |
|:---|:---|:---|:---|:---|:---|---:|
| SYSBP | SYSBP |  |  |  | BY_HEADER1 | 0 |
| SYSBP | N | 186 | 147 | 204 | N | 0 |
| SYSBP | Mean (SD) | 132.0 (12.02) | 127.5 (12.58) | 135.8 (23.65) | VALUE | 0 |
| SYSBP | Median | 131.0 | 130.0 | 132.5 | VALUE | 0 |
| SYSBP | Range | (100; 167) | (95; 151) | (95; 198) | VALUE | 0 |
| SYSBP | IQ range | (123.0; 138.0) | (122.0; 137.0) | (116.0; 150.5) | VALUE | 0 |
