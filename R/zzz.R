#' @importFrom rlang enexprs is_call expr sym %||% exprs syms as_string is_named
#'   eval_tidy enexpr as_name
#' @importFrom tidyr pivot_wider pivot_longer complete nesting nest unnest
#'   replace_na crossing fill
#' @importFrom dplyr select bind_rows ungroup mutate rowwise filter summarise
#'   bind_cols
#' @importFrom dplyr group_by distinct n left_join rename relocate across
#'   if_else
#' @importFrom dplyr arrange case_when desc cur_group_id inner_join rename_with
#'   row_number
#' @importFrom dplyr group_by_at slice mutate_if
#' @importFrom stringr str_detect str_remove str_replace str_split regex
#'   str_to_lower
#' @importFrom tidyselect all_of any_of everything starts_with ends_with
#' @importFrom forcats fct_expand fct_recode fct_relevel
#' @importFrom magrittr extract2
#' @importFrom purrr map walk map_dfr pmap discard compact map_lgl imap map_chr
#' @importFrom tibble tibble tribble add_row tibble_row
#' @importFrom methods formalArgs
#' @importFrom stats setNames lag
#' @importFrom utils head modifyList tail
#' @importFrom huxtable everywhere
#' @importFrom crayon bold underline
#' @importFrom readxl read_excel
#' @importFrom cellranger cell_cols
#' @importFrom png readPNG
#' @importFrom ggplot2 ggsave
#' @importFrom grDevices dev.cur dev.off pdf
#' @importFrom rstudioapi getSourceEditorContext isAvailable
NULL

globalVariables(c(".", "name", "alpha", "value", "N", "pct", "denom", "results",
                  "n_ord", "n_ord2", "n_ord3", "data_nest", "nested_treatment",
                  "nested_level", "nest_treatment", "stat", "extra", "prec",
                  "IDENTIFIER", "TABLE ID", "decode", "colnbr", "newrows", "el",
                  "sorter", "coldef", "V1"))
