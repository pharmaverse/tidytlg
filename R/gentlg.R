#' Output a tidytlg table
#'
#' Generate and output a huxtable with desired properties
#' During this function call, the huxtable can be written to an RTF or
#' displayed in HTML. `gentlg` is vectorized, see parameter descriptions
#' to learn for which arguments.
#'
#' @author Steven Haesendonckx <shaesen2@@its.jnj.com>
#' @author Pelagia Alexandra Papadopoulou <ppapadop@@its.jnj.com>
#'
#' @param huxme (optional) For tables and listings, A list of input dataframes
#' containing all columns of interest. For graphs, either NULL or a  list of ggplot
#' objects. Vectorized.
#' @param tlf (optional) String, representing the output choice. Choices are
#' "Table" "Listing" "Figure". Abbreviations are allowed eg "T" for Table.
#' Strings can be either upper- or lowercase. Vectorized. (Default = "Table")
#' @param format (optional) String, representing the output format. Choices are
#' "rtf" and "html". Strings can be either upper- or lowercase.(Default = "rtf")
#' @param colspan (optional) A list of character vectors representing the
#' spanning headers to be used for the table or listing. The first vector
#' represents the top spanning header, etc. Each vector should have a length
#' equal to the number of columns in the output data frame. A spanning header
#' is identified through the use of the same column name in adjacent elements.
#' Vectorized.
#' @param idvars (optional) Character vector defining the columns of a listing
#' where repeated values should be removed recursively. If NULL then
#' all column names are used in the algorithm. If NA, then the listing remains
#' as is.
#' @param plotnames (optional) Character vector containing the names of the png
#' files, with their extension to be incorporated for figure outputs.
#' The png files need to be located in the path defined by the parameter `opath`.
#' @param plotwidth (optional) Numerical value that indicates the plot width in
#' cm for figure outputs. (Default = 6)
#' @param plotheight (optional) Numerical value that indicates the plot height
#' in cm for figure outputs. (Default = 5)
#' @param wcol (optional) Can be a single numerical value that represents the
#' width of the first column or a vector, specifying the lengths of all columns
#' in the final table or listing.\cr
#' When a single numerical value is used, this will be taken as the column width
#' for the first column. The other columns will be equally spaced across the
#' remainder of the available space. Alternatively, a vector can be used to
#' represent the widths of all columns in the final output. The order of the
#' arguments needs to correspond to the order of the columns in the `huxme`
#' dataset, that are not part of the formatting algorithms
#' (eg anbr, roworder, newpage, newrow, indentme, boldme, by_value, by_order).
#' The sum of the widths in the vector needs to be less or equal to one. When
#' 'format="HTML"' wcol can take only one value, the width of the first column.
#' (Default = 0.45)
#' @param opath (optional) File path pointing to the output files
#' (including .png files for graphs). (Default = ".")
#' @param orientation (optional) String: "portrait" or "landscape".
#' (Default = "portrait")
#' @param file (required) String. Output identifier.
#' File name will be adjusted to be lowercase and have - and _ removed,
#' this will not affect table title.
#' @param title_file An Excel file that will be read in
#' with `readxl::read_excel()` to be used as the `title` and `footers` arugment.
#' The use of `title` or `footers` will override the values passed by this
#' argument. The file should be either an xls or xlsx file with the columns
#' 'TABLE ID', 'IDENTIFIER', and TEXT'. The file will be read in, subset to
#' where the tblid matches the tlf argument, and identifiers with 'title' or
#' 'footnote' will be used to populate the table.
#' @param title (required) String. Title of the output. Vectorized.
#' @param footers (optional) Character vector, containing strings of footnotes
#' to be included. Vectorized.
#' @param print.hux (optional) Logical, indicating whether the output should be
#' printed to RTF ('format' = "rtf") / displayed as HTML ('format' = "HTML").
#' (Default = TRUE) Note that RTF is written using `quick_rtf_jnj()`
#' function and that the HTML is displayed via the huxtable::print_html
#' function.
#' @param watermark (optional) String containing the desired watermark for
#' RTF outputs. Vectorized.
#' @param colheader (optional) Character vector that contains the column labels
#' for a table or listing. Default uses the column labels of huxme. Vectorized.
#' @param pagenum (optional) Logical. When true page numbers are added on the
#' right side of the footer section in the format page x/y.
#' Vectorized. (Default = FALSE)
#' @param bottom_borders (optional) Matrix or `"old_format"`. A matrix indicating where to add the bottom
#' borders. Vectorized. See [add_bottom_borders()] for more information. If `"old_format"`,
#' then borders are added to the `colspan` and `colheader` rows. (Default = "old_format").
#' @param border_fns (optional) List. A list of functions that transform the matrix
#' passed to `bottom_borders`. Vectorized. See [add_bottom_borders()] for more information.
#'
#' @section Huxme Details:
#' For tables and listings, formatting of the output can be dictated through the
#' formatting columns
#' (`newrows`, `indentme`, `boldme`, `newpage`), present in the input dataframe.
#' The final huxtable will display all columns of the input dataframe, except
#' any recognized formatting/sorting columns.
#' For tables, the algorithm uses
#' the column `label` as first column. The remaining columns are treated as
#' summary columns.
#' For graphs, you can pass a ggplot object directly into huxme and gentlg will
#' save a png with with `ggplot2::ggsave()` and output an rtf.
#'
#' @return A list of formatted huxtables with desired properties for output to an RTF/HTML
#' @export
#'
#' @examples
#'
#' final <- data.frame(
#'   label = c(
#'     "Overall", "Safety Analysis Set",
#'     "Any Adverse event{\\super a}", "- Serious Adverse Event"
#'   ),
#'   Drug_A = c("", "40", "10 (25%)", "0"),
#'   Drug_B = c("", "40", "10 (25%)", "0"),
#'   anbr = c(1, 2, 3, 4),
#'   roworder = c(1, 1, 1, 1),
#'   boldme = c(1, 0, 0, 0),
#'   newrows = c(0, 0, 1, 0),
#'   indentme = c(0, 0, 0, 1),
#'   newpage = c(0, 0, 0, 0)
#' )
#'
#' # Produce output in rtf format
#' gentlg(
#'   huxme = final,
#'   wcol = c(0.70, 0.15, 0.15),
#'   file = "TSFAEX",
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   )
#' )
#'
#' # Pass in column headers instead of using variable name
#' gentlg(
#'   huxme = final,
#'   wcol = c(0.70, 0.15, 0.15),
#'   file = "TSFAEX",
#'   colheader = c("", "Drug A", "Drug B"),
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   )
#' )
#'
#' # Add spanning bottom borders under the cells in the second row
#' gentlg(
#'   huxme = final,
#'   wcol = c(0.70, 0.15, 0.15),
#'   file = "TSFAEX",
#'   colheader = c("", "Drug A", "Drug B"),
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   ),
#'   border_fns = list(spanning_borders(2))
#' )
#'
#' # Use a watermark
#' gentlg(
#'   huxme = final,
#'   wcol = c(0.70, 0.15, 0.15),
#'   file = "TSFAEX",
#'   colheader = c("", "Drug A", "Drug B"),
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   ),
#'   watermark = "Confidential"
#' )
#'
#' # Produce output in HTML format
#' hux <- gentlg(
#'   huxme = final,
#'   file = "TSFAEX",
#'   colheader = c("", "Drug A", "Drug B"),
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   ),
#'   watermark = "Confidential",
#'   format = "HTML",
#'   print.hux = FALSE
#' )
#'
#' # Export to HTML page
#' huxtable::quick_html(hux, file = "TSFAEX.html", open = FALSE)
#'
#' # clean up.
#' file.remove("TSFAEX.html", "tsfaex.rtf")
#' @references \url{https://github.com/hughjonesd/huxtable}
gentlg <- function(huxme = NULL,
                   tlf = "Table",
                   format = "rtf",
                   colspan = NULL,
                   idvars = NULL,
                   plotnames = NULL,
                   plotwidth = NULL,
                   plotheight = NULL,
                   wcol = 0.45,
                   orientation = "portrait",
                   opath = ".",
                   title_file = NULL,
                   file = NULL,
                   title = NULL,
                   footers = NULL,
                   print.hux = TRUE,
                   watermark = NULL,
                   colheader = NULL,
                   pagenum = FALSE,
                   bottom_borders = "old_format",
                   border_fns = list()) {
  adjfilename <- stringr::str_replace_all(
    stringr::str_to_lower(file),
    "(-|_)", ""
  )
  if (is.null(huxme)) {
    ht <- gentlg_single(
      huxme = NULL,
      tlf = tlf,
      format = format,
      colspan = colspan,
      idvars = idvars,
      plotnames = plotnames,
      plotwidth = plotwidth,
      plotheight = plotheight,
      wcol = wcol,
      orientation = orientation,
      opath = opath,
      title_file = title_file,
      file = file,
      title = title,
      footers = footers,
      print.hux = print.hux,
      watermark = watermark,
      colheader = colheader,
      pagenum = pagenum,
      bottom_borders = bottom_borders,
      border_fns = border_fns
    )

    if (print.hux == FALSE) {
      return(ht$ht)
    } else if (print.hux == TRUE && is_format_rtf(format)) {
      quick_rtf_jnj(
        list(ht$ht),
        file = paste(file.path(opath, adjfilename), ".rtf", sep = ""),
        pagenum = pagenum,
        portrait = tolower(orientation) == "portrait",
        watermark = list(watermark),
        nheader = 1 + ifelse(is.null(ht$colspan), 0, ht$colspan),
        tlf = tlf,
      )
      return(invisible(NULL))
    } else if (print.hux == TRUE && toupper(format) == "HTML") {
      huxtable::print_html(ht$ht)
      return(invisible(NULL))
    }
  }

  if (inherits(huxme, "data.frame") || inherits(huxme, "ggplot")) {
    huxme <- list(huxme)
  }

  # If we leave NULLs in the arguments
  # then the mapply won't run, so we
  # wrap the NULLs in a list.
  # The same goes for scalar arguments that
  # can be arrays.
  if (!is.list(title)) {
    title <- list(title)
  }
  if (!is.list(footers)) {
    footers <- list(footers)
  }
  if (!is.list(watermark)) {
    watermark <- list(watermark)
  }
  if (!is.list(colheader)) {
    colheader <- list(colheader)
  }
  if (!is.list(bottom_borders)) {
    bottom_borders <- list(bottom_borders)
  }
  assertthat::assert_that(is.list(border_fns))
  if (length(border_fns) == 0 ||
    (length(border_fns) > 0 && !is.list(border_fns[[1]]))) {
    border_fns <- list(border_fns)
  }
  if (
    (is.list(colspan) && length(colspan) > 0 && !is.list(colspan[[1]])) ||
      is.null(colspan)
  ) {
    colspan <- list(colspan)
  }

  hts <- mapply(
    function(ht,
             colspan,
             title,
             footers,
             watermark,
             colheader,
             pagenum,
             bottom_borders,
             border_fns,
             index) {
      gentlg_single(
        huxme = ht,
        tlf = tlf,
        format = format,
        colspan = colspan,
        idvars = idvars,
        plotnames = plotnames,
        plotwidth = plotwidth,
        plotheight = plotheight,
        wcol = wcol,
        orientation = orientation,
        opath = opath,
        title_file = title_file,
        file = file,
        title = title,
        footers = footers,
        print.hux = FALSE,
        watermark = watermark,
        colheader = colheader,
        pagenum = pagenum,
        bottom_borders = bottom_borders,
        border_fns = border_fns,
        index_in_result = index
      )
    },
    huxme,
    colspan,
    title,
    footers,
    watermark,
    colheader,
    pagenum,
    bottom_borders,
    border_fns,
    seq_len(length(huxme)),
    SIMPLIFY = FALSE
  )

  if (print.hux == FALSE) {
    return(lapply(hts, function(ht) ht$ht))
  } else if (print.hux == TRUE && is_format_rtf(format)) {
    quick_rtf_jnj(lapply(hts, function(ht) ht$ht),
      file = paste(file.path(opath, adjfilename), ".rtf", sep = ""),
      pagenum = pagenum,
      portrait = tolower(orientation) == "portrait",
      watermark = watermark,
      nheader = 1 + as.numeric(lapply(hts, function(ht) length(ht$colspan))),
      tlf = tlf,
    )
  } else if (print.hux == TRUE && toupper(format) == "HTML") {
    lapply(hts, huxtable::print_html)
  }
}
