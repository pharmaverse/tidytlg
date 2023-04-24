#' Output a tidytlg table
#'
#' Generate and output a huxtable with desired properties
#' During this function call, the huxtable can be written to an RTF or
#' displayed in HTML.
#'
#' @author Steven Haesendonckx {shaesen2@@its.jnj.com}
#' @author Pelagia Alexandra Papadopoulou {ppapadop@@its.jnj.com}
#'
#' @param huxme (optional) For tables and listings, An input dataframe
#' containing all columns of interest. For graphs, either NULL or a ggplot
#' object.
#' @param tlf (optional) String, representing the output choice. Choices are
#' "Table" "Listing" "Figure". Abbreviations are allowed eg "T" for Table.
#' Strings can be either upper- or lowercase. (Default = "Table")
#' @param format (optional) String, representing the output format. Choices are
#' "rtf" and "html". Strings can be either upper- or lowercase.(Default = "rtf")
#' @param colspan (optional) A list of character vectors representing the
#' spanning headers to be used for the table or listing. The first vector
#' represents the top spanning header, etc. Each vector should have a length
#' equal to the number of columns in the output data frame. A spanning header
#' is identified through the use of the same column name in adjacent elements.
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
#' @param title (required) String. Title of the output.
#' @param footers (optional) Character vector, containing strings of footnotes
#' to be included.
#' @param print.hux (optional) Logical, indicating whether the output should be
#' printed to RTF ('format' = "rtf") / displayed as HTML ('format' = "HTML").
#' (Default = TRUE) Note that RTF is written using `quick_rtf_jnj()`
#' function and that the HTML is displayed via the huxtable::print_html
#' function.
#' @param watermark (optional) String containing the desired watermark for
#' RTF outputs.
#' @param colheader (optional) Character vector that contains the column labels
#' for a table or listing. Default uses the column labels of huxme.
#' @param pagenum (optional) Logical. When true page numbers are added on the
#' right side of the footer section in the format page x/y. (Default = FALSE)
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
#' @return Formatted huxtable with desired properties for output to an RTF/HTML

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
                   pagenum = FALSE) {
  # check all the arguments being passed in except ...
  arglist <- list()
  args_to_chk <- names(formals())[names(formals()) != "..."]
  purrr::walk(args_to_chk, .f = {
    function(x) arglist[[x]] <<- eval(rlang::sym(x))
    })
  check_gentlg(arglist)

  if (!is.null(title_file)) {
    title_df <- readxl::read_excel(title_file,
                                   sheet = 1,
                                   range = readxl::cell_cols("A:C"))
    if (!all(c("TABLE ID", "IDENTIFIER", "TEXT") %in% names(title_df))) {
      stop("'title_file' file must have columns: 'TABLE ID',
           'IDENTIFIER', and 'TEXT'")
    }
    title_df <- title_df %>%
      filter(`TABLE ID` == file)

    if (is.null(title) && nrow(title_df) > 0) {
      title <- title_df %>%
        filter(str_detect(IDENTIFIER, regex("title", ignore_case = TRUE))) %>%
        extract2("TEXT")
      if (length(title) == 0) {
        title <- NULL
      }
    }
    if (is.null(footers) && nrow(title_df) > 0) {
      footers <- title_df %>%
        filter(str_detect(IDENTIFIER,
                          regex("^footnote", ignore_case = TRUE))) %>%
        extract2("TEXT")
      if (length(footers) == 0) {
        footers <- NULL
      }
    }
  }

  if (!is.null(attr(huxme, "column_metadata")) && is.null(colspan)) {
    cm <- attr(huxme, "column_metadata")
    cm <- cm[, colSums(is.na(cm)) < nrow(cm)]
    if (any(str_detect(names(cm), "span"))) {
      colspan <- cm %>%
        select(starts_with("span")) %>%
        as.list() %>%
        unname()
      colspan <- colspan[!purrr::map_lgl(colspan, ~ all(is.na(.)))]
      colspan <- purrr::map(colspan, ~ c("", replace_na_with_blank(.))) %>%
        rev()
    }
  }
  if (!is.null(attr(huxme, "column_metadata")) && is.null(colheader)) {
    cm <- attr(huxme, "column_metadata")
    if ("decode" %in% names(cm)) {
      colheader <- cm %>%
        select(decode) %>%
        unlist() %>%
        unname()
      colheader <- c("", colheader)
      names(colheader) <- c("label", paste0("col", seq_len(nrow(cm))))
    }
  }

  if (is.null(colheader)) {
    if (substr(tolower(tlf), 1, 1) == "l") {
      null_labels <- purrr::map_lgl(map(huxme, ~ attr(., "label")), is.null)
      colheader <- names(huxme)
      colheader[!null_labels] <-
        unname(map_chr(huxme[!null_labels], ~ attr(., "label")))
    } else {
      colheader <- c("", names(huxme)[- 1])
    }
  }



  wrote_png <- FALSE
  if (substr(tolower(tlf), 1, 1) %in% c("g", "f") &&
    is.null(plotnames) &&
    inherits(huxme, "ggplot")) {
    a_file <- "__tempggplot__"
    tmpdir <- base::tempdir()
    ggsave(
      filename = paste0(tmpdir, "/__tempggplot__"),
      plot = huxme,
      device = "png",
      width = plotwidth %||% NA,
      height = plotheight %||% NA
    )
    huxme <- NULL
    plotnames <- a_file
    wrote_png <- TRUE
  }

  if (substr(tolower(tlf), 1, 1) %in% c("g", "f") &&
    is.null(plotwidth) &&
    is.null(plotheight)) {
    png_d <- dim(readPNG(ifelse(wrote_png,
                                paste0(tmpdir, "/__tempggplot__"), plotnames)))
    plotheight <- png_d[1]
    plotwidth <- png_d[2]
  }


  huxme <- data.frame(huxme, stringsAsFactors = FALSE, check.names = FALSE)

  # Check if the huxme is blank, this will error out in R < 4
  if (ncol(huxme) > 0) {
    huxme <- huxme %>%
      select(!c(any_of("func"), ends_with("_ord")))
  }

  ###########################
  ### Validation of input ###
  ###########################

  formatcolumns <- c(
    "anbr", "roworder", "boldme", "indentme", "newrows", "newpage",
    "rowvar", "row_type", "nested_level", "group_level"
  )

  # exploratory
  # restrict to only display label and variables that start with col followed by numbers
  # Pro:
  #   This allows users to keep whatever working variables they like and not worry about them
  #   displaying in the output.
  #   We can drop the byvars argument.
  # Con:
  #   You have to rename columns to label or col1-coln


  if (is.null(huxme) && !substr(tolower(tlf), 1, 1) %in% c("g", "f")) {
    stop("huxme argument is required for tables and listings. Consult the documentation via ?gentlg.")
  }

  if (!("label" %in% colnames(huxme)) && substr(tolower(tlf), 1, 1) %in% c("t")) {
    stop("No column present with the name 'label'. Consult the documentation via ?gentlg")
  }

  if (sum(!(colnames(huxme) %in% c(formatcolumns, "label"))) < 1 && !substr(tolower(tlf), 1, 1) %in% c("g", "f")) {
    stop("A column with summary measures are missing. Consult the documentation via ?gentlg.")
  }

  if (!substr(tolower(tlf), 1, 1) %in% c("t", "l", "f", "g")) {
    stop("tlf argument not correctly specified. Consult the documentation via ?gentlg.")
  }

  if (substr(tolower(tlf), 1, 1) %in% c("g", "f") && is.null(plotnames)) {
    stop("plotnames argument not used. Consult the documentation via ?gentlg.")
  }

  if (!is.null(wcol) && !substr(tolower(tlf), 1, 1) %in% c("g", "f")) {
    if ((length(wcol) == 1 && (!is.numeric(wcol) || wcol > 1)) ||
      (length(wcol) > 1 && tolower(format) == "rtf" &&
       (!is.numeric(wcol) ||
        sum(!(colnames(huxme) %in% c(formatcolumns))) != length(wcol) ||
        sum(wcol) > 1))) {
      stop("wcol not defined properly. Consult the documentation via ?gentlg.")
    }
    if (tolower(format) == "html" && length(wcol) > 1) {
      warning("Format = 'HTML'. Only the first argument of wcol will be used. For more information, consult the documentation via ?gentlg.")
    }
  }

  if (!is.null(colspan) && substr(tolower(tlf), 1, 1) %in% c("t", "l") && sum(lapply(colspan, length) == (sum(!(colnames(huxme) %in% c(formatcolumns))))) != length(colspan)) {
    stop("The length of each colspan argument needs to be equal to the amount of columns in the output data frame. Consult the documentation via ?gentlg.")
  }

  if (substr(tolower(tlf), 1, 1) %in% "l" && !is.na(idvars[1]) &&
      sum(!idvars %in% colnames(huxme)) > 0) {
    stop("One of the idvars is not part of the column headers. Consult the documentation via ?gentlg.")
  }

  if (!substr(tolower(tlf), 1, 1) %in% "l" && !is.null(idvars)) {
    stop("idvars argument is not available for non-listing outputs. Consult the documentation via ?gentlg.")
  }

  ###########################
  ### Formatting values   ###
  ###########################

  if ("boldme" %in% colnames(huxme) && length(which(huxme$boldme == 1)) > 0) {
    boldme <- which(huxme$boldme == 1)
  } else {
    boldme <- NULL
  }
  if ("indentme" %in% colnames(huxme) &&
      length(which(huxme$indentme == 0)) > 0) {
    indentme0 <- which(huxme$indentme == 0)
  } else {
    indentme0 <- NULL
  }
  if ("indentme" %in% colnames(huxme) &&
      length(which(huxme$indentme == 1)) > 0) {
    indentme1 <- which(huxme$indentme == 1)
  } else {
    indentme1 <- NULL
  }
  if ("indentme" %in% colnames(huxme) &&
      length(which(huxme$indentme == 2)) > 0) {
    indentme2 <- which(huxme$indentme == 2)
  } else {
    indentme2 <- NULL
  }
  if ("indentme" %in% colnames(huxme) &&
      length(which(huxme$indentme == 3)) > 0) {
    indentme3 <- which(huxme$indentme == 3)
  } else {
    indentme3 <- NULL
  }
  if ("indentme" %in% colnames(huxme) &&
      length(which(huxme$indentme == 4)) > 0) {
    indentme4 <- which(huxme$indentme == 4)
  } else {
    indentme4 <- NULL
  }
  if ("indentme" %in% colnames(huxme) &&
      length(which(huxme$indentme == 5)) > 0) {
    indentme5 <- which(huxme$indentme == 5)
  } else {
    indentme5 <- NULL
  }
  if ("indentme" %in% colnames(huxme) &&
      length(which(huxme$indentme == 6)) > 0) {
    indentme6 <- which(huxme$indentme == 6)
  } else {
    indentme6 <- NULL
  }
  if ("newpage" %in% colnames(huxme) &&
      length(which(huxme$newpage == 1)) > 0) {
    newpage <- which(huxme$newpage == 1)
  } else {
    newpage <- NULL
  }
  if ("newrows" %in% colnames(huxme) &&
      length(which(huxme$newrows == 1)) > 0) {
    newrows <- which(huxme$newrows == 1)
  } else {
    newrows <- NULL
  }

  # display columns columns only
  if (sum(colnames(huxme) %in% formatcolumns) != 0) {
    huxme[, colnames(huxme) %in% formatcolumns] <- NULL
  }

  if (substr(tolower(tlf), 1, 1) == "t") {
    huxme <- huxme[, c(which(colnames(huxme) == "label"),
                       which(!(colnames(huxme) %in% "label")))]
  }

  #############################
  ###       Translate       ###
  #############################

  .to_html <- function(df) {
    list_df <- df %>%
      as.list() %>%
      lapply(gsub, pattern = "(\\{\\\\super)(.*?)(\\})",
             replacement = "<sup>\\2</sup>") %>%
      lapply(gsub, pattern = "(\\{\\\\sub)(.*?)(\\})",
             replacement = "<sub>\\2</sub>") %>%
      lapply(gsub, pattern = "\\\\n", replacement = "<br/>") %>%
      lapply(gsub, pattern = "\\\\line", replacement = "<br/>")

    if (base::inherits(df, "data.frame")) {
      df <- list_df %>%
        data.frame(stringsAsFactors = FALSE, check.names = FALSE)
    } else if (base::inherits(df, "character") && !is.null(df)) {
      df <- list_df %>%
        unlist(use.names = FALSE)
    }

    return(df)
  }

  if (toupper(format) == "HTML") {
    huxme <- .to_html(huxme)
    if (!is.null(huxme) && dim(huxme)[1] > 0) huxme <- .to_html(huxme)
    if (!is.null(footers)) footers <- .to_html(footers)
    if (!is.null(colspan)) colspan <- .to_html(colspan)
    if (!is.null(title)) title <- .to_html(title)
    if (!is.null(colheader)) colheader <- .to_html(colheader)
    if (!is.null(idvars)) idvars <- .to_html(idvars)
  }

  #############################
  ###       Plotit!         ###
  #############################

  if (!is.null(plotnames) && tolower(substr(tlf, 1, 1)) %in% c("g", "f")) {
    pngrtfcode <- function(file = NULL, width = width, height = height) {
      # return a hexadecimal version of a file
      max_bytes <- 100000000 # maximum file size in bytes (~100MB)
      hexcode <- readBin(file, what = "raw", size = 1, signed = TRUE,
                         endian = "little", n = max_bytes)
      paste0("\\qc{\\pict\\pngblip\\picwgoal", round(width * 1440, 1),
             "\\pichgoal", round(height * 1440, 1), " ",
             paste0(hexcode, collapse = ""), "}")
    }

    huxme <- data.frame(cbind(col1 = seq_along(plotnames)))

    for (i in seq_along(plotnames)) {
      if (toupper(format) == "RTF") {
        huxme[i, ] <- paste0("", pngrtfcode(ifelse(wrote_png, paste0(tmpdir, "/", plotnames[i]), plotnames[i]), width = plotwidth, height = plotheight), "\\line")
      } else {
        huxme[i, ] <- paste0("<center>", "<img src=\"", ifelse(wrote_png, paste0(tmpdir, "/", plotnames[i]), plotnames[i]), "\"", " alt=\"", file, "\" style=\"width:", round(plotwidth * 96 / 2.54, 0), "px; height:", round(plotheight * 96 / 2.54, 0), "px; \"> <br/>", "</center>")
      }
    }
  }

  #############################
  ###       idvars          ###
  #############################

  if (tolower(substr(tlf, 1, 1)) %in% c("l")) {
    if (is.null(idvars)) {
      idvars <- colnames(huxme)
    } else if (!is.na(idvars[1])) {
      if (sum(idvars != colnames(huxme)[which(idvars %in% colnames(huxme))]) > 0) {
        idvars <- idvars[order(match(idvars, colnames(huxme)[which(idvars %in% colnames(huxme))]))]
      }
    }

    if (!is.na(idvars[1]) && dim(huxme)[1] >= 1) {
      for (i in rev(seq_len(length(idvars)))) {
        less <- idvars[1:i]
        huxme[duplicated(huxme[, less]), less] <- ""
      }
    } else if (is.na(idvars[1]) || dim(huxme)[1] < 1) {
      huxme <- huxme
    }
  }

  #############################
  ###         No data?      ###
  #############################

  ### Empty dataset: If there is no data then display "No data to report"
  if (!(tolower(substr(tlf, 1, 1)) %in% c("f", "g"))) {
    if (dim(huxme)[1] < 1) {
      col_names <- colnames(huxme)
      huxme <- data.frame(lapply(huxme, as.character), stringsAsFactors = FALSE)
      huxme[nrow(huxme) + 1, 1] <- "No data to report"
      colnames(huxme) <- col_names
    }
  }

  #############################
  ### Orientation of output ###
  #############################
  if (tolower(format) == "rtf") {
    if (tolower(orientation) == "portrait") {
      huxwidth <- 1.06352
    } else if (tolower(orientation) == "landscape") {
      huxwidth <- 1.47
    } else {
      stop("orientation can be only portrait or landscape")
    }
  } else if (tolower(format) == "html") {
    if (tolower(orientation) == "portrait") {
      huxwidth <- 0.65 # 0.5
    } else if (tolower(orientation) == "landscape") {
      huxwidth <- 1 # 0.8
    } else {
      stop("orientation can be only portrait or landscape")
    }
  }

  #############################
  ###       Huxit!          ###
  #############################
  if (tolower(format) == "rtf") {
    if (tolower(substr(tlf, 1, 1)) %in% c("t")) {
      ht <- huxtable::as_hux(huxme, add_colnames = TRUE) %>%
        huxtable::set_width(value = huxwidth)
      if (ncol(ht) == length(colheader)) {
        ht[1, ] <- colheader
      } else {
        usethis::ui_warn("Column header not used; {length(colheader)} column header provided, but data contain {ncol(ht)} columns")
      }
      ht[1, ] <- paste0("\\keepn\\trhdr ", ht[1, ]) # Make repeated treatments on each page
      formatindex <- 1
    } else if (tolower(substr(tlf, 1, 1)) %in% c("l")) {
      ht <- huxtable::as_hux(huxme, add_colnames = TRUE) %>%
        huxtable::set_width(value = huxwidth)
      ht[1, ] <- colheader
      ht[1, ] <- paste0("\\keepn\\trhdr ", ht[1, ]) # Make repeated treatments on each page
      formatindex <- 1
    } else if (tolower(substr(tlf, 1, 1)) %in% c("f", "g")) {
      ht <- huxtable::as_hux(huxme, add_colnames = FALSE) %>%
        huxtable::set_width(value = huxwidth)
    } else {
      stop("tlf can have following character values: Table, Listing, Graph, Figure")
    }
  } else if (tolower(format) == "html") {
    if (tolower(substr(tlf, 1, 1)) %in% c("t")) {
      ht <- huxtable::as_hux(huxme, add_colnames =  TRUE) %>%
        huxtable::set_width(value = huxwidth)
      if (ncol(ht) == length(colheader)) {
        ht[1, ] <- colheader
      } else {
        usethis::ui_warn("Column header not used; {length(colheader)} column header provided, but data contain {ncol(ht)} columns")
      }

      formatindex <- 1
    } else if (tolower(substr(tlf, 1, 1)) %in% c("l")) {
      ht <- huxtable::as_hux(huxme, add_colnames =  TRUE) %>%
        huxtable::set_width(value = huxwidth)
      ht[1, ] <- colheader
      formatindex <- 1
    } else if (tolower(substr(tlf, 1, 1)) %in% c("f", "g")) {
      ht <- huxtable::as_hux(huxme, add_colnames = FALSE) %>%
        huxtable::set_width(value = huxwidth)
    } else {
      stop("tlf can have following character values: Table, Listing, Graph, Figure")
    }
  }

  ht <- ht %>%
    huxtable::set_right_padding(value = getOption("tidytlg.right.padding")) %>%
    huxtable::set_left_padding(value = getOption("tidytlg.left.padding"))

  #############################
  ###       Column Width    ###
  #############################

  if (tolower(format) == "rtf") {
    if (length(wcol) == 1) {
      huxtable::col_width(ht) <- c(wcol, rep((1 - wcol) / (ncol(ht) - 1),
                                             ncol(ht) - 1))
    } else if (length(wcol) > 1) {
      huxtable::col_width(ht) <- wcol
    }
  } else if (tolower(format) == "html") {
    if (!(tolower(substr(tlf, 1, 1)) %in% c("f", "g"))) {
      huxtable::col_width(ht) <- c(wcol[1], rep((1 - wcol[1]) / (ncol(ht) - 1),
                                                ncol(ht) - 1))
    }
  }

  #############################
  ###       Colspan         ###
  #############################

  if (!is.null(colspan) && !(tolower(substr(tlf, 1, 1)) %in% c("f", "g"))) {
    cnames <- colnames(ht)

    ### add row one by one to maintain huxtable structure
    if (tolower(format) == "rtf") {
      for (i in rev(seq_len(length(colspan)))) {
        ht <- huxtable::insert_row(ht, paste0("\\keepn\\trhdr ", colspan[[i]]),
                                   after = 0, fill = "") %>%
          huxtable::set_number_format(row = 1, col = seq_len(ncol(ht)), value = NA)
      }
    } else if (tolower(format) == "html") {
      ### add row one by one to maintain huxtable structure
      for (i in rev(seq_len(length(colspan)))) {
        ht <- huxtable::insert_row(ht, paste0(colspan[[i]]), after = 0)
      }
    }

    rle_colspan <- lapply(colspan, rle)

    for (j in seq_along(colspan)) {
      new <- data.frame(colspan = rep(rle_colspan[[j]]$values,
                                      rle_colspan[[j]]$lengths),
                        rle = rep(rle_colspan[[j]]$lengths,
                                  rle_colspan[[j]]$lengths))
      ### remove only adjacent duplicates in cases such as A A _ A A
      new <- new[with(new, c(colspan[- 1] != colspan[- nrow(new)], TRUE)), ]
      ### merge only for those with occurence > 1
      new <- new[which(new$rle > 1), ]

      if (dim(new)[1] != 0) {
        new$end <- as.numeric(row.names(new))
        new$start <- new$end + 1 - new$rle
        for (k in 1:dim(new)[1]) {
          ht <- huxtable::merge_cells(ht, j, new$start[k]:new$end[k])
        }
      }
    }

    colnames(ht) <- cnames
    formatindex <- formatindex + length(colspan)
  }

  #############################
  ###       Formatting      ###
  #############################

  ### font size
  if ((tolower(substr(tlf, 1, 1)) %in% "l")) {
    ht <- ht %>%
      huxtable::set_font_size(value = getOption("tidytlg.fontsize.listing"))
  } else {
    ht <- ht %>%
      huxtable::set_font_size(value = getOption("tidytlg.fontsize.table"))
  }

  if (!substr(tolower(tlf), 1, 1) %in% c("g", "f")) {
    if (tolower(format) == "rtf") {
      ### bolding
      if (!is.null(boldme) && tolower(substr(tlf, 1, 1)) %in% c("t")) {
        ht <- huxtable::set_bold(ht, boldme + formatindex, "label",
                                 value = TRUE)
      }

      ### newpage
      if (!is.null(newpage)) {
        newpage <- newpage[order(newpage)]
        if (tolower(substr(tlf, 1, 1)) %in% c("l", "t")) {
          newpage <- (newpage + formatindex)
        }
        for (i in seq_along(newpage)) {
          ht[newpage[i], 1] <- paste0("\\pagebb ", ht[newpage[i], 1])
        }
      }
    } else if (tolower(format) == "html") {
      ### bolding
      if (!is.null(boldme) && tolower(substr(tlf, 1, 1)) %in% c("t")) {
        huxtable::bold(ht[boldme + formatindex, "label"]) <- TRUE
      } else {
        # Sometimes gives warnings if bold me is null
        suppressWarnings(huxtable::bold(ht[boldme + formatindex, ]) <- TRUE)
      }
    }
  }

  ### Indentation
  #  The indent is set by (180/1440 = 0.125in) time the value of the indent variable,
  #  plus an additional hanging indent of (87/1440=.06in).
  #  (from LSAF macros)

  # make these options too?
  space <- 180
  hang <- 87
  px <- 17.76

  if (!is.null(indentme0)) {
    indentme0 <- indentme0[order(indentme0)]
    if (tolower(substr(tlf, 1, 1)) %in% c("l", "t")) {
      indentme0 <- (indentme0 + formatindex)
    }
    if (tolower(format) == "rtf") {
      for (i in seq_along(indentme0)) {
        ht[indentme0[i], 1] <- glue::glue("\\intbl\\li{hang}\\fi-{hang} {ht[indentme0[i], 1]}")
      }
    } else if (tolower(format) == "html") {
      for (i in seq_along(indentme0)) {
        ht[indentme0[i], 1] <- glue::glue("<div style='text-indent: -{px}px; padding-left: {px}px'> {ht[indentme0[i], 1]}")
      }
    }
  }

  if (!is.null(indentme1)) {
    indentme1 <- indentme1[order(indentme1)]
    if (tolower(substr(tlf, 1, 1)) %in% c("l", "t")) {
      indentme1 <- (indentme1 + formatindex)
    }
    if (tolower(format) == "rtf") {
      for (i in seq_along(indentme1)) {
        ht[indentme1[i], 1] <- glue::glue("\\intbl\\li{space+hang}\\fi-{hang} {ht[indentme1[i], 1]}")
      }
    } else if (tolower(format) == "html") {
      for (i in seq_along(indentme1)) {
        ht[indentme1[i], 1] <- glue::glue("<div style='text-indent: -{px}px; padding-left: {px*2}px'> {ht[indentme1[i], 1]}")
      }
    }
  }

  if (!is.null(indentme2)) {
    indentme2 <- indentme2[order(indentme2)]
    if (tolower(substr(tlf, 1, 1)) %in% c("l", "t")) {
      indentme2 <- (indentme2 + formatindex)
    }
    if (tolower(format) == "rtf") {
      for (i in seq_along(indentme2)) {
        ht[indentme2[i], 1] <- glue::glue("\\intbl\\li{space*2+hang}\\fi-{hang} {ht[indentme2[i], 1]}")
      }
    } else if (tolower(format) == "html") {
      for (i in seq_along(indentme2)) {
        ht[indentme2[i], 1] <- glue::glue("<div style='text-indent: -{px}px; padding-left: {px*3}px'> {ht[indentme2[i], 1]}")
      }
    }
  }

  if (!is.null(indentme3)) {
    indentme3 <- indentme3[order(indentme3)]
    if (tolower(substr(tlf, 1, 1)) %in% c("l", "t")) {
      indentme3 <- (indentme3 + formatindex)
    }
    if (tolower(format) == "rtf") {
      for (i in seq_along(indentme3)) {
        ht[indentme3[i], 1] <- glue::glue("\\intbl\\li{space*3+hang}\\fi-{hang} {ht[indentme3[i], 1]}")
      }
    } else if (tolower(format) == "html") {
      for (i in seq_along(indentme3)) {
        ht[indentme3[i], 1] <- glue::glue("<div style='text-indent: -{px}px; padding-left: {px*4}px'> {ht[indentme3[i], 1]}")
      }
    }
  }

  if (!is.null(indentme4)) {
    indentme4 <- indentme4[order(indentme4)]
    if (tolower(substr(tlf, 1, 1)) %in% c("l", "t")) {
      indentme4 <- (indentme4 + formatindex)
    }
    if (tolower(format) == "rtf") {
      for (i in seq_along(indentme4)) {
        ht[indentme4[i], 1] <- glue::glue("\\intbl\\li{space*4+hang}\\fi-{hang} {ht[indentme4[i], 1]}")
      }
    } else if (tolower(format) == "html") {
      for (i in seq_along(indentme4)) {
        ht[indentme4[i], 1] <- glue::glue("<div style='text-indent: -{px}px; padding-left: {px*5}px'> {ht[indentme4[i], 1]}")
      }
    }
  }

  if (!is.null(indentme5)) {
    indentme5 <- indentme5[order(indentme5)]
    if (tolower(substr(tlf, 1, 1)) %in% c("l", "t")) {
      indentme5 <- (indentme5 + formatindex)
    }
    if (tolower(format) == "rtf") {
      for (i in seq_along(indentme5)) {
        ht[indentme5[i], 1] <- glue::glue("\\intbl\\li{space*5+hang}\\fi-{hang} {ht[indentme5[i], 1]}")
      }
    } else if (tolower(format) == "html") {
      for (i in seq_along(indentme5)) {
        ht[indentme5[i], 1] <- glue::glue("<div style='text-indent: -{px}px; padding-left: {px*6}px'> {ht[indentme5[i], 1]}")
      }
    }
  }

  if (!is.null(indentme6)) {
    indentme6 <- indentme6[order(indentme6)]
    if (tolower(substr(tlf, 1, 1)) %in% c("l", "t")) {
      indentme6 <- (indentme6 + formatindex)
    }
    if (tolower(format) == "rtf") {
      for (i in seq_along(indentme6)) {
        ht[indentme6[i], 1] <- glue::glue("\\intbl\\li{space*6+hang}\\fi-{hang} {ht[indentme6[i], 1]}")
      }
    } else if (tolower(format) == "html") {
      for (i in seq_along(indentme6)) {
        ht[indentme6[i], 1] <- glue::glue("<div style='text-indent: -{px}px; padding-left: {px*7}px'> {ht[indentme6[i], 1]}")
      }
    }
  }

  ### New rows
  if (!is.null(newrows)) {
    newrows <- newrows[order(newrows)]
    newrows <- newrows + formatindex - 1
    ht2 <- ht[formatindex, ]
    ht2[1, ] <- ""
    for (i in 1:length(newrows)) {
      line <- length(newrows[newrows < newrows[i]]) + newrows[i]
      ht <- huxtable::add_rows(ht, ht2, after = line)
    }
  }

  #############################
  ###       Header          ###
  #############################

  add_header <- function(dsnin, header) {
    if (tolower(format) == "rtf") {
      tmp <- t(huxtable::as_hux(c(header, rep("", ncol(dsnin) - 1)),
                                add_colnames = FALSE))
      huxtable::col_width(tmp) <- huxtable::col_width(dsnin)
      huxtable::width(tmp) <- huxtable::width(dsnin)
      huxtable::font_size(tmp) <- getOption("tidytlg.fontsize.title")
      huxtable::bold(tmp) <- TRUE
      ret <- huxtable::add_rows(dsnin, tmp, after = 0)
      ret <- ret[1:nrow(ret), ]
      return(ret)
    } else {
      ht2 <- dsnin[1, ]
      ht2[1, ] <- ""
      dsnin <- rbind(dsnin, ht2)
      dsnin <- huxtable::add_footnote(dsnin,
        bquote(.(header)),
        number_format = NA,
        bold =  TRUE,
        border = 0,
        font_size = getOption("tidytlg.fontsize.title")
      )
      dsnin <- rbind(dsnin[nrow(dsnin), ], dsnin[1:(nrow(dsnin) - 1), ])
      dsnin <- dsnin[seq_len(nrow(dsnin) - 1), ]
      return(dsnin)
    }
  }

  if (tolower(format) == "rtf") {
    # Used to prevent the Rplot.pdf from outputting in batch.
    if (dev.cur() == 1) {
      # If there is no graphics dev available. Create a NULL PDF device
      pdf(NULL)
      filin <- round(
        1440 / graphics::strwidth(
          expression(huxtable::bold(paste0(file, ":"))),
          family = "serif",
          font = getOption("tidytlg.fontsize.graph"),
          units = "in"
        ),
        0
      )
      # Close the device
      dev.off()
    } else {
      filin <- round(
        1440 / graphics::strwidth(
          expression(huxtable::bold(paste0(file, ":"))),
          family = "serif",
          font = getOption("tidytlg.fontsize.graph"),
          units = "in"
        ),
        0
      )
    }



    # Make repeated header on each page
    ht <- add_header(ht, paste0("\\pnhang\\trhdr\\fi-1152", "\\li1152\\keepn",
                                "\\s15 ", file, ":\\tab", " ", title))
  } else if (tolower(format) == "html") {
    # Make repeated header on each page
    ht <- add_header(ht, paste0('<div style = "text-indent: -36px; padding-left: 36px;"> ', file, ": &emsp; ", title, "</div> "))
  }


  #############################
  ###      Table borders    ###
  #############################
  if (tolower(format) == "rtf") {
    bordervalue <- ifelse(tlf %in% "l", getOption("tidytlg.fontsize.listing"),
                          getOption("tidytlg.fontsize.table")) / 10

    ht <- huxtable::set_top_border(ht, 1, value = bordervalue) %>%
      huxtable::set_bottom_border(1, value = bordervalue) %>%
      huxtable::set_bottom_border(nrow(ht), value = bordervalue)
  } else if (tolower(format) == "html") {
    ht[1, ] <- paste0("<div style='border-top :1pt solid; border-bottom :1pt solid; '> ", ht[1, ])
  }
  #############################
  ###       Footers         ###
  #############################
  fontsize <- switch(tolower(substr(tlf, 1, 1)),
    "t" = getOption("tidytlg.fontsize.table.footnote"),
    "l" = getOption("tidytlg.fontsize.listing.footnote"),
    "g" = getOption("tidytlg.fontsize.graph.footnote"),
    "f" = getOption("tidytlg.fontsize.graph.footnote")
  )

  if (tolower(format) == "rtf") {
    add_footer <- function(dsnin, footer, first = FALSE, size = fontsize) {
      if (first) {
      # In case hanging indent is required + hard enter: \\par\\pard\\pnhang\\fi-180\\li180
          dsnin <- huxtable::add_footnote(dsnin, paste0("\\line ", footer),
                                          number_format = NA, font_size = size,
                                          border = 0)
        } else {
        dsnin <- huxtable::add_footnote(dsnin, footer, number_format = NA,
                                        font_size = size, border = 0)
      }
      return(dsnin)
    }
  } else if (tolower(format) == "html") {
    add_footer <- function(dsnin, footer, first = FALSE, size = fontsize) {
      if (first) {
      # In case hanging indent is required + hard enter:
        #\\par\\pard\\pnhang\\fi-180\\li180

          dsnin <- huxtable::add_footnote(dsnin, paste0("<div style='border-top:1pt solid;'> ", "<br />", footer), number_format = NA, font_size = size, border = 0)
        } else {
        dsnin <- huxtable::add_footnote(dsnin, footer, number_format = NA,
                                        font_size = size, border = 0)
      }
      return(dsnin)
    }
  }

  if (!is.null(footers)) {
    ht <- add_footer(ht, footers[1], first =  TRUE)

    if (length(footers) > 1) {
      for (i in 2:length(footers)) {
        ht <- add_footer(ht, footers[i], first = FALSE)
      }
    }
  }

  adjfilename <- stringr::str_replace_all(stringr::str_to_lower(file),
                                          "(-|_)", "")


  ifelse(is.null(ht), ht <- ht,
    ifelse(getOption("tidytlg.add_datetime"),
      ht <- add_footer(ht, paste0("[", adjfilename, ".", tolower(format), "]",
                                  paste0("[", getFileName(), "] "),
                                  toupper(format(Sys.time(),
                                                 format = "%d%b%Y, %H:%M"))),
                       size = 8),
      ht <- ht
    )
  )


  if (tolower(format) == "rtf") {
    ht <- huxtable::set_top_border(ht, nrow(ht) - length(footers),
                                   value = huxtable::brdr(bordervalue,
                                                          color = "black"))
    ht <- huxtable::set_top_border(ht, nrow(ht),
                                   value = huxtable::brdr(bordervalue,
                                                          color = "black"))
  } else if (tolower(format) == "html") {
    ht[nrow(ht), ] <- paste0("<div style='border-bottom:1pt solid'> ",
                             ht[nrow(ht), ])
  }

  #############################
  ###       Alignments      ###
  #############################

  if (tolower(substr(tlf, 1, 1)) %in% c("t", "l")) {
    ht <- huxtable::set_valign(ht,seq_len(nrow(ht)), seq_len(ncol(ht)),
                               value = "bottom")

  }

  if (tolower(format) == "rtf") {
    if (tolower(substr(tlf, 1, 1)) %in% c("t")) {
      ht <- huxtable::set_align(ht, 2:nrow(ht), 2:ncol(ht), "center")

      for (z in 2:(1 + formatindex)) {
        borderlineme <- 2:ncol(ht)
        noborderline <- which(ht[z, 2:ncol(ht)] == "\\keepn\\trhdr ")

        if (length(noborderline) > 0) {
          borderlineme <- borderlineme[- noborderline]
        }

        ht[z, borderlineme] <- paste0("\\brdrb\\brdrs ", ht[z, borderlineme])
      }
    } else if (tolower(substr(tlf, 1, 1)) %in% c("l")) {
      ht <- huxtable::set_align(ht, 2:nrow(ht), 2:ncol(ht), "center")
      # Top center per JCEV-12. Only top align the first two rows, the title and
      # the column header, plus whatever colspans there are.
      ht <- huxtable::set_valign(ht, (2 + length(colspan)):nrow(ht),
                                 seq_len(ncol(ht)),
                                 "top")
      ht <- huxtable::set_valign(ht, 1:(2 + length(colspan)),
                                 seq_len(ncol(ht)),
                                 "bottom")

      for (z in 2:(1 + formatindex)) {
        borderlineme <- seq_len(ncol(ht))
        noborderline <- which(ht[z, ] == "\\keepn\\trhdr ")

        if (length(noborderline) > 0) {
          borderlineme <- borderlineme[- noborderline]
        }

        ht[z, borderlineme] <- paste0("\\brdrb\\brdrs ", ht[z, borderlineme])
      }
    } else if (tolower(substr(tlf, 1, 1)) %in% c("f", "g")) {
      ht <- huxtable::set_align(ht, 2:nrow(ht), seq_len(ncol(ht)), "center")
    }
  } else if (tolower(format) == "html") {
    if (tolower(substr(tlf, 1, 1)) %in% c("t")) {
      ht <- huxtable::set_align(ht, 2:nrow(ht), 2:ncol(ht), "center")

      for (z in 2:(1 + formatindex)) {
        borderlineme <- 2:ncol(ht)
        ht[z, borderlineme] <- paste0("<div style='border-bottom:1pt solid'> ",
                                      ht[z, borderlineme])
      }
    } else if (tolower(substr(tlf, 1, 1)) %in% c("l")) {
      ht <- huxtable::set_align(ht, 2:nrow(ht), 2:ncol(ht), "center")

      for (z in 2:(1 + formatindex)) {
        borderlineme <- seq_len(ncol(ht))
        ht[z, borderlineme] <- paste0("<div style='border-bottom:1pt solid'> ",
                                      ht[z, borderlineme])
      }
    }
  }

  # Alignment first column + footers is left
  ht <- huxtable::set_align(ht, seq_len(nrow(ht)), 1, "left")
  if (getOption("tidytlg.add_datetime")) {
    ht <- huxtable::set_align(ht, nrow(ht), seq_len(ncol(ht)), "right")
  } # Alignment fileloc, datetime to the right


  #############################
  ###       Escapeme!       ###
  #############################

  huxtable::escape_contents(ht) <- FALSE

  #############################
  ###       Returnme!       ###
  #############################


  # shiny needs to be able to save to object so return(ht) for both HTML and RTF
  # static rtf, just needs to run quick_rtf_jnj
  if (print.hux == FALSE) {
    return(ht)
  } else if (print.hux == TRUE && toupper(format) == "RTF") {
    quick_rtf_jnj(ht,
      file = paste(file.path(opath, adjfilename), ".rtf", sep = ""),
      pagenum = pagenum,
      portrait = tolower(orientation) == "portrait",
      watermark = watermark,
      nheader = 1 + length(colspan),
      tlf = tlf
    )
  } else if (print.hux == TRUE && toupper(format) == "HTML") {
    huxtable::print_html(ht)
  }
  if (exists("a_file") && file.exists(a_file)) file.remove(a_file)
}
