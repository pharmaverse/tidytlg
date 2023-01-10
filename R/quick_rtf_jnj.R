# This file includes utility functions from the huxtable package for creating rtf output
#' @references \url{https://github.com/hughjonesd/huxtable}
#'

# merge header updates rtf output to add merging tags to the title row
#' merger_header
#'
#' @param result The current RTF output
#'
#' @return Returns the RTF output but with the Title rows merged
#' @noRd
merger_header <- function(result) {
  result_sectioned <- result %>%
    stringr::str_split("\\\\row") %>%
    base::unlist() # breakes apart by section
  result_sectioned[1] <- result_sectioned[1] %>%
    stringr::str_replace_all("\\\\clbrdrt", "\\\\clmrg\\\\clbrdrt") %>%
    stringr::str_replace("\\\\clmrg\\\\clbrdrt", "\\\\clmgf\\\\clbrdrt")
  ret <- base::paste0(result_sectioned, collapse = "\\row")
  return(ret)
}

# Adds padding to `colvar` row to add line break between `colvar` headers
#' pad_header
#'
#' @param result The current RTF output
#' @param nheader Number of headers which is equal to `colspan` + 1
#'
#' @return Returns the RTF output but with padding added to colvar column
#' @noRd
pad_header <- function(result, nheader) {
  result_sectioned <- result %>%
    stringr::str_split("\\\\row") %>%
    base::unlist() # breakes apart by section

  result_sectioned[nheader + 1] <- result_sectioned[nheader + 1] %>%
    stringr::str_replace_all("\\\\cellx", "\\\\clpadt67\\\\clpadft3\\\\clpadr67\\\\clpadfr3\\\\cellx")
  result_sectioned[nheader + 1] <- result_sectioned[nheader + 1] %>%
    stringr::str_replace("\\\\clpadt67\\\\clpadft3\\\\clpadr67\\\\clpadfr3\\\\cellx", "\\\\cellx")

  ret <- base::paste0(result_sectioned, collapse = "\\row")

  return(ret)
}

ncharw <- function(x) nchar(x, type = "width")

blank_where <- function(text, cond) {
  stopifnot(length(text) == length(cond))
  text[cond] <- ""
  text
}

str_rep <- function(x, times) {
  mapply(function(s, t) paste0(rep(s, t), collapse = ""), x, times)
}

# pinched from HMS. Registers the method or sets a hook to
# register it on load of other package
register_s3_method <- function(pkg, generic, class = "huxtable") {
  assertthat::assert_that(assertthat::is.string(pkg),
                          assertthat::is.string(generic))
  fun <- get(paste0(generic, ".", class), envir = parent.frame())

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  setHook(packageEvent(pkg, "onLoad"), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  })
}

utf8_to_rtf <- function(mx) {
  utf8_codes <- function(x) utf8ToInt(enc2utf8(x))

  rtf_encode <- function(x) {
    code <- utf8_codes(x)
    x <- strsplit(x, split = "")[[1]]
    x[code > 127L & code <= 32767L] <- code[code > 127L & code <= 32767L]
    x[code > 32767L] <- code[code > 32767L] - 65536L
    x[code > 127L] <- paste0("\\u", x[code > 127L], "?")
    paste0(x, collapse = "")
  }

  needs_conv <- vapply(
    c(mx), function(x) any(utf8_codes(x) > 127L),
    logical(1)
  )
  mx[needs_conv] <- vapply(
    mx[needs_conv], rtf_encode,
    character(1)
  )

  mx
}


# return character matrix of formatted contents, suitably escaped
clean_contents <- function(ht,
                           type = c("latex", "html", "screen", "markdown",
                                    "word", "excel", "rtf"),
                           ...) {
  type <- match.arg(type)
  contents <- as.matrix(as.data.frame(ht))

  for (col in seq_len(ncol(contents))) {
    for (row in seq_len(nrow(contents))) {
      cell <- contents[row, col]
      num_fmt <- huxtable::number_format(ht)[[row, col]]
      cell <- format_numbers(cell, num_fmt)
      if (is.na(cell)) cell <- huxtable::na_string(ht)[row, col]
      contents[row, col] <- as.character(cell)
    }
  }
  contents[is.na(contents)] <- huxtable::na_string(ht)

  for (col in seq_len(ncol(contents))) {
    if (type %in% c("latex", "html", "rtf")) {
      to_esc <- huxtable::escape_contents(ht)[, col]
      contents[to_esc, col] <- huxtable::sanitize(contents[to_esc, col], type)
    }
    # has to be after sanitization because we add &nbsp; for HTML
    # later we can just use align for this:
    pad_chars <- 0
    align_pad <- ncharw(huxtable::align(ht)[, col]) == 1
    pad_chars[align_pad] <- huxtable::align(ht)[align_pad, col]
    contents[, col] <- decimal_pad(contents[, col], pad_chars, type)
  }

  if (type == "rtf") {
    contents <- utf8_to_rtf(contents)
  }

  contents
}

format_color <- function(r_color, default = "white") {
  r_color[is.na(r_color)] <- default
  apply(grDevices::col2rgb(r_color), 2, paste0, collapse = ", ")
}

# returns two rows(+1),cols(+1) arrays of border widths
collapsed_borders <- function(ht) {
  result <- do_collapse(ht, get_all_borders, default = 0)
  result$vert <- pmax(result$left, result$right)
  result$horiz <- pmax(result$top, result$bottom)

  result[c("vert", "horiz")]
}

# returns two rows(+1),cols(+1) arrays of border colors.
# right and top borders have priority.
# A border of 0 can still have a color.
collapsed_border_colors <- function(ht) {
  result <- do_collapse(ht, get_all_border_colors, default = NA)
  result$vert <- result$right
  result$vert[is.na(result$right)] <- result$left[is.na(result$right)]
  result$horiz <- result$bottom
  result$horiz[is.na(result$bottom)] <- result$top[is.na(result$bottom)]

  result[c("vert", "horiz")]
}

# returns two rows(+1),cols(+1) arrays of border styles.
# Non-"solid" styles have priority;
# if two styles are non-"solid" then right and top has priority
# A border of 0 can still have a style.
collapsed_border_styles <- function(ht) {
  result <- do_collapse(ht, get_all_border_styles, default = "solid")
  result$vert <- result$right
  result$vert[result$right == "solid"] <- result$left[result$right == "solid"]
  result$horiz <- result$bottom
  result$horiz[result$bottom == "solid"] <- result$top[result$bottom == "solid"]

  result[c("vert", "horiz")]
}

do_collapse <- function(ht, prop_fun, default) {
  res <- list()
  res$top <- res$left <- res$right <- res$bottom <- matrix(default, nrow(ht),
                                                           ncol(ht))
  dc <- display_cells(ht, all = TRUE)
  # provides large speedup:
  dc <- as.matrix(dc[, c("row", "col", "display_row", "display_col", "end_row",
                         "end_col")])
  dc_idx <- dc[, c("display_row", "display_col"), drop = FALSE]
  dc_map <- matrix(seq_len(nrow(ht) * ncol(ht)), nrow(ht), ncol(ht))
  dc_map <- dc_map[dc_idx]

  at <- list()
  at$left <- dc[, "col"] == dc[, "display_col"]
  at$right <- dc[, "col"] == dc[, "end_col"]
  at$top <- dc[, "row"] == dc[, "display_row"]
  at$bottom <- dc[, "row"] == dc[, "end_row"]

  properties <- prop_fun(ht)
  for (side in names(at)) {
    at_side <- at[[side]]
    res[[side]][at_side] <- properties[[side]][dc_map][at_side]
  }

  res$left <- cbind(res$left, default)
  res$right <- cbind(default, res$right)
  res$top <- rbind(res$top, default)
  res$bottom <- rbind(default, res$bottom)

  return(res)
}

# Format numeral generics
numeral_formatter <- function(x) {
  UseMethod("numeral_formatter")
}

numeral_formatter.default <- function(x) {
  stop("Unrecognized number_format. Please use a number, string or function.")
}

# If we are a function then return output from the function
numeral_formatter.function <- function(x) {
  return(x)
}

numeral_formatter.character <- function(x) {
  return(function(numeral) sprintf(x, numeral))
}

numeral_formatter.numeric <- function(x) {
  return(function(numeral) formatC(round(numeral, x), format = "f", digits = x))
}


# find each numeric substring, and replace it:
format_numbers <- function(string, num_fmt) {
  if (is.na(string)) {
    return(NA_character_)
  }

  # ! is.function avoids a warning if num_fmt is a function:
  if (!is.function(num_fmt) && is.na(num_fmt)) {
    return(string)
  }

  format_numeral <- numeral_formatter(num_fmt)
  # Breakdown:
  # -?                    optional minus sign
  # [0-9]*                followed by any number of digits
  # \\.?                  optionally followed by a decimal
  # [0-9]+                which may also be followed by any number of digits
  # ([eE]-?[0-9]+)?       optionally including e or E as in scientific notation
  #                       along with (optionally) a sign preceding the digits
  #                       specifying the level of the exponent.
  stringr::str_replace_all(string, "-?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?",
                           function(x) format_numeral(as.numeric(x)))
}


decimal_pad <- function(col, pad_chars, type) {
  # where pad_chars is NA we do not pad
  orig_col <- col
  na_pad <- is.na(pad_chars)
  col <- col[!na_pad]
  pad_chars <- pad_chars[!na_pad]
  if (length(col) == 0) {
    return(orig_col)
  }

  find_pos <- function(string, char) {
    regex <- gregexpr(char, string, fixed = TRUE)[[1]]
    regex[length(regex)]
  }
  pos <- mapply(find_pos, col, pad_chars)
  nchars <- nchar(col, type = "width")
  # take the biggest distance from the decimal point
  pos[pos == - 1L] <- nchars[pos == - 1L] + 1
  chars_after__ <- nchars - pos

  pad_n_spaces <- max(chars_after__) - chars_after__
  pad_char <- switch(type,
    "html"   = "&nbsp;",
    "latex"  = "~",
    "screen" = "\u00a0", # screen non-breaking space
    "rtf"    = "\\~",
    " "
  )
  col <- paste0(col, str_rep(pad_char, pad_n_spaces))

  orig_col[!na_pad] <- col
  orig_col
}


check_positive_dims <- function(ht) {
  if (nrow(ht) < 1) {
    warning("huxtable has zero rows")
    return(FALSE)
  }
  if (ncol(ht) < 1) {
    warning("huxtable has zero columns")
    return(FALSE)
  }

  return(TRUE)
}


# return data frame mapping real cell positions to cells displayed.
# cells, including those shadowed by others.
# data frame is ordered by row then column, i.e. the same as 1-based
# indexing into a matrix
# columns are row, col (of real cell);
# shadowed if cell is covered by another, the "display cell"; if not,
# it is its own "display cell";
# display_row, display_col, rowspan, colspan, end_row, end_col of the
# display cell.
display_cells <- function(ht, all = TRUE,
                          new_rowspan = huxtable::rowspan(ht),
                          new_colspan = huxtable::colspan(ht)) {
  rowspan <- new_rowspan
  colspan <- new_colspan
  display_row <- end_row <- row <- row(ht)
  display_col <- end_col <- col <- col(ht)
  displayers <- rowspan > 1 | colspan > 1
  touched <- shadowed <- matrix(FALSE, nrow(ht), ncol(ht))
  for (idx in which(displayers)) {
    rr <- row[idx]
    cc <- col[idx]
    end_r <- rr + rowspan[idx] - 1
    end_c <- cc + colspan[idx] - 1
    da_rows <- seq(rr, end_r)
    da_cols <- seq(cc, end_c)
    if (any(touched[da_rows, da_cols])) {
      stop(glue::glue(
        "Overlapping multirow/multicolumn cells in",
        " [{da_rows}, {da_cols}] of huxtable"
      ))
    }
    display_row[da_rows, da_cols] <- rr
    display_col[da_rows, da_cols] <- cc
    rowspan[da_rows, da_cols] <- rowspan[idx]
    colspan[da_rows, da_cols] <- colspan[idx]
    end_row[da_rows, da_cols] <- end_r
    end_col[da_rows, da_cols] <- end_c
    shadowed[da_rows, da_cols] <- TRUE
    touched[da_rows, da_cols] <- TRUE
    shadowed[rr, cc] <- FALSE
  }

  dcells <- data.frame(
    row         = c(row),
    col         = c(col),
    rowspan     = c(rowspan),
    colspan     = c(colspan),
    display_row = c(display_row),
    display_col = c(display_col),
    shadowed    = c(shadowed),
    end_row     = c(end_row),
    end_col     = c(end_col)
  )
  if (!all) dcells <- dcells[!dcells$shadowed, ]

  return(dcells)
}


get_caption_hpos <- function(ht) {
  hpos <- sub(".*(left|center|right)", "\\1", huxtable::caption_pos(ht))
  if (!hpos %in% c("left", "center", "right")) hpos <- position_no_wrap(ht)

  hpos
}

position_no_wrap <- function(ht) {
  switch(huxtable::position(ht),
    "wrapleft"  = "left",
    "wrapright" = "right",
    huxtable::position(ht)
  )
}

real_align <- function(ht) {
  # align(ht) can be e.g. "." for aligning on a decimal point
  al <- huxtable::align(ht)
  al[!al %in% c("left", "center", "right")] <- "right"

  al
}

smart_hux_from_df <- function(dfr) {
  col_nchars <- sapply(dfr, function(col) max(nchar(as.character(col),
                                                    type = "width")))

  ht <- huxtable::as_hux(dfr, add_colnames = TRUE, autoformat = TRUE)

  huxtable::wrap(ht)[- 1, col_nchars > 15] <- TRUE
  width <- sum(col_nchars) / 90
  huxtable::width(ht) <- min(1, max(0.2, width))

  ht
}

# get_set
get_all_borders <- function(ht, row, col, drop = TRUE) {
  list(
    left   = huxtable::left_border(ht)$thickness[row, col, drop = drop],
    right  = huxtable::right_border(ht)$thickness[row, col, drop = drop],
    top    = huxtable::top_border(ht)$thickness[row, col, drop = drop],
    bottom = huxtable::bottom_border(ht)$thickness[row, col, drop = drop]
  )
}

get_all_border_colors <- function(ht, row, col, drop = TRUE) {
  list(
    left   = huxtable::left_border_color(ht)[row, col, drop = drop],
    right  = huxtable::right_border_color(ht)[row, col, drop = drop],
    top    = huxtable::top_border_color(ht)[row, col, drop = drop],
    bottom = huxtable::bottom_border_color(ht)[row, col, drop = drop]
  )
}

get_all_border_styles <- function(ht, row, col, drop = TRUE) {
  list(
    left   = huxtable::left_border_style(ht)[row, col, drop = drop],
    right  = huxtable::right_border_style(ht)[row, col, drop = drop],
    top    = huxtable::top_border_style(ht)[row, col, drop = drop],
    bottom = huxtable::bottom_border_style(ht)[row, col, drop = drop]
  )
}

get_all_padding <- function(ht, row, col, drop = TRUE) {
  list(
    left   = huxtable::left_padding(ht)[row, col, drop = drop],
    right  = huxtable::right_padding(ht)[row, col, drop = drop],
    top    = huxtable::top_padding(ht)[row, col, drop = drop],
    bottom = huxtable::bottom_padding(ht)[row, col, drop = drop]
  )
}

rtf_fc_tables <- function(..., extra_fonts = "Times",
                          extra_colors = character(0)) {
  hts <- list(...)
  assertthat::assert_that(all(sapply(hts, huxtable::is_huxtable)))

  fonts <- unlist(lapply(hts, function(ht) huxtable::font(ht)))
  fonts <- unique(c(extra_fonts, fonts))
  fonts <- stats::na.omit(fonts)

  colors <- unlist(lapply(hts, function(ht) {
    c(huxtable::text_color(ht), huxtable::background_color(ht),
      unlist(collapsed_border_colors(ht)))
  }))
  colors <- unique(c(extra_colors, colors))
  colors <- stats::na.omit(colors)

  result <- list()
  result$fonts <- fonts
  result$colors <- colors
  class(result) <- "rtfFCTables"

  result
}

to_rtf_01 <- function(ht, ...) UseMethod("to_rtf")

to_rtf.huxtable <- function(ht, fc_tables = rtf_fc_tables(ht), watermark,
                            nheader, tlf, ...) {
  assertthat::assert_that(inherits(fc_tables, "rtfFCTables"))
  color_index <- function(color) {
    res <- match(color, fc_tables$colors)
    if (any(is.na(res) & !is.na(color))) {
      warning(
        "Color not found in color table.\n",
        "(Did you change colors after calling `rtf_fc_tables`?)"
      )
    }
    res
  }

  cb <- collapsed_borders(ht)
  cbc <- collapsed_border_colors(ht)
  cbs <- collapsed_border_styles(ht)
  bgc <- huxtable::background_color(ht)
  tc <- huxtable::text_color(ht)

  ## MAKE CELLX DEFINITIONS ----

  left_merge <- ifelse(huxtable::colspan(ht) > 1, "\\clmgf", "")
  top_merge <- ifelse(huxtable::rowspan(ht) > 1, "\\clvmgf", "")
  dc <- display_cells(ht, all = TRUE)
  right_merge <- ifelse(dc$col > dc$display_col, "\\clmrg", "")
  bottom_merge <- ifelse(dc$row > dc$display_row, "\\clvmrg", "")
  merge_def <- paste0(left_merge, top_merge, right_merge, bottom_merge)

  bdr_width_vert <- paste0("\\brdrw", cb$vert * 20)
  bdr_width_horiz <- paste0("\\brdrw", cb$horiz * 20)
  bdr_style_map <- c(
    "solid"  = "\\brdrs",
    "double" = "\\brdrdb",
    "dashed" = "\\brdrdash",
    "dotted" = "\\brdrdot"
  )
  bdr_style_vert <- bdr_style_map[cbs$vert]
  bdr_style_horiz <- bdr_style_map[cbs$horiz]
  bdr_color_vert <- sprintf("\\brdrcf%d", color_index(cbc$vert))
  bdr_color_horiz <- sprintf("\\brdrcf%d", color_index(cbc$horiz))
  bdr_color_vert <- blank_where(bdr_color_vert, is.na(cbc$vert))
  bdr_color_horiz <- blank_where(bdr_color_horiz, is.na(cbc$horiz))

  # these are matrices (horiz = nr+1 * nc, vert = nr * nc+1).
  # For cell (i, j), top and left are i, j; right is i,
  # j+1; bottom is i+1,j; in respective matrices
  bdr_def_vert <- paste0(bdr_style_vert, bdr_width_vert, bdr_color_vert)
  bdr_def_horiz <- paste0(bdr_style_horiz, bdr_width_horiz, bdr_color_horiz)
  dim(bdr_def_vert) <- dim(cb$vert)
  dim(bdr_def_horiz) <- dim(cb$horiz)

  bdr_def_left <- bdr_def_vert[, - ncol(bdr_def_vert), drop = FALSE]
  bdr_def_right <- bdr_def_vert[, - 1, drop = FALSE]
  bdr_def_top <- bdr_def_horiz[- nrow(bdr_def_horiz), , drop = FALSE]
  bdr_def_bottom <- bdr_def_horiz[- 1, , drop = FALSE]

  bdr_def_left <- blank_where(bdr_def_left, cb$vert[, - ncol(cb$vert),
                                                    drop = FALSE] == 0)
  bdr_def_right <- blank_where(bdr_def_right, cb$vert[, - 1, drop = FALSE] == 0)
  bdr_def_top <- blank_where(bdr_def_top, cb$horiz[- nrow(cb$horiz), ,
                                                   drop = FALSE] == 0)
  bdr_def_bottom <- blank_where(bdr_def_bottom, cb$horiz[- 1, ,
                                                         drop = FALSE] == 0)

  bdr_def_left <- paste0("\\clbrdrl", bdr_def_left)
  bdr_def_right <- paste0("\\clbrdrr", bdr_def_right)
  bdr_def_top <- paste0("\\clbrdrt", bdr_def_top)
  bdr_def_bottom <- paste0("\\clbrdrb", bdr_def_bottom)

  bdr_def <- paste0(bdr_def_top, bdr_def_left, bdr_def_bottom, bdr_def_right)

  bg_def <- sprintf("\\clcbpat%d", color_index(bgc))
  bg_def <- blank_where(bg_def, is.na(bgc))

  valign_map <- c(top = "\\clvertalt", middle = "\\clvertalc",
                  bottom = "\\clvertalb")
  valign_def <- valign_map[huxtable::valign(ht)]
  # also handles rotation:
  valign_def[huxtable::rotation(ht) == 90] <- "\\cltxbtlr"
  valign_def[huxtable::rotation(ht) == 270] <- "\\cltxtbrl"

  wrap_def <- ifelse(huxtable::wrap(ht), "", "\\clNoWrap")

  pad_def <- NULL

  table_width <- huxtable::width(ht)
  col_width <- huxtable::col_width(ht)

  col_width <- if (is.numeric(col_width)) {
    col_width
  } else if (all(grepl("pt", col_width))) {
    as.numeric(sub("((\\d|\\.)+).*", "\\1", col_width)) * 20
  } else {
    if (!all(is.na(col_width))) {
      warning("to_rtf can only handle numeric or \"pt\" col_width")
    }
    rep(1 / ncol(ht), ncol(ht))
  }

  if (!is.numeric(table_width)) {
    warning("to_rtf can only handle numeric table width")
    table_width <- huxtable::get_default_properties("width")[[1]]
  }
  text_width_twips <- 6 * 72 * 20
  col_width <- col_width * text_width_twips * table_width
  # \cellx specifies the position of the RH cell edge:
  right_edges <- ceiling(cumsum(col_width))

  cellx_def <- sprintf("\\cellx%d ", right_edges)

  # cellx_def has to go along rows:
  cellx <- paste0(
    merge_def, bdr_def, bg_def, valign_def, wrap_def, pad_def,
    rep(cellx_def, each = nrow(ht))
  )

  dim(cellx) <- dim(ht)

  ## MAKE CELL CONTENTS ----
  cc <- clean_contents(ht, type = "rtf")
  ## removed brackets "{}"
  cells <- paste0("", cc, "")
  cells[huxtable::bold(ht)] <- paste0("\\b ", cells[huxtable::bold(ht)],
                                      "\\b0")
  cells[huxtable::italic(ht)] <- paste0("\\i ", cells[huxtable::italic(ht)],
                                        "\\i0")
  fs <- ceiling(huxtable::font_size(ht) * 2) # "half-points", must be integer
  ## removed "{}", put space at the end (so that indentation works)
  cells[!is.na(fs)] <- paste0("\\fs", fs[!is.na(fs)], " ", cells[!is.na(fs)],
                              " ")
  cells[!is.na(tc)] <- paste0(
    "{\\cf", match(tc[!is.na(tc)], fc_tables$colors), " ",
    cells[!is.na(tc)], "}"
  )

  ft <- huxtable::font(ht)
  findex <- match(ft[!is.na(ft)], fc_tables$fonts) - 1
  if (any(is.na(findex))) {
    warning(
      "Font not found in font table.\n",
      "(Did you change a font after calling `rtf_fc_table`?)"
    )
  }
  cells[!is.na(ft)] <- paste0("{\\f", findex, " ", cells[!is.na(ft)], "}")

  align_map <- c("left" = "\\ql", "center" = "\\qc", "right" = "\\qr")
  cells <- paste0(align_map[real_align(ht)], cells)
  cells <- paste0("\\pard\\intbl", cells, "\\cell")
  dim(cells) <- dim(ht)

  if (!is.null(watermark)) {
    watermark_bf <- "{{\\shp{\\*\\shpinst\\shpleft0\\shptop0\\shpright13921\\shpbottom2320\\shpfhdr0\\shpbxcolumn\\shpbxignore\\shpbypara\\shpbyignore\\shpwr3
    {\\sp{\\sn gtextUNICODE}   {\\sv "
    watermark_af <- "}}
    {\\sp{\\sn gtextSize}      {\\sv 65536}}
    {\\sp{\\sn gtextFont}      {\\sv Calibri}}
    {\\sp{\\sn gtextFStretch}  {\\sv 1}}
    {\\sp{\\sn shapeType}      {\\sv 136}}
    {\\sp{\\sn fFlipH}         {\\sv 0}}
    {\\sp{\\sn fFlipV}         {\\sv 0}}
    {\\sp{\\sn rotation}       {\\sv 20643840}}
    {\\sp{\\sn fGtext}         {\\sv 1}}
    {\\sp{\\sn fillColor}      {\\sv 12632256}}
    {\\sp{\\sn fillOpacity}    {\\sv 15000}}
    {\\sp{\\sn fLine}          {\\sv 0}}
    {\\sp{\\sn wzName}         {\\sv PowerPlusWaterMarkObject357476642}}
    {\\sp{\\sn posh}           {\\sv 2}}
    {\\sp{\\sn posrelh}        {\\sv 0}}
    {\\sp{\\sn posv}           {\\sv 2}}
    {\\sp{\\sn posrelv}        {\\sv 0}}
    {\\sp{\\sn dhgt}           {\\sv 251659264}}
    {\\sp{\\sn fLayoutInCell}  {\\sv 0}}
    {\\sp{\\sn fBehindDocument}{\\sv 0}}
    }}}"
    # paste the rtf string for watermark behind the "\cell" on the first row of
    # last column
    cells[1, ncol(cells)] <- paste0(cells[1, ncol(cells)],
                                    paste0(watermark_bf,
                                           watermark,
                                           watermark_af))
  }
  ## CREATE ROWS ----
  cellx_rows <- apply(cellx, 1, paste0, collapse = "\n")
  cell_content_rows <- apply(cells, 1, paste0, collapse = "\n")

  row_align_map <- c(
    "left"      = "\\trql ",
    "center"    = "\\trqc ",
    "right"     = "\\trqr ",
    "wrapleft"  = "\\trql \\dfrmtxtx480 ",
    "wrapright" = "\\trqr \\dfrmtxtx480 "
  )
  row_align <- row_align_map[huxtable::position(ht)]

  rh <- huxtable::row_height(ht)
  table_height <- huxtable::height(ht)
  row_heights <- ""
  if (any(!is.na(rh)) || !is.na(table_height)) {
    if (!is.numeric(rh) && !all(is.na(rh))) {
      warning("to_rtf can only handle numeric row_height.")
    }
    if (!is.numeric(table_height) && !is.na(table_height)) {
      warning(
        "to_rtf can only handle numeric table height."
      )
    }
    if (!is.numeric(table_height) || is.na(table_height)) table_height <- 0.33
    page_height <- 10 * 72 * 20 # 10 inches in twips
    if (any(is.na(as.numeric(rh)))) rh <- rep(1 / nrow(ht), nrow(ht))
    rh <- ceiling(rh * page_height * table_height)
    row_heights <- sprintf("\\trrh%d ", rh)
  }

  rows <- paste0("{\n\\trowd\n", row_align, row_heights, cellx_rows,
                 cell_content_rows, "\n\\row\n}\n")

  ## CAPTION ----

  caption <- huxtable::caption(ht)
  cap_align <- align_map[get_caption_hpos(ht)]
  caption_par <- if (is.na(caption)) {
    ""
  } else {
      sprintf("{\\pard %s {%s} \\par}", cap_align, caption)
    }


  ## PASTE EVERYTHING TOGETHER ----
  result <- paste(rows, collapse = "\n")
  result <- if (grepl("top", huxtable::caption_pos(ht))) {
    paste(caption_par, result, sep = "\n")
  } else {
    paste(
      result, caption_par,
      sep = "\n"
    )
  }
  attr(result, "fc_tables") <- fc_tables
  result <- merger_header(result)
  if (tolower(substr(tlf, 1, 1)) == "t") {
    result <- pad_header(result, nheader)
  }
  return(result)
}

print_rtf_01 <- function(ht, fc_tables = rtf_fc_tables(ht),
                         watermark, nheader, tlf, ...) {
  cat(to_rtf_01(ht, fc_tables, watermark, nheader, tlf, ...))
}


huxtableize <- function(obj_list, borders) {
  lapply(obj_list, function(obj) {
    if (!inherits(obj, "huxtable")) {
      obj <- huxtable::as_huxtable(obj)
      obj <- huxtable::set_all_borders(obj, borders)
    }
    obj
  })
}


confirm <- function(file) {
  if (!interactive()) stop("Please specify a `file` argument for non-interactive use of quick_xxx functions.")
  if (file.exists(file)) {
    answer <- readline(paste0("File '", file, "' already exists. Overwrite? [yN]"))
    if (!answer %in% c("y", "Y")) stop("OK, stopping")
  }
  file
}

auto_open <- function(path) {
  sysname <- Sys.info()["sysname"]
  switch(sysname,
    Darwin  = system2("open", path),
    Windows = system2("start", path),
    Linux   = system2("xdg-open", path),
    warning("Could not determine OS to open document automatically")
  )
}

#' @title quick_rtf_jnj
#' @aliases {quick_rtf}
#'
#' @description Write a series of huxtables to a RTF document.
#' Please refer to quick_rtf of the huxtable package for more information.
#'   Adjusted from huxtable::quick_rtf.
#'
#' @seealso \code{\link{gentlg}} \code{\link[huxtable]{quick_rtf}}
#'
#' @author Steven Haesendonckx {shaesen2@@its.jnj.com}
#' @author Pelagia Alexandra Papadopoulou {ppapadop@@its.jnj.com}
#' @author Jiaqi Song {JSong67@@its.jnj.com}
#'
#' @inheritParams huxtable::quick_rtf
#' @inheritParams gentlg
#' @param portrait String: "portrait" or "landscape". Default is portrait.
#' @param mode Permissions to apply to file (default to 770)
#' @param debug logical to turn on browser(), defaults to FALSE
#' @param nheader number of header rows
#' @noRd
#'
#' @references \url{https://github.com/hughjonesd/huxtable}

quick_rtf_jnj <- function(...,
                          file = confirm("huxtable-output.rtf"),
                          borders = 0.4,
                          open = FALSE,
                          portrait = TRUE,
                          pagenum = FALSE,
                          watermark = NULL,
                          mode = "0770",
                          debug = FALSE,
                          nheader = 1,
                          tlf = "Table") {
  if (debug == TRUE) browser()

  assertthat::assert_that(assertthat::is.number(borders))
  assertthat::assert_that(assertthat::is.flag(open))
  force(file)
  hts <- huxtableize(list(...), borders)

  fc_tbls <- do.call(huxtable::rtf_fc_tables, hts)

  portrait_t <- "{\\rtf1\\ansi\\deff0\\portrait\\paperw12240\\paperh15840\\margl1440\\margr1440\\margt1440\\margb1440\\headery1440\\footery1440{\\stylesheet{\\ql \\li0\\ri0\\widctlpar\\wrapdefault\\faauto\\adjustright\\rin0\\lin0\\itap0 \\rtlch\\fcs1 \\af0\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\lang9\\langfe3081\\loch\\f0\\hich\\af0\\dbch\\af31505\\cgrid\\langnp9\\langfenp3081 \\snext0 \\sqformat \\spriority0 Normal;}{\\s15\\ql \\fi-1152\\li1152\\ri0\\keepn\\widctlpar\\tx1152\\wrapdefault\\faauto\\rin0\\lin1152\\itap0 \\rtlch\\fcs1 \\af0\\afs18\\alang1025 \\ltrch\\fcs0 \\b\\fs20\\lang1033\\langfe1033\\loch\\f0\\hich\\af0\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 \\sbasedon0 \\snext0 \\sqformat caption;}}\n"
  portrait_f <- "{\\rtf1\\ansi\\deff0\\portrait\\paperw15840\\paperh12240\\margl1440\\margr1440\\margt1440\\margb1440\\headery1440\\footery1440{\\stylesheet{\\ql \\li0\\ri0\\widctlpar\\wrapdefault\\faauto\\adjustright\\rin0\\lin0\\itap0 \\rtlch\\fcs1 \\af0\\afs20\\alang1025 \\ltrch\\fcs0 \\fs20\\lang9\\langfe3081\\loch\\f0\\hich\\af0\\dbch\\af31505\\cgrid\\langnp9\\langfenp3081 \\snext0 \\sqformat \\spriority0 Normal;}{\\s15\\ql \\fi-1152\\li1152\\ri0\\keepn\\widctlpar\\tx1152\\wrapdefault\\faauto\\rin0\\lin1152\\itap0 \\rtlch\\fcs1 \\af0\\afs18\\alang1025 \\ltrch\\fcs0 \\b\\fs20\\lang1033\\langfe1033\\loch\\f0\\hich\\af0\\dbch\\af31505\\cgrid\\langnp1033\\langfenp1033 \\sbasedon0 \\snext0 \\sqformat caption;}}\n"
  pagenum_t <- "\\par {\\footer\\pard\\sb240\\qr\\fs16{\\insrsid2691151 Listing Page }{\\field{\\*\\fldinst {\\insrsid2691151 PAGE }}{\\fldrslt {\\insrsid26911511}}}{\\insrsid2691151  of }{\\field{\\*\\fldinst {\\insrsid2691151  NUMPAGES }} {\\fldrslt {\\insrsid112265262}}}{\\insrsid2691151 \\par }}\n\n\n}"

  sink(file)
  tryCatch( {
      cat(ifelse(portrait, portrait_t, portrait_f))
      cat("\n\n\n")
      lapply(hts, print_rtf_01, watermark = watermark, nheader = nheader,
             tlf = tlf)
      cat(ifelse(pagenum, pagenum_t, "\n\n\n}"))
    },
    error = identity,
    finally = {
      sink()
    }
  )

  # update permissions
  Sys.chmod(file, mode, use_umask = FALSE)

  if (open) auto_open(file)
  invisible(NULL)
}
