#' @title getFileName
#'
#' @description This function returns the file path up until the program's
#'   name.
#'
#' @author Steven haesendonckx {shaesen2@@its.jnj.com}
#'
#' @return The file path up untill the program's name.
#' @param file name of the file
#'
#' @noRd
getFileName <-  function() {

  ##### If run from MCD BATCH\
  file_name <- getFileNameBatch(commandArgs(trailingOnly = FALSE))
  if (!is.null(file_name)) return(file_name)

  #### If Run from Source
  ret <- base::sys.frame(1)$ofile #gets file path of sourced file

  if (!is.null(ret))
    return(stringr::str_replace(ret, "~", base::path.expand("~")))

  ### If Run from rstudio server console
  if (rstudioapi::isAvailable()) {
    # Gets file path of current active Rstuido editor
  ret <- rstudioapi::getSourceEditorContext()$path
  if (!is.null(ret))
    return(stringr::str_replace(ret, "~", base::path.expand("~"))
    )
  }

  return(NULL)

}

#' @noRd
getFileNameBatch <- function(cmdArgs) {
  res <- NULL

  timber_needle <- "timber::axecute"
  timber_match <- base::grep(timber_needle, cmdArgs)
  file_needle <- "--file="
  file_match <- base::grep(file_needle, cmdArgs)


  if (any(timber_match)) {
    file_arg <- stringr::str_extract(cmdArgs[timber_match],
                                     'file~\\+~\\=~\\+~"(.+?)"')
    if (!is.na(file_arg)) {
      file_arg2 <- stringr::str_extract(file_arg, '"(.+?)"')
      res <- normalizePath(gsub("\"", "", file_arg2))
    } else {
      try({
        res <- rlang::call_args(str2lang(cmdArgs[timber_match]))$file
        }
        , silent = TRUE)
    }
  } else if (any(file_match)) {

    res <- normalizePath(sub(file_needle, "", cmdArgs[file_match]))
  }

  res
}
