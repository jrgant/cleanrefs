#' @title Format references for Notion
#' @name format_refs
#'
#' @description A function to format CSV Paperpile exports for import into Notion.
#'
#' @param refdat Name of CSV file containing Paperpile export (without .csv extension).
#' @param indir Directory containing Paperile CSV export file.
#' @param outdir Directory to write reformatted CSV file. If not specified, function will use `indir` as default write directory.
#' @param write Write formatted references to CSV (default: TRUE).
#'
#' @import dplyr
#' @import stringr
#' @importFrom readr read_csv
#'
#' @export format_refs

format_refs <- function(refdat, indir, outdir = NULL, write = TRUE) {

  file <- paste0(indir, "/", refdat, ".csv")

  dat <-
    read_csv(file) %>%
    select(
      "Title",
      "Journal",
      "Pages",
      "Volume",
      "Issue",
      "Publication year",
      "DOI",
      "Abstract",
      "Authors",
      ) %>%
    mutate(
      Journal = str_remove_all(Journal, "\\."),
      `First author` = str_extract(Authors, "^[A-Za-z]+"),
      Authors = str_replace_all(Authors, "\\,", "\\, "),
      Pages = str_replace(Pages, "\\-", "\\-\\-")
      ) %>%
    select(Title, `First author`, everything())

  if (is.null(outdir)) {
    flout <- paste0(indir, "/", refdat, "_reformat", ".csv")

  } else {
    flout <- paste0(indir, "/", refdat, "_reformat", ".csv")
  }

  if (write) {
    write.csv(dat, flout, row.names = F)
  }

  read_csv(flout)
}
