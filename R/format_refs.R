#' @title Format references for Notion
#' @name format_refs
#'
#' @description A function to format CSV Paperpile exports for import into Notion.
#'
#' @param refdat Name of CSV file containing Paperpile export (without .csv extension).
#' @param indir Directory containing Paperile CSV export file.
#' @param outdir Directory to write reformatted CSV file.
#'
#' @import dplyr
#' @importFrom readr read_csv
#'
#' @export format_refs

format_refs <- function(refdat, indir, outdir = NULL) {

  file <- paste0(indir, "/", refdat, ".csv")

  dat <-
    read_csv(file) %>%
    select(
      "auth" = "Authors",
      "title" = "Title",
      "jnl" = "Journal",
      "yr" = "Publication year",
      "vol" = "Volume",
      "iss" = "Issue",
      "pgs" = "Pages",
      "doi" = "DOI",
      "pmid" = "PMID",
      "pmcid" = "PMC ID",
      "abs" = "Abstract"
      )

  if (is.null(outdir)) {
    flout <- paste0(indir, "/", refdat, "_reformat", ".csv")
    write.csv(dat, flout, row.names = F)
    read_csv(flout)

  } else {
    flout <- paste0(indir, "/", refdat, "_reformat", ".csv")
    write.csv(dat, flout, row.names = F)
    read_csv(flout)
  }


}
