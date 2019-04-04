#' PubMed search
#' @description This functions returns dataframe, several .
#' @usage pubmed_search(search_str, years, outfmt)
#' @param search_str character: search strings using pubmed filter. E.g. [ti],[tiab],[tw]
#'   \href{https://www.ncbi.nlm.nih.gov/books/NBK269313/}{Literature Search Methods}
#' @param years integer vector: E.g. 1980:2017
#' @param outfmt character: "tab" is a table of results ,or "cnt" is a count of hits per years. The default value is "tab".
#' @return dataframe
#' @examples ## pubmed table
#' \dontrun{
#' str1 <- "microarray[tiab]"
#' str2 <- "next-generation sequencing[tiab]"
#' str3 <- "microarray[tiab] AND next-generation sequencing[tiab]"
#' str4 <- "cpa2[gs]"
#' str4 <- "MinION[tiab]"
#' yr <- 1980:2018
#'
#' res1 <- pubmed_search(search_str = str1, years = yr, outfmt = "tab")
#' res2 <- pubmed_search(search_str = str2, years = yr, outfmt = "tab")
#' res3 <- pubmed_search(search_str = str3, years = yr, outfmt = "tab")
#' res4 <- pubmed_search(search_str = str4, years = yr, outfmt = "tab")
#'
#' ## count
#' cnt1 <- pubmed_search(search_str = str1, years = yr, outfmt = "cnt")
#' cnt2 <- pubmed_search(search_str = str2, years = yr, outfmt = "cnt")
#'
#' ## count plot
#' barplot(cnt1, las=2, ylim=c(0,6000), col=adjustcolor(2, 0.5), border = adjustcolor(2, 0.5));
#' par(new=T);
#' barplot(cnt2, las=2, ylim=c(0,6000), col=adjustcolor(4, 0.5), border = adjustcolor(4, 0.5))
#' legend("topleft",
#'        legend = c("microarray[title & abstract]",
#'                   "next-generation sequencing[title & abstract]"),
#'        col = adjustcolor(c(2,4), 0.5), pch = 15, bg = "transparent", box.lwd = NA)
#' }
#' @export

pubmed_search <- function(search_str, years, outfmt = "tab") {

  if (outfmt == "cnt") {
    # RISmed::summary(res) RISmed::QueryCount(res)
    nres <- unlist(lapply(years, function(i) {
      res <- RISmed::EUtilsSummary(query = search_str, mindate = i, maxdate = i, type = "esearch",
                                   db = "pubmed")
      RISmed::QueryCount(res)
    }))
    names(nres) <- years
    return(nres)

  } else if (outfmt == "tab") {
    # create dataframe of medline object ----
    eusum <- RISmed::EUtilsSummary(query = search_str, mindate = min(years), maxdate = max(years),
                                   type = "esearch", db = "pubmed")
    res_records <- RISmed::EUtilsGet(eusum)
    dat <- dplyr::bind_rows(list(Title = RISmed::Title(res_records),
                                 ArticleTitle = RISmed::ArticleTitle(res_records),
                                 Year = RISmed::YearPubmed(res_records),
                                 PMID = RISmed::PMID(res_records),
                                 Journal = RISmed::MedlineTA(res_records),
                                 Authors = RISmed::Author(res_records),
                                 Abstract = RISmed::AbstractText(res_records)))
    # fst_auth <- sapply(Authors, function(x)paste(x[1,1:2], collapse = ' '))

  }
  return(dat)

}

