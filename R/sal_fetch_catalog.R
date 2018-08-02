#' Fetch catalog of available data sets.
#' 
#' @param rss_url `character`, RSS feed URL. \cr
#'     Default: `"https://cityofsalinas.opendatasoft.com/api/v2/catalog/exports/rss"`
#'     
#' @return [`tibble::tibble`] data frame with columns:
#'     \item{\bold{`title`} }{`character`, data set title.}
#'     \item{\bold{`dataset_id`} }{`character`, data set ID to use in API requests.}
#'     \item{\bold{`pub_date`} }{`POSIXct`, date-time published.}
#'     \item{\bold{`description`} }{`character`, data set description.}
#'     \item{\bold{`link`} }{`character`, URL linking to dataset.}
#'
#' @references \url{https://cityofsalinas.opendatasoft.com/pages/homepage/}
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' suppressPackageStartupMessages(library(tidyverse))
#' 
#' catalog <- sal_fetch_catalog()
#' 
#' catalog
#' 
#' catalog %>% glimpse()
#' 
#' grocery_stores <- catalog %>% 
#'   filter(grepl("grocery stores", description))
#'   
#' grocery_stores %>% pull(description)
#' 
#' grocery_stores %>% pull(dataset_id)
#'
#' @importFrom dplyr arrange mutate select
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom xml2 as_list read_xml read_html
#' @export
sal_fetch_catalog <- function(rss_url = "https://cityofsalinas.opendatasoft.com/api/v2/catalog/exports/rss") {
  xml_raw <- try_connection(xml2::read_xml(rss_url))
  xml_parsed <- xml2::as_list(xml_raw)
  
  channels <- nap_extract(xml_parsed, "channel")
  # channel_titles <- nap_extract(channels, "title")       # may be useful as the portal grows
  # channel_titles_rss <- nap_chr(channel_titles, unlist)
  # channel_link <- nap_extract(channels, "link")
  channels_rss <- channels$rss

  items <- nap(channels_rss, unlist, recursive = FALSE)
  items <- keep_if_named(items, "item")

  out <- tibble(title = nap_extract_chr(items, "title"),
                link = nap_extract_chr(items, "link"),
                description = clean_html(nap_extract_chr(items, "description")),
                pub_date = nap_extract_chr(items, "pubDate"))
  out <- mutate(out, dataset_id = stringr::str_extract(link, "(?<=dataset/).*?(?=/$)"))
  out <- mutate(out, pub_date = pub_date %>% 
                  str_remove_all("^[A-Z][a-z]{2},\\s|\\sGMT$") %>% 
                  as.POSIXct(format = "%d %b %Y %H:%M:%S")
               )
  out <- select(out, title, dataset_id, pub_date, description, link)
  out <- arrange(out, desc(pub_date))
  
  out
}


#' Output catalog information in a format more conducive to reading.
#' 
#' This is intended to be helpful when specific search parameters are unknown
#' or simply for exploration of available data.
#' 
#' @param sal_catalog [`tibble::tibble`], as returned by `sal_fetch_catalog`.
#' @param n `integer`, the number of items to return.
#' @param width `integer`, number of characters per line. \cr
#'     Default: `getOption("width")`
#' @param ... Additional arguments to be passed to other methods. Not currently used.
#' 
#' @return an `invisible` `NULL`)
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' suppressPackageStartupMessages(library(tidyverse))
#' 
#' catalog <- sal_fetch_catalog()
#' 
#' catalog %>% 
#'   sal_research_catalog()
#' 
#' @importFrom crayon bgBlack blue bold cyan green magenta red yellow
#' @importFrom dplyr slice
#' @importFrom glue glue
#' @importFrom stringr str_conv str_wrap
#' @export
sal_research_catalog <- function(sal_catalog, n = 5L, width = getOption("width"), ...) {
  if(!all(names(sal_catalog) %in% c("title", "dataset_id", "pub_date", "description", "link"))) {
    stop("`sal_research()` requires a data frame as created by `sal_fetch_catalog()`.")
  }
  if(n > nrow(sal_catalog)) {
    n <- nrow(sal_catalog)
  }
  temp <- slice(sal_catalog, seq_len(n))
  
  for(row_num in seq_len(n)) {
    cat(gum("{bold(red('Title:'))} {temp$title[[row_num]]}"), "\n")
    cat(gum("{bold(cyan('ID:'))}    {temp$dataset_id[[row_num]]}"), "\n")
    cat(gum("{bold(magenta('Date Published:'))} {temp$pub_date[[row_num]]}"), "\n")
    cat(gum("{bold(blue('Link:'))}\n{temp$link[[row_num]]}"), "\n")
    descrip <- temp$description[[row_num]]
    cat(gum("{bold(green('Description:'))}"), "\n")
    cat(str_wrap(descrip, width = width))
    cat("\n", rep("-", width), "\n\n", sep = "")
  }
}
