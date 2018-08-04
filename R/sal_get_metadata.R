#' Fetch a data set's meta data.
#' 
#' @param dataset_id `character`
#' 
#' @return `dataset` object, with
#' 
#' @examples 
#' library(salinasr)
#' 
#' meta <- sal_get_metadata("bikeways")
#' 
#' meta$description
#' 
#' meta$keyword
#'
#' @importFrom dplyr bind_rows everything select select_if
#' @importFrom glue glue
#' @importFrom httr GET http_type modify_url stop_for_status
#' @importFrom jsonlite fromJSON
sal_get_metadata <- function(dataset_id) {
  url_stem <- "https://cityofsalinas.opendatasoft.com/"
  path <- glue::glue("api/v2/catalog/datasets/{dataset_id}")
  url_data <- httr::modify_url(url_stem, path = path)
  
  resp <- httr::GET(url_data)
  
  httr::stop_for_status(resp, "connect to API")
  
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  init <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "utf-8"),
                             simplifyVector = FALSE)
  
  metadata <- init$dataset$metas$default
  metadata <- nap(metadata, unlist)
  metadata <- nap(metadata, if_null_as_na)
  metadata <- nap(metadata, clean_html)
  
  metadata$dataset_id <- init$dataset$dataset_id
  metadata$dataset_uid <- init$dataset$dataset_uid
  metadata$attachments <- init$dataset$attachments
  metadata$has_records <- init$dataset$has_records
  metadata$data_visible <- init$dataset$data_visible
  metadata$features <- unlist(init$dataset$features)
  metadata$links <- select(bind_rows(init$links), rel, everything())
  
  metadata$fields <- lapply(init$dataset$fields, lapply, function(x) {
      ifelse(is.null(x) || length(x) == 0, NA, x)
    }) 
  metadata$fields <- nap(metadata$fields, nap, if_empty_as_na)
  metadata$fields <- bind_rows(metadata$fields)
  metadata$fields <- select_if(metadata$fields, ~ all(!is.na(.)))
  
  metadata <- metadata[order(names(metadata))]

  class(metadata) <- "metadata"
  
  metadata
}

print.metadata <- function(x) {
  cat("Data Set Title:", x$title, "\n")
  cat("Data Set ID:", x$dataset_id, "\n\n")
  cat("All Meta Data:", "\n")
  out <- paste0("$", names(x))
  
  n1 <- out[1:(length(out) / 2)]
  n2 <- out[(length(out) / 2):length(out)]
  
  for(i in seq_len(length(out) / 2)) {
    cat(n1[[i]], rep("", 30 - nchar(n1[[i]])), n2[[i]], "\n")
  }
}
