#' Fetch a dataset's meta data.
#' 
#' @param dataset_id `character`, ID used to identify dataset. 
#'     See \url{https://cityofsalinas.opendatasoft.com/api/v2/console#!/dataset}.
#' @param cache `logical`, whether to cache data locally to prevent redundant API calls.
#' @param cache_path `character`, path to read/write data.
#' 
#' @return `metadata` object
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' 
#' meta <- sal_get_metadata("bikeways")
#' 
#' meta
#' 
#' meta$description
#'
#' @importFrom dplyr bind_rows everything select select_if
#' @importFrom glue glue
#' @importFrom httr GET http_type modify_url stop_for_status
#' @importFrom jsonlite fromJSON
#' @export
sal_get_metadata <- function(dataset_id, cache = TRUE, cache_path = "~/salinasr-cache") {
  if(cache) {
    if(file.exists(glue::glue("{cache_path}/metadata/{dataset_id}.rds"))) {
      return(readr::read_rds(glue::glue("{cache_path}/metadata/{dataset_id}.rds")))
    }
  }
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
  
  cache_data(metadata, dataset_id = metadata$dataset_id, type = "metadata")
  
  metadata
}

#' @describeIn sal_get_metadata
#' 
#' @param x `metadata` object, as obtained by `sal_get_metadata()`.
#' 
#' 
#' @importFrom glue glue
#' @export
print.metadata <- function(x, cache_path = "~/salinasr-cache") {
  cat("dataset_id:                   ", x$dataset_id, "\n")

  metadata_cache <- glue::glue("{cache_path}/metadata/{x$dataset_id}-metadata.rds")
  metadata_cache <- ifelse(file.exists(metadata_cache), metadata_cache, "")
  metadata_mod <- ifelse(file.exists(metadata_cache), 
                         as.character(file.info(metadata_cache)$mtime),
                         "N/A")
  
  dataset_cache <- glue::glue("{cache_path}/dataset/{x$dataset_id}.rds")
  dataset_cache <- ifelse(file.exists(dataset_cache), dataset_cache, "")
  dataset_mod <- ifelse(file.exists(dataset_cache), 
                        as.character(file.info(dataset_cache)$mtime),
                        "N/A")
  
  cat(glue::glue("metadata cache last modified:  {metadata_mod}"), "\n")
  cat(glue::glue("dataset cache last modified:   {dataset_mod}"), "\n")
  cat("meta data attributes:", "\n")
  
  out <- paste0("$", names(x))
  if(length(out) %% 2 != 0) {
    out[length(out) + 1] <- list("")
  }
  n1 <- out[1:(length(out) %/% 2)]
  n2 <- out[(length(out) %/% 2 + 1):length(out)]
  
  for(i in seq_len(length(out) %/% 2)) {
    cat("   ", n1[[i]], rep("", 25 - nchar(n1[[i]])), n2[[i]], "\n")
  }
  invisible(x)
}

#' @importFrom rlang arg_match
sal_get_export_link <- function(metadata, type = c("geojson", "json", "csv", "shp"),
                                describe_vars = TRUE) {
  if(class(metadata) != "metadata") {
    stop("Not a `metadata` object. `metadata` must be obtained by `sal_fetch_dataset()`.")
  }
  
  type <- rlang::arg_match(type, c("geojson", "json", "csv", "shp"))
  links <- metadata$links
  
  out <- links[links$rel == "exports", ]$href
  paste0(out, "/", type)
}