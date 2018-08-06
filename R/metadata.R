#' Get a dataset's `metadata`.
#' 
#' The `metadata` class is a thin wrapper around a nested `list` object. It
#' provides a consistent start point to explore information about `dataset`s and
#' safely import them into R.
#' 
#' @param x `character` representing a `dataset_id` or `tibble` obtained via
#' `sal_get_dataset()`.
#' @param dataset_id `character`, ID used to identify dataset. 
#' @param prioritize_cache `logical`, whether to cache data locally to prevent redundant API calls.
#' @param cache_dir `character`, path to read/write data.
#' 
#' @return `metadata` object
#' 
#' @references \url{https://cityofsalinas.opendatasoft.com/explore/}
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
#' @importFrom purrr map
#' @export
#' 
sal_get_metadata <- function(x, prioritize_cache = TRUE, cache_dir = "~/salinasr-cache") {
  UseMethod("sal_get_metadata")
}
 
#' @describeIn sal_get_metadata
#' 
#' Method to obtain a `metadata` object from a `tbl_df` obtained from a `metadata` object
#' in the event something occurred to the original `metadata` object.
#' 
#' @export
#' 
sal_get_metadata.tbl_df <- function(x, prioritize_cache = TRUE, cache_dir = "~/salinasr-cache") {
  metadata <- attr(x, "dataset_id")
  sal_get_metadata(metadata, prioritize_cache, cache_dir)
}

#' @describeIn sal_get_metadata
#' 
#' Method to obtain a `metadata` object from a `dataset_id`.
#' 
#' @importFrom dplyr bind_rows everything select select_if
#' @importFrom glue glue
#' @importFrom httr content GET http_type modify_url stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom readr read_rds
#' @export
sal_get_metadata.character <- function(dataset_id, prioritize_cache = TRUE, cache_dir = "~/salinasr-cache") {
  dataset_id <- dataset_id
  if(prioritize_cache) {
    path <- glue("{cache_dir}/metadata/{dataset_id}-metadata.rds")
    if(file.exists(path)) {
      return(read_rds(path))
    }
  }
  url_stem <- "https://cityofsalinas.opendatasoft.com/"
  path <- glue("api/v2/catalog/datasets/{dataset_id}")
  
  url_data <- modify_url(url_stem, path = path)
  resp <- GET(url_data)
  stop_for_status(resp, glue("find a dataset with ID '{substitute(dataset_id)}'"))
  
  if(http_type(resp) != "application/json") {
    stop("API did not return json.", call. = FALSE)
  }
  
  init <- fromJSON(content(resp, as = "text", encoding = "utf-8"), simplifyVector = FALSE)
  
  metadata <- init$dataset$metas$default
  metadata <- map(metadata, unlist)
  metadata <- map(metadata, if_null_as_na)
  metadata <- map(metadata, clean_html)
  
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
  metadata$fields <- map(metadata$fields, map, if_empty_as_na)
  metadata$fields <- bind_rows(metadata$fields)
  metadata$fields <- select_if(metadata$fields, ~ all(!is.na(.)))
  
  metadata <- metadata[order(names(metadata))]

  class(metadata) <- "metadata"
  
  cache_data(metadata, dataset_id = metadata$dataset_id, type = "metadata")
  
  metadata
}

#' @describeIn sal_get_metadata
#' 
#' Method to `print` `metadata` object. Empty `metadata` attributes are colored in red.
#' 
#' @importFrom glue glue
#' @export
#' 
print.metadata <- function(x, cache_dir = "~/salinasr-cache") {
  cat("dataset_id:                   ", x$dataset_id, "\n")

  metadata_cache <- glue("{cache_dir}/metadata/{x$dataset_id}-metadata.rds")
  metadata_cache <- ifelse(file.exists(metadata_cache), metadata_cache, "")
  metadata_mod <- ifelse(file.exists(metadata_cache), 
                         as.character(file.info(metadata_cache)$mtime),
                         "N/A")
  
  dataset_cache <- glue("{cache_dir}/dataset/{x$dataset_id}.rds")
  dataset_cache <- ifelse(file.exists(dataset_cache), dataset_cache, "")
  dataset_mod <- ifelse(file.exists(dataset_cache), 
                        as.character(file.info(dataset_cache)$mtime),
                        "N/A")
  
  cat(glue("metadata cache last modified:  {metadata_mod}"), "\n")
  cat(glue("dataset cache last modified:   {dataset_mod}"), "\n")
  cat("metadata attributes:", "\n")
  
  x_names <- names(x)
  empties <- x_names[vapply(x_names, function(y) {
    is.null(x[[y]]) || is.na(x[[y]]) || is_empty(x[[y]])
  }, logical(1))]
  empties <- paste0("$", empties)

  out <- paste0("$", x_names)
  out <- vapply(out, function(x) {
    ifelse(x %in% empties, crayon::red(x), x)
    }, character(1), USE.NAMES = FALSE)
  if(length(out) %% 2 != 0) {
    out[length(out) + 1] <- list("")
  }
  
  n1 <- out[1:(length(out) %/% 2)]
  n2 <- out[(length(out) %/% 2 + 1):length(out)]
  
  for(i in seq_len(length(out) %/% 2)) {
    cat("   ", n1[[i]], rep("", 30 - nchar(x_names[[i]])), n2[[i]], "\n")
  }
  invisible(x)
}

#' Test if an object is `metadata`.
#' 
#' @param x Any R object
#' 
#' @return `logical`, `TRUE` if the object inherits from the `metadata` class.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' 
#' meta <- sal_get_metadata("bikeways")
#' 
#' is_metadata(meta)
#'   
#' @export
is_metadata <- function(x) {
  "metadata" %in% class(x)
}
