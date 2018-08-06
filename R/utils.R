# {knapply}
# as_snake_case <- function(string) {
#   out <- stringr::str_replace_all(string, "([a-z])([A-Z])", "\\1_\\2")
#   stringr::str_to_lower(out)
# }
# 
# if_empty_as_na <- function(x) {
#   ifelse(length(unlist(x)) == 0, NA, x)
# }
# 
# are_null <- function(x) {
#   vapply(x, is.null, logical(1))
# }

# if_null_as_na <- function(x, .type = NULL) {
#   if(is.null(.type)) {
#     return(ifelse(is.null(x), NA, x))
#   }
#   return(ifelse(is.null(x), as(NA, .type), x))
# }

# if_null_as_na_chr <- function(x) {
#   if_null_as_na(x, .type = "character")
# }

# drop_if_named <- function(x, .names) {
#   x[!names(x) %in% .names]
# }

keep_if_named <- function(x, .names) {
  x[names(x) %in% .names]
}


clean_html <- function(x) {
  out <- stringr::str_replace_all(x, "<.*?>", " ")
  out <- stringr::str_squish(out)
  stringr::str_trim(out)
}


# # Basic map function wrapping `lapply()`.
# nap <- function(.x, .f, ...) {
#   lapply(.x, .f, ...)
# }
# 
# # Map function to extract elements by name.
# nap_extract <- function(.x, .f) {
#   lapply(.x, `[[`, .f)
# }
# 
# # Type-enforced map.
# nap_chr <- function(.x, .f, ...) {
#   vapply(.x, .f, FUN.VALUE =  character(1), ..., USE.NAMES = FALSE)
# }
# 
# # Type-enforced map to extract elements by name.
# nap_extract_chr <- function(x, .name, ...) {
#   vapply(x, function(x) x[[.name]], FUN.VALUE = character(1), ..., USE.NAMES = FALSE)
# }

#' @importFrom glue glue
try_connection <- function(expr, url) {
  mess <- "Are you connected to the Internet?\nDid the URL change?"
  tryCatch(
    expr = expr,
    error = function(e) {
      stop(glue::glue("\nCannot connect to:\n\n{url}\n\n{mess}"), call. = FALSE)
      })
}

#' @importFrom glue glue
gum <- function(..., .envir = parent.frame()) {
  as.character(glue(..., .envir = .envir))
}

# check_status <- function(response) {
#   if(status_code(response) != 200) {
#     status <- status_code(response)
#     opts <- c("Moved Temporarily" = 302,
#               "Bad Request" = 400,
#               "Access denied due to missing subscription key...\n\t...Make sure to include subscription key when making requests to an API." = 401,
#               "Not Found" = 404,
#               "Internal Server Error" = 500,
#               "Service Unavailable" = 503,
#               "Unknown Response" = -99)
#     resp <- names(opts)[[match(status, opts, nomatch = length(opts))]]
#     details <- glue("API Request Failed.\nStatus Code: {status}\nResponse: {resp}")
#     stop(details, call. = FALSE)
#   }
# }


#' @importFrom glue glue
#' @importFrom readr write_rds
#' @importFrom rlang arg_match
cache_data <- function(x, dataset_id, type = c("metadata", "dataset"), 
                       cache_dir = "~/salinasr-cache", ...) {
  type <- rlang::arg_match(type, c("metadata", "dataset"))
  metadata_path <- glue::glue("{cache_dir}/metadata")
  dataset_path <- glue::glue("{cache_dir}/dataset")
  if(!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  if(!dir.exists(metadata_path)) {
    dir.create(metadata_path)
  }
  if(!dir.exists(dataset_path)) {
    dir.create(dataset_path)
  }
  if(type == "metadata") {
    target_path <- glue::glue("{metadata_path}/{dataset_id}-metadata.rds")
  }
  if(type == "dataset") {
    target_path <- glue::glue("{dataset_path}/{dataset_id}.rds")
  }
  mess <- glue::glue('Caching "{dataset_id}" {type} at {target_path}.')
  message(mess)
  
  readr::write_rds(x, path = target_path, ...)
}


#
is_geo <- function(x) {
  UseMethod("is_geo")
}

is_geo.metadata <- function(x) {
  "geo" %in% x$features
}

is_geo.tbl_df <- function(x) {
  metadata <- sal_get_metadata(x)
  is_geo(metadata)
}

#' Obtain a URL pointing to a specific dataset in the desired file type.
#' 
#' @examples 
#' library(salinasr)
#' 
#' meta <- sal_get_metadata("bikeways")
#' 
#' meta %>% 
#'   sal_get_export_link()
#' 
#' @importFrom rlang arg_match
#' @export
sal_get_export_link <- function(metadata, file_type = c("geojson", "json", "csv", "shp")) {
  if(!is_metadata(metadata)) {
    stop("Not a `metadata` object. `metadata` must be obtained by `sal_fetch_dataset()`.")
  }
  
  type <- rlang::arg_match(file_type, c("geojson", "json", "csv", "shp"))
  links <- metadata$links
  
  out <- links[links$rel == "exports", ]$href
  paste0(out, "/", file_type)
}














