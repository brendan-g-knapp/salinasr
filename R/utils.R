# {knapply}
#' @importFrom stringr str_replace_all str_to_lower
as_snake_case <- function(string) {
  out <- stringr::str_replace_all(string, "([a-z])([A-Z])", "\\1_\\2")
  stringr::str_to_lower(out)
}

if_empty_as_na <- function(x) {
  ifelse(length(unlist(x)) == 0, NA, x)
}

are_null <- function(x) {
  vapply(x, is.null, logical(1))
}

if_null_as_na <- function(x, .type = NULL) {
  if(is.null(.type)) {
    return(ifelse(is.null(x), NA, x))
  }
  return(ifelse(is.null(x), as(NA, .type), x))
}

if_null_as_na_chr <- function(x) {
  if_null_as_na(x, .type = "character")
}

drop_if_named <- function(x, .names) {
  x[!names(x) %in% .names]
}

keep_if_named <- function(x, .names) {
  x[names(x) %in% .names]
}


clean_html <- function(x) {
  out <- stringr::str_replace_all(x, "<.*?>", " ")
  out <- stringr::str_squish(out)
  stringr::str_trim(out)
}


#' Basic map function wrapping `lapply()`.
nap <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

#' Map function to extract elements by name.
nap_extract <- function(.x, .f) {
  lapply(.x, `[[`, .f)
}

#' Type-enforced map.
nap_chr <- function(.x, .f, ...) {
  vapply(.x, .f, FUN.VALUE =  character(1), ..., USE.NAMES = FALSE)
}

#' Type-enforced map to extract elements by name.
nap_extract_chr <- function(x, .name, ...) {
  vapply(x, function(x) x[[.name]], FUN.VALUE = character(1), ..., USE.NAMES = FALSE)
}

#' @importFrom glue glue
try_connection <- function(expr, url) {
  sug <- "Are you connected to the Internet?\nDid the URL change?"
  # url <- "https://cityofsalinas.opendatasoft.com/api/v2/catalog/exports/rss"
  tryCatch(
    expr = expr,
    error = function(e) {
      stop(glue::glue("\nCannot connect to:\n\n{url}\n\n{sug}"), call. = FALSE)
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
                       cache_path = "~/salinasr-cache") {
  type <- rlang::arg_match(type, c("metadata", "dataset"))
  metadata_path <- glue::glue("{cache_path}/metadata")
  dataset_path <- glue::glue("{cache_path}/dataset")
  if(!dir.exists(cache_path)) {
    dir.create(cache_path)
  }
  if(!dir.exists(metadata_path)) {
    dir.create(metadata_path)
  }
  if(!dir.exists(metadata_path)) {
    dir.create(dataset_path)
  }
  if(type == "metadata") {
    target_path <- glue::glue("{metadata_path}/{dataset_id}-metadata.rds")
  }
  if(type == "dataset") {
    target_path <- glue::glue("{dataset_path}/{dataset_id}.rds")
  }
  
  readr::write_rds(x, target_path)
}















