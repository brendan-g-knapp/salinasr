# {knapply}
#' @importFrom stringr str_replace_all str_to_lower
as_snake_case <- function(string) {
  out <- stringr::str_replace_all(string, "([a-z])([A-Z])", "\\1_\\2")
  stringr::str_to_lower(out)
}

are_null <- function(x) {
  vapply(x, is.null, logical(1))
}

if_null_as_na <- function(x, .type = NULL) {
  if(is_null(.type)) {
    return(ifelse(is.null(x), NA, x))
  }
  return(ifelse(is.null(x), as(NA, .type), x))
}

if_null_as_na_chr <- function(x) {
  if_null_as_na(x, .type = "character")
}

drop_if_named <- function(x, .names) {
  x[!names(x) %in% .name]
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







