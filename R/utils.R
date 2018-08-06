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

#' Does an object contain anything?
#' 
#' @param x An R object, typically a `list` or `vector`.
#' @param recursive `logical`, whether to `unlist` `x` to check for contents at any depth.
#' 
#' @return `logical`, whether `x` contains anything.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [`base::unlist`]
#' 
#' @examples
#' library(salinasr)
#'
#' vec <- NULL
#' vec
#' is_empty(vec)
#' 
#' nested_list <- list(a = list(b = list(c = list(d = NULL), e = NULL), f = NULL), g = NULL)
#' nested_list
#' is_empty(nested_list)
#' is_empty(nested_list, recursive = FALSE)
#' 
#' @export
is_empty <- function(x, recursive = TRUE) {
  if(recursive) {
    return(length(unlist(x)) == 0L)
  }
  length(x) == 0L
}

#' Filter an object to only contained elements with desired `.names`.
#' 
#' @param x A named R object.
#' @param .names `character`, vector of names by which to filter `x`.
#' 
#' @return Those elements of `x` whose name is in `.names`.
#' 
#' @seealso [`rlang::is_named()`]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' 
#' ages <- c(Tom = 20, Mary = 19, Sue = 43, Joe = 62)
#' ages
#' keep_if_named(ages, c("Mary", "Sue"))
#' 
#' @importFrom rlang is_named
#' @export
#' 
keep_if_named <- function(x, .names) {
  if(!is_named(x)) {
    stop("`x` does not have names.")
  }
  x[names(x) %in% .names]
}

#' Remove HTML tags from a string.
#' 
#' @param x A `character` `vector`.
#' 
#' @return `character`, `x` with HTML tags removed.
#' 
#' @seealso 
#' [`stringr::str_replace_all`], [`stringr::str_squish()`], [`stringr::str_trim()`]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' 
#' html <- "<html>
#'          <body>
#'          <p>This is a paragraph.</p>
#'          <p>This is another paragraph.</p>
#'          </body>
#'          </html>"
#' 
#' clean_html(html)
#' 
#' @importFrom stringr str_replace_all str_squish str_trim
#' @export
#' 
clean_html <- function(x) {
  if(!is.character(x)) {
    stop("`x` is not a `character`.")
  }
  out <- str_replace_all(x, "<.*?>", " ")
  out <- str_squish(out)
  str_trim(out)
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

#' A wrapper around [`glue::glue()`] to remove `glue` class.
#' 
#' `glue` provides string interpolation similar to Python's F-strings, but returned values
#' inherit the `glue` class. This seems to cause unexpected results in certain environments,
#' e.g. [`base::cat()`].
#' 
#' @param ... `character`, expressions to format.
#' @param .envir `environment` in which to evaluate expressions.
#' 
#' @return `character`
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso 
#' [`glue::glue()`]
#' [`glue::glue_data()`]
#' 
#' @examples
#' library(salinasr)
#' 
#' my_name <- "Brendan"
#' my_lang <- "R"
#' 
#' gum("My name is {my_name} and I am using {my_lang}.")
#' 
#' @importFrom glue glue
#' @export
#' 
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

#' Cache data obtained from the API.
#' 
#' Convenience function to minimize redundant API calls and generally be a polite Internet 
#' citizen.
#' 
#' @param x Either a `metadata` object or a [`tibble::tibble`].
#' @param dataset_id `character`, ID used to identify dataset. 
#' @param type `character`, `"metadata"` or `"dataset"`.
#' @param cache_dir `character`, path to read/write data.
#' @param ... Additional arguments passed on to [`readr::write_rds()`].
#' 
#' @return `readr::write_rds` returns `x` invisibly.
#' 
#' @importFrom glue glue
#' @importFrom readr write_rds
#' @importFrom rlang arg_match
cache_data <- function(x, dataset_id, type = c("metadata", "dataset"), 
                       cache_dir = "~/salinasr-cache", ...) {
  type <- arg_match(type, c("metadata", "dataset"))
  metadata_path <- glue("{cache_dir}/metadata")
  dataset_path <- glue("{cache_dir}/dataset")
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
    target_path <- glue("{metadata_path}/{dataset_id}-metadata.rds")
  }
  if(type == "dataset") {
    target_path <- glue("{dataset_path}/{dataset_id}.rds")
  }
  mess <- glue('Caching "{dataset_id}" {type} at {target_path}.')
  message(mess)
  
  write_rds(x, path = target_path, ...)
}

#' Does a dataset contain geospatial features?
#' 
#' @param x A `metadata` object obtained by `sal_get_metadata()`, a `tibble` obtained via
#' `sal_get_dataset()`, or a `character` representing a `dataset_id`.
#' 
#' @return `logical`, whether a dataset is annotated as containing geospatial features.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' 
#' is_geo("bikeways")
#' 
#' sal_get_metadata("bikeways") %>% 
#'   is_geo()
#'   
#' sal_get_metadata("bikeways") %>% 
#'   sal_get_dataset() %>% 
#'   is_geo()
#' 
#' @export
is_geo <- function(x) {
  UseMethod("is_geo")
}

#' @describeIn is_geo
#' 
#' Method to check if a `metadata` object obtained via `sal_get_metadata()` references a 
#' dataset that `is_geo()`.
#' 
#' @export
#' 
is_geo.metadata <- function(x) {
  "geo" %in% x$features
}
#' @describeIn is_geo
#' 
#' Method to check if a `tibble` obtained via `sal_get_dataset()` references a dataset 
#' that `is_geo()`.
#' 
#' @export
#' 
is_geo.tbl_df <- function(x) {
  metadata <- sal_get_metadata(x)
  is_geo(metadata)
}
#' @describeIn is_geo
#' 
#' Method to check if a `character` representing a `dataset_id` references a dataset that
#' `is_geo()`.
#' 
#' @export
#' 
is_geo.character <- function(x) {
  metadata <- sal_get_metadata(x)
  is_geo(metadata)
}
