#' Describe a dataset's variables.
#' 
#' @param x A `character` describing a `dataset_id`, a `tibble` as obtained by 
#' `sal_get_dataset()` or a `metadata` object as obtained by `sal_get_metadata()`.
#' 
#' @return [`tibble::tibble`] with columns:
#'     \item{\bold{`name`} }{names of variables}
#'     \item{\bold{`label`} }{(hopefully) helpful clarification of variables' names}
#'     \item{\bold{`type`} }{original data type of variables}
#'     
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' 
#' sal_get_metadata("bikeways") %>% 
#'   sal_get_dataset() %>% 
#'   sal_describe_dataset()
#' 
#' @export
sal_describe_dataset <- function(x) {
  UseMethod("sal_describe_dataset")
}

#' @describeIn sal_describe_dataset
#' 
#' Method to obtain description from a `tibble::tibble`
#' 
#' @export
#' 
sal_describe_dataset.tbl_df <- function(x) {
  x <- x
  if(is.null(attr(x, "dataset_id"))) {
    stop("`x` has no `dataset_id` attribute.\n
          Did you obtain `x` using `sal_get_dataset()`?
          You can also provide the `dataset_id`.")
  }
  dataset_id <- attr(x, "dataset_id")
  metadata <- sal_get_metadata(dataset_id)
  sal_describe_dataset(metadata)
}


#' @describeIn sal_describe_dataset
#' 
#' Method to obtain description from a `character`.
#' 
#' @export
#' 
sal_describe_dataset.character <- function(x) {
  metadata <- sal_get_metadata(x)
  sal_describe_dataset(metadata)
}

#' @describeIn sal_describe_dataset
#' 
#' Method to obtain description from a `metadata` object obtained by `sal_get_metadata()`.
#' 
#' @export
sal_describe_dataset.metadata <- function(x) {
  list(
    title = x$title,
    dataset_id = x$dataset_id,
    data_processed = x$data_processed,
    geospatial = is_geo(x),
    variables = x$fields
    )
}
