#' Enforce a `dataset`'s data types, as described in its `metadata`.
#' 
#' Intended for internal use, but exported as it may prove useful.
#' 
#' @param tbl [`tibble::tibble`]
#' @param metadata A `metadata` object as obtained by `sal_get_metadata()`.
#' 
#' @return [`tibble::tibble`]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @importFrom dplyr mutate_at select
#' @importFrom lubridate as_date as_datetime
sal_finalize_dataset <- function(tbl, metadata, cache_data = TRUE, ...) {
  # TODO there must be a better solution to enforce data types
  col_specs <- metadata$fields$type
  names(col_specs) <- metadata$fields$name
  
  ints <- names(col_specs[col_specs == "int"])
  dbls <- names(col_specs[col_specs == "double"])
  lgls <- names(col_specs[col_specs == "boolean"])
  dates <- names(col_specs[col_specs == "date"])
  dttms <- names(col_specs[col_specs == "datetime"])
  chrs <- names(col_specs[col_specs == "string"])
  geos <- names(col_specs[col_specs %in% c("geo_shape", "geo_point_2d")])
  
  out <- mutate_at(tbl, ints, as.integer)
  out <- mutate_at(out, dbls, as.double)
  out <- mutate_at(out, lgls, as.logical)
  out <- mutate_at(out, dates, as.Date)
  out <- mutate_at(out, dttms, lubridate::as_datetime)
  out <- mutate_at(out, chrs, as.character)
  out <- out[, !names(out) %in% geos]
  out <- select(out, metadata$fields$name[metadata$fields$name %in% names(out)])
  attr(out, "dataset_id") <- metadata$dataset_id
  
  if(cache_data) {
    cache_data(out, metadata$dataset_id, type = "dataset", ...)
  }
  
  out
}