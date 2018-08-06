#' Obtain a URL pointing to a specific dataset in the desired file type.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
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
#' 
sal_get_export_link <- function(x, type = c("json", "csv", "geojson", "xls", "shp", "ov2")) {
  type <- arg_match(type, c("json", "csv", "geojson", "xls", "shp", "ov2"))
  UseMethod("sal_get_export_link")
}

#' @describeIn sal_get_export_link
#' 
#' Method to obtain a an export link from a `character` representing a `dataset_id`.
#' 
#' @importFrom rlang arg_match
#' @export
sal_get_export_link.character <- function(dataset_id, type = c("json", "csv", "geojson", "xls", "shp", "ov2")) {
  type <- arg_match(type, c("json", "csv", "geojson", "xls", "shp", "ov2"))
  metadata <- sal_get_metadata(dataset_id)
  sal_get_export_link(metadata, type)
}

#' @describeIn sal_get_export_link
#' 
#' Method to obtain a an export link from a `metadata` object.
#' 
#' @importFrom rlang arg_match
#' @export
sal_get_export_link.metadata <- function(x, type = c("json", "csv", "geojson", "xls", "shp", "ov2")) {
  type <- arg_match(type, c("json", "csv", "geojson", "xls", "shp", "ov2"))
  out <- x$links[x$links$rel == "exports", ]$href
  paste0(out, "/", type)
}