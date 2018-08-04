#' Read a data set into an R object
#' 
#' @param metadata an object obtained by `sal_get_metadata()`
#' 
#' @return [`tibble::tibble`], which will also be an [`sf::st_sf`] object if
#'  `metadata` refers to data marked as having `"geo"` features.
#'    
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' library(ggplot2)
#' 
#' bikeways_sf <- sal_get_metadata("bikeways") %>% 
#'   sal_read_data()
#'   
#' bikeways_sf %>% 
#'   ggplot() +
#'   geom_sf()
#' 
#' @importFrom glue glue
#' @importFrom sf read_sf st_as_sf
#' @importFrom tibble as_tibble
#' @export
sal_read_data <- function(metadata) {
  if(class(metadata) != "metadata") {
    stop("`metadata` object as obtained by `sal_get_dataset()` required.")
  }
  if(!metadata$has_records) {
    stop(glue::glue("{metadata$dataset_id} has no records."))
  }
  
  if("geo" %in% metadata$features) {
    export_link <- sal_get_export_link(metadata, "geojson")
    out <- sf::read_sf(export_link, stringsAsFactors = FALSE)
    out <- sf::st_as_sf(tibble::as_tibble(out))
  }
  
  name_descrip <- unlist(metadata$fields)
  
  attr(out, "dataset_id") <- metadata$dataset_id
  
  out
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

#' Describe a data set's variables.
#' 
#' @param dataset [`tibble::tibble`] as obtained by `sal_read_data()`.
#' 
#' @return [`tibble::tibble`] with columns:
#'     \item{\bold{`name`} }{names of variables}
#'     \item\bold{`label`} }{(hopefully) helpful clarification of variables' names}
#'     \item{\bold{`type`} }{original data type of variables}
#'     
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(salinasr)
#' 
#' sal_get_metadata("bikeways") %>% 
#'   sal_read_data() %>% 
#'   sal_describe_vars()
#'   
#' @export
sal_describe_vars <- function(dataset) {
  if(is.null(attr(dataset, "dataset_id"))) {
    stop("`dataset` has no `dataset_id` attribute. Did you obtain `dataset` from `sal_read_data()`")
  }
  dataset_id <- attr(dataset, "dataset_id")
  
  sal_get_metadata(dataset_id)$fields
}