#' Get a dataset as a useful R object.
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
#' library(sf, quietly = TRUE)
#' library(ggmap, quietly = TRUE)
#' library(ggplot2)
#' library(leaflet)
#' 
#' bikeways_sf <- sal_get_metadata("bikeways") %>% 
#'   sal_get_dataset()
#' 
#' bikeways_sf %>% 
#'   ggplot() +
#'   geom_sf()
#' 
#' sal_base_map <- get_map("Salinas, CA", zoom = 11)
#' 
#' ggmap(sal_base_map) +
#'   geom_sf(data = bikeways_sf, color = "red", inherit.aes = FALSE) 
#' 
#' # outputs interactive leaflet map:
#' bikeways_sf %>% 
#'   leaflet() %>% 
#'   addProviderTiles(leaflet::providers$OpenStreetMap, group = "Street") %>% 
#'   addProviderTiles(providers$Esri.WorldImagery, group = "Optical") %>% 
#'   addPolylines(label = ~ name,
#'                popup = paste0(
#'                  "<b>Name:</b> ", bikeways_sf$name, "<br>",
#'                  "<b>Jurisdiction:</b> ", bikeways_sf$juris, "<br>",
#'                  "<b>Start:</b> ", bikeways_sf$start, "<br>",
#'                  "<b>End</b>: ", bikeways_sf$end, "<br>",
#'                  "<b>Miles:</b> ", round(bikeways_sf$miles, 2), "<br>",
#'                  "<b>Feet:</b> ", round(bikeways_sf$feet, 2) 
#'                  )
#'                ) %>% 
#'   addLayersControl(baseGroups = c("Street", "Optical"),
#'                    options = layersControlOptions(collapsed = FALSE))
#'   
#' @references [sf: Simple Features for R](https://r-spatial.github.io/sf/)
#' 
#' @seealso [`ggplot2::geom_sf()`]
#' 
#' 
#' @importFrom glue glue
#' @importFrom sf read_sf st_as_sf
#' @importFrom tibble as_tibble
#' @export
sal_get_dataset <- function(metadata) {
  if(class(metadata) != "metadata") {
    stop("`metadata` object as obtained by `sal_get_dataset()` required.")
  }
  if(!metadata$has_records) {
    stop(glue::glue("{metadata$dataset_id} has no records."))
  }
  
  if("geo" %in% metadata$features) {
    export_link <- sal_get_export_link(metadata, "geojson")
    out <- sf::read_sf(export_link, stringsAsFactors = FALSE)
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

#' Describe a dataset's variables.
#' 
#' @param dataset [`tibble::tibble`] as obtained by `sal_get_dataset()`.
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
#'   sal_describe_vars()
#'   
#' @export
sal_describe_vars <- function(dataset) {
  if(is.null(attr(dataset, "dataset_id"))) {
    stop("`dataset` has no `dataset_id` attribute. Did you obtain `dataset` from `sal_get_dataset()`")
  }
  dataset_id <- attr(dataset, "dataset_id")
  
  sal_get_metadata(dataset_id)$fields
}