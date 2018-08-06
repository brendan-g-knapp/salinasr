#' Import a dataset as a `tibble` or `sf` `tibble`.
#' 
#' @param metadata A `metadata` object as obtained by `sal_get_metadata()`.
#' @param prioritize_cache `logical`, whether to prioritize cached data and avoid 
#'     unnecessary API calls. \cr
#'     Default: `TRUE`
#' @param cache_dir `character`, path to the directory in which data is cached. \cr
#'     Defailt: `"~/salinasr-cache"`
#' @param ... Additional arguments to be passed to or from methods.
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
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_rds
#' @importFrom sf st_as_sf st_read
#' @importFrom tibble as_tibble
#' @export
sal_get_dataset <- function(metadata, prioritize_cache = TRUE, 
                            cache_dir = "~/salinasr-cache", ...) {
  if(prioritize_cache) {
    path <- glue("{cache_dir}/dataset/{metadata$dataset_id}.rds")
    if(file.exists(path)) {
      return(read_rds(path))
    }
  }
  if(is_geo(metadata)) {
    geojson <- paste0(metadata$links[metadata$links$rel == "exports", ]$href, "/geojson")
    sf <- st_read(geojson, stringsAsFactors = FALSE, quiet = TRUE)
    sf <- as_tibble(sf)
    sf <- st_as_sf(sf)
    out <- sal_finalize_dataset(sf, metadata, ...)
    
    return(out)
  }
  json <- paste0(metadata$links[metadata$links$rel == "exports", ]$href, "/json")
  resp <- GET(json)
  cont <- content(resp, as = "text")
  raw_list <- fromJSON(cont, simplifyVector = FALSE)
  out <- bind_rows(raw_list)
  out <- sal_finalize_dataset(out, metadata, ...)
  
  out
}
