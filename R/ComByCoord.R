#'Get City by Coordinates 
#'
#'Get informations about a french city by its Coordinates (WGS-84). Please note that this package works only with french cities.
#'
#'Takes the latitude and longitude of a city, returns a data.frame with the available values.
#'@param lat a character string with latitude (WGS-84) 
#'@param lon a character string with longitude (WGS-84)
#'@param postal wether or not to include postal codes. Default is FALSE.  
#'@return Returns a data.frame with name, INSEE code, postal code, INSEE department code, INSEE region code, population (approx), surface (in hectares), lat and long (WGS-84).
#'@export
#'@note If you don't know the coordinates of the city you're looking for, you can find it by using the \code{\link{ComByName}} function.
#'@examples
#'ComByCoord(lat = "48.11023", lon = "-1.678872") 

ComByCoord <- function(lat, lon, postal=FALSE) {
  . <- NULL 
  if(postal){
    default <- data.frame(name = vector("character"), 
                          codeInsee = vector("character"),
                          codesPostaux = vector("character"),
                          codeDepartement = vector("character"),
                          codeRegion = vector("character"),
                          population = vector("character"),
                          surface  = vector("character"),
                          lat  = vector("character"),
                          long = vector("character"))
  } else {
    default <- data.frame(name = vector("character"), 
                          codeInsee = vector("character"),
                          codeDepartement = vector("character"),
                          codeRegion = vector("character"),
                          population = vector("character"),
                          surface  = vector("character"),
                          lat  = vector("character"),
                          long = vector("character"))
  }
  if(postal){
    url <- paste0("https://geo.api.gouv.fr/communes?lon=", lon, "&lat=",lat, "&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  } else {
    url <- paste0("https://geo.api.gouv.fr/communes?lon=", lon, "&lat=",lat, "&fields=nom,code,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  } 
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("No Content for that coordinates : your input may not be actual coordinates (were ", lat," and ", lon, ")")
      identity <- default
    } else {
      if(postal){
        identity <- lapply(content, function(obj){
          data.frame(name = obj$nom %||% NA, 
                     codeInsee = obj$code %||% NA,
                     codesPostaux = obj$codesPostaux %||% NA,
                     codeDepartement = obj$codeDepartement %||% NA,
                     codeRegion = obj$codeRegion %||% NA,
                     population = obj$population %||% NA,
                     surface  = obj$surface %||% NA,
                     lat  = obj$centre$coordinates [2] %||% NA,
                     long = obj$centre$coordinates [1] %||% NA,
                     stringsAsFactors = FALSE)
        }) %>% do.call(rbind, .)  
      } else {
        identity <- lapply(content, function(obj){
          data.frame(name = obj$nom %||% NA, 
                     codeInsee = obj$code %||% NA,
                     codeDepartement = obj$codeDepartement %||% NA,
                     codeRegion = obj$codeRegion %||% NA,
                     population = obj$population %||% NA,
                     surface  = obj$surface %||% NA,
                     lat  = obj$centre$coordinates [2] %||% NA,
                     long = obj$centre$coordinates [1] %||% NA,
                     stringsAsFactors = FALSE)
        }) %>% do.call(rbind, .)  
      }
    }
    return(identity)
  } else {
    warning("Bad API request : your input may not be actual coordinates (were ", lat," and ", lon, ")")
    identity <- default
    return(identity)
  }
}