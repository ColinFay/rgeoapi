#'Get City by Coordinates 
#'
#'Get informations about a french city by its Coordinates (WGS-84). Please note that this package works only with french cities.
#'
#'Takes the latitude and longitude of a city, returns a data.frame with the available values.
#'@param lat a character string with latitude (WGS-84) 
#'@param lon a character string with longitude (WGS-84)
#'@return Returns a data.frame with name, INSEE code, postal code, INSEE department code, INSEE region code, population (approx), surface (in hectares), lat and long (WGS-84).
#'@export
#'@note If you don't know the coordinates of the city you're looking for, you can find it by using the \code{\link{ComByName}} function.
#'@examples
#'ComByCoord(lat = "48.11023", lon = "-1.678872") 

ComByCoord <- function(lat, lon) {
  url <- paste0("https://geo.api.gouv.fr/communes?lat=", lat, "&lon=",lon, "&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    content <- content[[1]]
    if(length(content) == 0) {
      print("No Content for that coordinates : your input may not be actual coordinates")
    } else {
      if(is.null(content$nom)) {
        nom <- NA
      } else {
        nom <- content$nom
      }
      if(is.null(content$code)) {
        codeInsee <- NA
      } else {
        codeInsee <- content$code
      }
      if(is.null(content$codesPostaux)) {
        codesPostaux <- NA
      } else {
        codesPostaux <- content$codesPostaux
      }
      if(is.null(content$surface)) {
        surface <- NA
      } else {
        surface <- content$surface
      }
      if(is.null(content$centre)) {
        lat <- NA
        long <- NA
      } else {
        coord <- content$centre$coordinates
        lat <- coord[2]
        long <- coord[1]
      }
      if(is.null(content$codeDepartement)) {
        codeDepartement <- NA
      } else {
        codeDepartement <- content$codeDepartement
      }
      if(is.null(content$codeRegion)) {
        codeRegion <- NA
      } else {
        codeRegion <- content$codeRegion
      }
      if(is.null(content$population)) {
        population <- NA
      } else {
        population <- content$population
      }
      identity <- data.frame(nom = nom, codeInsee = codeInsee, codesPostaux = codesPostaux, codeDepartement = codeDepartement, codeRegion = codeRegion, population = population, surface = surface, lat=lat, long=long, stringsAsFactors = FALSE)
      return(identity)
    }
  } else {
    print("Bad API request : your input may not be actual coordinates")
  }
}

