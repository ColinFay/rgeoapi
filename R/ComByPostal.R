#'Get City by Postal Code
#'
#'Get informations about a french city by its INSEE Code. Please note that this package works only with french cities.
#'
#'Takes a postal code, returns a data.frame with the available values.
#'@param codePostal a numeric vector with a postal code.
#'@return Returns a data.frame with name, INSEE code, postal code, INSEE department code, INSEE region code, population (approx), surface (in hectares), lat and long (WGS-84). 
#'@export
#'@note If you don't know the postal code of the city you're looking for, you can find it by using the \code{\link{ComByName}} function.
#'@examples
#'ComByPostal(codePostal = 29000)

ComByPostal <- function(codePostal) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"),
                        codesPostaux = vector("character"),
                        codeDepartement = vector("character"),
                        codeRegion = vector("character"),
                        population = vector("character"),
                        surface  = vector("character"),
                        lat  = vector("character"),
                        long = vector("character"))
  if(nchar(codePostal) == 4) {
    codePostal <- paste0("0", codePostal)
  }
  url <- paste0("https://geo.api.gouv.fr/communes?codePostal=", codePostal, "&fields=nom,codeInsee,codesPostaux,codeDepartement,codeRegion,codeRegion,population,centre,surface&format=json&geometry=centre")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("No Content for that code : your input may not be an actual postal code (was ", codePostal,")")
      identity <- default
    } else {
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
    }
    return(identity)
  } else {
    warning("Bad API request : your input may not be an actual postal code (was ", codePostal,")")
    identity <- default
    return(identity)
  }
}
