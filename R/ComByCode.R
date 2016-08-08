#'Get City by INSEE Code
#'
#'Get informations about a french city by its INSEE code. Please note that this package works only with french cities.
#'
#'Takes an INSEE Code, returns a data.frame with the available values.
#'@param codeInsee numeric vector with an INSEE code.
#'@param postal wether or not to include postal codes. Default is FALSE.
#'@return Returns a data.frame with names, INSEE code, postal code, INSEE department code, INSEE region code, population (approx), surface (in hectares), lat and long (WGS-84). 
#'@export 
#'@importFrom magrittr %>%
#'@importFrom httr GET
#'@importFrom rjson fromJSON
#'@note If you don't know the INSEE code of the city you're looking for, you can find it by using the \code{\link{ComByName}} function.
#'@examples
#' ComByCode(codeInsee = 29019) 
#' ComByCode(codeInsee = 31555, postal = TRUE)
#' ComByCode(codeInsee = ComByName("Rennes")[1,"codeInsee"])

ComByCode <- function(codeInsee, postal = FALSE) {
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
  if(nchar(codeInsee) == 4) {
    codeInsee <- paste0("0", codeInsee)
  }
  if(postal){
    url <- paste0("https://geo.api.gouv.fr/communes/", codeInsee, "?fields=nom,code,codesPostaux,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  } else {
    url <- paste0("https://geo.api.gouv.fr/communes/", codeInsee, "?fields=nom,code,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  }
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    contentlist <- list()
    contentlist[[1]] <- content
    if(length(content) == 0) {
      warning("No Content for that INSEE code : your input may not be an actual INSEE code (was ", codeInsee,")")
      identity <- default
    } else {
      if(postal){
        identity <- lapply(contentlist, function(obj){
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
        identity <- lapply(contentlist, function(obj){
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
    warning("Bad API request : your input may not be an actual INSEE code (was ", codeInsee,")")
    identity <- default
    return(identity)
  }
}
