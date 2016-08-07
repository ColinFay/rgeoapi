#'Get Cities by Region 
#'
#'Get informations about all the cities in a french region by its INSEE code. Please note that this package works only with french cities.
#'
#'Takes a region INSEE ode, returns a data.frame with the available values.
#'@param codeRegion a numeric vector with a region INSEE code. 
#'@param postal wether or not to include postal codes. Default is FALSE.
#'@return Returns a data.frame with names of the cities, INSEE codes, postal codes, INSEE department codes, INSEE region codes, population of the cities (approx), surface of the cities (in hectares), lat and long (WGS-84). 
#'@export
#'@note If you don't know the INSEE code of the region you're looking for, you can find it by using the \code{\link{RegByName}} function.
#'@examples
#'ComByReg(codeRegion = 53) 

ComByReg <- function(codeRegion, postal=FALSE) {
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
  if(nchar(codeRegion) == 1) {
    codeRegion <- paste0("0", codeRegion)
  }
  if(postal){
    url <- paste0("https://geo.api.gouv.fr/communes?codeRegion=", codeRegion, "&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  } else {
    url <- paste0("https://geo.api.gouv.fr/communes?codeRegion=", codeRegion, "&fields=nom,code,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  }
  reg <- GET(url)
  if (reg$status_code == 200){
    content <- rjson::fromJSON(rawToChar(reg$content)) 
    if(length(content) == 0) {
      warning("No Content for that INSEE code : your input may not be an actual INSEE code (was ", codeRegion,")")
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
    warning("Bad API request : your input may not be an actual INSEE code (was ", codeRegion,")")
    identity <- default
    return(identity)
  }
}