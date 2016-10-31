#'Get Cities by Department 
#'
#'Get informations about all the cities in a French department by its INSEE Code. Please note that this package works only with French cities.
#'
#'Takes a department INSEE Code, returns a data.frame with the available values.
#'@param codeDepartement a numeric vector with a department INSEE Code. 
#'@param postal wether or not to include postal codes. Default is FALSE.  
#'@return Returns a data.frame with names of the cities, INSEE codes, postal codes, INSEE department codes, INSEE region codes, population of the cities (approx), surface of the cities (in hectares), lat and long of the cities (WGS-84). 
#'@export
#'@note If you don't know the INSEE code of the department you're looking for, you can find it by using the \code{\link{DepByName}} function.
#'@examples
#'ComByDep(codeDepartement = 35) 
#'ComByDep(codeDepartement = 29, postal = TRUE)

ComByDep <- function(codeDepartement, postal=FALSE) {
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
  if(nchar(codeDepartement) == 1) {
    codeDepartement <- paste0("0", codeDepartement)
  }
  if(postal){
    url <- paste0("https://geo.api.gouv.fr/communes?codeDepartement=", codeDepartement, "&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  } else {
    url <- paste0("https://geo.api.gouv.fr/communes?codeDepartement=", codeDepartement, "&fields=nom,code,codeDepartement,codeRegion,population,centre,surface&format=json&geometry=centre")
  }  
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("No Content for that INSEE code : your input may not be an actual INSEE code (was ", codeDepartement,")")
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
    warning("Bad API request : your input may not be an actual INSEE code (was ", codeDepartement,")")
    identity <- default
    return(identity)
  }
}
