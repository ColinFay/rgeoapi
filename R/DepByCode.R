#'Get Department by INSEE Code
#'
#'Get informations about a french Department by its INSEE Code. Please note that this package works only with french Department.
#'
#'Takes an INSEE Code, returns a data.frame with the available values.
#'@param codeDepartement a numeric vector with an INSEE Code.
#'@return Returns a data.frame with name, INSEE code, and region code of the department. 
#'@export 
#'@note If you don't know the INSEE code of the department you're looking for, you can find it by using the \code{\link{DepByName}} function.
#'@examples
#'DepByCode(codeDepartement = 35)

DepByCode <- function(codeDepartement) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"),
                        codeRegion = vector("character"))
  if(nchar(codeDepartement) == 1) {
    codeDepartement <- paste0("0", codeDepartement)
  }
  url <- paste0("https://geo.api.gouv.fr/departements?code=", codeDepartement, "&fields=nom,code,codeRegion")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("No Content for that INSEE code : your input may not be an actual INSEE code (was ", codeDepartement,")")
      identity <- default    
    } else {
      identity <- lapply(content, function(obj){
        data.frame(name = obj$nom %||% NA, 
                   codeInsee = obj$code %||% NA, 
                   codeRegion = obj$codeRegion %||% NA, 
                   stringsAsFactors = FALSE)
      }) %>% do.call(rbind, .)
    }
    return(identity)
  } else {
    warning("Bad API request : your input may not be an actual INSEE code (was ", codeDepartement,")")
    identity <- default
    return(identity)
  }
}