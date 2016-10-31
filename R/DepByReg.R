#'Get Departments by Region 
#'
#'Get informations about all the departments in a French region by its INSEE code. Please note that this package works only with French departments.
#'
#'Takes a region INSEE code, returns a data.frame with the available values.
#'@param codeRegion a numeric vector with an INSEE code.
#'@return Returns a data.frame with departments names, INSEE codes, and region codes of the departments. 
#'@export
#'@note If you don't know the INSEE code of the region you're looking for, you can find it by using the \code{\link{RegByName}} function.
#'@examples
#'DepByReg(codeRegion = 53)

DepByReg <- function(codeRegion) {
  . <- NULL 
  default <- data.frame(name = vector("character"), 
                        codeInsee = vector("character"),
                        codeRegion = vector("character"))
  if(nchar(codeRegion) == 1) {
    codeRegion <- paste0("0", codeRegion)
  }
  url <- paste0("https://geo.api.gouv.fr/departements?codeRegion=", codeRegion, "&fields=nom,code,codeRegion")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      warning("No Content for that INSEE code : your input may not be an actual INSEE code (was ", codeRegion,")")
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
    warning("Bad API request : your input may not be an actual INSEE code (was ", codeRegion,")")
    identity <- default
    return(identity)
  }
}
