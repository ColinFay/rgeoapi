#'Get Departments by Region 
#'
#'Get informations about all the departments in a french region by its INSEE code. Please note that this package works only with french departments.
#'
#'Takes a region INSEE code, returns a data.frame with the available values.
#'@param codeRegion a numeric vector with an INSEE code.
#'@return Returns a data.frame with departments names, INSEE codes, and region codes of the departments. 
#'@export
#'@note If you don't know the INSEE code of the region you're looking for, you can find it by using the \code{\link{RegByName}} function.
#'@examples
#'DepByReg(53)

DepByReg <- function(codeRegion) {
  if(nchar(codeRegion) == 1) {
    codeRegion <- paste0("0", codeRegion)
  }
  url <- paste0("https://geo.api.gouv.fr/departements?codeRegion=", codeRegion, "&fields=nom,code,codeRegion")
  ville <- GET(url)
  if (ville$status_code == 200){
    content <- rjson::fromJSON(rawToChar(ville$content)) 
    if(length(content) == 0) {
      print("No Content for that INSEE code : your input may not be an actual INSEE code")
    } else {
      identity <- data.frame()
      for(i in 1:length(content)) {
        obj <- content[[i]]
        if(is.null(obj$nom)) {
          nom <- NA
        } else {
          nom <- obj$nom
        }
        if(is.null(obj$code)) {
          codeInsee <- NA
        } else {
          codeInsee <- obj$code
        }
        if(is.null(obj$codeRegion)) {
          codeRegion <- NA
        } else {
          codeRegion <- obj$codeRegion
        }
        objbis <- data.frame(name = nom, codeInsee = codeInsee, codeRegion = codeRegion, stringsAsFactors = FALSE)
        identity <- rbind(identity,objbis)
      }
      return(identity)
    }
  } else {
    print("Bad API request : your input may not be an actual INSEE code")
  }
}
