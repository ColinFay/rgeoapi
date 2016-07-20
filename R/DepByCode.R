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
#'DepByCode(35)

DepByCode <- function(codeDepartement) {
  if(nchar(codeDepartement) == 1) {
    codeDepartement <- paste0("0", codeDepartement)
  }
  url <- paste0("https://geo.api.gouv.fr/departements?code=", codeDepartement, "&fields=nom,code,codeRegion")
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
        identity <- data.frame(name = nom, codeInsee = codeInsee, codeRegion = codeRegion, stringsAsFactors = FALSE)
      }
      return(identity)
    }
  } else {
    print("Bad API request : your input may not be an actual INSEE code")
  }
}
