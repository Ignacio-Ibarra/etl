# api undp human development report office -----------

apis_fuentes <- function(x = NULL) {
  
  apis <- list(
    undp = ""
    )
  
  apis_metadata <- list(
    undp = "https://api.hdrdata.org/swagger/index.html"
  )
  
  if (is.null(x)) {
    list(api = unlist(apis), metadata = unlist(apis_metadata))
  } else {
    list(api = apis[[x]], metadata = apis_metadata[[x]])
  }
  
}
