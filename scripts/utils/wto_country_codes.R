
library(httr)
library(rvest)


get_wto_codes <- function(){

url <- "https://wits.worldbank.org/wits/wits/WITSHELP-es/content/codes/country_codes.htm"

html <- read_html(url)

tabla <- html %>%
  html_node("table.WT1") %>%  # busca <table class="WT1">
  html_nodes("tr")            # obtiene todas las filas

# Extraer los datos por fila y celda
datos <- lapply(tabla, function(fila) {
  fila %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
})


country_codes <- datos[3:length(datos)]
country_codes <- do.call(rbind, lapply(country_codes, function(x) as.data.frame(t(x), stringsAsFactors = FALSE)))

colnames(country_codes) <- c("name_es","wto_code","m49_code")

return(country_codes)
}
