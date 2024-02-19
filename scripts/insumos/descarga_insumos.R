metadata <- read_csv("metadata.csv")

metadata %>% 
  distinct(subtopico_nombre, dataset_nombre, url_path, fuente_nombre, institucion) %>% 
  count(url_path, fuente_nombre, institucion, sort = T) %>% 
  print()


# tabla de insumos --------------------------------------------------------

# insumos <- tibble(id = numeric(), nombre = character(), url = character(),
#                       path = character(), actualizable = logical(), fecha = Date())


agregar_insumo <- function(df, nombre = NULL, url  = NULL, path  = NULL, 
                           actualizable = NULL, fecha = NULL) {
  
  
  if (any(length(nombre) == 0, length(url) == 0, length(path) == 0, length(actualizable) == 0) |
      any(is.null(nombre), is.null(url), is.null(path), is.null(actualizable))) {
    stop("Parametros invalidos, todos los parametros deben ser de largo 1 y no nulos")
  }
  
  
  
  df %>% 
    add_row(id = replace_na(last(df$id)+1, 1), nombre = nombre, url = url,
            path =  path, actualizable = actualizable, fecha  = Sys.Date())
  
}

# weo imf -----------------------------------------------------------------

# IMF outlook database   ----------- 
# descargo la base entera por mayor facilidad de referencia
# unidades 
# "NGDP_R" (pib) esta en miles de millones de moneda nacional constantes (1e9)
# "NGDPRPC" (pib per capita) esta en moneda nacional constantes
# "LP" (poblacion) esta en millones (1e6)

download.file(url = "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx",
              mode = "wb",
              destfile = glue::glue("data/_INSUMOS/raw/WEOOct2023all.xls"))

# insumos <- insumos %>% 
#   agregar_insumo(
#           nombre = "World Economic Outlook Database",
#           url = "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx",
#           path = "WEOOct2023all.xls",
#           actualizable = T,
#           fecha = Sys.Date()
#   )


# cedlas ---------------------------------------------------------------


# descarga

url_req <-  httr2::request("https://www.cedlas.econo.unlp.edu.ar/wp/en/estadisticas/isa/")
web_cedlas <- httr2::req_perform(url_req)

links <- web_cedlas$body %>% 
  xml2::read_html() %>% 
  xml2::xml_find_all("//*[contains(@class, 'vc_btn3-size-md')]") %>% 
  xml2::xml_attr("href")

links <- paste0("https://www.cedlas.econo.unlp.edu.ar", links)  


for(l in links) {
  
  name <- gsub("https://www.cedlas.econo.unlp.edu.ar/wp/wp-content/uploads/", "", l) %>% 
    tolower() %>%
    janitor::make_clean_names() %>%
    gsub("_xlsx", ".xlsx", .)
  
  download.file(l,
                mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
                destfile = sprintf("data/_INSUMOS/raw/%s", name)
  )
  print(l)
  # insumos <- insumos %>% 
  #   agregar_insumo(nombre = name, url = l, path = name, actualizable = T, fecha = Sys.Date())
}




# cuentas nacionales fundacion norte y sur   -----------
# ngdp_r = PIB moneda nacional constante 2004 (esta en miles)
# ngdprpc = PIB per capita moneda nacional constante 2004

download.file("https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls",
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/cuentas-nacionales-fundacion-norte-y-sur.xlsx"))


# insumos <- insumos %>% 
#   agregar_insumo(nombre = "cuentas nacionales fundacion norte y sur",
#                  url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls",
#                  path = "cuentas-nacionales-fundacion-norte-y-sur.xlsx",
#                  actualizable = T, fecha = Sys.Date())

# Maddison database  ----------- 
# GDP pc	Real GDP per capita in 2011$
# Population	Population, mid-year (thousands)

download.file(url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx", 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/mpd2020.xlsx"))


# insumos <- insumos %>% 
#   agregar_insumo(nombre = "Maddison Project Database 2020",
#                  url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx",
#                  path = "mpd2020.xlsx",
#                  actualizable = T, fecha = Sys.Date())

# api undp human development report office -----------
# https://api.hdrdata.org/swagger/index.html

# oferta y demanda global trimestral INDEC cuentas nacionales  -----------
oyd_cn_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_12_23.xls"  

download.file(url = oyd_cn_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/sh_oferta_demanda_12_23.xls"))

# insumos <- insumos %>% 
#   agregar_insumo(nombre = "oferta y demanda global trimestral INDEC",
#                  url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_12_23.xls",
#                  path = "sh_oferta_demanda_12_23.xls",
#                  actualizable = T, fecha = Sys.Date())

# proyeccion nacional poblacion indec   -----------
pob_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/c1_proyecciones_nac_2010_2040.xls"


download.file(url = pob_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_INSUMOS/raw/c1_proyecciones_nac_2010_2040.xls"))

# insumos <- insumos %>% 
#   agregar_insumo(nombre = "proyeccion nacional poblacion INDEC",
#                  url = "https://www.indec.gob.ar/ftp/cuadros/poblacion/c1_proyecciones_nac_2010_2040.xls",
#                  path = "c1_proyecciones_nac_2010_2040.xls",
#                  actualizable = T, fecha = Sys.Date())

# Valor agregado bruto e insumo de mano de obra por sector de actividad económica  -----------
serie_cgi <- "https://www.indec.gob.ar/ftp/cuadros/economia/serie_cgi_01_24.xls"

download.file(serie_cgi,
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/{subtopico}/datasets/raw/serie_cgi.xls"))
# 
# insumos <- insumos %>% 
#   agregar_insumo(nombre = "Valor agregado bruto e insumo de mano de obra por sector INDEC",
#                  url = "https://www.indec.gob.ar/ftp/cuadros/economia/serie_cgi_01_24.xls",
#                  path = "serie_cgi_01_24.xls",
#                  actualizable = T, fecha = Sys.Date())

# insumos %>% 
#   write_csv("data/_INSUMOS/insumos.csv")
