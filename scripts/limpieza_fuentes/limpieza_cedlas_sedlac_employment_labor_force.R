# Codigo de limpieza de datos de Encuesta Nacional de Uso del Tiempo 2021 Cuadro 3

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 115
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

descargar_fuente_raw(id_fuente = id_fuente, tempdir())
descargar_fuente("R84C14")

sheets <- tidyxl::xlsx_sheet_names(argendataR::get_temp_path(fuente_raw1)) 
sheets <- sheets[sheets!="Index"]

# Cargo funciones para hacer limpieza
source("./scripts/limpieza_fuentes/funciones_limpieza_cedlas_sedlac.R")

# for (sheet in sheets){
#   
# }


sheet <- "labor force"

cedlas_df <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), sheet = sheet, col_names = F) %>%
  select_if(~ !all(is.na(.))) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  mutate(across(everything(), as.character)) %>%
  as.data.frame()


columnas_sheet <- obtengo_columnas(df = cedlas_df)

tabla_numeros <- obtengo_tabla_numeros(df = cedlas_df)

df <- data.frame(cedlas_df)


obtengo_paises_fuentes <- function(df, lista.paises) {
  df <- df %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(rowSums(is.na(.)) != ncol(.)) %>%
    as.data.frame()
  
  locs <- cols_and_data_row_locations(df)
  start_data <- locs$start_data
  
  col0 <- df[[1]]
  strings <- lapply(col0[(start_data-2):length(col0)], function(x) strip_chars(x) ) %>% unlist()
  
 
   paises_fuentes_dates <- data.frame(
    pais = character(),
    fuente = character(),
    fuente_orden = integer(),
    encuesta_date = character(),
    stringsAsFactors = FALSE
  )
  
  fuente_orden <- 0
  
  for (v in strings) {
    cat(v)
    if (!es_entero(v)) {
      if (v %in% lista_paises) {
        pais <- v
        fuente <- NULL
        fuente_orden <- 0
      } 
      else {
        if (is.na(as.numeric(strsplit(v, "-")[[1]][1]))) {
          fuente <- v
          fuente_orden <- fuente_orden + 1
        } 
        else {
          encuesta_date <- v
          # paises_fuentes_dates <- append(paises_fuentes_dates, list(list(pais, fuente, fuente_orden, encuesta_date)))
        }
      }
    } 
    else {
      encuesta_date <- v
      # paises_fuentes_dates <- append(paises_fuentes_dates, list(list(pais, fuente, fuente_orden, encuesta_date)))
    }
    paises_fuentes_dates <- rbind(paises_fuentes_dates,
      data.frame(pais = pais, fuente = fuente, fuente_orden = fuente_orden, encuesta_date = encuesta_date, stringsAsFactors = FALSE))
  }
}  




lista_paises <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                  "Costa Rica", "Dominican Rep", "Ecuador", "El Salvador",
                  "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama",
                  "Paraguay", "Peru", "Uruguay", "Venezuela")


mapeo_pais_iso3_adhoc <- c(
  'Argentina' = 'ARG', 
  'Bolivia' = 'BOL', 
  'Brazil' = 'BRA', 
  'Chile' = 'CHL', 
  'Colombia' = 'COL', 
  'Costa Rica' = 'CRI', 
  'Dominican Rep' = 'DOM', 
  'Ecuador' = 'ECU', 
  'El Salvador' = 'SLV', 
  'Guatemala' = 'GTM', 
  'Honduras' = 'HND', 
  'Mexico' = 'MEX', 
  'Nicaragua' = 'NIC', 
  'Panama' = 'PAN', 
  'Paraguay' = 'PRY', 
  'Peru' = 'PER', 
  'Uruguay' = 'URY', 
  'Venezuela' = 'VEN'
)



