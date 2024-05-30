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

sheet <- "labor force"
cell_range = "A8:D31"


enut_df <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), sheet = sheet, range = cell_range, col_names = col_names)

iso_pais <- argendataR::get_nomenclador_geografico() %>%
  select(iso3 = codigo_fundar, iso3_desc_fundar = desc_fundar)


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

strip_chars <- function(x) {
  if (is.numeric(x)) {
    return(x)
  } else {
    return(str_replace_all(str_trim(x), "\\.", ""))
  }
}

quitar_coma_adelante <- function(s) {
  if (startsWith(s, ",")) {
    s <- substr(s, 2, nchar(s))
  }
  return(str_trim(s))
}

cols_and_data_row_locations <- function(input_sheet) {
  input_sheet <- input_sheet %>%
    drop_na() %>%
    mutate_all(as.character)
  
  col0 <- input_sheet[[1]]
  upper_loc <- which(cumsum(is.na(col0)) == 1)[1]
  
  lower_loc <- which(!is.na(col0) & grepl("^\\d+$", col0))[1] - 2
  
  col_loc <- upper_loc + 1
  data_row_loc <- upper_loc + 2
  
  return(list(col_loc = col_loc, data_row_loc = data_row_loc))
}

normalizo_cedlas <- function(input_sheet, topico, lista_paises, multidx) {
  locs <- cols_and_data_row_locations(input_sheet)
  col_loc <- locs$col_loc
  data_row_loc <- locs$data_row_loc
  
  col_names <- input_sheet[col_loc,] %>%
    as.character() %>%
    unlist()
  
  input_sheet <- input_sheet[-c(1:data_row_loc - 1),]
  names(input_sheet) <- col_names
  
  normalized_df <- input_sheet %>%
    pivot_longer(cols = !c(pais, fuente, fecha_sedlac), 
                 names_to = "apertura", values_to = "valor") %>%
    mutate(apertura = quitar_coma_adelante(apertura),
           iso3 = mapeo_pais_iso3_adhoc[pais],
           # pais = mapeo_iso_pais[iso3],
           topico_sedlac = topico,
           subtopico_sedlac = NA,
           variable = NA,
           valor = valor) %>%
    select(topico_sedlac, subtopico_sedlac, variable, iso3, fuente, fecha_sedlac, apertura, valor)
  
  return(normalized_df)
}
