
#limpio la memoria
rm( list=ls())  #Borro todos los objetos
gc()   #Garbage Collection

# Función para obtener la ruta del archivo, compatible tanto en RStudio como en la consola
get_file_location <- function() {
  # Intenta obtener la ruta del archivo en RStudio
  if (interactive() && "rstudioapi" %in% rownames(installed.packages())) {
    return(rstudioapi::getSourceEditorContext()$path)
  }
  
  # Alternativa para obtener la ruta si se usa source()
  this_file <- (function() { attr(body(sys.function(1)), "srcfile") })()
  
  # Si no se obtiene el path (e.g., en consola sin RStudio), asigna un valor por defecto
  if (!is.null(this_file)) {
    return(this_file$filename)
  } else {
    return("Archivo no especificado o ruta predeterminada")
  }
}

code_name <- get_file_location() %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 293
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


xlsx_path <- argendataR::get_raw_path(fuente_raw)

sheets <- readxl::excel_sheets(xlsx_path)[-(1:2)] # Saco la primera y segunda hoja 

white_cols <- function(df) {
  sapply(df, function (col) all(is.na(col)))
}


traduccion_regiones <- c(
  'Latin America' = 'América Latina y El Caribe', 
  'Eastern Europe' = 'Europa Oriental',
  'Sub Saharan Africa' = 'África Subsahariana',
  'North Africa' = 'África del Norte', 
  'Middle East' = 'Medio Oriente',
  'East Asia' = 'Asia Oriental', 
  'South Asia' = 'Asia del Sur',
  'Western Offshoots' = 'Ramificaciones de Occidente',
  'Western Europe' = 'Europa Occidental',
  'World' = 'Mundo', 
  'OECD' = 'OCDE', 
  'The Rest' = 'No OCDE'
)


codigos_iso_regiones <- c(
  'América Latina y El Caribe' = 'AHDI.LAC',
  'Europa Oriental' = 'AHDI.EEU',
  'Asia Oriental' = 'AHDI.EAS',
  'Medio Oriente' = 'AHDI.MEA',
  'África del Norte' = 'AHDI.NAF',
  'OCDE' = 'AHDI.OECD',
  'No OCDE' = 'AHDI.ROW',
  'Asia del Sur' = 'AHDI.SAS',
  'África Subsahariana' = 'AHDI.SSA',
  'Europa Occidental' = 'AHDI.WEU',
  'Mundo' = "WLD",
  'Ramificaciones de Occidente' = 'AHDI.WOF'
)

limpiar_sheet <- function(sheet_name){
  
  cat(sheet_name, "\n")
  
  cols_ <- readxl::read_excel(xlsx_path, sheet = sheet_name, col_names = F) %>% 
    slice(3) %>% select(-(1:2))
  
  regiones <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    pull() 
  
  if (sum(regiones == "West Offshoots") == 1){
    regiones[regiones == "Western Offshoots"] = "Western Europe"
    regiones[regiones == "West Offshoots"] = "Western Offshoots"
  }
  
  cols <- c('anio', regiones)
  
  str_variable_name <- readxl::read_excel(xlsx_path, sheet = sheet_name, col_names = F) %>% 
    slice(1) %>% 
    select(where(~ any(grepl("Table.*", .)))) %>% 
    pull() %>% 
    str_remove(., "Table \\d+\\. |Table \\d+\\.|Table \\d+\\ .")
  
  str_variable_id <- readxl::read_excel(xlsx_path, sheet = sheet_name, range = "A1", col_names = F) %>% 
    pull()
  
  sheet_data <- readxl::read_excel(xlsx_path, sheet = sheet_name, skip = 3, col_names = F) 
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  names(sheet_data) <- cols
  
  df <- sheet_data %>% 
    pivot_longer(
      !all_of(c('anio')),
      names_to = 'region',
      values_to = 'valor',
      values_transform = as.numeric
    ) %>% 
    dplyr::filter(region %in% names(traduccion_regiones)) %>% 
    mutate(
      variable_id = str_variable_id,
      variable_name = str_variable_name,
      region = traduccion_regiones[region],
      iso3 = codigos_iso_regiones[region]
    ) %>% 
    select(anio, iso3, region, variable_id, variable_name, valor)
  
  
  return(df)
  
}


df_clean <- purrr::map_dfr(sheets, limpiar_sheet)

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 162
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio", "iso3", "variable_id"))

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
