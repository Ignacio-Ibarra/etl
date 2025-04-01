code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

id_fuente <- 221
fuente_raw <- sprintf("R%sC0",id_fuente)

# Función para verificar si el número de NAs en cada fila es mayor o igual a un umbral
check_na_threshold <- function(df, threshold) {
  apply(df, 1, function(row) {
    sum(is.na(row)) >= threshold
  })
}


clean_sheet <- function(sheet_name){
  
  # Leo datos
  sheet_data <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                                   sheet = sheet_name, 
                                   skip = 5, 
                                   col_names = T)
  
  sheet_name_normalized <- sheet_name %>% str_replace_all(., "_"," ")
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  data <- sheet_data %>% dplyr::filter(!filter_bool)
  
  # saco la fila ultima porque es una fila agregada
  data <- data[1:(nrow(data)-1),]
  
  #obtengo nombre de primer columna
  primera_col <- colnames(data)[1]
  
  # obtengo los anios
  primer_anio <- as.integer(colnames(data)[2])
  ultimo_anio <- primer_anio + num_cols - 2
  anios <- primer_anio:ultimo_anio
  
  # genero nuevo vector de columnas
  wide_cols <- c(primera_col, anios)
  colnames(data) <- wide_cols
  
  # pivoteo datos y genero columna con nombre de provincia
  df <- data %>% pivot_longer(cols = -all_of(primera_col), 
                              names_to = 'anio', 
                              names_transform = as.numeric, 
                              values_to = 'vab_pb', 
                              values_transform = as.numeric) %>% 
    mutate(provincia = sheet_name_normalized) %>% 
    janitor::clean_names()
  
}


sheet_names <- readxl::excel_sheets(get_raw_path(fuente_raw))
valid_sheet_names <- sheet_names[2:length(sheet_names)]

df_provincias <- purrr::map_dfr(.x = valid_sheet_names, .f = clean_sheet)

totales_provinciales <- df_provincias %>% 
  group_by(provincia, anio) %>% 
  summarise(vab_pb = sum(vab_pb, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(sector_de_actividad_economica = "Total sectores")

df_clean <- df_provincias %>% bind_rows(totales_provinciales)


dicc_provs <- read_csv(get_raw_path("R84C0")) %>% 
  select(prov_cod, prov_desc, reg_desc_fundar) %>% 
  mutate(region = case_when(
    reg_desc_fundar %in% c("Partidos del GBA", "Pampeana", "CABA") ~ "Pampeana y AMBA",
    reg_desc_fundar == "Noreste" ~ "NEA",
    reg_desc_fundar == "Noroeste" ~ "NOA",
    TRUE ~ reg_desc_fundar
  )) %>% 
  distinct(provincia_id = prov_cod, provincia = prov_desc, region) %>% 
  dplyr::filter(!(provincia_id == 6 & region == "Patagonia"))



normalizar_provincia <- function(name){
  norm_name <- geoAr::get_provincias(nombre=name)$nombre[1]
  cat(norm_name, "\n")
  if(is.null(norm_name)){
    return(name)
  }else{
    return(norm_name)
  }
} 

provincias_vec <- unique(df_clean$provincia)
provincias_normalizadas <- map_chr(.x = provincias_vec, .f = normalizar_provincia)
names(provincias_normalizadas) <- provincias_vec

# corrijo nombres de provincias
df_clean <- df_clean %>%
  mutate(provincia = recode(provincia, !!!provincias_normalizadas)) %>% 
  mutate(provincia = case_when(
    provincia == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
    provincia == "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ "Tierra del Fuego",
    TRUE ~ provincia)) %>% 
  left_join(dicc_provs, join_by(provincia))



# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

clean_filename <- glue::glue("{nombre_archivo_raw}_por_sector_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = "Desagregación provincial del valor agregado bruto de la Argentina, base 2004",
#                      script = code_name)

id_fuente_clean <- 92
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("sector_de_actividad_economica", "anio", "provincia_id"))

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)

