code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 274
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


# Función para verificar si el número de NAs en cada fila es mayor o igual a un umbral
check_na_threshold <- function(df, threshold) {
  apply(df, 1, function(row) {
    sum(is.na(row)) >= threshold
  })
}


clean_opex_provincia_rubro <- function(sheet_name, skip, lista_provincias){
  
  
  
  df <- readxl::read_excel(get_raw_path(fuente_raw),
                                 sheet = sheet_name, 
                                 col_names = F,
                                 skip = skip) 
  
  anios <- str_split_1(sheet_name, "-") %>% str_extract(., "(\\d{4}).*", group=1) %>%  as.integer(.)
  
  cols <- c("provincia_rubro",anios[1]:anios[2])
  
  names(df) <- cols
  
  # cuento cantidad de columnas
  num_cols <- length(cols)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(df, num_cols-1)
  
  
  lista_prov_norm <- lista_provincias %>% trimws(.) %>% janitor::make_clean_names() 
  
  df_clean <- df %>%  
    dplyr::filter(!filter_bool) %>% 
    mutate(
      provincia_rubro = case_when(
        provincia_rubro == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
        provincia_rubro == "Santa Fe" ~ "Santa Fé",
        provincia_rubro == "Entre Rios" ~ "Entre Ríos",
        provincia_rubro == "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~ "Tierra del Fuego",
        TRUE ~ provincia_rubro
      )) %>% 
    mutate(
      provincia_rubro_norm = str_replace(janitor::make_clean_names(trimws(provincia_rubro)), "_\\d$",""),
      provincia = trimws(ifelse(provincia_rubro_norm %in% lista_prov_norm, provincia_rubro, NA)),
      rubro = ifelse(!(provincia_rubro_norm %in% lista_prov_norm), provincia_rubro, "Total rubros")
    ) %>% 
    fill(provincia, .direction = "downup") %>% 
    select(-provincia_rubro, - provincia_rubro_norm) %>% 
    pivot_longer(!all_of(c('provincia', 'rubro')), 
                 names_to = 'anio', 
                 names_transform = as.integer,
                 values_to = "valor_expo",
                 values_transform = as.numeric) 
  
  
  return(df_clean)
  
}


lista_provincias <- c("Buenos Aires",
                "CABA",
                "Catamarca",
                "Chaco",
                "Chubut",
                "Córdoba",
                "Corrientes",
                "Entre Ríos",
                "Formosa",
                "Jujuy",
                "La Pampa",
                "La Rioja",
                "Mendoza",
                "Misiones",
                "Neuquén",
                "Río Negro",
                "Salta",
                "San Juan",
                "San Luis",
                "Santa Cruz",
                "Santa Fe",
                "Santiago del Estero",
                "Tierra del Fuego",
                "Tucumán",
                "Extranjero",
                "Indeterminado",
                "Plataforma continental")



skip <- 8

all_sheets <- readxl::excel_sheets(argendataR::get_raw_path(fuente_raw))

df_clean <- purrr::map_dfr(.x = all_sheets, 
                           .f = function(x) clean_opex_provincia_rubro(sheet_name = x, skip = skip, lista_provincias = lista_provincias))

clean_filename <- glue::glue("{nombre_archivo_raw}_all_sheets_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 143
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) %>% 
  mutate(anio = as.integer(anio))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio","provincia")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
