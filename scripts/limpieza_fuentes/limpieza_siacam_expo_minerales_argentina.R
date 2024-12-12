code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 268
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


tmp_file <- tempfile(pattern = "mineria", tmpdir = tempdir(), fileext = ".csv")

lineas <- readLines(argendataR::get_raw_path(fuente_raw), encoding='latin1')
lineas <- gsub('\"', '', lineas)
writeLines(lineas, tmp_file)

df_clean <- read.csv(tmp_file, sep=";",
                     na.strings = c("NA", "#N/A")) %>% 
  dplyr::filter(!is.na(ANYO)) %>% 
  janitor::clean_names() %>% 
  mutate(fob = as.numeric(gsub(",", ".", gsub("\\.", "", fob))),
         anyo = as.integer(anyo)) %>% 
  drop_na(fob) %>% 
  mutate(prov = ifelse(is.na(prov) | prov == "", "sin dato", prov),
         provincia = ifelse(prov == "sin dato", "sin dato", ifelse(provincia == "", "sin nombre", provincia))
         ) %>% 
  group_by(across(-fob)) %>% 
  summarise(fob = sum(fob, na.rm = T)) %>%
  ungroup() %>% 
  mutate(
    provincia = case_when(
      provincia == "Capital Federal" ~ "CABA",
      provincia == "Cordoba" ~ "Córdoba",
      provincia == "Entre Rios" ~ "Entre Ríos",
      provincia == "Neuquen" ~ "Neuquén",
      provincia == "Rio Negro" ~ "Río Negro",
      provincia == "Santiago Del Estero" ~ "Santiago del Estero",
      provincia == "Tierra Del Fuego" ~ "Tierra del Fuego",
      provincia == "Tucuman" ~ "Tucumán",
      provincia == "Santa Fe" ~ "Santa Fé",
      TRUE ~ provincia
    ))

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 137
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anyo", "prov", "destino", "mes", "grupo"))

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)

