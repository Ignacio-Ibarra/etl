# Codigo de limpieza de datos de cuadro de Jacques Charmes

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 96
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()



# Extraigo países y anios

extraer_pais <- function(string){
  word_vec <- str_split_1(string, " ")
  words <- str_extract(word_vec, "[a-zA-Z]+")
  country <- paste0(words[!is.na(words)], collapse = " ")
  return(country)
}

normalize_year <- function(year){
  if (is.na(year)){
    return(NA)
  }
  if (nchar(year)==4){
    return(as.numeric(year))
  }else{
    return(as.numeric(paste0("20",year)))
  }
}

extraer_anios <- function(string){
  word_vec <- str_split_1(string, "-")
  words <- str_extract(word_vec, "\\d+")
  anio_inicio <- NA
  anio_fin <- NA
  for (w in words){
    if (!is.na(w)){
      if (is.na(anio_inicio)){
        anio_inicio <- w
      }else{
        anio_fin <- w
      }
    }
  }
  
  if (is.na(anio_fin)){
    
    anio_fin <- anio_inicio
  }
  return(list(anio_inicio = normalize_year(anio_inicio), anio_fin = normalize_year(anio_fin)))
}


countries_years_range <- "A1:A87"
countries_years_df <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), col_names = T, range = countries_years_range)

countries_years_df <- countries_years_df %>% 
  select(country_year = `WOMEN (minutes per day)`) %>% 
  mutate( country = purrr::map(country_year, extraer_pais) %>% unlist(), 
          anio = purrr::map(country_year, extraer_anios)) 

countries_years_df <- countries_years_df %>% unnest_wider(anio) %>% select(2:4)
  

traduccion <- c("Trabajo no remunerado" = "Unpaid work",
                   "Trabajo remunerado" = "Paid work",
                   "Trabajo total" = "Total work",
                   "Aprendizaje y estudio" = "Learning",
                   "Ocio" = "Leisure",
                   "Cuidado personal" = "Personal care",
                   "Tiempo total" = "TOTAL",
                   "Sueño (incluido en cuidado personal)" = "Sleep (included in personal care)",
                   "Medios masivos de comunicación (incluido en ocio)" = "Mass media (included in leisure)")


women_range <- "G1:O87"
jcharm_women <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), col_names = T, range = women_range) %>% 
  rename(traduccion) %>% 
  mutate(sexo = "Mujeres")

jcharm_women <- bind_cols(countries_years_df, jcharm_women) %>% 
  pivot_longer(!any_of(c("country", "anio_inicio", "anio_fin", "sexo")), names_to = "subtipo_actividad", values_to = "minutos") 

men_range <- "W1:AE87"
jcharm_men <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), col_names = T, range = men_range)  %>% 
  rename(traduccion) %>% 
  mutate(sexo = "Varones")


jcharm_men <- bind_cols(countries_years_df, jcharm_men) %>% 
  pivot_longer(!any_of(c("country", "anio_inicio", "anio_fin", "sexo")), names_to = "subtipo_actividad", values_to = "minutos") 

jcharm <- bind_rows(jcharm_men, jcharm_women)

# Descargo Country Code de USDA para tenerlo en inglés
library(curl)
url <- "https://www.ars.usda.gov/ARSUserFiles/50620500/Bibliographies/country_code_web.xls"
destfile <- glue::glue("{tempdir()}/country_code_web.xls")

h <- new_handle()
handle_setheaders(h,
                  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
                  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                  "Accept-Language" = "en-US,en;q=0.5",
                  "Accept-Encoding" = "gzip, deflate, br",
                  "Connection" = "keep-alive",
                  "Upgrade-Insecure-Requests" = "1")

curl_download(url, destfile, handle = h)

#########################################################

#Leo country codes y me traigo los codigos. 
country_codes_en <- readxl::read_xls(destfile, col_names = c("country","iso3"), skip = 3)
jcharm <- jcharm %>% left_join(country_codes_en, by = join_by(country) )


# Los que no encontró se los imputo a mano. 
imputaciones <- jcharm %>% dplyr::filter(is.na(iso3)) %>% select(country) %>% distinct()
imputaciones$iso3_imput <- c("IRN", "KOR", "LUX", "MKD", "PSE", "PAN", "TWN", "TZA", "GBR", "USA", NA)

jcharm <- jcharm %>% left_join(imputaciones, by = join_by(country)) %>% 
  mutate(iso3 = case_when(
    is.na(iso3_imput) & is.na(iso3) ~ NA,
    is.na(iso3) ~ iso3_imput,
    TRUE ~ iso3)
  ) %>% 
  select(-iso3_imput)



geonomenclador.fundar <- argendataR::get_nomenclador_geografico() %>% select(iso3 = codigo_fundar, pais_desc = desc_fundar, continente_fundar )

# Agrego nombre en castellano de los países
jcharm_cleaned <- jcharm %>% left_join(geonomenclador.fundar, by = join_by(iso3)) %>% select(-country) %>% dplyr::filter(!is.na(iso3))

df_clean <- jcharm_cleaned %>% 
  mutate(tipo_actividad = ifelse(grepl("Trabajo", subtipo_actividad), "Trabajo", "No trabajo")) %>% 
  dplyr::filter(!(subtipo_actividad %in% c("Trabajo total", "Tiempo total"))) %>% 
  mutate(anios_observados = purrr::map2_chr(anio_inicio, anio_fin, ~ paste0(c(.x, .y), collapse = " - "))) %>%
  select(iso3, pais_desc, continente_fundar, anios_observados, sexo, tipo_actividad, subtipo_actividad, minutos)


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      directorio = tempdir(),
#                      nombre = "Uso del tiempo por país, sexo, tipo y subtipo de actividad, ultimo año disponible (Fundar en base a Jacques Charmes (2022)",
#                      script = code_name,
#                      descripcion = "Normalización de sheet de excel, traduccion de nombres de columnas, pivoteo de filas y columnas, normalizacion de nombres de países")


id_fuente_clean <- 24
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("iso3", "anios_observados","sexo","tipo_actividad","subtipo_actividad"))

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)



