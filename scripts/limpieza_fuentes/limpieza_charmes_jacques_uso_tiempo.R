# Codigo de limpieza de datos de cuadro de Jacques Charmes

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 96
fuente_raw1 <- sprintf("R%sC0",id_fuente)

descargar_fuente_raw(id_fuente = id_fuente, tempdir())

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]


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
countries_years_df <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), col_names = T, range = countries_years_range)

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
jcharm_women <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), col_names = T, range = women_range) %>% 
  rename(traduccion) %>% 
  mutate(sexo = "Mujeres")

jcharm_women <- bind_cols(countries_years_df, jcharm_women) %>% 
  pivot_longer(!any_of(c("country", "anio_inicio", "anio_fin", "sexo")), names_to = "actividad", values_to = "minutos") 

men_range <- "W1:AE:87"
jcharm_men <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), col_names = T, range = women_range)  %>% 
  rename(traduccion) %>% 
  mutate(sexo = "Varones")


jcharm_men <- bind_cols(countries_years_df, jcharm_men) %>% 
  pivot_longer(!any_of(c("country", "anio_inicio", "anio_fin", "sexo")), names_to = "actividad", values_to = "minutos") 

jcharm <- bind_rows(jcharm_men, jcharm_women)

# Dejo acá falta terminar desarrollar. 
country_codes_en <- readxl::read_excel("https://www.ars.usda.gov/ARSUserFiles/50620500/Bibliographies/country_code_web.xls")
