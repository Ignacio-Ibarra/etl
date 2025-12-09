# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "empleo_turistico_grandes_ramas.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R49C16' # EPH Total Urbano


df_eph_tot <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

es_turistica <- function(x) x %in% c(4806, 4901, 4903, 5000, 5100, 5202, 5500, 5601, 5602, 7702, 7900, 9000)| (x >= 9000 & x < 9303)

clasificar_rama <- function(cod4) {
  dplyr::case_when(
    cod4 == 7900                                   ~ "Agencias de viaje",
    cod4 %in% c(9000, 9100, 9301, 9302, 9200)      ~ "Arte, recreación y cultura",
    cod4 %in% c(4806, 7702)                        ~ "Resto de actividades",
    cod4 == 5500                                   ~ "Alojamiento",
    cod4 %in% c(5601, 5602)                        ~ "Gastronomía",
    cod4 %in% c(4901, 4903, 5000, 5100, 5202)      ~ "Transporte de pasajeros",
    TRUE                                           ~ "Otras/No turísticas"
  )
}


df_intermediate <- df_eph_tot %>%
  dplyr::filter(ano4 == 2024, trimestre == 3) %>% 
  mutate(
    provincia_desc = case_when(
      provincia ==  2 ~ "CABA",
      provincia ==  6 ~ "Buenos Aires",
      provincia == 10 ~ "Catamarca",
      provincia == 14 ~ "Cordoba",
      provincia == 18 ~ "Corrientes",
      provincia == 22 ~ "Chaco",
      provincia == 26 ~ "Chubut",
      provincia == 30 ~ "Entre Rios",
      provincia == 34 ~ "Formosa",
      provincia == 38 ~ "Jujuy",
      provincia == 42 ~ "La Pampa",
      provincia == 46 ~ "La Rioja",
      provincia == 50 ~ "Mendoza",
      provincia == 54 ~ "Misiones",
      provincia == 58 ~ "Neuquen",
      provincia == 62 ~ "Rio Negro",
      provincia == 66 ~ "Salta",
      provincia == 70 ~ "San Juan",
      provincia == 74 ~ "San Luis",
      provincia == 78 ~ "Santa Cruz",
      provincia == 82 ~ "Santa Fe",
      provincia == 86 ~ "Santiago Del Estero",
      provincia == 90 ~ "Tucuman",
      provincia == 94 ~ "Tierra Del Fuego",
      TRUE ~ "Otro / No identificado"
    ),
    # tipos clave
    pp04b_cod = as.integer(pp04b_cod),
    ch04      = as.integer(ch04),   # sexo
    ch06      = as.integer(ch06),   # edad
    cat_ocup  = as.integer(cat_ocup),
    pp07h     = as.integer(pp07h),  # aporta jubilación
    pp04a     = as.integer(pp04a),  # público/privado
    estado    = as.integer(estado),
    act_turisticas = as.integer(es_turistica(pp04b_cod))
  ) %>% 
  group_by(ano4, trimestre, provincia_id = provincia, provincia_desc, ch04, ch06, estado, cat_ocup, pp07h, pp04a, pp04b_cod, act_turisticas) %>% 
  summarise(
    pondera = sum(pondera, na.rm = T)
  ) %>% 
  ungroup()


df_output <- df_intermediate %>% 
  dplyr::filter(estado == 1, act_turisticas == 1) %>% 
  mutate(
    rama_etq = clasificar_rama(pp04b_cod)
  ) %>%
  group_by(rama_etq) %>%
  summarise(emp = sum(pondera, na.rm = TRUE), .groups = "drop") %>%
  mutate(prop = 100 * emp / sum(emp, na.rm = TRUE)) %>%
  arrange(desc(prop))

