
descargar_fuente("R134C0")

archivo <- "consumo_hogares_finalidad_porcentual_2017_2018_engho.csv"

df <- readxl::read_xls(get_temp_path("R134C0"), sheet = "Cuadro 13", trim_ws = F)

columnas <- df[3:4,] %>% 
  t() %>% 
  as_tibble() %>% 
  fill(V1) %>% 
  filter(!is.na(V2)) %>% 
  mutate(columnas =  paste(V2, V1, sep = "_")) %>% 
  pull(columnas)

df <- df %>% 
  select(-c(...4, ...6))

colnames(df) <- c("indicador", columnas)

df <- df %>% 
  filter(!is.na(indicador))

df <- df %>% 
  filter(if_any(-c(indicador), function(x) !is.na(x)))

df <- df[-1,] %>% 
  mutate(categoria = case_when(
    !str_detect(indicador, "^\\s") ~ indicador,
    T ~ NA),
    subcategoria = case_when(
      str_detect(indicador, "^\\s") ~ indicador,
      T ~ "Total"
    )
  )

df <- df %>% 
  select(-indicador)

df <- df %>% 
  fill(categoria)

df <- df %>% 
  mutate(subcategoria = gsub("^\\s+", "", subcategoria))

df <- df %>%   
  pivot_longer(-c(categoria, subcategoria))

df <- df %>% 
  separate(name, sep = "_", into = c("indicador", "periodo"))

df <- df %>% 
  select(periodo, indicador, categoria, subcategoria, valor = value)

df <- df %>% 
  arrange(periodo, indicador, categoria, subcategoria)

df <- df %>% 
  mutate(indicador = ifelse(indicador == "Porcentaje", "Porcentaje del gasto de consumo de los hogares", indicador))

df <- df %>% 
  mutate(valor = as.numeric(valor))

df %>% 
  write_csv_fundar(glue::glue("{tempdir()}/{archivo}"))

# agregar_fuente_clean(id_fuente_raw = 134,
#                      path_clean = archivo,
#                      nombre = "Cuadro 13.Gasto de consumo de los hogares por finalidad del gasto, en porcentaje. Total del país. Localidades de 5.000 y más habitantes. Años 2017-2018, 2004-2005 y 1996-1997",
#                      script = "scripts/limpieza_fuentes/limpieza_consumo_hogares_finalidad_porcentaje_engho_2018.R")
# 


actualizar_fuente_clean(61)
