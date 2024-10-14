
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 231
fuente_raw <- sprintf("R%sC0",id_fuente)

get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

df_clean <- haven::read_dta(get_raw_path(fuente_raw)) %>%
  mutate(across(everything(), ~ `attr<-`(.x, "label", NULL)))

names(df_clean) <- c("cnt", "var", "year", "Agriculture",
                    "Mining", "Manufacturing", "Utilities", "Construction",
                    "Trade services", "Transport services",
                    "Business services", "Financial services",
                    "Real estate", "Government services", "Other services" )

df_clean <- df_clean  %>% 
  pivot_longer(!all_of(c("cnt", "var", "year")), names_to = "sector") %>% 
  rename(var_code = var) %>% 
  mutate(
    var_desc = case_when(
      var_code == "VA" ~ "Gross value added at current basic prices (millons, local currency)",
      var_code == "VA_Q15" ~ "Gross value added at constant 2015 prices (millions, local currency)",
      var_code == "EMP" ~ "Number of persons engaged (thousands)"
    ),
    isic_4_code = case_when(
      sector == "Agriculture" ~ "A",
      sector == "Mining" ~ "B",
      sector == "Manufacturing" ~ "C",
      sector == "Utilities" ~ "D+E",
      sector == "Construction" ~ "F",
      sector == "Trade services" ~ "G+I",
      sector == "Transport services" ~ "H",
      sector == "Business services" ~ "J+M+N",
      sector == "Financial services" ~ "K",
      sector == "Real estate" ~ "L",
      sector == "Government services" ~ "O+P+Q",
      sector == "Other services" ~ "R+S+T+U"
    )
  )


# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 102, path_clean = clean_filename, directorio = tempdir(), nombre = clean_title, script = code_name)



