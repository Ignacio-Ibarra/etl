#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


id_fuente <- 230
fuente_raw <- sprintf("R%sC0",id_fuente)

sheet_name <- "Data"

df_clean <- readxl::read_excel(get_raw_path(fuente_raw), sheet = sheet_name) %>% 
  pivot_longer(!all_of(c("country","cnt", "var", "year", "War flag")), names_to = "sector") %>% 
  janitor::clean_names() %>% 
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
      sector == "Other services" ~ "R+S+T+U",
      sector == "TOTAL" ~ NA
    )
  )





# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - {sheet_name}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 101, path_clean = clean_filename, directorio = tempdir(), nombre = clean_title, script = code_name)

