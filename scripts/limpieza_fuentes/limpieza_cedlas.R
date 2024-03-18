df_fuentes_raw <- fuentes_raw()

df_fuentes_raw_cedlas <-df_fuentes_raw %>% 
  filter(str_detect(nombre, "Indicadores Sociales de Argentina"))

map2(.x = df_fuentes_raw_cedlas$id_fuente,
     .y = df_fuentes_raw_cedlas$path_raw,
     .f = function(x,y) {
      
       descargar_fuente_raw(id_fuente = x,
                            dir = "data/_FUENTES/raw/")
       
       df <- readxl::read_excel(glue::glue("data/_FUENTES/raw/{y}"))
       
       df <- df %>% janitor::clean_names()
       
       path_clean <- gsub("\\.xlsx", "\\.csv", y)
       
       nombre <- df_fuentes_raw_cedlas$nombre[df_fuentes_raw_cedlas$id_fuente == x]
       
       df %>% 
         write_csv_fundar(glue::glue("data/_FUENTES/clean/{path_clean}"))

       # agregar_fuente_clean(id_fuente_raw = x,
       #                      path_clean = path_clean,
       #                      nombre = nombre,
       #                      script = "limpieza_cedlas.R")
  
       
       
     }  ) 
