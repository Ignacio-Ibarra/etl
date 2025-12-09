#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 157
fuente_raw <- sprintf("R%sC0",id_fuente)

## descargo la fuente


# Función para procesar cada hoja
procesar_hoja <- function(hoja) {
  df <- readxl::read_excel(glue::glue(get_raw_path(fuente_raw)), sheet = hoja) 
  
  # Asignar nombres de columna desde la tercera fila
  columnas <- df[3,] %>% as.character()
  colnames(df) <- columnas %>% janitor::make_clean_names()
  
  df <- df %>% filter(0 == rowSums(is.na(pick(starts_with("x")))))
  df <- df[-1,]
  df <- df %>% 
    mutate(cod = gsub("\\..*|:","", sector_categoria),
           sector_categoria =  gsub(".*\\.|:","", sector_categoria) %>% 
             str_trim())

  
  df <- df %>% 
    mutate(sector = ifelse(!grepl("[A-Z]", cod), sector_categoria, NA))
  
  df <- df %>% 
    mutate(sector = ifelse(cod == "Total Jurisdiccion", "Total Jurisdiccion", sector))
  
  df <- df %>% 
    fill(sector, .direction = "down")
  
  df <- df %>% 
    rename(categoria = sector_categoria)
  
  df <- df %>% 
    pivot_longer(cols = starts_with("x"), names_to = "anio", values_to = "valor", names_transform = ~ gsub("x|_.*", "", .x), values_transform = as.character)
  
  print("### SUMA NA valores char ####")
  print(sum(is.na(df$valor)))
  
  print("### SUMA NA valores num ####")
  print(sum(is.na(as.numeric(df$valor))))
  
  # Limpiar nombres de columnas y crear la nueva columna 'sector'
  df <- df %>% 
    mutate(valor = as.numeric(valor),
           provincia = hoja)
  
  return(df)
}

# Obtener todas las hojas del archivo
hojas <- readxl::excel_sheets(get_raw_path(fuente_raw))

# Procesar todas las hojas a partir de la hoja 3
resultados <- lapply(hojas[3:length(hojas)], procesar_hoja)

# Combinar todos los resultados en un solo data frame
df_final <- bind_rows(resultados)


df_final <- df_final  %>% 
  mutate(cod_provincia = gsub("_.*", "", provincia),
         provincia = gsub(".*_", "", provincia))

df_final <- df_final %>% 
  select(cod_provincia, provincia, sector, categoria, anio, valor)
# # guardo csv
# write_csv_fundar(x = df_long,
#                  file = glue::glue("{tempdir()}/emisiones_prov_2010_2018.csv"))
# 
# # agrego fuente clean
# agregar_fuente_clean(id_fuente_raw = 157, 
#                      dir = tempdir(),
#                      path_clean = "emisiones_prov_2010_2018.csv",
#                      nombre = "Emisiones por sector y Provincia. Argentina 2010-2018",
#                      script = "limpieza_emisiones_provincia_2010_2018.R")

lista_comparacion <- comparar_fuente_clean(df_final , id = 67, pk = c("anio", "provincia", "sector"))

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 67,
                        df = df_final,
                        script = code_name,
                        comparacion = list("No comparables", "Cambios en agrupamiento de sectores"))



