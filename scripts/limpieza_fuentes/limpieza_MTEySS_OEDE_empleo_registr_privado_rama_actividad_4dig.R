
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

id_fuente <- 247
fuente_raw <- sprintf("R%sC0",id_fuente)

## descargo fuente

get_fuente_path("R247C0")

## limpio fuente

df_clean <- tibble()
hojas <- c(3:27)
nombre_hojas <- provincia <- openxlsx::getSheetNames(get_fuente_path("R247C0"))
for(i in hojas){
  empleo <- readxl::read_xlsx(get_fuente_path("R247C0"),sheet = i,skip = 6)
  empleo <- empleo %>% 
    filter(!is.na(Descripción))
  tmp <- empleo %>% 
    pivot_longer(-c(Ramas,Descripción),names_to='Trim',values_to='Empleo')
  tmp <- tmp %>% 
    mutate(Empleo = as.double(Empleo))
  tmp <- tmp %>% 
    mutate(provincia = nombre_hojas[i])
  df_clean <- bind_rows(df_clean,tmp)
  print(i)
}

nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
# df = df_clean,
# path_clean = clean_filename,
# nombre = clean_title,
# script = code_name)

id_fuente_clean <- 140
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)

df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("Trim", "Ramas","provincia")
)

actualizar_fuente_clean(id_fuente_clean = 140, 
                        path_clean = clean_filename, 
                        directorio = tempdir(), 
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)








