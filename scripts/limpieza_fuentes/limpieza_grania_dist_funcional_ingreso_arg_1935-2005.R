#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 211
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

descargar_fuente_raw(id_fuente = id_fuente, tempdir())


procesar_string <- function(s ,n) {
  # Dividir la cadena por espacios para obtener los números
  numeros <- unlist(strsplit(s, " "))
  
  numeros <- numeros %>% str_replace(., ",",".")
  
  # Convertir los elementos a números
  numeros <- as.numeric(numeros)
  
  # Retornar el vector de longitud n
  return(numeros[1:n])
}


procesar_vector <- function(string_vector, n, cols) {
  # Aplicar la función procesar_string a cada elemento del vector
  lista_resultados <- lapply(string_vector, function(x) procesar_string(s = x, n = n))
  
  # Convertir la lista de resultados en una matriz y luego en un data.frame
  matriz_resultados <- do.call(rbind, lista_resultados)
  df_resultados <- as.data.frame(matriz_resultados)
  
  # Nombrar las columnas del data.frame
  colnames(df_resultados) <- cols
  
  return(df_resultados)
}


corregir_numero <- function(numero){return(numero/100)}

corregir_vector_numeros <- function(num_vector){
  return(unlist( lapply(num_vector, corregir_numero) ))
  }
  
  


tab <- tabulapdf::extract_tables(
  file = get_temp_path(fuente_raw1),
  pages = c(82),
  col_names = F,
  # method = "lattice",
  output = "tibble")

anio <- as.integer(tab[[1]]$X1[grepl("\\d+",tab[[1]]$X1)])
cons_priv <- as.numeric( tab[[1]]$X2[grepl("\\d+",tab[[1]]$X2)] %>% str_replace(., ",",".") )


ibif_vector <- tab[[1]]$X3[4:length(tab[[1]]$X3)]
ibif_df <- procesar_vector(ibif_vector, n = 3, cols = c("ibif_total","ibif_priv","ibif_pub"))

w_vector <- tab[[1]]$X4[4:length(tab[[1]]$X4)]
w_df <- procesar_vector(w_vector, n = 2, cols = c("masa_salarial","ingreso_asalariado"))

sbexp_vector <- tab[[1]]$X5[4:length(tab[[1]]$X5)]
sbexp_df <- procesar_vector(sbexp_vector, n = 2, cols = c("sup_bruto_exp","ingreso_capitalista"))

df_tab1 <- cbind(anio, cons_priv, ibif_df, w_df, sbexp_df)


df_tab2 <- tabulapdf::extract_tables(
  file = get_temp_path(fuente_raw1),
  pages = c(83),
  col_names = names(df_tab1),
  method = "lattice",
  output = "tibble")[[1]] 

anio_ <- df_tab2$anio

df_tab2 <- df_tab2 %>% 
  mutate_if(., ~ is.numeric(.) , corregir_vector_numeros) %>% 
  mutate_if(., is.character, function(vec) {as.numeric(str_replace(vec, ",","."))}) %>% 
  mutate(anio = anio_)

df_clean <- rbind(df_tab1, df_tab2)

titulo <- "Cuadro 2. Participación del Consumo Privado, Inversión Bruta Interna Total, Privada y Pública, Masa Salarial, Ingreso Asalariado, Superávit Bruto de Explotación e Ingreso Capitalista en el PBIpm.
1935-2005"

clean_filename <- glue::glue("cuadro2_{nombre_archivo_raw}_CLEAN.csv")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% write_csv_fundar(., file = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = glue::glue("Graña, Juan M. (2007) CEPED - {titulo}"),
#                      descripcion = "Se extraen datos de tabla de PDF",
#                      script = code_name)

comparacion <- comparar_fuente_clean(df = df_clean, df_anterior = read_fuente_clean("R211C77"), pk = "anio")

actualizar_fuente_clean(id_fuente_clean = 77,df = df_clean,
                        comparacion = comparacion)

