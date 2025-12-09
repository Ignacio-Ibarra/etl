#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 486
fuente_raw <- sprintf("R%sC0",id_fuente)


clean_table <- function(raw_data){
  
  table_n <- raw_data %>% 
    slice_head(., n=1) %>% 
    pull(`...1`)
  
  variable_name <- raw_data %>% 
    slice(., 2:2) %>% 
    pull(`...1`)

  categorias <- raw_data %>% 
    slice(., 3:nrow(raw_data)) %>% 
    fill(`...1`, .direction = "down") %>% 
    pull(`...1`) 
    
  
  sub_categorias <- raw_data %>% 
    slice(., 3:nrow(raw_data)) %>% 
    fill(`...2`, .direction = "down") %>% 
    pull(`...2`) 
  
  
  values <- raw_data %>% 
    select(4:length(raw_data)) %>% 
    slice(., 3:nrow(raw_data))
  
  
  cols_ <- raw_data %>% 
    select(4:length(raw_data)) %>% 
    slice(., 1:2) %>% 
    t() %>% 
    as.data.frame()
  
  cols <- apply(cols_, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  
  names(values) <- cols
  
  df <- data.frame(nro_tabla = table_n, 
                         nombre_variable = variable_name, 
                         categoria = categorias,
                         sub_categoria = sub_categorias
                         ) %>% 
    bind_cols(values) %>% 
    pivot_longer(
      cols = !all_of(c('nro_tabla', 'nombre_variable', 'categoria', 'sub_categoria')),
      names_to = c('anio','mes'),
      values_to = 'valor',
      names_sep = "#",
      values_transform = as.numeric
    ) %>% 
    mutate(anio = as.integer(anio))

  cat(table_n," procesada\n")  
  
  return(df)
}


clean_sheet <- function(raw, idxs){
  
  results <- data.frame()
  for (i in 1:length(idxs)){
    
    start_row = idxs[i]
    
    if (i == length(idxs)){
      end_row = nrow(raw)
    }else{
    end_row = idxs[i+1] - 2
    }
    
    raw_data <- raw %>% 
      slice(., start_row:end_row) 
    
   results <- results %>% 
     bind_rows(.,
     clean_table(raw_data)
     )
  }
  
  return(results)
}



df_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  readxl::read_excel(., sheet = "OUT", 
                     col_names = F)


idxs <- which(grepl("TABLA", df_raw$...1))


df_clean <- clean_sheet(df_raw, idxs)



# Guardado de archivo
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
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)


id_fuente_clean <- 313
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = names(df_clean)[names(df_clean)!="valor"]
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)