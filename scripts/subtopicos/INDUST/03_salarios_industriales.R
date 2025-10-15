#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "salarios_industriales.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R238C146' # Descriptor 
fuente2 <- 'R239C300' # salarios 2 digitos C4
fuente3 <- 'R238C145' # puestos 2 digitos C3
fuente4 <- 'R239C111' # salario_letra


df_dicc <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_wage <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_puestos <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()

df_sal_letra <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet()

# Filtrar diccionario 
df_dicc_2d <- df_dicc %>% 
  filter(digitos == 2) %>% 
  select(-digitos) %>% 
  mutate(codigo = str_pad(codigo,2,pad = '0',side='left'))

# Salario de los privados 
df_sal_privados <- df_wage %>% 
  filter(rama_de_actividad == 'Total')


descripcion_abrev <- c(
  "15" = "Alimentos y bebidas",
  "16" = "Tabaco",
  "17" = "Textiles",
  "18" = "Prendas de vestir y pieles",
  "19" = "Cueros y calzado",
  "20" = "Madera y corcho",
  "21" = "Papel",
  "22" = "Edición e impresión",
  "23" = "Productos del petróleo",
  "24" = "Productos químicos",
  "25" = "Caucho y plástico",
  "26" = "Productos minerales no metálicos",
  "27" = "Metales comunes",
  "28" = "Productos de metal",
  "29" = "Maquinaria y equipo",
  "30" = "Maquinaria de oficina e informática",
  "31" = "Maquinaria eléctrica",
  "32" = "Radio, TV y comunicaciones",
  "33" = "Instrumentos médicos y de precisión",
  "34" = "Vehículos automotores",
  "35" = "Equipos de transporte",
  "36" = "Muebles y otras manufacturas",
  "37" = "Reciclamiento",
  "99" = "Promedio industria"
)


df_output <- df_wage %>% 
  dplyr::filter(rama_de_actividad != "Total") %>% 
  select(ciiu_rev3_4d,anio,salario_promedio_puestos_privados) %>% 
  inner_join(
    # Join con puestos
    df_puestos %>% 
      dplyr::filter(rama_de_actividad != "Total") %>%  
      select(ciiu_rev3_4d,anio,cant_promedio_puestos_privados),
    join_by(ciiu_rev3_4d, anio)
  ) %>% 
  mutate(
    # Creo ciiu 2digitos
    ciiu_rev3_2d = str_pad(ciiu_rev3_4d,4,pad = '0',side='left')%>% 
           str_extract(.,'\\d{2}')) %>% 
  drop_na(cant_promedio_puestos_privados) %>% 
  group_by(anio, ciiu_rev3_2d) %>% 
  summarize(
    # salario ponderado a 2d
    salario_ponderado = stats::weighted.mean(salario_promedio_puestos_privados, cant_promedio_puestos_privados)) %>% 
  ungroup() %>% 
  left_join(df_dicc_2d, join_by(ciiu_rev3_2d == codigo)) %>% 
  filter(as.integer(ciiu_rev3_2d) %in% c(15:37)) %>% 
  bind_rows(., 
            # Agrego promedio de industria
            df_sal_letra %>% 
              filter(letra == 'D')  %>% 
              mutate(ciiu_rev3_2d = "99",
                     descripcion = 'Promedio industria')  %>% 
              select(anio, ciiu_rev3_2d, salario_ponderado = salario_promedio_privados)) %>% 
  left_join(
    # Tomo media del sector privado
    df_sal_privados %>% select(anio, salario_promedio_puestos_privados),
    join_by(anio)
    
  ) %>% 
  mutate(salario_respecto_media = ((salario_ponderado - salario_promedio_puestos_privados)/salario_promedio_puestos_privados)*100,
         descripcion_corta = descripcion_abrev[ciiu_rev3_2d]) %>% 
  select(-c(salario_ponderado,salario_promedio_puestos_privados)) %>% 
  select(anio, ciiu_rev3_2d, descripcion, descripcion_corta, salario_respecto_media)




df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico, drive = T) 


df_comparable <- df_output %>% 
  rename(ciiu_rev3_4d = ciiu_rev3_2d) %>% 
  mutate(anio = as.numeric(anio), 
         ciiu_rev3_4d = as.numeric(ciiu_rev3_4d))


pks_comparacion <- c('anio','ciiu_rev3_4d')

comparacion <- argendataR::comparar_outputs(
  df = df_comparable,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks_comparacion
)



armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidad = list("poblacion" = "unidades", "share" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")



