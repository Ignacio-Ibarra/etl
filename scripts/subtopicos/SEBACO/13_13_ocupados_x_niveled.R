#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "13_ocupados_x_niveled"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R50C0' 
fuente2 <- 'R51C0' 
fuente3 <- 'R52C0' 
fuente4 <- 'R53C0' 
fuente5 <- 'R54C0' 
fuente6 <- 'R55C0' 
fuente7 <- 'R56C0' 
fuente8 <- 'R57C0' 
fuente9 <- 'R58C0' 
fuente10 <- 'R59C0' 
fuente11 <- 'R60C0' 
fuente12 <- 'R61C0' 
fuente13 <- 'R62C0' 
fuente14 <- 'R63C0' 
fuente15 <- 'R64C0' 
fuente16 <- 'R65C0' 
fuente17 <- 'R66C0' 
fuente18 <- 'R67C0' 
fuente19 <- 'R68C0' 
fuente20 <- 'R69C0' 
fuente21 <- 'R70C0'


codigos <- fuentes_raw() %>% 
  dplyr::filter(grepl("^Encuesta Permanente de Hogares, Individual.*", nombre)) %>% 
  pull(codigo)

# # Codigo muteado para imprimir las fuentes
# for (i in 1:length(codigos)){
#   c <- codigos[i]
#   s <- glue::glue("fuente{i} <- '{c}'")
#   cat(s,"\n")
# }

files <- codigos %>% purrr::map_chr(argendataR::get_raw_path)

# Datos de formalidad 
t1 <- tibble()
t2 <- tibble()
t3 <- tibble()
i <- 1
for (i in 1:length(files)){
  # Software
  suppressMessages(tmp <- data.table::fread(files[i]))
  # Filtrar por estado ocupacional
  tmp <- tmp %>% 
    dplyr::filter(ESTADO == 1) # Me quedo con los ocupados
  # Me quedo con los que estan dentro del sector de software 
  tmp <- tmp %>% 
    dplyr::filter(PP04B_COD %in% c(59,5900,# Actividades cinematográficas
                            62,63,6200,6300, # Informatica 
                            69,6900,70,7000,71,71000,72,7200,73,7301,7302,74,7400, # Actividades profesionales, científicas y técnicas
                            78,7800 # suministro de empleo
    ))
  # Armo dato 
  tmp <- tmp %>% 
    group_by(ANO4,TRIMESTRE,NIVEL_ED) %>% 
    summarize(Muestra = n(),
              Poblacion = sum(PONDERA,na.rm=T))
  # Corrijo labels 
  tmp <- tmp %>% 
    ungroup() %>% 
    mutate(Nivel = case_when(NIVEL_ED == 1 ~ 'a. Primario incompleto',
                             NIVEL_ED == 2 ~'b. Primario completo',
                             NIVEL_ED == 3~ 'c. Secundario incompleto',
                             NIVEL_ED == 4 ~ 'd. Secundario completo',
                             NIVEL_ED == 5 ~ 'e. Superior incompleto',
                             NIVEL_ED == 6 ~ 'f. Superior completo',
                             NIVEL_ED == 7 ~'g. Sin instrucción',
                             NIVEL_ED == 9 ~ 'h. Ns/Nr',
                             TRUE ~ 'i. Otros'),
           Prop_educ = Poblacion / sum(Poblacion))
  # Unir datos 
  t1 <- bind_rows(t1,tmp)
  
  # Total economia 
  suppressMessages(tmp <- data.table::fread(files[i]))
  # Filtrar por estado ocupacional
  tmp <- tmp %>% 
    dplyr::filter(ESTADO == 1) # Me quedo con los ocupados
  # Armo dato 
  tmp <- tmp %>% 
    group_by(ANO4,TRIMESTRE,NIVEL_ED) %>% 
    summarize(Muestra = n(),
              Poblacion = sum(PONDERA,na.rm=T))
  # Corrijo labels 
  tmp <- tmp %>% 
    ungroup() %>% 
    mutate(Nivel = case_when(NIVEL_ED == 1 ~ 'a. Primario incompleto',
                             NIVEL_ED == 2 ~'b. Primario completo',
                             NIVEL_ED == 3~ 'c. Secundario incompleto',
                             NIVEL_ED == 4 ~ 'd. Secundario completo',
                             NIVEL_ED == 5 ~ 'e. Superior incompleto',
                             NIVEL_ED == 6 ~ 'f. Superior completo',
                             NIVEL_ED == 7 ~'g. Sin instrucción',
                             NIVEL_ED == 9 ~ 'h. Ns/Nr',
                             TRUE ~ 'i. Otros'),
           Prop_educ = Poblacion / sum(Poblacion))
  # Unir datos 
  t2<- bind_rows(t2,tmp)
  
  # Privados 
  suppressMessages(tmp <- data.table::fread(files[i]))
  # Filtrar por estado ocupacional
  tmp <- tmp %>% 
    dplyr::filter(ESTADO == 1) # Me quedo con los ocupados
  # Me quedo con los del sector privado 
  tmp <- tmp %>% 
    dplyr::filter(PP04A == 2)
  # Armo dato 
  tmp <- tmp %>% 
    group_by(ANO4,TRIMESTRE,NIVEL_ED) %>% 
    summarize(Muestra = n(),
              Poblacion = sum(PONDERA,na.rm=T))
  # Corrijo labels 
  tmp <- tmp %>% 
    ungroup() %>% 
    mutate(Nivel = case_when(NIVEL_ED == 1 ~ 'a. Primario incompleto',
                             NIVEL_ED == 2 ~'b. Primario completo',
                             NIVEL_ED == 3 ~ 'c. Secundario incompleto',
                             NIVEL_ED == 4 ~ 'd. Secundario completo',
                             NIVEL_ED == 5 ~ 'e. Superior incompleto',
                             NIVEL_ED == 6 ~ 'f. Superior completo',
                             NIVEL_ED == 7 ~'g. Sin instrucción',
                             NIVEL_ED == 9 ~ 'h. Ns/Nr',
                             TRUE ~ 'i. Otros'),
           Prop_educ = Poblacion / sum(Poblacion))
  # Unir datos 
  t3 <- bind_rows(t3,tmp)
  
  print(i)
}

# Final 
final <- t1 %>% 
  mutate(Sector = 'SBC') %>% 
  union_all(t2 %>% mutate(Sector = 'Total economía')) %>% 
  union_all(t3 %>% mutate(Sector = 'Sector privado'))

anios_seleccion <- final %>% distinct(ANO4) %>% arrange(-ANO4) %>% slice(1:2) %>% pull(ANO4)


# Armar tabla informalidad final: ultimos 2 años 
df_output <- final %>% 
  dplyr::filter(ANO4 %in% anios_seleccion) %>% 
  group_by(Nivel,Sector) %>% 
  summarize(Muestra = sum(Muestra),
            Poblacion = sum(Poblacion)) %>% 
  ungroup() %>% 
  group_by(Sector) %>% 
  mutate(prop_educ = Poblacion / sum(Poblacion)) %>% 
  select(nivel = Nivel, sector = Sector, prop_educ)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") 


df_comparable <- final %>% 
  dplyr::filter(ANO4 %in% c(2021,2022)) %>% # años del dataset original del analista
  group_by(Nivel,Sector) %>% 
  summarize(Muestra = sum(Muestra),
            Poblacion = sum(Poblacion)) %>% 
  ungroup() %>% 
  group_by(Sector) %>% 
  mutate(prop_educ = Poblacion / sum(Poblacion)) %>% 
  select(nivel = Nivel, sector = Sector, prop_educ)
  

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_comparable,
  nombre = output_name,
  pk = c('nivel','sector'),
  drop_joined_df =  F
)


#-- Exportar Output ----

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
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)

colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}

str_anios <- sort(anios_seleccion) %>% paste0(., collapse = " y ")

aclaracion <- glue::glue("A fin de garantizar la mejor estimación posible se trabajó con un promedio de los ultimos dos años ({str_anios}), de forma tal de evitar que el tamaño muestral de la EPH afecte el resultado reportado.")

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    fuentes = colectar_fuentes(),
    subtopico = subtopico,
    analista = analista,
    pk = c('nivel','sector'),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    aclaraciones = aclaracion,
    unidades = list("prop_educ" = "porpoción")
  )
