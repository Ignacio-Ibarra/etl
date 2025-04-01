#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "17_tasa_teletrabajo"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R50C0' # Encuesta Permanente de Hogares, Individual (2003) 
fuente2 <- 'R51C0' # Encuesta Permanente de Hogares, Individual (2004) 
fuente3 <- 'R52C0' # Encuesta Permanente de Hogares, Individual (2005) 
fuente4 <- 'R53C0' # Encuesta Permanente de Hogares, Individual (2006) 
fuente5 <- 'R54C0' # Encuesta Permanente de Hogares, Individual (2007) 
fuente6 <- 'R55C0' # Encuesta Permanente de Hogares, Individual (2008) 
fuente7 <- 'R56C0' # Encuesta Permanente de Hogares, Individual (2009) 
fuente8 <- 'R57C0' # Encuesta Permanente de Hogares, Individual (2010) 
fuente9 <- 'R58C0' # Encuesta Permanente de Hogares, Individual (2011) 
fuente10 <- 'R59C0' # Encuesta Permanente de Hogares, Individual (2012) 
fuente11 <- 'R60C0' # Encuesta Permanente de Hogares, Individual (2013) 
fuente12 <- 'R61C0' # Encuesta Permanente de Hogares, Individual (2014) 
fuente13 <- 'R62C0' # Encuesta Permanente de Hogares, Individual (2015) 
fuente14 <- 'R63C0' # Encuesta Permanente de Hogares, Individual (2016) 
fuente15 <- 'R64C0' # Encuesta Permanente de Hogares, Individual (2017) 
fuente16 <- 'R65C0' # Encuesta Permanente de Hogares, Individual (2018) 
fuente17 <- 'R66C0' # Encuesta Permanente de Hogares, Individual (2019) 
fuente18 <- 'R67C0' # Encuesta Permanente de Hogares, Individual (2020) 
fuente19 <- 'R68C0' # Encuesta Permanente de Hogares, Individual (2021) 
fuente20 <- 'R69C0' # Encuesta Permanente de Hogares, Individual (2022) 
fuente21 <- 'R70C0' # Encuesta Permanente de Hogares, Individual (2023) 


sources_eph <- fuentes_raw() %>% 
  dplyr::filter(grepl("^Encuesta Permanente de Hogares, Individual.*", nombre)) %>% 
  select(codigo, nombre)


# 
# # Codigo muteado para imprimir las fuentes
# for (i in 1:nrow(sources_eph)){
#   c <- sources_eph$codigo[i]
#   n <- sources_eph$nombre[i]
#   s <- glue::glue("fuente{i} <- '{c}' # {n}")
#   cat(s,"\n")
# }



codigos <- sources_eph$codigo

files <- codigos %>% purrr::map_chr(argendataR::get_raw_path)


# Datos de formalidad 
data_final_anual <- tibble()
data_final <- tibble()

i <- 1
for(i in 1:length(files)){
  
  suppressMessages(tmp <- readr::read_csv(files[i]))
  personas.filtro <- tmp %>%
    filter(ESTADO == "1") %>%
    mutate(CNO = case_when(str_detect(PP04D_COD, "^\\d{3}3\\d$") ~ "Informatizado",
                           TRUE ~ "No.informatizado"),
           Trab_vivienda = case_when(PP04G == "6" ~ "Vivienda",
                                     TRUE ~ "No.vivienda"),
           Empleo_remoto = case_when((CNO == "Informatizado" & Trab_vivienda == "Vivienda") ~ "Remoto",
                                     TRUE ~ "No.remoto"))
  
  #Tabla proporcion de trabajadores remotos por actividad
  personas.filtro_sbc <- personas.filtro %>% 
    mutate(sector = if_else(PP04B_COD %in% c(59,5900,# Actividades cinematográficas
                                             62,63,6200,6300, # Informatica 
                                             69,6900,70,7000,71,71000,72,7200,73,7301,7302,74,7400, # Actividades profesionales, científicas y técnicas
                                             78,7800 # suministro de empleo
    ),'SBC','Resto de la economía')) %>% 
    filter(sector == 'SBC')
  
  personas.filtro_ssi <- personas.filtro %>% 
    mutate(sector = if_else(PP04B_COD %in% c(62,63,6200,6300),'SSI','Resto de la economía')) %>% 
    filter(sector == 'SSI')
  
  personas.filtro_total <- personas.filtro %>% 
    mutate(sector = 'Total ocupados')
  
  personas.filtro_2 <- personas.filtro_total %>% 
    union_all(personas.filtro_ssi) %>% 
    union_all(personas.filtro_sbc)
  
  t_remotos_actividad <- personas.filtro_2 %>% 
    group_by(ANO4,TRIMESTRE,sector,Empleo_remoto) %>% 
    summarise(Poblacion = sum(PONDERA),
              Muestra = n()) %>%
    ungroup() %>%
    group_by(TRIMESTRE,sector) %>%
    mutate(proporcion.sobre.total.ocupado = (Poblacion / sum(Poblacion))) %>% 
    ungroup() %>% 
    arrange(ANO4,TRIMESTRE) %>% 
    select(Empleo_remoto,sector,proporcion.sobre.total.ocupado,Poblacion,Muestra,TRIMESTRE,ANO4) %>% 
    pivot_wider(names_from=c(Empleo_remoto,sector),values_from=c(proporcion.sobre.total.ocupado,Poblacion,Muestra))
  # Anual
  aux <- personas.filtro_2 %>% 
    group_by(ANO4,TRIMESTRE,sector,Empleo_remoto) %>% 
    summarise(Poblacion = sum(PONDERA),
              Muestra = n())
  data_final_anual <- bind_rows(data_final_anual,aux)
  data_final <- bind_rows(data_final,t_remotos_actividad)
  print(i)
}

data_final <- data_final %>% 
  arrange(ANO4,TRIMESTRE)

t_anual <- data_final_anual %>% 
  group_by(ANO4,Empleo_remoto,sector) %>% 
  summarize(Poblacion = sum(Poblacion),
            Muestra = sum(Muestra)) %>% 
  ungroup() %>% 
  group_by(ANO4,sector) %>% 
  mutate(proporcion.sobre.total.ocupado = (Poblacion / sum(Poblacion))) %>% 
  ungroup() %>% 
  arrange(ANO4) %>% 
  select(Empleo_remoto,proporcion.sobre.total.ocupado,sector,Poblacion,Muestra,ANO4) %>% 
  pivot_wider(names_from=c(Empleo_remoto,sector),values_from=c(proporcion.sobre.total.ocupado,Poblacion,Muestra))

t_anual <- t_anual %>% 
  select(anio=ANO4,SBC = proporcion.sobre.total.ocupado_Remoto_SBC,SSI=proporcion.sobre.total.ocupado_Remoto_SSI,Total=`proporcion.sobre.total.ocupado_Remoto_Total ocupados`)

df_output <- t_anual %>% 
  pivot_longer(-anio,names_to='sector',values_to='prop_teletrabajo') %>% 
  group_by(anio) %>% 
  dplyr::filter(sum(is.na(prop_teletrabajo)) == 0) %>% 
  ungroup()





df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") 


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('anio','sector'),
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


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    fuentes = colectar_fuentes(),
    subtopico = subtopico,
    analista = analista,
    pk = c('anio','sector'),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    unidades = list("prop_teletrabajo" = "porpoción")
  )
