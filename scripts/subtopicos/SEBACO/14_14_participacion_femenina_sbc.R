#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(lubridate)
library(forecast)

subtopico <- "SEBACO"
output_name <- "14_participacion_femenina_sbc"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R315C0' 
fuente2 <- 'R316C0' 

df_participacion <- argendataR::get_raw_path(fuente1) %>% 
  readr::read_csv(.)

df_puestos<- argendataR::get_raw_path(fuente2) %>% 
  readr::read_csv(.)


# Selección de sectores 
sectores_elegidos <- c(592000, # Equivalente a Edición de grabaciones (2213)
                       303000, # Equivalente a Fabricación y reparación de aeronaves (3530)
                       c(620000:639999), # SSI
                       c(721010:722020), # I+D
                       c(691001,691002), # Juridicos
                       692000, # Contabilidad
                       c(702091:702099), # Servicios de asesoramiento (dentro de contabilidad)
                       732000, # Estudios de mercado (dentro de contabilidad)
                       c(731001,731009), # Servicios de publicidad
                       780000, # Dotación de personal
                       742000, # Servicios de fotografía
                       c(591110,591120,591200) # Filmes
)


df_merge <- df_participacion %>% 
  left_join(df_puestos, join_by(fecha, clae6))


df_sbc <- df_merge %>% 
  dplyr::filter(clae6 %in% sectores_elegidos) %>% 
  mutate(sector = 'SBC') %>% 
  arrange(fecha) %>% 
  group_by(fecha, sector) %>% 
  summarize(share_mujer = stats::weighted.mean(share_mujer,puestos)) %>% 
  ungroup() %>%
  mutate(anio = year(fecha), mes = month(fecha)) %>%  # Extraer año y mes
  group_by(anio, sector) %>%
  filter(n() >= 11) %>%  # Filtrar años con al menos 11 meses disponibles
  complete(mes = 1:12, fill = list(share_mujer = NA)) %>%  # Asegurar todos los meses
  fill(share_mujer, .direction = "down") %>%  # Rellenar valores faltantes con el mes anterior
  ungroup() %>%
  mutate(fecha = make_date(anio, mes, 1)) %>%    # Reconstruir la columna fecha
  select(fecha, sector, share_mujer)


df_total <- df_merge %>% 
  mutate(sector = "Total privados") %>% 
  arrange(fecha) %>% 
  group_by(fecha, sector) %>% 
  summarize(share_mujer = stats::weighted.mean(share_mujer,puestos)) %>% 
  ungroup() %>%
  mutate(anio = year(fecha), mes = month(fecha)) %>%  # Extraer año y mes
  group_by(anio, sector) %>%
  filter(n() >= 11) %>%  # Filtrar años con al menos 11 meses disponibles
  complete(mes = 1:12, fill = list(share_mujer = NA)) %>%  # Asegurar todos los meses
  fill(share_mujer, .direction = "down") %>%  # Rellenar valores faltantes con el mes anterior
  ungroup() %>%
  mutate(fecha = make_date(anio, mes, 1)) %>%    # Reconstruir la columna fecha
  select(fecha, sector, share_mujer)
 

# completo diciembre del ultimo año. 


df_output <- bind_rows(df_sbc, df_total) %>% 
  mutate(anio = year(fecha)) %>% 
  group_by(anio, sector) %>% 
  summarise(
    prop_mujeres = mean(share_mujer)
  ) %>% 
  ungroup()



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(sector = ifelse(sector == "Total economia", "Total privados", "SBC"),
         anio = as.integer(anio))


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



etiquetas_nuevas <- data.frame(
  variable_nombre = c("sector", 
                      "prop_mujeres"),
  descripcion = c("Sector de actividad: 'SBC': 'Servicios basados en conocimiento' o 'Total privados': 'Total de sector privado'",
                  "Proporción puestos de trabajo privados ocupados por mujeres")
)



descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
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

aclaracion <- "Se tomaron los años que tuvieran al menos 11 meses, para el mes faltante se imputó el dato del mes anterior, con ello se construyeron luego datos anuales promediando los 12 meses. En sector == 'Total economia' se imputó 'Total privados' dado que el registro corresponde a los puestos de trabajo privados únicamente"
  
  
  
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
    aclaraciones = aclaracion,
    unidades = list("prop_mujeres" = "porpoción")
  )
