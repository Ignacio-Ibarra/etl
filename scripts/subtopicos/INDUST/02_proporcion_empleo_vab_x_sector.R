#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "proporcion_empleo_vab_x_sector.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R228C98' # CGI Asalariados registrados 
fuente2 <- 'R228C99' # CGI Asalariados no registrados 
fuente3 <- 'R228C100' # CGI No Asalariados
fuente4 <- 'R229C0' # fichas sectoriales cepxxi
fuente5 <- 'R223C94' # agregados macroeconomicos - cuadro 4

########################
# EMPLEO
########################


df_cgi_1 <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_cgi_2 <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_cgi_3 <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()

df_fs <- argendataR::get_raw_path(fuente4) %>% 
  read.csv()


# calcular datos de empleo cgi 
df_cgi <- df_cgi_1 %>% 
  left_join(df_cgi_2) %>% 
  left_join(df_cgi_3)
df_cgi <- df_cgi %>% 
  dplyr::filter(edad_sexo == 'Total general') %>% 
  dplyr::mutate(puestos_ar = tidyr::replace_na(puestos_ar,0),
                puestos_anr = tidyr::replace_na(puestos_anr,0),
                puestos_na = tidyr::replace_na(puestos_na,0)) %>% 
  dplyr::mutate(puestos = puestos_ar + puestos_anr + puestos_na) %>% 
  select(letra,letra_desc,anio,puestos) %>% 
  dplyr::mutate(letra = dplyr::case_when(letra == 'N' ~ 'N_O',
                                         letra == 'O' ~ 'N_O',
                                         letra == 'A + B' ~ 'A_B',
                                         TRUE ~ letra),
                letra_desc = dplyr::case_when(letra == 'N_O' ~ 'Servicios personales, sociales y de salud',
                                              TRUE ~ letra_desc)) %>% 
  dplyr::group_by(letra,letra_desc,anio) %>% 
  dplyr::summarize(puestos = sum(puestos))%>% 
  ungroup() %>% 
  dplyr::mutate(puestos = puestos * 1000) # multiplicar por mil para llevar a puestos en lugar de miles de puestos

# calcular datos de empleo de fichas 
df_fs_empleo <- df_fs %>% 
  dplyr::filter(nombre_variable %in% c('AR_PUESTOS','ANR_PUESTOS','NOASAL_PUESTOS')) %>% 
  mutate(valor = as.double(valor)) %>% 
  group_by(anio, id_sector_productivo,nombre_sector_productivo) %>% 
  summarize(puestos = sum(valor)) %>% 
  ungroup() %>% 
  filter(!stringr::str_detect(id_sector_productivo,'[0-9]')) %>% 
  filter(!stringr::str_detect(id_sector_productivo,'_PRIV')) %>%
  filter(!stringr::str_detect(id_sector_productivo,'_PUB')) %>%
  filter(!stringr::str_detect(id_sector_productivo,'_SPSP')) %>% 
  filter(!stringr::str_detect(id_sector_productivo, 'TE')) %>% 
  mutate(id_sector_productivo = if_else(id_sector_productivo == 'L_Q','L',id_sector_productivo))  %>% 
  dplyr::select(anio,puestos,letra = id_sector_productivo) %>% 
  dplyr::filter(anio < 2016)


df_empleo <- bind_rows(df_fs_empleo,
                       df_cgi %>% 
                         dplyr::select(anio,puestos,letra)) %>% 
  left_join(df_cgi %>% distinct(letra,letra_desc), join_by(letra)) %>% 
  mutate(letra_desc = case_when(letra == 'A_B' ~ 'Agro y pesca',
                                letra == 'C' ~ 'Petróleo y minería',
                                letra == 'D' ~ 'Industria manufacturera',
                                letra == 'E' ~ 'Electricidad, gas y agua',
                                letra == 'F' ~ 'Construcción',
                                letra == 'G' ~ 'Comercio',
                                letra == 'H' ~ 'Hotelería y restaurantes',
                                letra == 'I' ~ 'Transporte y comunicaciones',
                                letra == 'J' ~ 'Finanzas',
                                letra == 'K' ~ 'Serv. inmobiliarios y profesionales',
                                letra == 'L' ~ 'Adm. pública y defensa',
                                letra == 'M' ~ 'Enseñanza',
                                letra == 'N_O' ~ 'Serv. comunitarios, sociales y personales',
                                letra == 'P' ~ 'Servicio doméstico')) %>% 
  group_by(anio) %>% 
  mutate(prop_empleo = puestos / sum(puestos)) %>% 
  ungroup() %>% 
  select(anio,letra, letra_desc, prop_empleo)



########################
# VAB
########################

df_am_c4 <- argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet() 

df_vab <- df_am_c4 %>% 
  filter(trimestre == 'Total', sub_sector == 'Total sector')  %>% 
  mutate(sector = if_else(letra_desc_abrev %in% c('Agro','Pesca'),'Agricultura, ganadería, caza y silvicultura + Pesca',letra_desc_abrev),
         letra = if_else(letra %in% c('A','B'),'A_B',letra),
         sector = if_else(letra %in% c('N','O'),'Servicios personales, sociales y de salud',letra_desc_abrev),
         letra = if_else(letra %in% c('N','O'),'N_O',letra)) %>% 
  group_by(anio,letra) %>% 
  summarize(vab_pb = sum(vab_pb)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(prop_vab = vab_pb / sum(vab_pb)) %>% 
  ungroup() %>% 
  select(anio,letra, prop_vab)


#####################
# OUTPUT
#####################


# Joinear datos 
df_output <- df_vab %>% 
  left_join(df_empleo,by=c('anio','letra')) %>% 
  drop_na(prop_empleo) %>% 
  drop_na(prop_vab) %>% 
  select(anio, letra, letra_desc, prop_empleo, prop_vab)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 


pks <- c('anio','letra')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks
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

