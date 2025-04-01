################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "11_ocupados_x_condicion"
analista = "Nicolas Sidicaro"
fuentes <- c("R63C0", "R64C0", "R65C0", "R66C0", "R67C0", "R68C0", "R69C0")#, "R70C0")

# Inicializar lista para almacenar los datos procesados
data_list <- list()

# Iterar sobre las fuentes
for (fuente in fuentes) {
  # Leer y procesar cada fuente
  data <- read_csv(get_raw_path(fuente)) %>% 
    select(
      ANO4,
      TRIMESTRE,
      PONDERA,
      PP04B_COD,
      CAT_OCUP,
      ESTADO,
      PP07H,
      PP04A)
  
  # Almacenar en la lista
  data_list[[fuente]] <- data
}

# Combinar todos los datos en un solo data.frame
data_final <- bind_rows(data_list)

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R"
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

#readr::read_csv(argendataR::get_temp_path("RXXCX"))

get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

t1 <- tibble()
t2 <- tibble()
t3 <- tibble()
t4 <- tibble()

# Filtrar por estado ocupacional
tmp1 <- data_final %>% 
  filter(ESTADO == 1) # Me quedo con los ocupados

# Me quedo con los que estan dentro del sector de SBC 
tmp1 <- tmp1 %>% 
  filter(PP04B_COD %in% c(59,5900,# Actividades cinematográficas
                          62,63,6200,6300, # Informatica 
                          69,6900,70,7000,71,71000,72,7200,73,7301,7302,74,7400, # Actividades profesionales, científicas y técnicas
                          78,7800 # suministro de empleo
  ))
# Corrijo labels 
tmp1 <- tmp1 %>% 
  ungroup() %>% 
  mutate(estado_ocupacional = case_when(CAT_OCUP == 1 ~ 'No asalariado',
                                        CAT_OCUP == 2 ~ 'No asalariado',
                                        CAT_OCUP == 3 & PP07H == 1 ~ 'Asalariado registrado',
                                        CAT_OCUP == 3 & PP07H == 2 ~ 'Asalariado no registrado',
                                        CAT_OCUP == 4 ~ 'No asalariado',
                                        TRUE ~ 'Otros')) %>% 
  group_by(ANO4,TRIMESTRE,estado_ocupacional) %>% 
  summarize(Muestra = n(),
            Poblacion = sum(PONDERA,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Prop_formalidad = Poblacion / sum(Poblacion))
# Unir datos 
t1 <- plyr::rbind.fill(t1,tmp1)

## Tasa de asalariados en Total economia ## 

# Filtrar por estado ocupacional
tmp2 <- data_final %>% 
  filter(ESTADO == 1) # Me quedo con los ocupados

# Corrijo labels 
tmp2 <- tmp2 %>% 
  ungroup() %>% 
  mutate(estado_ocupacional = case_when(CAT_OCUP == 1 ~ 'No asalariado',
                                        CAT_OCUP == 2 ~ 'No asalariado',
                                        CAT_OCUP == 3 & PP07H == 1 ~ 'Asalariado registrado',
                                        CAT_OCUP == 3 & PP07H == 2 ~ 'Asalariado no registrado',
                                        CAT_OCUP == 4 ~ 'No asalariado',
                                        TRUE ~ 'Otros')) %>% 
  group_by(ANO4,TRIMESTRE,estado_ocupacional) %>% 
  summarize(Muestra = n(),
            Poblacion = sum(PONDERA,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Prop_formalidad = Poblacion / sum(Poblacion))
# Unir datos 
t2 <- plyr::rbind.fill(t2,tmp2)

## Tasa de asalariados en Total economia privada ## 

# Filtrar por estado ocupacional
tmp3 <- data_final %>% 
  filter(ESTADO == 1) # Me quedo con los ocupados

# Seleccionar sector privado 
tmp3 <- tmp3 %>% 
  filter(PP04A == 2)

# Corrijo labels 
tmp3 <- tmp3 %>% 
  ungroup() %>% 
  mutate(estado_ocupacional = case_when(CAT_OCUP == 1 ~ 'No asalariado',
                                        CAT_OCUP == 2 ~ 'No asalariado',
                                        CAT_OCUP == 3 & PP07H == 1 ~ 'Asalariado registrado',
                                        CAT_OCUP == 3 & PP07H == 2 ~ 'Asalariado no registrado',
                                        CAT_OCUP == 4 ~ 'No asalariado',
                                        TRUE ~ 'Otros')) %>% 
  group_by(ANO4,TRIMESTRE,estado_ocupacional) %>% 
  summarize(Muestra = n(),
            Poblacion = sum(PONDERA,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Prop_formalidad = Poblacion / sum(Poblacion))
# Unir datos 
t3 <- plyr::rbind.fill(t3,tmp3)

# Con apertura sectorial
# En SBC

# Filtrar por estado ocupacional
tmp4 <- data_final %>% 
  filter(ESTADO == 1) # Me quedo con los ocupados

# Me quedo con los que estan dentro del sector de SBC 
tmp4 <- tmp4 %>% 
  filter(PP04B_COD %in% c(59,5900,# Actividades cinematográficas
                          62,63,6200,6300, # Informatica 
                          69,6900,70,7000,71,71000,72,7200,73,7301,7302,74,7400, # Actividades profesionales, científicas y técnicas
                          78,7800 # suministro de empleo
  ))

# Armar sectores 
tmp4 <- tmp4 %>% 
  mutate(sector = case_when(PP04B_COD %in% c(69,6900,70,7000,71,71000,72,7200,73,7301,7302,74,7400) ~ 'a. Actividades profesionales, cientificas y técnicas',
                            PP04B_COD %in% c(62,63,6200,6300) ~ 'b. SSI'))
# Corrijo labels 
tmp4 <- tmp4 %>% 
  filter(!is.na(sector)) %>% 
  ungroup() %>% 
  mutate(estado_ocupacional = case_when(CAT_OCUP == 1 ~ 'No asalariado',
                                        CAT_OCUP == 2 ~ 'No asalariado',
                                        CAT_OCUP == 3 & PP07H == 1 ~ 'Asalariado registrado',
                                        CAT_OCUP == 3 & PP07H == 2 ~ 'Asalariado no registrado',
                                        CAT_OCUP == 4 ~ 'No asalariado',
                                        TRUE ~ 'Otros')) %>% 
  group_by(ANO4,TRIMESTRE,sector,estado_ocupacional) %>% 
  summarize(Muestra = n(),
            Poblacion = sum(PONDERA,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(sector) %>% 
  mutate(Prop_formalidad = Poblacion / sum(Poblacion))
# Unir datos 
t4 <- plyr::rbind.fill(t4,tmp4)

# Unificar los datasets y sacar "Otros" 
t_final <- t1 %>%
  mutate(sector = 'c. SBC') %>% 
  bind_rows(t2 %>% 
              mutate(sector = 'd. Total economía')) %>% 
  bind_rows(t3 %>% 
              mutate(sector = 'e. Sector privado')) %>% 
  bind_rows(t4)
t_final <- t_final %>% 
  filter(estado_ocupacional != 'Otros')

# Armar datos pooled. Opcion con 2 años y desde 2016
t_final1 <- t_final %>% 
  filter(ANO4 %in% c(max(t_final$ANO4),(max(t_final$ANO4)-1))) %>%
  group_by(sector,estado_ocupacional) %>% 
  summarize(Muestra = sum(Muestra),
            Poblacion = sum(Poblacion)) %>% 
  ungroup() %>% 
  group_by(sector) %>% 
  mutate(prop_formalidad = Poblacion / sum(Poblacion)) %>% 
  select(sector,estado_ocupacional,prop_formalidad_2021_2022 = prop_formalidad)

t_final2 <- t_final %>% 
  #filter(ANO4 %in% c(max(t_final$ANO4),(max(t_final$ANO4)-1))) %>%
  group_by(sector,estado_ocupacional) %>% 
  summarize(Muestra = sum(Muestra),
            Poblacion = sum(Poblacion)) %>% 
  ungroup() %>% 
  group_by(sector) %>% 
  mutate(prop_formalidad = Poblacion / sum(Poblacion)) %>% 
  select(sector,estado_ocupacional,prop_formalidad_2016_2022 = prop_formalidad)

t_final3 <- t_final1 %>% 
  left_join(t_final2)

# Ver cantidad de asalariados 
t_test <- t_final %>% 
  filter(str_detect(estado_ocupacional,'^Asalariado')) %>% 
  group_by(sector,estado_ocupacional) %>% 
  summarize(Muestra = sum(Muestra),
            Poblacion = sum(Poblacion)) %>% 
  ungroup() %>% 
  group_by(sector) %>% 
  mutate(prop_formalidad = Poblacion / sum(Poblacion)) %>% 
  select(estado_ocupacional,sector,prop_formalidad_2016_2022 = prop_formalidad)

t_final3$prop_formalidad_2016_2022 <- NULL
t_final3 <- t_final3 %>% 
  rename(prop_ocupados=prop_formalidad_2021_2022)

# Limpiar nombres de columnas 
df_clean <- t_final3 

##########################

df_output <- df_clean 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

## data anterior

df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("sector","estado_ocupacional"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = subtopico,
    fuentes = c("R63C0", "R64C0", "R65C0", "R66C0", "R67C0", "R68C0", "R69C0"),
    analista = analista,
    pk = c("sector","estado_ocupacional"),
    es_serie_tiempo = T,
#    columna_indice_tiempo = "",
    #columna_geo_referencia = "",
    nivel_agregacion = "aglomerados eph",
    etiquetas_indicadores = list("sector"="Sector económico al que pertenecen los trabajadores",
                                 "estado_ocupacional"="Situación ocupacional de los trabajadores",
                                 "prop_ocupados"="Proporción de trabajadores ocupados según condición en el sector de referencia"),
    unidades = list("prop_ocupados" = "proporción")
  )

#temp_files <- list.files(temp_dir, full.names = TRUE)
#view(temp_files)
#list.files(tempdir())

