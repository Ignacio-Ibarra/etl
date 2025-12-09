# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "peso_turismo_empleo_provincial.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R240C0' # Distribución geográfica de establecimientos productivos por sexo

df_cep <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()

claes6_turisticos <- c('473000','491120','492120','492150','492160',
                       '492170','492180','501100','502101','511000',
                       '524110','524320','551021','551022','551023',
                       '551090','552000','561011','561012','561013',
                       '561014','561019','561020','561030','562099',
                       '591300','681098','771110','791100','791200',
                       '791901','791909','900011','900021','900030',
                       '900040','900091','910100','910200','910300',
                       '910900','920001','920009','931010','931020',
                       '931030','931041','931042','931050','931090',
                       '939010','939020','939030','939090')


df_intermediate <- df_cep %>%
  dplyr::filter(anio == max(anio))%>% 
  mutate(act_turisticas = as.integer(clae6 %in% claes6_turisticos)) %>% 
  group_by(anio, provincia_id, provincia, act_turisticas) %>% 
  summarise(
    empleo = sum(Empleo, na.rm = T)
  )



df_total <- df_intermediate %>% 
  mutate(
    provincia_id = 99,
    provincia = "Nacional"
  ) %>% 
  group_by(anio, provincia_id, provincia, act_turisticas) %>% 
  summarise(
    empleo = sum(empleo, na.rm = T)
  ) 


df_output <- bind_rows(df_intermediate, df_total) %>% 
  group_by(anio, provincia_id, provincia) %>% 
  mutate(
    prop = 100 * empleo / sum(empleo, na.rm = T)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(act_turisticas == 1) %>% 
  arrange(-prop) %>% 
  select(anio, provincia_id, provincia, empleo_turistico = empleo, prop_turistico = prop)

