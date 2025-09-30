#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "12_estructura_industria"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R223C94' # agregados macroeconomicos - cuadro 4
fuente2 <- 'paper_kulfas_salles.csv'

# rutas 
instub <- 'indust/raw'
outstub <- 'indust/input'

# carga de fuentes - Argendata 

# df_am_c4 <- argendataR::get_clean_path(fuente1) %>% 
#   arrow::read_parquet()
# df_historico <- 

# carga de fuentes - nico 
df_am_c4 <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/sh_VBP_VAB_03_25_cuadro_4_CLEAN.parquet')
df_historico <- read_csv(file.path(instub,fuente2))

# seleccionar trimestre e industria 
df_am_c4 <- df_am_c4 %>% 
  filter(trimestre == 'Total')
df_am_c4 <- df_am_c4 %>% 
  filter(sector == 'Industria manufacturera')
df_am_c4 <- df_am_c4 %>% 
  filter(sub_sector != 'Total sector')

# Seleccionar variables 
df_am_c4 <- df_am_c4 %>% 
  select(anio,sub_sector,vab_pb)

# llevar a long historico 
df_historico <- df_historico %>% 
  pivot_longer(-c(sector_indec,sector),names_to='anio',values_to='vab_pb')
df_historico <- df_historico %>% 
  filter(sector != 'Industria total')
# Seleccionar var
df_historico <- df_historico %>% 
  select(anio,sub_sector=sector_indec,vab_pb)
df_historico <- df_historico %>% 
  mutate(anio = as.numeric(anio))
# unir datos
df_industria <- bind_rows(df_historico,df_am_c4)

# calcular proporcion anual
df_industria <- df_industria %>% 
  group_by(anio) %>% 
  mutate(prop = vab_pb / sum(vab_pb))
df_industria <- df_industria %>% 
  select(anio,sub_sector,prop)

# Unificar sectores 
df_industria <- df_industria %>% 
  mutate(sub_sector2=case_when(sub_sector %in% c('Elaboración de productos alimenticios y bebidas','Elaboración de productos de tabaco') ~ 
                                 'Alimentos, bebidas y tabaco',
                              # Textiles
                               sub_sector == 'Fabricación de productos textiles' ~ 
                                'Textiles, cuero y calzado',
                              sub_sector == 'Fabricación de prendas de vestir; terminación y teñido de pieles' ~ 
                                'Textiles, cuero y calzado',
                              sub_sector == 'Curtido y terminación de cueros; fabricación de artículos de marroquinería, talabartería y calzado y de sus partes' ~ 
                                'Textiles, cuero y calzado',
                              # Madera 
                              sub_sector %in% c('Producción de madera y fabricación de productos de madera y corcho, excepto muebles; fabricación de artículos de paja y de materiales trenzables',
                                                'Fabricación de papel y de productos de papel',
                                                'Edición e impresión; reproducción de grabaciones') ~ 
                                'Madera, papel y edición',
                              # Quimicos 
                              sub_sector %in% c('Fabricación de coque, productos de la refinación del petróleo y combustible nuclear', 
                                                'Fabricación de sustancias y productos químicos', 
                                                'Fabricación de productos de caucho y plástico',
                                                'Fabricación de productos minerales no metálicos') ~ 
                                'Químicos, minerales no metálicos',
                              # Metales
                              sub_sector %in% c('Fabricación de metales comunes',
                                                'Fabricación de productos elaborados de metal, excepto maquinaria y equipo'
                                                ) ~ 'Metales básicos y elaborados de metal',
                              # Maquinaria 
                              sub_sector %in% c("Fabricación de maquinaria y equipo n.c.p.") ~ 
                                'Maquinarias y equipos',
                              # Electronicos 
                              sub_sector %in% c('Fabricación de maquinaria de oficina, contabilidad e informática',
                                                'Fabricación de equipos y aparatos de radio, televisión y comunicaciones',
                                                'Fabricación de maquinaria y aparatos eléctricos n.c.p.',
                                                'Fabricación de instrumentos médicos, ópticos y de precisión; fabricación de relojes'
                                                ) ~ 'Equipos informáticos, electrónicos y eléctricos',
                              # Transporte
                              sub_sector %in% c('Fabricación de vehículos automotores, remolques y semirremolques',
                                                'Fabricación de equipo de transporte n.c.p.'
                                                ) ~ 'Equipos de transporte',
                              # Otros 
                              sub_sector %in% c('Fabricación de muebles y colchones; industrias manufactureras n.c.p.'
                              ) ~ 'Otras manufacturas',
                              TRUE ~ NA_character_))

# Agregar
df_industria <- df_industria %>% 
  filter(!is.na(sub_sector2)) %>% 
  group_by(anio,sub_sector2) %>% 
  summarize(prop = sum(prop))

# Definir tecnologia 
df_industria <- df_industria %>% 
  mutate(intensidad_tecnologica = case_when(sub_sector2 %in% c('Alimentos, bebidas y tabaco',
                                                               'Otras manufacturas',
                                                               'Textiles, cuero y calzado',
                                                               'Madera, papel y edición') ~
                                              'Low tech',
                                            sub_sector2 %in% c('Químicos, minerales no metálicos',
                                                               'Metales básicos y elaborados de metal',
                                                               'Maquinarias y equipos',
                                                               'Equipos informáticos, electrónicos y eléctricos',
                                                               'Equipos de transporte') ~ 
                                              'High tech',
                                            TRUE ~ NA_character_))
df_industria <- df_industria %>% 
  rename(sector = sub_sector2)
# guardar resultados 
readr::write_csv(df_industria,file.path(outstub,paste0(output_name,'.csv')))
