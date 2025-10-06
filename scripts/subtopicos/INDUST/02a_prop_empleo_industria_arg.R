prop_empleo <- function(){
# Metadatos 
subtopico <- "INDUST"
output_name <- "02a_prop_empleo_industria_arg"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R228C98' # CGI Asalariados registrados 
fuente2 <- 'R228C99' # CGI Asalariados no registrados 
fuente3 <- 'R228C100' # CGI No Asalariados
fuente4 <- 'R229C0' # fichas sectoriales cepxxi

# rutas 
outstub <- 'indust/input'

# carga de fuentes - Argendata 
# df_cgi_1 <- argendataR::get_clean_path(fuente1) %>% 
#      arrow::read_parquet()
# df_cgi_2 <- argendataR::get_clean_path(fuente2) %>% 
#   arrow::read_parquet()
# df_cgi_3 <- argendataR::get_clean_path(fuente3) %>% 
#   arrow::read_parquet()
# df_fs <- argendataR::get_clean_path(fuente4) %>% 
#   arrow::read_parquet()

# carga de fuentes - nico 
df_cgi_1 <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/series_cgi_sexo_edad_puestos_ar_CLEAN.parquet')
df_cgi_2 <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/series_cgi_sexo_edad_puestos_anr_CLEAN.parquet')
df_cgi_3 <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/series_cgi_sexo_edad_puestos_na_CLEAN.parquet')
df_fs <- read_csv('http://149.50.137.164:2147/static/etl-fuentes/raw/Fichas-Sectoriales-FINAL.csv')

# calcular datos de empleo cgi 
df_cgi <- df_cgi_1 %>% 
  left_join(df_cgi_2) %>% 
  left_join(df_cgi_3)
df_cgi <- df_cgi %>% 
  dplyr::filter(edad_sexo == 'Total general')
df_cgi <- df_cgi %>% 
  dplyr::mutate(puestos_ar = tidyr::replace_na(puestos_ar,0),
                puestos_anr = tidyr::replace_na(puestos_anr,0),
                puestos_na = tidyr::replace_na(puestos_na,0))
df_cgi <- df_cgi %>% 
  dplyr::mutate(puestos = puestos_ar + puestos_anr + puestos_na)
df_cgi <- df_cgi %>% 
  select(letra,letra_desc,anio,puestos)

# modificar sector para oder juntar con fichas sectoriales 
df_cgi <- df_cgi %>% 
  dplyr::mutate(letra = dplyr::case_when(letra == 'N' ~ 'N_O',
                           letra == 'O' ~ 'N_O',
                           letra == 'A + B' ~ 'A_B',
                           TRUE ~ letra),
         letra_desc = dplyr::case_when(letra == 'N_O' ~ 'Servicios personales, sociales y de salud',
                           TRUE ~ letra_desc))
df_cgi <- df_cgi %>% 
  dplyr::group_by(letra,letra_desc,anio) %>% 
  dplyr::summarize(puestos = sum(puestos))

df_cgi <- df_cgi %>% 
  dplyr::mutate(puestos = puestos * 1000) # multiplicar por mil para llevar a puestos en lugar de miles de puestos
df_cgi <- df_cgi %>% 
  dplyr::ungroup()
# calcular datos de empleo de fichas 
df_fs_empleo <- df_fs %>% 
  dplyr::filter(nombre_variable %in% c('AR_PUESTOS','ANR_PUESTOS','NOASAL_PUESTOS'))
df_fs_empleo <- df_fs_empleo %>% 
  mutate(valor = as.double(valor)) %>% 
  group_by(anio,id_sector_productivo,nombre_sector_productivo) %>% 
  summarize(puestos = sum(valor))
df_fs_empleo <- df_fs_empleo %>% 
  filter(!stringr::str_detect(id_sector_productivo,'[0-9]')) %>% 
  filter(!stringr::str_detect(id_sector_productivo,'_PRIV')) %>%
  filter(!stringr::str_detect(id_sector_productivo,'_PUB')) %>%
  filter(!stringr::str_detect(id_sector_productivo,'_SPSP'))

# arreglar las letras para poder juntar con cgi 
df_fs_empleo <- df_fs_empleo %>% 
  mutate(id_sector_productivo = if_else(id_sector_productivo == 'L_Q','L',id_sector_productivo))

# quitar total empleo
df_fs_empleo <- df_fs_empleo %>% 
  filter(id_sector_productivo != 'TE')

# unir cgi y fichas sectoriales 
df_fs_empleo <- df_fs_empleo %>% 
  dplyr::select(anio,puestos,letra = id_sector_productivo)
df_fs_empleo <- df_fs_empleo %>% 
  dplyr::filter(anio < 2016)
df_cgi_empleo <- df_cgi %>% 
  dplyr::select(anio,puestos,letra)
df_empleo <- dplyr::bind_rows(df_fs_empleo,df_cgi_empleo)

# sumar descripcion
df_empleo <- df_empleo %>% 
  left_join(df_cgi %>% select(letra,letra_desc) %>% distinct(),by='letra')

# Modificar los nombres de los sectores para que queden igual a ESPROD 
df_empleo <- df_empleo %>% 
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
                                letra == 'P' ~ 'Servicio doméstico'))

}