code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


require(eph)

id_fuente <- 49
fuente_raw <- sprintf("R%sC0",id_fuente)

df_raw <- argendataR::get_raw_path(fuente_raw) %>%
  readr::read_csv()


letra_desc = c('Agricultura, ganadería, caza, silvicultura y pesca', 'Explotación de minas y canteras', 
               'Industria manufacturera', 'Suministro de electricidad, gas, vapor y aire acondicionado', 
               'Suministro de agua; alcantarillado; gestión de desechos y actividades de saneamiento', 
               'Construcción', 'Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas', 
               'Transporte y almacenamiento', 'Alojamiento y servicios de comidas', 'Información y comunicación', 
               'Actividades financieras y de seguros', 'Actividades inmobiliarias', 
               'Actividades profesionales, científicas y técnicas', 'Actividades administrativas y servicios de apoyo', 
               'Administración pública y defensa; planes de seguro social obligatorio', 'Enseñanza', 
               'Salud humana y servicios sociales', 'Artes, entretenimiento y recreación', 
               'Otras actividades de servicios', 'Actividades de los hogares como empleadores de personal doméstico; actividades de los hogares como productores de bienes o servicios para uso propio', 
               'Actividades de organizaciones y organismos extraterritoriales', 'Actividad no especificada claramente')
letra_desc_abrev = c('Agro y pesca', 'Petróleo y minería', 'Industria manufacturera', 'Electricidad y gas', 
                     'Agua y saneamiento', 'Construcción', 'Comercio', 'Transporte', 'Hotelería y restaurantes', 
                     'Información y comunicaciones', 'Finanzas', 'Serv. inmobiliarios', 
                     'Serv. profesionales', 'Actividades administrativas', 'Administración pública', 
                     'Enseñanza', 'Salud', 'Cultura', 'Serv. comunitarios, sociales y personales', 'Servicio doméstico', 'Organizaciones extraterritoriales', 'Otros')

df_clean <- df_raw %>% 
  eph::organize_cno() %>% 
  eph::organize_caes() 


caes <- df_clean %>% 
  distinct(caes_seccion_cod) %>% 
  drop_na(caes_seccion_cod) %>% 
  arrange(caes_seccion_cod) %>% 
  mutate(letra_desc = letra_desc,
         letra_desc_abrev = letra_desc_abrev)


df_clean <- df_clean %>% 
  left_join(caes, join_by(caes_seccion_cod))


df_clean <- df_clean %>% 
  mutate(
    caes_seccion_cod = ifelse(PP04B_COD == "8403", "O", caes_seccion_cod),
    letra_desc = ifelse(PP04B_COD == "8403", "Administración pública y defensa; planes de seguro social obligatorio", letra_desc),
    letra_desc_abrev = ifelse(PP04B_COD == "8403", "Administración pública", letra_desc_abrev),
    ) %>% 
  janitor::clean_names()


# Guardado de archivo
nombre_archivo_raw <-  sub("\\.[^.]*$", "", fuentes_raw() %>% 
                             filter(codigo == fuente_raw) %>% 
                             select(path_raw) %>% 
                             pull())


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw}")

# agregar_fuente_clean(id_fuente_raw = 49, 
#                      path_clean ="eph_total_urbano_individual_2016_2023_CLEAN.csv",
#                      dir = tempdir(),
#                      nombre = "Encuesta Permanente de Hogares Total Urbano, Individual (2016 - 2023)",
#                      descripcion_clean = "Columnas en minúscula",
#                      script = code_name)

id_fuente_clean <- 16
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('codusu', 'trimestre', 'ano4', 'provincia', 'aglomerado', 'nro_hogar', 'componente')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)


