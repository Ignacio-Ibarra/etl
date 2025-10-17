#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "estructura_industria.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R223C94' # agregados macroeconomicos - cuadro 4
fuente2 <- 'R460C0' # Kulfas-Salles

df_am_c4 <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_historico <-argendataR::get_raw_path(fuente2) %>% 
  read.csv()

# seleccionar trimestre e industria 
df_am_final <- df_am_c4 %>% 
  dplyr::filter(trimestre == 'Total', 
         sector == 'Industria manufacturera', 
         sub_sector != 'Total sector') %>% 
  select(anio,sub_sector,vab_pb)

# llevar a long historico 
df_historico <- df_historico %>% 
  pivot_longer(-c(sector_indec,sector),
               names_to='anio',
               values_to='vab_pb', 
               names_transform = function(x){as.numeric(str_remove(x, "X"))}) %>% 
  dplyr::filter(sector != 'Industria total') %>% 
  select(anio,sub_sector=sector_indec,vab_pb) 

# unir datos
df_output <- bind_rows(df_historico, df_am_final) %>% 
  group_by(anio) %>% 
  mutate(prop = vab_pb / sum(vab_pb)) %>% 
  ungroup() %>% 
  select(anio,sub_sector,prop) %>% 
  mutate(
    sub_sector2=case_when(
      sub_sector %in% c('Elaboración de productos alimenticios y bebidas',
                        'Elaboración de productos de tabaco') ~ 
                                 'Alimentos, bebidas y tabaco',
      # Textiles
       sub_sector == 'Fabricación de productos textiles' ~ 'Textiles, cuero y calzado',
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
      TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(sub_sector2)) %>% 
  group_by(anio,sub_sector2) %>% 
  summarize(prop = sum(prop)) %>% 
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
                                            TRUE ~ NA_character_)) %>% 
  rename(sector = sub_sector2)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 

pks_comparacion <- c('anio','sector')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
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
