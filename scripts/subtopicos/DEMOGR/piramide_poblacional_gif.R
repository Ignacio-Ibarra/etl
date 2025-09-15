# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMTDR"
output_name <- "piramide_poblacional_1869_2022.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R432C278'
fuente2 <- 'R435C280'

df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_censos <- df_indec  %>% 
  group_by(anio = censo, sexo, rango_etario = edad) %>% 
  summarise(poblacion = sum(poblacion, na.rm = T)) %>% 
  ungroup()  %>% 
  mutate(rango_etario = case_when(
    rango_etario == "0-4" ~ "00-04",
    rango_etario == "5-9" ~ "05-09",
    TRUE ~ rango_etario
  ))

df_prospects <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG") %>% 
  select(anio = time, rango_etario = age_grp, V = pop_male, M = pop_female) %>% 
  pivot_longer(cols = c(V, M), names_to = "sexo", values_to = "poblacion_wpp" , values_transform = ~ .x *1000) %>% 
  mutate(rango_etario = case_when(
    rango_etario == "0-4" ~ "00-04",
    rango_etario == "5-9" ~ "05-09",
    rango_etario %in% c("85-89","90-94","95-99", "100+") ~ "85+",
    TRUE ~ rango_etario
  )) %>% 
  group_by(anio, sexo, rango_etario) %>% 
  summarise(poblacion = sum(poblacion_wpp, na.rm = T)) %>% 
  ungroup()


df_stage <- df_censos %>%
  dplyr::filter(anio < min(df_prospects$anio)) %>% 
  bind_rows(df_prospects)


grafico_df <- df_stage %>%
  complete(
    sexo,
    rango_etario,
    anio = full_seq(1869:max(df_prospects$anio), 1)  # todos los años
  ) %>%
  group_by(sexo, rango_etario) %>%
  arrange(anio) %>%
  mutate(
    poblacion_interpolada = as.integer(stats::approx(
      x = anio[!is.na(poblacion)],  # años conocidos
      y = poblacion[!is.na(poblacion)],  # valores conocidos
      xout = anio,                  # todos los años
      method = "linear",
      rule = 2                      # 2 = no extrapola fuera de rango, repite extremos
    )$y)
  ) %>%
  ungroup() %>%
  group_by(anio) %>%
  mutate(share_interpolado = 100 *poblacion_interpolada / sum(poblacion_interpolada, na.rm = T)) %>%
  ungroup() %>%
  select(anio, rango_etario, sexo, poblacion, poblacion_interpolada, share_interpolado)



max_share <- max(abs(grafico_df$share_interpolado), na.rm = TRUE)


N <- 1
framerate <- 12

seleccion <- grafico_df %>%
  dplyr::filter(anio %in% seq(1869, 2025, N))

dir.create("frames_mp4", showWarnings = FALSE)
years <- unique(seleccion$anio)

purrr::walk2(years, seq_along(years), function(year, i) {
  df_plot <- seleccion %>%
    mutate(share = ifelse(sexo == "V", -share_interpolado, share_interpolado)) %>%
    filter(anio == year)
  
  p <- ggplot(df_plot, aes(x = rango_etario, y = share, fill = sexo)) +
    geom_bar(stat = "identity", width = 0.8) +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) paste0(abs(x), "%"),
      limits = c(-max_share, max_share),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    scale_fill_manual(values = c("V" = "#00c19c", 
                                 "M" = "#ff6c1a")) + 
    labs(x = NULL, y = NULL, fill = NULL, title = year) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)  # centra el título
    )
  
  ggsave(sprintf("frames_mp4/frame_%04d.png", i), p, width = 800, height = 600, dpi = 150, bg = "white", units = "px")
})

filename <- glue::glue("animacion_{N}_anios_{framerate}_framerate.mp4")
commando <- glue::glue("ffmpeg -framerate {framerate} -i frames_mp4/frame_%04d.png -c:v libx264 -pix_fmt yuv420p {filename}")
system(commando)




df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  rename(geocodigoFundar = codigo_pais, geonombreFundar = pais)


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar")
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

etiquetas_nuevas <- data.frame(
  variable_nombre = c("geocodigoFundar", 
                      "geonombreFundar",
                      "fuente"),
  descripcion = c("Códigos de país ISO 3166 - alfa 3",
                  "Nombre de país",
                  "Fuente de información utilizada")
)


descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = c("geocodigoFundar"),
    descripcion_columnas = descripcion,
    unidades = list("gasto_publico_promedio" = "Gasto público consolidado promedio del periodo 2014 a la fecha (en porcentaje del PIB)")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
