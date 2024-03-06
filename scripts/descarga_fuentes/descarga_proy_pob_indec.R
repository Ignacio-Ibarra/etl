# proyeccion nacional poblacion indec   -----------
pob_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/c1_proyecciones_nac_2010_2040.xls"


download.file(url = pob_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_FUENTES/raw/c1_proyecciones_nac_2010_2040.xls"))

# agregar_fuente(url = pob_indec_url,institucion = "INDEC", actualizable = T, 
#                fecha_descarga = Sys.Date(),path_raw = "c1_proyecciones_nac_2010_2040.xls",
#                script = "descarga_proy_pob_indec.R",
#                nombre = "Población. Proyecciones y estimaciones"
#                 )

read_csv("data/_FUENTES/fuentes_lista.csv")$id_fuente[read_csv("data/_FUENTES/fuentes_lista.csv")$url == pob_indec_url]
actualizar_fuente(id_fuente = 37, fecha_descarga = Sys.Date())
