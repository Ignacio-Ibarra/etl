url <- "https://www.indec.gob.ar/ftp/cuadros/economia/sh_ipc_05_24.xls"

archivo <- "sh_ipc.xls"

download.file(url,
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/{archivo}")
)

# readxl::excel_sheets(archivo)


argendataR::agregar_fuente_raw(url = url, nombre = "Índices y variaciones porcentuales mensuales e interanuales según divisiones de la canasta, bienes y servicios, clasificación de grupos. Diciembre de 2016 - a fecha disponible",
                               institucion = "INDEC",actualizable = T,
                               path_raw = archivo,
                               script = "descarga_ipc_indec.R", directorio = tempdir())
