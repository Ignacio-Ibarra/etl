

url_evol_nivel_mar_1993_2022="https://s3.waw3-1.cloudferro.com/mdl-native-14/native/OMI_CLIMATE_SL_GLOBAL_area_averaged_anomalies/omi_climate_sl_global_area_averaged_anomalies_202311/omi_climate_sl_global_area_averaged_anomalies_19930101_P20230403.nc?x-cop-client=MyOcean&x-cop-usage=FileBrowser"

download.file(url = url_evol_nivel_mar_1993_2022, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/evol_nivel_mar_1993_2022.nc"))

datos<- nc_open(glue::glue("{tempdir()}/evol_nivel_mar_1993_2022.nc"))
head(datos)

list.files(tempdir())

ruta_archivo <- file.path(tempdir(), "evol_nivel_mar_1993_2022.nc")

# Cargar el archivo netCDF
datos <- nc_open(ruta_archivo)
view(datos)

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

nombre <- "evol_nivel_mar_1993_2022_copernicus"

url <- glue::glue("https://climate.copernicus.eu/sites/default/files/custom-uploads/ESOTC%202023/DATA/CLIMATE_INDICATORS_2023_DATA_SEA_LEVEL_FIGURE_1.zip")

destfile <- glue::glue("{tempdir()}/{nombre}.zip")

# Descargar el archivo
download.file(url, destfile, mode = "wb")

temp_dir <- tempdir() # directorio temporal
temp_unzip_dir <- file.path(temp_dir, "unzip") # subdirectorio temporal para descomprimir

# Crear el directorio temporal para descomprimir
dir.create(temp_unzip_dir, recursive = TRUE)

# Descomprimir el archivo
unzip(destfile, exdir = temp_unzip_dir)

files_and_dirs <- list.files(temp_unzip_dir, recursive = TRUE, full.names = T)

# Filtrar solo los archivos (excluir directorios)
files <- files_and_dirs[file.info(files_and_dirs)$isdir == FALSE]

# Asegurarse de que sólo hay un archivo
if (length(files) != 1) {
  stop("Se esperaba un solo archivo dentro del archivo ZIP.")
}

# Definir la ruta y nombre del archivo de destino
target_file <- glue::glue("{tempdir()}/{nombre}.csv")

# Mover y renombrar el archivo
file.rename(files, target_file)

# Confirmar que el archivo ha sido movido y renombrado
if (file.exists(dest_file)) {
  cat("Archivo descomprimido y renombrado exitosamente a:", target_file, "\n")
} else {
  stop("Error al mover y renombrar el archivo.")
}

path_raw <- glue::glue("{nombre}.csv")

agregar_fuente_raw(url = url,
                   nombre = nombre_fuente,
                   institucion = "Copernicus Climate Change Service",
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   directorio = tempdir(),
                   path_raw = path_raw,
                   script = code_name
)

list.files(temp_dir)

actualizar_fuente_raw(id_fuente = 118, dir = tempdir())

ver_nivel_mar <- readr::read_csv(get_temp_path("R118C0"))
view(ver_nivel_mar)
