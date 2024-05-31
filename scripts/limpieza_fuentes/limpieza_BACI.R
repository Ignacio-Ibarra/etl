# Genero archivos parquet en /srv para consultar en scripts para outputs
# Load necessary package
library(arrow)


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

fuente_baci <- "BACI"
url_fuente <- "http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37"
institution <- "Centre d'Études Prospectives et d'Informations Internationales (CEPII)"



#### Obtener datos crudos en .csv ----

# Specify the directory to search in
directory_path <- glue::glue("{Sys.getenv('BACI_PATH')}")


# Get the list of all zip files in the specified directory
all_files <- list.files(directory_path, full.names = T, pattern = "zip")

directorio_temporal <- tempdir() # ZIP files


# Unzip in temp
unzip(all_files[1], exdir = directorio_temporal) # [1] "/srv/server_data/argendata/baci_comext/BACI_HS07_V202401b.zip"

unzip(all_files[2], exdir = directorio_temporal) # [2] "/srv/server_data/argendata/baci_comext/BACI_HS96_V202401b.zip"


# Get the list of all files in the specified directory
all_csv_files <- list.files(directorio_temporal, full.names = T)



## Importar y transformar ----



###### HS07  ----

#Filter the files that contain both "HS07" and ".csv"
HS07files <- grep("BACI_HS07.*\\.csv$", all_csv_files, value = TRUE)




# Define input and output folder paths
folder_input <- tempdir()
folder_output <- "/srv/server_data/argendata/baci_comext/"

# Loop over the years from 1996 to 2021
for (i in 2007:2022) {
  # Construct file paths
  input_file <- paste0(folder_input, "/BACI_HS07_Y", i, "_V202401b.csv")
  output_file <- paste0(folder_output, "BACI_HS07/BACI_HS07_Y", i, "_V202401b.parquet")
  
  # Import CSV file
  data <- read_csv(input_file)
  
  # Save the data frame as a Parquet file
  write_parquet(data, output_file)
}






##### HS96 -----

# Filter the files that contain both "HS96" and ".csv"

HS96files <- grep("BACI_HS96.*\\.csv$", all_csv_files, value = TRUE)


# Loop over the years from 1996 to 2021
for (i in 1996:2021) {
  # Construct file paths
  input_file <- paste0(folder_input, "/BACI_HS96_Y", i, "_V202401b.csv")
  output_file <- paste0(folder_output, "BACI_HS96/BACI_HS96_Y", i, "_V202401b.parquet")
  
  # Import CSV file
  data <- read_csv(input_file)
  
  # Save the data frame as a Parquet file
  write_parquet(data, output_file)
}


