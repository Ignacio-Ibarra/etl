#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("scripts/utils/atlas_of_economic_complexity_harvard_api.R")

url_consultada = "https://atlas.hks.harvard.edu/explore/overtime?exporter=country-32&productClass=SITC&endYear=2022&productLevel=4"


query <- "query CPY($productClass: ProductClass!, $servicesClass: ServicesClass, $productLevel: Int!, $countryId: Int, $productId: Int, $yearMin: Int, $yearMax: Int) {
  data: countryProductYear(
    productClass: $productClass
    servicesClass: $servicesClass
    productLevel: $productLevel
    countryId: $countryId
    productId: $productId
    yearMin: $yearMin
    yearMax: $yearMax
  ) {
    countryId
    locationLevel
    productId
    productLevel
    year
    exportValue
    importValue
    __typename
  }
}"

countryID = 32

operation_name <- "CPY"
variables <- list(
  productClass = "SITC",
  yearMin = 1962,
  yearMax = year(Sys.Date())-2,
  productLevel = 4,
  servicesClass = "unilateral",
  countryId = countryID
)


data_df <- ATLAS_OF_ECONOMIC_COMPLEXITY.get_data_df(variable_list = variables, 
                                                operation_name = operation_name,
                                                query = query)

year_min <- min(data_df$year)

year_max <- max(data_df$year)

download_filename <- glue::glue("atlas_of_economic_complexity_{operation_name}_{variables$productClass}_product_id_all_{year_min}_{year_max}_country_id_32_service_class_{variables$servicesClass}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

data_df %>% argendataR::write_csv_fundar(., destfile)

nombre = glue::glue("International Trade Data - Clasificación: {variables$productClass} - Nombre Operación: 'countryProductYear (CPY)' - Pais: Argentina")
institucion = "The Growth Lab at Harvard University - Harvard Dataverse"

# agregar_fuente_raw(url = url_consultada,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    api = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 303,
                      url = url_consultada,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)