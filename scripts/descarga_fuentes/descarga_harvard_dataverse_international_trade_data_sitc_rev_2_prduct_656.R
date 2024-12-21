#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("scripts/utils/atlas_of_economic_complexity_harvard_api.R")

url_consultada = "https://atlas.hks.harvard.edu/explore/overtime?productClass=SITC&product=product-SITC-656&endYear=2021&view=markets&exporter=group-1&locationLevel=country&layout=share&ordering=totals"


query <- "query GCPY2($productClass: ProductClass!, $productLevel: Int, $groupId: Int!, $productId: Int, $yearMin: Int, $yearMax: Int) {
  data: groupCountryProductYear(
    productClass: $productClass
    productLevel: $productLevel
    groupId: $groupId
    productId: $productId
    yearMin: $yearMin
    yearMax: $yearMax
  ) {
    groupId
    locationLevel
    partnerCountryId
    partnerLevel
    productId
    productLevel
    year
    exportValue
    importValue
    __typename
  }
}"


productID = 656

operation_name <- "GCPY2"
variables <- list(
  productClass = "SITC",
  yearMin = 1962,
  yearMax = year(Sys.Date())-2, # TODO Hoy estamos en 2024 y el año maximo disponible es 2022
  productId = productID,
  groupId = 1
)

data_df <- ATLAS_OF_ECONOMIC_COMPLEXITY.get_data_df(variable_list = variables,
                                                    operation_name = operation_name, query = query)


products_df <- ATLAS_OF_ECONOMIC_COMPLEXITY.get_stic2_4d_df()

nombre_producto <- products_df %>% dplyr::filter(grepl(productID, productId)) %>% distinct(nameShortEn) %>% pull()


year_min <- min(data_df$year)

year_max <- max(data_df$year)

download_filename <- glue::glue("atlas_of_economic_complexity_{operation_name}_{variables$productClass}_product_id_{productID}_{year_min}_{year_max}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

data_df %>% argendataR::write_csv_fundar(., destfile)

nombre = glue::glue("International Trade Data - Clasificación: {variables$productClass} - Nombre Operación: {operation_name} - Producto: {nombre_producto}")
institucion = "The Growth Lab at Harvard University - Harvard Dataverse"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    directorio = "~/etl",
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 301,
                      url = url_consultada,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
