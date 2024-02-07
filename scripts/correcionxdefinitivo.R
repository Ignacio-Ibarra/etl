# script de correccion de badepa entrega definitiva 29/01/2023
source("scripts/main.R")
# pasar df de wide a long
subtopico <- "DESIGU"

dir.create(glue::glue("data/_temp/{subtopico}"))

outputs <- subtopico_init(subtopico_nombre = subtopico, entrega = "primera_entrega")

length(outputs$name)
print(outputs$name)

## funciones -----------------------------------------------------------------


read_output <- function(id) {
  
  temp <- tempfile(fileext = ".csv")
  
  googledrive::drive_download(file = outputs$id[i],
                              path = temp, overwrite = T)
  
  
  
  if (read_lines(temp, n_max = 1) %>% str_detect(";")) {
    df <- read.csv2(temp)
  } else if (read_lines(temp, n_max = 1) %>% str_detect(",")) {
    df <- read.csv(temp)
  }
  
  df <- df %>% janitor::clean_names()
  
  df
}


joinear_paises <- function(df, iso_col) {
  
  iso_col <- "iso3"
  
  names(iso_col) <- iso_name 
  
  df[iso_name] <- sapply(df[iso_name], 
                         \(x) toupper(gsub("\\W", replacement = "", x)))
  df <- df %>% 
    left_join(get_iso_paises(), by = iso_col)
  
  df
}


limpiar_char_column <- function(df, var_char) {
  df <- df %>% 
    mutate(across(matches(var_char),\(x) {
      gsub(" Sin ", " sin ",
           gsub(" El ", " el ",
                gsub(" E ", " e ",
                     gsub(" De ", " de ",
                          gsub(
                            "Sobre", "sobre",
                            gsub(" Y ", " y ",
                                 str_to_title(gsub("_", " ",
                                                   x))))))))
    }
    )
    ) %>% 
    mutate(across(matches(var_char),\(x) {
      gsub("Fmi", "FMI",
           gsub(" Usd", " USD",
                gsub(" En ", " en ",
                     gsub(
                       " Ied", " IED",
                       gsub("Ied ", "IED ",
                            gsub(
                              "Resevas ", "Reservas ",
                              gsub(" Anio ", " Año ",
                                   gsub(" Del ", " del ",
                                        gsub(
                                          " Pib", " PIB",
                                          gsub(" Y ", " y ",
                                               x)
                                        )))
                            ))
                     ))))
    }
    )
    ) 
  
  df
  
}

limpiar_pais_col <- function(df, pais_col, iso_col) {
  
  df <- df %>% 
    select(-all_of(pais_col))
  
  iso_var <- "iso3"
  
  names(iso_var) <- iso_col 
  
  df <- df %>% 
    left_join(get_iso_paises(), by = iso_var)
  
  df
}

# Proceso -----------------------------------------------------------------

i = 1

sprintf("df %1.d de %2.d", i, length(outputs$name))


name_i <- outputs$name[i]
print(name_i)

df <- read_output(id = outputs$id[i])

str(df)
## Pasar columnas a numeric ------------------------------------------------

df <- df %>%
  mutate(across(.cols = 3:length(df), as.numeric))

str(df)

## Limpiar columnas char ---------------------------------------------------

df <- limpiar_char_column(df, var_char = )

unique(df[""])

## limpieza adicional ad hoc

df <- df %>% 
  mutate(across(matches("indicador"), \(x) {
                gsub("Deudafmi", "Deuda con el FMI",
                          gsub("PIBuss", "el PIB",
                               x))
      }
    )
  )

unique(df["indicador"])
str(df)

## Columnas de paises ------------------------------------------------------

limpiar_pais_col(df, pais_col = "pais", "iso3")

print(head(df))
str(df)

## Guardar -----------------------------------------------------------------

write.csv(x = df, 
          file = glue::glue("data/_temp/{subtopico}/{outputs$name[i]}"),
          eol = "\n", na = "",row.names = F, fileEncoding = "UTF-8")



# Pivot longer ------------------------------------------------------------

archivos_def <- list.files(glue::glue("data/_temp/{subtopico}/definitivos"), full.names = T)[!grepl("_old", list.files(glue::glue("data/_temp/{subtopico}/definitivos")))]

length(archivos_def)

x <- archivos_def[6]
      
print(x)
df <- read_csv(x)

str(df)
head(df)

write.csv(x = df, 
          file = gsub("\\.csv","_old\\.csv", x),
          eol = "\n", na = "",row.names = F, fileEncoding = "UTF-8")

print("Pivotear?")


df <- df %>% 
  pivot_longer(cols = -c(iso3, anio, pais),
               values_to = "valor", names_to = "indicador")

df <- limpiar_char_column(df, "indicador")

str(df)
print(unique(df$indicador))

print("Crear columna unidad?")


df <- df %>%
  mutate(unidad = ifelse(str_detect(indicador, "sobre PIB| PIB"),
                         "porcentaje del PIB", "millones de USD"))

distinct(df, indicador, unidad) %>% arrange(unidad)


str(df)
sum(is.na(df$pais))

df <- df %>% select(-iso3) 

write.csv(x = df, 
          file = x,
          eol = "\n", na = "",row.names = F, fileEncoding = "UTF-8")




