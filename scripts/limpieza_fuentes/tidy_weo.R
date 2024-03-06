
tidy_weo <- function(x) {


  x <- x |>
    # limpio nombres de columnas: pasar a minusculas, remove non-ascii chars y cambia " " por "_"
    janitor::clean_names()


  # proceso weo_imf

  x <- x |>
    # selecciono vars de interes
    dplyr::select(-c(weo_country_code,country, subject_descriptor, subject_notes, units,
              scale, country_series_specific_notes, estimates_start_after))

  # limpieza de n/a y strings de los valores
  x <- x |>
    dplyr::mutate(dplyr::across(tidyselect::matches("\\d{4}"), \(x) readr::parse_number(x, na  =c("n/a", "--"))))

  # le doy formato longer adecuado
  x <- x |>
    tidyr::pivot_longer(cols = -c(iso, weo_subject_code), names_to = "anio")

  # una columna por indicador
  x <- x |>
    tidyr::pivot_wider(names_from = weo_subject_code, values_from = value)

  # limpio nombres de columnas (nombre de indicadores)
  x <- janitor::clean_names(x )

  x <- x |>
    dplyr::mutate(anio = as.numeric(gsub("\\D","", anio)))

  x <- x |>
    dplyr::mutate(anio = as.numeric(gsub("\\D","", anio)))

  x

}
