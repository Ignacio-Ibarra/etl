
tidy_indec <- function(x, tabla = NULL) {

  tablas_posibles <- c("sh_oferta_demanda", "serie_cgi")

  if (is.null(tabla)) {
    stop(glue::glue("Tabla no puede ser nulo. Tablas posibles:\n{paste0(tablas_posibles, collapse = '\n')}"))
  }

  if (tabla == "sh_oferta_demanda") {
    x <- x[-c(1:3),] |>
      t() |>
      tibble::as_tibble(.name_repair = "unique")

    names(x) <- x[1,] |>
      janitor::make_clean_names()

    x <- x |>
      dplyr::rename(anio = na, trim = na_2)

    x <- x |>
      dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))

    x <- x |>
      tidyr::fill(anio)

    x <- x |>
      dplyr::filter(!is.na(trim))

    x

  } else if (tabla == "serie_cgi") {
    x <- x[-c(1,4:5),] |>
      t() |>
      tibble::as_tibble(.name_repair = "unique")

    names(x) <- x[2,] |>
      janitor::make_clean_names()

    x <- x[-c(1:2),]

    x <- x |>
      dplyr::rename(anio = na, trim = na_2)

    x <- x |>
      dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))

    x <- x |>
      tidyr::fill(anio)

    x <- x |>
      dplyr::filter(!is.na(trim))

    x

  } else {
    stop(glue::glue("Tabla no contemplada. Tablas posibles:\n{paste0(tablas_posibles, collapse = '\n')}"))
  }


}
