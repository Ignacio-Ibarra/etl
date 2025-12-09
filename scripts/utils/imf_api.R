# Este es un wrapper de la API del FMI. 
# https://portal.api.imf.org/api-details#api=idata-sdmx-api-2-1&operation=get-data-flowref-key

library(httr2)

imf_api_get_raw <- function(
    flowRef,
    key,
    startPeriod = NULL,
    endPeriod = NULL,
    updatedAfter = NULL,
    firstNObservations = NULL,
    lastNObservations = NULL,
    dimensionAtObservation = NULL,
    detail = NULL,
    includeHistory = NULL,
    format = c("json", "xml")
) {
  
  format <- match.arg(format)
  accept_header <- ifelse(format == "json", "application/json", "application/xml")
  
  base_url <- "https://api.imf.org/external/sdmx/2.1/data"
  url <- sprintf("%s/%s/%s", base_url, flowRef, key)
  
  req <- request(url) %>% req_headers(Accept = accept_header)
  
  add_param <- function(req, name, value) {
    if (!is.null(value)) {
      req <- req_url_query(req, !!name := value)
    }
    req
  }
  
  req <- req |>
    add_param("startPeriod", startPeriod) |>
    add_param("endPeriod", endPeriod) |>
    add_param("updatedAfter", updatedAfter) |>
    add_param("firstNObservations", firstNObservations) |>
    add_param("lastNObservations", lastNObservations) |>
    add_param("dimensionAtObservation", dimensionAtObservation) |>
    add_param("detail", detail) |>
    add_param("includeHistory", includeHistory)
  
  final_url <- req$url
  
  resp <- req_perform(req)
  
  if (resp_status(resp) >= 400) {
    stop("Error en la API del FMI: ", resp_status(resp))
  }
  
  body <- if (format == "json") {
    resp_body_json(resp, simplifyVector = FALSE)
  } else {
    resp_body_string(resp)
  }
  
  return(list(
    body = body,
    url = final_url
  ))
}



library(xml2)
library(dplyr)
library(purrr)
library(tidyr)

imf_parse_xml <- function(xml_string) {
  
  doc <- read_xml(xml_string)
  
  series_nodes <- xml_find_all(doc, ".//Series")
  
  df_list <- map(series_nodes, function(sn) {
    
    # Atributos de la serie
    series_attrs <- xml_attrs(sn)
    
    obs_nodes <- xml_find_all(sn, ".//Obs")
    
    tibble(
      TIME_PERIOD = xml_attr(obs_nodes, "TIME_PERIOD"),
      OBS_VALUE = suppressWarnings(as.numeric(xml_attr(obs_nodes, "OBS_VALUE")))
    ) %>%
      bind_cols(as_tibble_row(as.list(series_attrs)))
  })
  
  bind_rows(df_list)
}


library(dplyr)
library(tidyr)
library(purrr)

imf_parse_json <- function(json) {
  
  series <- json$dataSets[[1]]$series
  structure <- json$structure
  
  series_dims <- structure$dimensions$series
  obs_dim <- structure$dimensions$observation
  
  # Parseo cada serie
  parsed <- map2(
    .x = series,
    .y = names(series),
    .f = function(s, seriesKey) {
      
      # seriesKey = "0:0:0:0:0"
      idx <- as.integer(strsplit(seriesKey, ":")[[1]]) + 1
      
      # Obtener valores de dimensiones
      series_meta <- map2_chr(
        .x = series_dims$id,
        .y = seq_along(series_dims$id),
        .f = function(dim_id, pos) {
          dim_vals <- series_dims$values[[pos]]
          dim_vals[[ idx[pos] ]]$id
        }
      )
      
      names(series_meta) <- series_dims$id
      
      # Observaciones
      obs <- s$observations
      
      tibble(
        TIME_PERIOD = names(obs),
        OBS_VALUE = map_dbl(obs, ~ suppressWarnings(as.numeric(.x[1])))
      ) %>%
        bind_cols(as_tibble(series_meta))
    }
  )
  
  bind_rows(parsed)
}


imf_get <- function(
    flowRef,
    key,
    startPeriod = NULL,
    endPeriod = NULL,
    updatedAfter = NULL,
    firstNObservations = NULL,
    lastNObservations = NULL,
    dimensionAtObservation = NULL,
    detail = NULL,
    includeHistory = NULL,
    format = c("json", "xml")
) {
  
  format <- match.arg(format)
  
  raw <- imf_api_get_raw(
    flowRef = flowRef,
    key = key,
    startPeriod = startPeriod,
    endPeriod = endPeriod,
    updatedAfter = updatedAfter,
    firstNObservations = firstNObservations,
    lastNObservations = lastNObservations,
    dimensionAtObservation = dimensionAtObservation,
    detail = detail,
    includeHistory = includeHistory,
    format = format
  )
  
  df <- if (format == "json") {
    imf_parse_json(raw$body)
  } else {
    imf_parse_xml(raw$body)
  }
  
  return(list(
    data = df,
    url = raw$url
  ))
}



imf_get_codelist <- function(format = c('json','xml')){
  format <- match.arg(format)
  
  # Elegir cabecera Accept según el formato.  Para la API SDMX 3.0
  # la cabecera "Accept" puede ser simplemente "application/json" o
  # "application/xml", ya que el servidor devolverá la estructura de datos
  # en el formato solicitado.
  accept_header <- if (format == "json") {
    "application/json"
  } else {
    "application/xml"
  }
  
}



imf_get_dataflows <- function(format = c("json", "xml")) {
  format <- match.arg(format)
  
  # Elegir cabecera Accept según el formato.  Para la API SDMX 3.0
  # la cabecera "Accept" puede ser simplemente "application/json" o
  # "application/xml", ya que el servidor devolverá la estructura de datos
  # en el formato solicitado.
  accept_header <- if (format == "json") {
    "application/json"
  } else {
    "application/xml"
  }
  
  # Construir la URL base para la API SDMX 3.0.  A diferencia de la versión 2.1,
  # la API 3.0 expone los dataflows directamente bajo la ruta
  # `structure/dataflow` y no requiere especificar flowRef, providerRef ni versión.
  base_url <- "https://api.imf.org/external/sdmx/3.0/structure/dataflow"
  url <- base_url
  
  # Construir la solicitud con httr2
  req <- httr2::request(url) %>% httr2::req_headers(Accept = accept_header)
  
  # Conservar la URL expandida antes de ejecutar la petición
  final_url <- req$url
  
  # Ejecutar la solicitud
  resp <- httr2::req_perform(req)
  
  # Manejar errores HTTP
  if (httr2::resp_status(resp) >= 400) {
    stop("Error al obtener dataflows del FMI: ", httr2::resp_status(resp))
  }
  
  # Parsear el cuerpo según el formato solicitado
  body <- if (format == "json") {
    httr2::resp_body_json(resp, simplifyVector = FALSE)
  } else {
    httr2::resp_body_string(resp)
  }
  
  # Si el formato es JSON, extraer la información de cada dataflow
  data <- if (format == "json") {
    # Los dataflows se encuentran dentro de body$data$dataflows según la especificación de la API
    flows <- body$data$dataflows
    if (is.null(flows)) {
      stop("La respuesta no contiene dataflows.")
    }
    # Construir un tibble con la información relevante
    purrr::map_dfr(flows, function(df) {
      # Extraer la fecha de última actualización, si existe
      last_upd <- NA_character_
      if (!is.null(df$annotations) && length(df$annotations) > 0) {
        idx <- purrr::detect_index(df$annotations, ~ "lastUpdatedAt" %in% .x$id)
        if (!is.na(idx)) {
          last_upd <- df$annotations[[idx]]$value
        }
      }
      tibble::tibble(
        id = df$id[[1]],
        name = df$name[[1]],
        description = df$description[[1]],
        version = df$version[[1]],
        agency = df$agencyID[[1]],
        last_updated = last_upd
      )
    })
  } else {
    # Para XML, delegar el parseo a la función xml2; cada Dataflow es un nodo structure:Dataflow
    doc <- xml2::read_xml(body)
    nodes <- xml2::xml_find_all(doc, ".//structure:Dataflow", xml2::xml_ns(doc))
    purrr::map_dfr(nodes, function(node) {
      # Obtener atributos
      id <- xml2::xml_attr(node, "id")
      agency <- xml2::xml_attr(node, "agencyID")
      version <- xml2::xml_attr(node, "version")
      name <- xml2::xml_text(xml2::xml_find_first(node, ".//common:Name", xml2::xml_ns(doc)))
      description <- xml2::xml_text(xml2::xml_find_first(node, ".//common:Description", xml2::xml_ns(doc)))
      # Buscar la anotación lastUpdatedAt
      ann_nodes <- xml2::xml_find_all(node, ".//common:Annotation", xml2::xml_ns(doc))
      last_upd <- NA_character_
      for (ann in ann_nodes) {
        id_node <- xml2::xml_find_first(ann, ".//common:AnnotationID", xml2::xml_ns(doc))
        if (!is.na(xml2::xml_text(id_node)) && xml2::xml_text(id_node) == "lastUpdatedAt") {
          last_upd <- xml2::xml_text(xml2::xml_find_first(ann, ".//common:AnnotationText", xml2::xml_ns(doc)))
          break
        }
      }
      tibble::tibble(
        id = id,
        name = name,
        description = description,
        version = version,
        agency = agency,
        last_updated = last_upd
      )
    })
  }
  
  return(list(data = data, url = final_url))
}

imf_get_dimension_codes <- function(
    dimension,
    format = c("json", "xml")
) {
  format <- match.arg(format)
  
  if (!is.character(dimension) || length(dimension) != 1L || is.na(dimension) || !nzchar(dimension)) {
    stop("'dimension' debe ser una cadena no vacía.")
  }
  
  codelist_id <- sprintf("CL_%s", dimension)
  accept_header <- if (format == "json") "application/json" else "application/xml"
  
  base_url <- "https://api.imf.org/external/sdmx/3.0/structure/codelist"
  url <- sprintf("%s/all/%s/", base_url, codelist_id)
  
  req <- httr2::request(url) %>% httr2::req_headers(Accept = accept_header)
  resp <- httr2::req_perform(req)
  
  if (httr2::resp_status(resp) >= 400) {
    stop("Error al obtener codelist ", codelist_id, ": ", httr2::resp_status(resp))
  }
  
  body <- if (format == "json") {
    json_str <- httr2::resp_body_string(resp)
    jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  } else {
    httr2::resp_body_string(resp)
  }
  
  if (format == "json") {
    clists <- body$data$codelists
    if (is.null(clists) || length(clists) < 1) {
      stop("No se encontraron codelists para ", codelist_id)
    }
    
    codes <- clists[[1]]$codes
    if (is.null(codes)) {
      stop("La codelist ", codelist_id, " no tiene códigos.")
    }
    
    tibble::tibble(
      code = vapply(codes, function(x) x$id[[1]], character(1)),
      name = vapply(codes, function(x) x$name[[1]], character(1)),
      description = vapply(codes, function(x) {
        if (!is.null(x$description)) x$description[[1]] else NA_character_
      }, character(1))
    )
    
  } else {
    
    doc <- xml2::read_xml(body)
    ns <- xml2::xml_ns(doc)
    code_nodes <- xml2::xml_find_all(doc, ".//structure:Code", ns)
    
    tibble::tibble(
      code = xml2::xml_attr(code_nodes, "id"),
      name = xml2::xml_text(xml2::xml_find_first(code_nodes, ".//structure:Name", ns)),
      description = xml2::xml_text(xml2::xml_find_first(code_nodes, ".//structure:Description", ns))
    )
  }
}  

# Ahora sí, la segunda función queda afuera:
imf_get_dimensions_codes <- function(
    dimensions,
    format = c("json", "xml")
) {
  format <- match.arg(format)
  
  if (!is.character(dimensions) || length(dimensions) < 1L) {
    stop("'dimensions' debe ser un vector de cadenas no vacío.")
  }
  
  dims <- unique(trimws(dimensions))
  
  purrr::map_dfr(dims, function(d) {
    df <- imf_get_dimension_codes(d, format = format)
    df$dimension <- d
    df
  }) %>%
    dplyr::relocate(dimension)
}
