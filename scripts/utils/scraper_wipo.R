# Así me traigo los indicadores que le puedo pedir de la temática patent. https://api.ipstatsdc.deda.prd.web1.wipo.int/api/v1/public/ips-search/formcontrols?selectedTab=patent
# Me trae el indicador, los años (cuál es el ultimo año disponible) y los tipos de informe.
# 
# Esto me trae la lista de oficinas (países) y Tech (???) que puedo pedir https://api.ipstatsdc.deda.prd.web1.wipo.int/api/v1/public/ips-search/loadOffOrgTechList?indicator=10&rpType=11
# 
# Así pido datos. https://api.ipstatsdc.deda.prd.web1.wipo.int/api/v1/public/ips-search/table-result?selectedTab=patent&indicator=100&reportType=13&fromYear=1980&toYear=2023&ipsOriSelValues=AF,AL,DE,AD,AO,AG,AN,SA,DZ,AR,AM,AU,AT,AZ,BS,BH,BD,BB,BY,BE,BZ,BJ,BT,BO,BQ,BA,BW,BR,BN,BG,BF,BI,CV,KH,CM,CA,TD,CS,CL,CN,HK,MO,CY,CO,KM,CG,CR,CI,HR,CU,CW,DK,DJ,DM,EC,EG,SV,AE,ER,SK,SI,ES,US,EE,SZ,ET,RU,FJ,PH,FI,FR,GA,GM,GE,GH,GD,GR,GT,GN,GQ,GW,GY,HT,HN,HU,IN,ID,IR,IQ,IE,IS,CK,MH,SB,IL,IT,JM,JP,JO,KZ,KE,KG,KI,KW,LS,LV,LB,LR,LY,LI,LT,LU,MK,MG,MY,MW,MV,ML,MT,MA,MU,MR,MX,FM,MC,MN,ME,MZ,MM,NA,NR,NP,NI,NE,NG,NU,NO,NZ,OM,NL,PK,PW,PA,PG,PY,PE,PL,PT,QA,GB,SY,CF,CZ,KR,MD,DD,CD,LA,DO,KP,TZ,RO,RW,KN,WS,SM,SX,VC,LC,VA,ST,SN,RS,SC,SL,SG,SO,LK,ZA,SD,SS,SE,CH,SR,TH,TJ,TL,TG,TO,TT,TN,TR,TM,TV,UA,UG,SU,UY,UZ,VU,VE,VN,YE,YU,ZR,ZM,ZW,WD
# 
# Así también https://api.ipstatsdc.deda.prd.web1.wipo.int/api/v1/public/ips-search/table-result?selectedTab=patent&indicator=10&reportType=11&fromYear=1980&toYear=2023&ipsOffSelValues=AL&ipsTechSelValues=900
# Esto muestra que el parámetro ipsTechSelValues es opcional o dependiente del indicator.


library(httr)
library(jsonlite)


# Función auxiliar para construir URLs
build_url <- function(endpoint, params) {
  
  base_url <- "https://api.ipstatsdc.deda.prd.web1.wipo.int/api/v1/public/ips-search"
  query <- paste0("?", paste(names(params), params, sep="=", collapse="&"))
  return(paste0(base_url, endpoint, query))
}

# Función wrapper para manejar errores en las solicitudes GET
safe_get <- function(url) {
  
  user_agent_str <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36"
  accept_str <- "application/json, text/plain, */*"
  accept_encoding_str <- "gzip, deflate, br, zstd"
  accept_language_str <- "en"
  origin_str <- "https://www3.wipo.int"
  referer_str <- "https://www3.wipo.int/"
  
  response <- tryCatch(
    {
      res <- httr::GET(url, 
                       httr::user_agent(user_agent_str), 
                       httr::accept(accept_str),
                       httr::add_headers(
                         `accept-encoding` = accept_encoding_str,
                         `accept-language` = accept_language_str,
                         `origin` = origin_str,
                         `referer` = referer_str,
                         `sec-ch-ua` = '"Chromium";v="134", "Not:A-Brand";v="24", "Google Chrome";v="134"',
                         `sec-ch-ua-mobile` = "?0",
                         `sec-ch-ua-platform` = '"Windows"',
                         `sec-fetch-dest` = "empty",
                         `sec-fetch-mode` = "cors",
                         `sec-fetch-site` = "same-site"
                       )
      )
      if (http_type(res) != "application/json") {
        stop("Respuesta no es JSON")
      }
      if (http_error(res)) {
        stop("Error en la solicitud: ", status_code(res))
      }
      res
    },
    error = function(e) {
      message("Error en la solicitud: ", e$message)
      return(NULL)
    }
  )
  return(response)
}

# 1. Obtener indicadores disponibles
WIPO.get_indicators <- function(selected_tab = "patent") {
  url <- build_url("/formcontrols", list(selectedTab = selected_tab))
  # print(url)  # Debugging
  response <- safe_get(url)
  if (is.null(response)) return(NULL)
  content <- content(response, as = "text", encoding = "UTF-8")
  return(fromJSON(content, flatten = TRUE))
}

# 2. Obtener lista de oficinas y tecnologías para un indicador y tipo de informe
WIPO.get_offices_tech <- function(indicator, rp_type) {
  url <- build_url("/loadOffOrgTechList", list(indicator = indicator, rpType = rp_type))
  # print(url)  # Debugging
  response <- safe_get(url)
  if (is.null(response)) return(NULL)
  content <- content(response, as = "text", encoding = "UTF-8")
  return(fromJSON(content, flatten = TRUE))
}

# 3. Obtener datos específicos de la API
WIPO.get_data <- function(selected_tab, indicator, report_type, from_year, to_year, offices = NULL, tech_values = NULL) {
  params <- list(
    selectedTab = selected_tab,
    indicator = indicator,
    reportType = report_type,
    fromYear = from_year,
    toYear = to_year
  )
  
  
  if (!is.null(tech_values) & !is.null(offices)) {
    params$ipsOffSelValues <- paste(offices, collapse = ",")
    params$ipsTechSelValues <- paste(tech_values, collapse = ",")
  }
  
  
  if (!is.null(offices) & is.null(tech_values)) {
    params$ipsOriSelValues <-  paste(offices, collapse = ",")
  }
  
  print(params)
  url <- build_url("/table-result", params)
  print(url)  # Debugging
  response <- safe_get(url)
  if (is.null(response)) return(NULL)
  content <- content(response, as = "text", encoding = "UTF-8")
  
  result <- list(url = url, output = fromJSON(content, flatten = TRUE))
  return(result)
}


# 
# 
# indicators <- WIPO.get_indicators(selected_tab = 'patent')
