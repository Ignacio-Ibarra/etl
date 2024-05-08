req <- httr2::request("https://ourworldindata.org/grapher/global-energy-consumption-source?facet=none")

rpta <- httr2::req_perform(req)

html <- rpta %>% 
  httr2::resp_body_html()

links <- rvest::html_elements(html, "link") %>% 
  rvest::html_attr("href") 

links_recursos <- links[grepl("ourworldindata.org/v1/", links)]  

links_data <- links_recursos[grepl(".data.json", links_recursos, fixed = T)]

links_metadata <- links_recursos[grepl(".metadata.json", links_recursos, fixed = T)]

data_i <- jsonlite::fromJSON(links_data[1]) %>% 
  as_tibble() 

metadata_i <- jsonlite::fromJSON(links_metadata[1]) 

"https://ourworldindata.org/grapher/energy-consumption-by-source-and-country?stackMode=absolute&country=~ARG"
"https://ourworldindata.org/grapher/energy-consumption-by-source-and-country?country=~ARG"                   
"https://ourworldindata.org/grapher/low-carbon-share-energy?tab=chart&country=ARG~OWID_WRL~BRA~CHL~SWE"      
"https://ourworldindata.org/grapher/hydropower-consumption"                                                  
"https://ourworldindata.org/grapher/nuclear-energy-generation"                                               
"https://ourworldindata.org/grapher/installed-solar-PV-capacity?tab=map"                                     
"https://ourworldindata.org/grapher/cumulative-installed-wind-energy-capacity-gigawatts?tab=map"             
"https://ourworldindata.org/grapher/biofuel-production?tab=chart&country=~ARG"                               
"https://ourworldindata.org/grapher/electricity-prod-source-stacked"                                         
"https://ourworldindata.org/grapher/electricity-prod-source-stacked?country=~ARG"                            
"https://ourworldindata.org/grapher/electricity-prod-source-stacked?time=2000..latest&country=PRY~SWE"       
"https://cammesaweb.cammesa.com/potencia-instalada/"                                                         
"https://ourworldindata.org/grapher/kaya-identity-co2?facet=none&uniformYAxis=0"                             
"https://ourworldindata.org/grapher/kaya-identity-co2?facet=none&uniformYAxis=0&country=~ARG"                
"https://ourworldindata.org/grapher/energy-intensity?country=CHE~DEU~IRN~ARG"                                
"https://ourworldindata.org/grapher/carbon-intensity-electricity?tab=chart&time=2004..latest&country=~ARG"   
"https://ourworldindata.org/grapher/carbon-intensity-electricity?country=~ARG"