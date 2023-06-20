get_UNWPP_inputs <- function(countries, my_startyr, my_endyr, sex = "Female", variant = "Median"){
  
  print("Getting API ready...")
  print("Make sure you have a working internet connection!")
  # Get data from UN using API
  
  base_url <- 'https://population.un.org/dataportalapi/api/v1'
  
  # First, identify which indicator codes we want to use
  
  target <- paste0(base_url,'/indicators/?format=csv')
  codes <- read.csv(target, sep='|', skip=1) 
  
  qx_code <- codes$Id[codes$ShortName == "qx1"]
  asfr_code <- codes$Id[codes$ShortName == "ASFR1"]
  
  # Get location codes
  
  target <- paste0(base_url, '/locations?sort=id&format=csv')
  df_locations <- read.csv(target, sep='|', skip=1)
  
  # find the codes for countries
  
  my_location <- 
    df_locations %>% 
    dplyr::filter( Name %in% countries) %>% 
    dplyr::pull(Id) %>% 
    paste(collapse = ",")
  
  # Get px values
  
  print(paste0("Getting mortality data for ", paste(countries, collapse = ", ")))
  
  my_indicator <- qx_code
  my_location  <- my_location
  
  target <- paste0(base_url,
                   '/data/indicators/',my_indicator,
                   '/locations/',my_location,
                   '/start/',my_startyr,
                   '/end/',my_endyr,
                   '/?format=csv')
  
  px <- 
    read.csv(target, sep='|', skip=1) %>% 
    dplyr::filter(Variant %in% variant) %>% 
    dplyr::filter(Sex == sex) %>% 
    dplyr::mutate(px = 1- Value) %>% 
    dplyr::select(Location, year = TimeLabel, age = AgeStart, px)
  
  # ASFR
  
  if(sex == "Female"){
    print(paste0("Getting fertility data for ", paste(countries, collapse = ", ")))
    
    my_indicator <- asfr_code
    
    target <- paste0(base_url,
                     '/data/indicators/',my_indicator,
                     '/locations/',my_location,
                     '/start/',my_startyr,
                     '/end/',my_endyr,
                     '/?format=csv')
    
    asfr <- 
      read.csv(target, sep='|', skip=1) %>% 
      dplyr::filter(Variant %in% variant) %>% 
      dplyr::select(Location, year = TimeLabel, age = AgeStart, fx = Value) %>% 
      dplyr::mutate(fx = fx/1000)
    
    data <- 
      dplyr::left_join(px, asfr, by = c("Location", "year", "age")) %>% 
      dplyr::mutate(fx = replace(fx,is.na(fx),0)) 
  }else{
    data <- px
  }
  
  return(data)
}
