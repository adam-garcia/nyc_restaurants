#   ____________________________________________________________________________
#   1-import_clean.R                                                        ####

##  ............................................................................
##  Setup                                                                   ####

# Library loading
library(tidyverse)
library(magrittr)
library(lubridate)
library(janitor)
library(purrr)
library(glue)
library(maptools)
library(viridis)
library(httr)
library(jsonlite)
library(rvest)
library(broom)
library(rgdal)
library(feather)

# ggplot2 theme
theme_set(theme_minimal())

# metadata environment
meta <- new.env()

##  ............................................................................
##  Restaurant data from nyc domh                                           ####

# Load in the data
nyc_in <- read_csv("data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv") %>% 
  # Make names more manageable
  clean_names() %>% 
  # Initial data parsing from data dictionary
  # Let's rename cuisine_desciption to cuisine
  rename(cuisine = cuisine_description) %>% 
  mutate(dba = str_to_title(dba),
         boro = str_to_title(boro) %>% 
           fct_relevel(c("Manhattan",
                         "Bronx",
                         "Brooklyn",
                         "Queens",
                         "Staten Island")),
         street = str_to_title(street),
         cuisine = ifelse(cuisine == "CafÃ©/Coffee/Tea", 
                          "Cafe/Coffee/Tea", 
                          cuisine),
         inspection_date = mdy(inspection_date),
         grade_date = mdy(grade_date),
         record_date = mdy(record_date),
         critical_flag = critical_flag == "Critical") %>% 
  # Sort by unique restaurant id (camis) and inpsection date
  arrange(camis, inspection_date) %>% 
  # From the data dictionary:
  # inspection_dates of 1/1/1900 have not yet been inspected
  filter(inspection_date != mdy("1/1/1900")) %>% 
  # Drop 'Missing' Boroughs (there's only 7 of the > 25,000)
  filter(boro != "Missing")

# Some columns (grade) don't hold fidelity within inspection dates
fix_na_cols <- nyc_in %>% 
  filter(!is.na(grade)) %>% 
  select(camis, inspection_date, grade, grade_date)

# Do some minimal cleaning
nyc <- nyc_in %>% 
  select(-grade, -grade_date) %>%
  left_join(fix_na_cols) %>%
  mutate(
    street = str_replace_all(street, "\\s{2,}", " "),
    place = str_c(building, street, sep = " "),
    address = str_c(place, boro, "NY", zipcode, sep = ", "),
    grade = case_when(
      str_detect(action, "(C|c)losed") ~ "X",
      T ~ grade
    ),
    grade_date = case_when(
      grade == "X" ~ inspection_date,
      T ~ grade_date
    ),
    no_grade = is.na(grade) %>% 
      ifelse("Missing", "Graded")
  ) %>% 
  # Finally, count violations and critical violations
  group_by(camis, inspection_date) %>% 
  mutate(viol = n(),
         viol_crit = sum(critical_flag)) %>% 
  ungroup() %>% 
  # We're only going to look at the most recent health rating
  group_by(camis) %>% 
  arrange(desc(inspection_date)) %>% 
  slice(1) %>% 
  ungroup()

##  ............................................................................
##  City of New York borough spatial data                                   ####

# Read Geospatial data of boroughs
boro_sp <- readOGR("sandbox/geo_export_bf24ed11-bed6-49a3-8af5-009cdbec9260.shp")

# Extract rownames
boro_sp@data$id <- rownames(boro_sp@data)

# Create mapping dataframe
boro_map <- fortify(nyc_sp) %>% 
  mutate(boro_code = group %>% 
           as.character() %>% 
           as.numeric() %>% 
           ceiling() %>% 
           as_factor()) %>% 
  as_tibble() %>% 
  # Correct for cityofnewyork.us data codes
  left_join(tibble(
    boro_code = as_factor(c(1, 2, 3, 4, 5)),
    boro = c("Manhattan", "Bronx", "Staten Island", "Brooklyn", "Queens") %>% 
      fct_relevel("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")
  ))


##  ............................................................................
##  Pediacities neighborhood spatial data                                   ####

# Read GeoJSON data into Spatial Polygon data frame
nbhd_geojson <- readOGR(
  "sandbox/pediacities-nycneighborhoods.geojson", 
  "OGRGeoJSON",
  verbose = F
)

# Create dictionary for neighborhood name
nbhd_dict <- nbhd_geojson@data %>% 
  as_tibble() %>% 
  mutate(group = nbhd_geojson %>% 
           fortify() %>% 
           group_by(group) %>% 
           pull(group) %>% 
           levels() %>% 
           as.character())

# Create ggplot-ready table of lat-long pairs
nbhd_map <- nbhd_geojson %>% 
  broom::tidy() %>% 
  left_join(nbhd_dict) %>% 
  mutate(group = as.numeric(group) %>% floor(),
         nbhd_boro = borough %>% 
           fct_relevel(c("Manhattan", "Bronx", "Brooklyn", "Queens"))) %>% 
  rename(nbhd = neighborhood)

##  ............................................................................
##  MapQuest Geocoding                                                      ####

# MapQuest limits to 15,000 requests per key
# Manhattan and the Bronx have 12,585 entries, so we'll split there
mq_split <- nyc %>% 
  mutate(manbx = boro %in% c("Manhattan", "Bronx")) %>% 
  split(.$manbx) %>% 
  # clean_names()
  set_names(c("bknqs", "manbx"))

# Load api keys from private directory
source("private/api_key.R")

# Function get_mq_loc
## Mapquest location scrape function
# Arguments
## loc: Address or address-like location to geocode
## id: identifier value for relational merging
## boro: which borough group api key to use
get_mq_loc <- function(loc, id, boro) {
  # Return NA lat/lng for missing addresses
  if(is.na(loc)) {
    tibble(camis = id, lat = NA, lng = NA, status = "NA Address") %>% 
      return()
  }
  # URL Template
  mq_url <-glue("http://www.mapquestapi.com/geocoding/v1/address?key=",
                "{get_mq_key(boro)}&location={loc}&maxResults=1") %>% 
      URLencode()
  # Perform the request
  request <- mq_url %>% 
    httr::GET() %>% 
    content("text") %>% 
    fromJSON()
  # Only parse the data if the request was successful
  if(request$info$statuscode == 0) {
    # Pull lat/lng field
    request %>% 
      .[["results"]] %>% 
      .[["locations"]] %>% 
      first() %>% 
      .[["latLng"]] %>% 
      as_tibble() %>% 
      mutate(camis = id, status = "Success") %>% 
      rename(long = lng) %>% 
      return()
  } else {
    tibble(
      camis = id, 
      long = NA, lat = NA, 
      stauts = request$info$statuscode
    ) %>% 
      return()
  }
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Map over locations by borough                                           ####
# Only run if it hasn't been run before
if(!file.exists("data/nyc_locs.csv")) {
  # Manhattan and the Bronx
  manbx_start <- Sys.time()
  manbx_locs <- list(
    mq_split$manbx$address,
    mq_split$manbx$camis,
    "manbx"
  ) %>% 
    pmap_df(get_mq_loc)
  manbx_time <- Sys.time() - manbx_start
  
  # Brooklyn, Queens, Staten Island
  bknqs_start <- Sys.time()
  bknqs_locs <- list(
    mq_split$bknqs$address,
    mq_split$bknqs$camis,
    "bknqs"
  ) %>% 
    pmap_df(get_mq_loc)
  
  # Some of the geocoding wasn't accurate
  # Filter to bounding box of our map data to double check
  bbox <- nbhd_geojson@bbox %>% 
    as_tibble()
  
  # Attach locations to nyc data
  nyc_locs_in <- bind_rows(manbx_locs, bknqs_locs) %>% 
    filter(between(long, bbox$min[1], bbox$max[1]),
           between(lat, bbox$min[2], bbox$max[2]))
  
  nyc_locs <- left_join(nyc, nyc_locs_in)
  
  # Write geocoded locations to disk
  write_csv(nyc_locs, "data/nyc_locs.csv")
} else {
  nyc_locs <- read_csv("data/nyc_locs.csv")
}

##  ............................................................................
##  Neighborhood coding using sp::over                                      ####
nyc_sp <- nyc_locs %>% 
  filter(!is.na(lat))
coordinates(nyc_sp) <- ~ long + lat
proj4string(nyc_sp) <- proj4string(nbhd_geojson)

nyc_map <- nyc_locs %>% 
  filter(!is.na(lat)) %>% 
  mutate(nbhd = over(nyc_sp, nbhd_geojson)$neighborhood)

##  ............................................................................
##  Data inspection                                                         ####


# First, let's explore our dataset

nyc_skim <- skimr::skim(nyc) 

print(nyc_skim)

# Next, let's get into missing data
nyc_skim %>% 
  as_tibble() %>% 
  filter(stat == "missing" | stat == "n") %>% 
  select(variable, stat, value) %>% 
  spread(stat, value) %>% 
  mutate(pct_missing = missing / n) %>% 
  arrange(desc(pct_missing))



# Restaurant grades is one of the variables we're most interested in
# Let's take a look at the nature of those missing values

nyc %>% 
  mutate(no_grade = is.na(grade) %>% 
           ifelse("Missing", "Graded")) %>% 
  count(boro, no_grade) %>% 
  ggplot(aes(x = boro, y = n, fill = no_grade)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Missing grades by borough",
    x = "Borough",
    y = "Percent",
    fill = "Restaurant Grade"
  )

# There doesn't seem to be too much difference
# Between missingness across boroughs. Let's test!
nyc %>% 
  count(boro, no_grade) %>% 
  spread(no_grade, n) %>% 
  select(Graded, Missing) %>% 
  chisq.test()

# Looks like there's a *statistically significant* difference
# In rating fidelity across brough. What are the numbers, though?

nyc %>% 
  count(boro, no_grade) %>% 
  spread(no_grade, n) %>% 
  clean_names() %>% 
  mutate(total = graded + missing,
         missing_pct = scales::percent(missing / total)) %>% 
  arrange(desc(missing_pct)) %>% 
  select(boro, missing_pct) %>% 
  rename(Borough = boro,
         `Percent Missing` = missing_pct) %>% 
  DT::datatable(
    options = list(paging = FALSE,
                   searching = FALSE)
  )

##  ............................................................................
##  Cuisine is pretty messy, let’s look closer                              ####
 
nyc %>% 
  count(cuisine, sort = T)

