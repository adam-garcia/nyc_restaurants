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
library(leaflet)
library(RANN)

# ggplot2 customization
theme_set(theme_minimal())
scale_fill_continuous <- function(...) scale_fill_viridis()

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
  # Deselect grade and grade date for correction
  select(-grade, -grade_date) %>%
  left_join(fix_na_cols) %>%
  # Filter to restaurants with a current grade
  filter(!is.na(grade) & grade != "Not Yet Graded") %>% 
  mutate(
    boro = boro %>% 
      as.character() %>% 
      fct_relevel("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"),
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
  ungroup() %>% 
  # From the guidelines: 0 < A < 13 < B < 28 < C. Grade pending: Z
  filter((grade == "A" & score < 14) | 
         (grade == "B" & score > 13) |
         (grade == "C" & score > 27) |
         (grade == "Z"))

##  ............................................................................
##  City of New York borough spatial data                                   ####

# Read Geospatial data of boroughs
boro_sp <- readOGR("sandbox/geo_export_bf24ed11-bed6-49a3-8af5-009cdbec9260.shp")

# Extract rownames
boro_sp@data$id <- rownames(boro_sp@data)

# Create mapping dataframe
boro_map <- fortify(boro_sp) %>% 
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
##  HERE Geocoding                                                          ####

# Load api keys from private directory
source("private/api_key.R")

# Only run if it hasn't been run before
if(!file.exists("data/nyc_locs.csv")) {
  here_url <- "https://geocoder.api.here.com/6.2/geocode.json"
  nyc_locs_list <- pmap(
    list(nyc$camis, nyc$address),
    function(id, addr) {
      here_req <- GET(
        here_url,
        query = list(
          app_id = here_creds[["app_id"]],
          app_code = here_creds[["app_code"]],
          searchtext = addr
        )
      )
      here_json <- here_req %>%
        content("text") %>%
        fromJSON()
      latlng <- here_json$Response$View$Result[[1]]$Location$DisplayPosition #%>%
      list(id = id,
           latlng = latlng)
    })
  
  nyc_locs <- nyc_locs_list %>% 
    map_df(function(ll){
      tibble(
        camis = ll$id,
        lat = ll$latlng$Latitude,
        long = ll$latlng$Longitude
      )
    }) %>% 
    distinct(camis, .keep_all = T)
  # Write geocoded locations to disk
  write_csv(nyc_locs, "data/nyc_locs.csv")
} else {
  nyc_locs <- read_csv("data/nyc_locs.csv")
}

##  ............................................................................
##  Pediacities neighborhood spatial data                                   ####

# Read GeoJSON data into Spatial Polygon data frame
nbhd_geojson <- readOGR(
  "sandbox/nyc_neighborhoods.geojson", 
  "OGRGeoJSON",
  verbose = F
)

##  ............................................................................
##  Neighborhood coding using sp::over                                      ####
nyc_sp <- nyc_locs 
coordinates(nyc_sp) <- ~ long + lat
proj4string(nyc_sp) <- proj4string(nbhd_geojson)

nyc_map <- nyc %>% 
  left_join(nyc_locs) %>% 
  mutate(nbhd = over(nyc_sp, nbhd_geojson)$ntaname,
         boro = droplevels(boro, "Missing")) %>% 
  filter(long <= -73.70001,
         long >= -74.25559,
         lat <= 40.91553,
         lat >= 40.49612)



##  ............................................................................
##  Subway Stop Data                                                        ####
# https://data.ny.gov/widgets/i9wp-a4ja
subway <- read_csv("data/subway_stops.csv") %>% 
  clean_names() %>% 
  rename(long = station_longitude,
         lat = station_latitude) %>% 
  # Staten Island Railway Stops (from Wikipedia)
  bind_rows(tribble(
    ~lat, ~long, ~station_name,
    40.54043, -74.17840, "Annadale",
    40.5168, -74.2416, "Arthur Kill",
    40.55658, -74.13663, "Bay Terrace",
    40.6215, -74.0715, "Clifton",
    40.5890, -74.0959, "Dongan Hills",
    40.5444, -74.1651, "Eltingville",
    40.55125, -74.15132, "Great Kills",
    40.5793, -74.1093, "Grant City",
    40.60347, -74.08378, "Grasmere",
    40.5336, -74.1919, "Huguenot",
    40.5838, -74.1030, "Jefferson Avenue",
    40.5736, -74.1171, "New Dorp",
    40.5647, -74.1269, "Oakwood Heights",
    40.5964, -74.0875, "Old Town",
    40.5224, -74.2179, "Pleasant Plains",
    40.5254, -74.2003, "Prince's Bay",
    40.5196, -74.2293, "Richmond Valley",
    40.643333, -74.074167, "St. George",
    40.627889, -74.075139, "Stapleton",
    40.6368, -74.0748, "Tompkinsville",
    40.5125, -74.2523, "Tottenville"
  ))

# The NYC Open Data subway dataset doesn't provide a UID
# We'll need to seed our RNG then generate and assign an identifier
set.seed(1738)
subway$station_uid <- ids::random_id(nrow(subway), bytes = 3, use_openssl = F)

subway <- subway %>% 
  select(station_uid, everything())
# Confirmation:
#> (subway %>% distinct(station_uid) %>% nrow()) == nrow(subway)
#[1] TRUE

##  ............................................................................
##  Distance to nearest subway stop                                         ####
# Many thanks to aichao on StackOverflow:
# https://stackoverflow.com/questions/39454249/checking-whether-coordinates-fall-within-a-given-radius

long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}
## Assuming that all points are within a zone (within 6 degrees in longitude),
## we use the first FSE's longitude to get the zone.
z <- long2UTM(nyc_map[1,"long"])


## convert the bus lat/long coordinates to UTM for the computed zone
## using the other Josh O'Brien linked answer
subway_sp <- subway
coordinates(subway_sp) <- c("long", "lat")
proj4string(subway_sp) <- CRS("+proj=longlat +datum=WGS84")

subway_xy <- spTransform(subway_sp, CRS(paste0("+proj=utm +zone=",z," ellps=WGS84")))

## convert the FSE lat/long coordinates to UTM for the computed zone
nyc_map_sp <- nyc_map
coordinates(nyc_map_sp) <- c("long", "lat")
proj4string(nyc_map_sp) <- CRS("+proj=longlat +datum=WGS84")

nyc_map_xy <- spTransform(nyc_map_sp, CRS(paste0("+proj=utm +zone=", z, " ellps=WGS84")))

## find the nearest neighbor in subway_xy@coords for each nyc_map_sp@coords
nyc_nn <- nn2(subway_xy@coords, nyc_map_xy@coords, 1)

# nn2 returns a list with the subway stop index and distance in meters for each 
# restaurant
distance <- tibble(
  camis = nyc_map$camis,
  station_uid = subway$station_uid[nyc_nn$nn.idx],
  dist_m = nyc_nn$nn.dists
)

nyc_full <- nyc_map %>% 
  left_join(distance, by = "camis") %>% 
  left_join(subway, by = "station_uid")



##  ............................................................................
##  Summarize to neighborhood level                                         ####

##  ............................................................................
##  Neighborhood Population Data                                            ####

# https://data.cityofnewyork.us/City-Government/New-York-City-Population-By-Neighborhood-Tabulatio/swpk-hqdp

nbhd_pop <- read_csv(
  "sandbox/New_York_City_Population_By_Neighborhood_Tabulation_Areas.csv"
) %>% 
  clean_names() %>% 
  arrange(desc(year), nta_name) %>% 
  distinct(nta_name, .keep_all = T) %>% 
  filter(nta_code %in% nbhd_geojson@data$ntacode) %>% 
  select(population, nta_code, nta_name) %>% 
  mutate(nbhd_code = fct_relevel(nta_code, nbhd_geojson@data$ntacode %>% as.character()),
         nbhd = fct_relevel(nta_name, nbhd_geojson@data$ntaname %>% as.character()))


# Creating summary data frame
nbhd_summary <- nyc_map %>% 
  filter(grade %in% c(LETTERS[1:3], "Z")) %>% 
  mutate(nbhd = fct_explicit_na(nbhd)) %>% 
  group_by(boro) %>% 
  mutate(boro_rest_n = n(),
         boro_pct_a = sum(grade == "A", na.rm = T) / boro_rest_n) %>% 
  ungroup() %>% 
  # group_by(boro_rest_n, boro, nbhd) %>% 
  group_by(nbhd) %>% 
  summarize(
    # Carry forward boro summary data
    boro = boro[1],
    boro_rest_n = boro_rest_n[1],
    boro_pct_a = boro_pct_a[1],
    # Total nyc restaurants
    nyc_rest_n = nrow(nyc_map),
    # Restaurants in neighborhood
    nbhd_rest_n = n(), # Total
    # Violations and critical violations
    nbhd_viol_mean = mean(viol, na.rm = T), # Total
    nbhd_crit_mean = mean(viol_crit, na.rm = T), # Total\
    # Percent A Grade Rating
    nbhd_pct_a = sum(grade == "A", na.rm = T) / nbhd_rest_n,
    # Mean Score
    nbhd_score_mean = mean(score, na.rm = T),
    # Cuisine
    nbhd_cuisine_variety = unique(cuisine) %>% 
      length() %>% 
      divide_by(nbhd_rest_n),
    # Top 10 Cuisine Types, Count
    nbhd_american_n = sum(cuisine == "American"),
    nbhd_chinese_n = sum(cuisine == "Chinese"),
    nbhd_cafe_n = sum(cuisine == "Cafe/Coffee/Tea"),
    nbhd_pizza_n = sum(cuisine == "Pizza"),
    nbhd_italian_n = sum(cuisine == "Italian"),
    nbhd_mexican_n = sum(cuisine == "Mexican"),
    nbhd_japanese_n = sum(cuisine == "Japanese"),
    nbhd_latin_n = sum(cuisine == "Latin (Cuban, Dominican, Puerto Rican, South & Central American)"),
    nbhd_bakery_n = sum(cuisine == "Bakery"),
    nbhd_caribbean_n = sum(cuisine == "Caribbean"),
    # Top 10 Cuisine Types, Percent
    nbhd_american_pct = sum(cuisine == "American") / nbhd_rest_n, # nbhd_american_pct
    nbhd_chinese_pct = sum(cuisine == "Chinese") / nbhd_rest_n, # nbhd_chinese_pct
    nbhd_cafe_pct = sum(cuisine == "Cafe/Coffee/Tea") / nbhd_rest_n, # nbhd_cafe_pct
    nbhd_pizza_pct = sum(cuisine == "Pizza") / nbhd_rest_n, # nbhd_pizza_pct
    nbhd_italian_pct = sum(cuisine == "Italian") / nbhd_rest_n, # nbhd_italian_pct
    nbhd_mexican_pct = sum(cuisine == "Mexican") / nbhd_rest_n, # nbhd_mexican_pct
    nbhd_japanese_pct = sum(cuisine == "Japanese") / nbhd_rest_n, # nbhd_japanese_pct
    nbhd_latin_pct = sum(cuisine == "Latin (Cuban, Dominican, Puerto Rican, South & Central American)") / nbhd_rest_n, # nbhd_latin_pct
    nbhd_bakery_pct = sum(cuisine == "Bakery") / nbhd_rest_n, # nbhd_bakery_pct
    nbhd_caribbean_pct = sum(cuisine == "Caribbean") / nbhd_rest_n # nbhd_caribbean_pct
  ) %>% 
  ungroup() %>% 
  select(nbhd, nyc_rest_n, 
         starts_with("nbhd_"), everything()) %>% 
  filter(!is.na(nbhd)) %>% 
  left_join(nbhd_pop)


