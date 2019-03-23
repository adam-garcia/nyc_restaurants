#   ____________________________________________________________________________
#   3-plot.R                                                                ####

# Initial setup
theme_set(theme_minimal())


##  ............................................................................
##  Descriptive Stats                                                       ####

# Cuisine
nyc %>% 
  count(cuisine) %>% 
  ggplot(aes(x = reorder(cuisine, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

##  ............................................................................
##  Maps                                                                    ####
# Base NYC map with neighborhood boundaries
nbhd_map %>% 
  ggplot(aes(long, lat, group = group, fill = nbhd_boro)) +
  geom_polygon()

# Chloropleth of restaurant counts
manbx_freqs <- manbx_map %>% 
  group_by(boro) %>% 
  mutate(boro_tot = n()) %>% 
  ungroup() %>% 
  group_by(nbhd) %>% 
  mutate(nbhd_tot = n(),
         boronbhd_pct = nbhd_tot / boro_tot,
         neighborhood = nbhd) %>% 
  ungroup() %>% 
  distinct(boro, nbhd, .keep_all = T) %>% 
  select(boro, nbhd, boro_tot, nbhd_tot, boronbhd_pct)


nbhd_map %>% 
  left_join(manbx_freqs) %>% 
  filter(boroughCode %in% 1:2) %>%
  ggplot(aes(long, lat, group = group, fill = boronbhd_pct)) +
  geom_polygon() +
  # geom_point(data = manbx_map, aes(long, lat, group = NA, fill = as.numeric(NA))) +
  scale_fill_viridis() +
  coord_map()
