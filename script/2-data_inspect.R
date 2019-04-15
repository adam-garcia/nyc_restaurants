#   ____________________________________________________________________________
#   2-data_inspect.R                                                        ####


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

