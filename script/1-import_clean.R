#   ____________________________________________________________________________
#   1-import_clean.R                                                        ####

# Load in the data
nyc_in <- read_csv("data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv") %>% 
  # Make names more manageable
  clean_names() %>% 
  # Initial data parsing from data dictionary
  mutate(dba = str_to_title(dba),
         boro = str_to_title(boro),
         street = str_to_title(street),
         inspection_date = mdy(inspection_date),
         grade_date = mdy(grade_date),
         record_date = mdy(record_date)) %>% 
  # Sort by unique restaurant id (camis) and inpsection date
  arrange(camis, inspection_date) %>% 
  # From the data dictionary:
  # inspection_dates of 1/1/1900 have not yet been inspected
  filter(inspection_date != mdy("1/1/1900"))

# Some columns (grade) don't hold fidelity within inspection dates
fix_na_cols <- nyc_in %>% 
  filter(!is.na(grade)) %>% 
  select(camis, inspection_date, grade, grade_date)

# Do some minimal cleaning
nyc <- nyc_in %>% 
  select(-grade, -grade_date) %>%
  left_join(fix_na_cols) %>%
  mutate(
    grade = case_when(
      str_detect(action, "(C|c)losed") ~ "X",
      T ~ grade
    ),
    grade_date = case_when(
      grade == "X" ~ inspection_date,
      T ~ grade_date
    )
  ) %>% 
  # We're only going to look at the most recent health rating
  group_by(camis) %>% 
  arrange(desc(inspection_date)) %>% 
  slice(1) %>% 
  ungroup() 


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

