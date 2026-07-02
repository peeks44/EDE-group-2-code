# table 4 work
library(tidyverse)
library(haven)

# loading the data sets
census_data_raw = read_dta("data/matched_censusdata.dta")
view(census_data_raw) ; glimpse(census_data_raw)

# looking at the variables
names(census_data_raw)

# creating a table of the variables and their labels for convenience
var_labels <- tibble(
  variable = names(census_data_raw),
  label = map_chr(census_data_raw, ~ attr(.x, "label") %||% NA_character_)
)
str(census_data_raw)

# in the generation of table 4 she uses the following critical varaibles
# gradient, km to grid, household density, poverty rate,
# female-headed house holds, adult sex ratio, porportion headed by indian/white adults, km to road
# km to town, men with high school, women with high school, change in water access and 
# change in toilet access. 

# I'm going to look through the data and isolate the rows containing those specific 
# variables and the indicator variable (T) for whether or not a household was treated

control_variables = c("mean_grad_new", # gradient
                      "sexratio0_a", # adult sex ratio
                      "prop_head_f_a0", # female headed households
                      "prop_indianwhite0", # proportion of households headed by white/indian people
                      "kms_to_road0", 
                      "kms_to_town0",
                      "prop_matric_f0", # proportion of females with highschool
                      "prop_matric_m0", # poportion of males with highschool
                      "d_prop_waterclose", # change in water access
                      "d_prop_flush" # change in toilet access
                      )
dep_variables = c("d_prop_emp_f", # change in proportion of employed females
                       "d_prop_emp_m" # change in proportion of employed males
                       )
vars = c(control_variables, dep_variables)

# filtering the data to select only these critical columns
cencus_data_clean = census_data_raw %>% select(all_of(vars)) ; glimpse(cencus_data_clean)
# this selection preserved all the observations, no we want to isolate to see if there are any omitted values
cencus_data_clean2 = cencus_data_clean %>% na.omit()
