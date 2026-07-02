# table 4 work
library(tidyverse)
library(haven)

# loading the data sets
census_data_raw = read_dta("data/matched_censusdata.dta")
# view(census_data_raw) ; glimpse(census_data_raw)

# looking at the variables
# names(census_data_raw)

# creating a table of the variables and their labels for convenience
var_labels = tibble(
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
                      "baseline_hhdens0", # household density
                      "base_hhpovrate0", # poverty rate
                      "prop_indianwhite0", # proportion of households headed by white/indian people
                      "kms_to_road0", 
                      "kms_to_town0",
                      "kms_to_subs0",
                      "prop_matric_f0", # proportion of females with highschool
                      "prop_matric_m0", # poportion of males with highschool
                      "d_prop_waterclose", # change in water access
                      "d_prop_flush" # change in toilet access
                      )
dep_variables = c("d_prop_emp_f", # change in proportion of employed females
                       "d_prop_emp_m" # change in proportion of employed males
                       )
vars = c(control_variables, dep_variables, "T")

# filtering the data to select only these critical columns
census_data_clean = census_data_raw %>% select(all_of(vars)) ; glimpse(census_data_clean)
# nrow(census_data_clean) - nrow(census_data_raw) 

# renaming variables for ease of use in final wokring dataset
census_data = census_data_clean %>% rename(
    gradient = mean_grad_new,
    sexratio = sexratio0_a,
    female_hh = prop_head_f_a0,
    indianwhite = prop_indianwhite0,
    hh_density = baseline_hhdens0,
    poverty = base_hhpovrate0,
    kms_road = kms_to_road0,
    kms_town = kms_to_town0,
    kms_grid = kms_to_subs0,
    matric_f = prop_matric_f0,
    matric_m = prop_matric_m0,
    d_water = d_prop_waterclose,
    d_toilet = d_prop_flush,
    d_emp_f = d_prop_emp_f,
    d_emp_m = d_prop_emp_m,
    treatment = T
  )

glimpse(census_data)

# mutating some variables for the regression
census_data = census_data %>% mutate(
  gradient10 = gradient*10,
  kms_grid10 = kms_grid*10,
  kms_road10 = kms_road*10,
  kms_town10 = kms_town*10,
  hh_density10 = hh_density*10,
  indianwhite10 = indianwhite*10
)

# plotting first column without controls (bare treatment effect)
r1 = lm(data=census_data, formula = d_emp_f ~ treatment)
summary(r1)

# plotting the regression with the controls
r4 = lm(data=census_data, formula = d_emp_f ~ treatment + gradient10 + kms_grid10 + kms_road10
        + kms_town10 + hh_density10 + indianwhite10 + matric_f + matric_m + d_water + d_toilet
        + poverty + sexratio)
summary(r4)
