# table 4 work

library(tidyverse)
library(haven)
library(modelsummary)
library(broom)
source("helpers.R")

# ———————————————————————————
# Loading and cleaning data
# ———————————————————————————

# loading the data sets
census_data_raw = read_dta("data/matched_censusdata.dta")
# view(census_data_raw) ; glimpse(census_data_raw)

# looking at the variables
names(census_data_raw) ; nrow(census_data_raw)

# creating a table of the variables and their labels for convenience
var_labels = tibble(
  variable = names(census_data_raw),
  label = map_chr(census_data_raw, ~ attr(.x, "label") %||% NA_character_)
) %>% data.frame()

str(census_data_raw)

# from the original dataset she reduced it from 1992 to 1816 communities by eliminating
# communities that had fewer than 100 adults in either year
# we'll do the same for accuracy of replicaton
# in her paper she uses the indicator "largeareas" to isolate these variables
census_data_v1 = census_data_raw %>% filter(largeareas==1)

nrow(census_data_v1) # observed total # of rows is 1818 not the 1816 used in her sample


# creating lists with critical variables
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
                      "d_prop_flush", # change in toilet access
                      "dccode0" # district code
                      )
dep_variables = c("d_prop_emp_f", # change in proportion of employed females
                  "d_prop_emp_m" # change in proportion of employed males
                  )
vars = c(control_variables, dep_variables, "T")

# filtering the data to select only these critical columns
census_data_v2 = census_data_v1 %>% select(all_of(vars)) ; glimpse(census_data_v2)
# nrow(census_data_clean) - nrow(census_data_raw) 

# renaming variables for ease of use in final wokring dataset
census_data = census_data_v2 %>% rename(
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
    treatment = T,
    district = dccode0,
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

# treating district as a factor to isolate district effects
census_data$district = as.factor(census_data$district)
# checking we have 10 levels like in her paper
nlevels(census_data$district)

# ———————————————————————————
# Table 4 : Female Employment
# ———————————————————————————

# plotting first column without controls (bare treatment effect)
t4_c1 = lm(data=census_data, formula = d_emp_f ~ treatment)
summary(t4_c1) # observed we're roughly at the same estimate for treatment effect
# slightly different for the standard errors ( observed ~0.004 vs. ~0.005 in the paper)
# given the differences in sample size this is likely a knock on effect


# plotting the 4th column
t4_c4 = lm(data=census_data, formula = d_emp_f ~ treatment +
          poverty + female_hh + sexratio + hh_density + indianwhite + kms_road +
          kms_town + kms_grid + matric_f + matric_m + d_water + d_toilet + district)

summary(t4_c4)

# generating the results table
table4 = results_table_4_5_generator(t4_c1,t4_c4)

# TODO: t4_c8
#
#
#

# printing output in terminal
table4

# ———————————————————————————
# Table 5 : Male Employment
# ———————————————————————————


# repeating for table 5 simply changing the dependant variable
t5_c1 = lm(data=census_data, formula = d_emp_m ~ treatment)
summary(t5_c1)
# plotting the 4th column
t5_c4 = lm(data=census_data, formula = d_emp_m ~ treatment +
             poverty + female_hh + sexratio + hh_density + indianwhite + kms_road +
             kms_town + kms_grid + matric_f + matric_m + d_water + d_toilet + district)
summary(t5_c4)
table5 = results_table_4_5_generator(t5_c1,t5_c4)

# TODO: t5_c8
#
#
#

# printing output in terminal
table5

# ——————————————————————————
#
#
write_csv(out)
