library(tidyverse)
library(leaflet)
library(corrplot)
library(purrr)
library(sf)
library(readxl)
library(spdep)  #adjacency matrix
rm(list = ls())


# ========================================================================================================
   # reading the raw datasets and translating the column headers
# ========================================================================================================
# white collar rate 2019
white_collar <- read_excel("white collar rate 2019.xlsx") %>% 
  rename(nom = Libellé, white_collar_rate = `Part des cadres et prof. intellectuelles sup. dans le nb d’emplois au LT 2019`,
         code = Code)

# unemployment rate 2021
unemployment<- read_excel("unemployment rate 2021.xlsx") %>% 
  rename(nom = Libellé, unemployment_rate = `Taux de chômage annuel moyen 2021`, code = Code)

# poverty rate 2019
poverty <- read_excel("poverty rate 2019.xlsx") %>% 
  select(Code, Libellé, `Taux de pauvreté 2019`) %>% 
  rename(nom = Libellé, poverty_rate = `Taux de pauvreté 2019`, code = Code)

# life expectancy 2021
life_expectancy <- read_excel("life expectancy 2021.xlsx") %>% 
  select(Code, Libellé, `Espérance de vie des femmes à la naissance 2021`, `Espérance de vie des hommes à la naissance 2021`) %>% 
  rename(nom = Libellé, life_expectancy_female = `Espérance de vie des femmes à la naissance 2021`,
         life_expectancy_male = `Espérance de vie des hommes à la naissance 2021`, code = Code)

# higher education rate 2019
higher_education <- read_excel("higher education rate 2019.xlsx") %>%
  rename(higher_education_rate = "Part des diplômés d'un BAC+2 dans la pop. non scolarisée de 15 ans ou + 2019", nom = department,
         code = Code)

# election 2022 results
election_result <- read_csv("election result.csv") %>% 
  select(dep_code, dep_name, cand_nom, cand_prenom, cand_nb_voix, inscrits_nb, abstention_nb) %>% 
  rename(nom = dep_name, cand_lastname = cand_nom, cand_name = cand_prenom, cand_num_vote = cand_nb_voix, 
         registered = inscrits_nb, abstention = abstention_nb, code = dep_code)

# population 2019
population <- read_excel("population 2019.xlsx") %>% select(-REG) %>% rename(population = P19_POP,
                                                                             code = DEP) %>% 
  group_by(code) %>% summarise(population = sum(population))

# immigrants data 2019
immigration <- read_excel("immigration 2019.xlsx")

# shape file
deps <- readRDS("departments.rds")
deps = st_as_sf(deps)

# ========================================================================================================
  # data wrangling
# ========================================================================================================

result <- election_result %>% pivot_wider(names_from = cand_lastname, values_from = cand_num_vote, 
                                          id = c(code, nom))


#preparing immigration dataset for departments
df_immigration <- immigration %>% rowwise() %>% 
  mutate(female_immigrants = round(sum(across(ends_with("IMMI1_SEXE2")), na.rm = T), 0),
         male_immigrants = round(sum(across(starts_with("IMMI1_SEXE1")))),
         total_imm = female_immigrants + male_immigrants,
         code = str_sub(CODGEO, 1, 2)) %>%
  dplyr::select(code, total_imm )

df_immigration <- df_immigration %>% 
  group_by(code) %>% summarise(total_imm = sum(total_imm)) 



df_merged <- list(result, white_collar, unemployment, poverty, life_expectancy, higher_education, 
                  df_immigration, population, deps) %>% 
  reduce(left_join) %>% na.omit()

df_merged <- df_merged %>% rowwise() %>% mutate(immigration_rate = total_imm/ population,
                                                life_expectancy = (life_expectancy_female + life_expectancy_male)/2) %>% 
  select(-c(life_expectancy_female, life_expectancy_male, total_imm))
  

  
  
## to find the number of NAs in each column
#apply(df_merged, 2, function(x) sum(is.na(x)))

# Adding 2021 immigration rate
imi_rate <- read_excel("immigration_rate_2021.xlsx")
  
df_merged = left_join(df_merged, imi_rate)

# save the prepared data
saveRDS(df_merged, "df_merged.rds")
