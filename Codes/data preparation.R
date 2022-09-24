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
white_collar <- read_excel("white collar rate 2019.xlsx") %>% 
  rename(nom = Libellé, white_collar_rate = `Part des cadres et prof. intellectuelles sup. dans le nb d’emplois au LT 2019`,
         code = Code)


unemployment<- read_excel("unemployment rate 2021.xlsx") %>% 
  rename(nom = Libellé, unemployment_rate = `Taux de chômage annuel moyen 2021`, code = Code)


poverty <- read_excel("poverty rate 2019.xlsx") %>% 
  select(Code, Libellé, `Taux de pauvreté 2019`) %>% 
  rename(nom = Libellé, poverty_rate = `Taux de pauvreté 2019`, code = Code)


life_expectancy <- read_excel("life expectancy 2021.xlsx") %>% 
  select(Code, Libellé, `Espérance de vie des femmes à la naissance 2021`, `Espérance de vie des hommes à la naissance 2021`) %>% 
  rename(nom = Libellé, life_expectancy_female = `Espérance de vie des femmes à la naissance 2021`,
         life_expectancy_male = `Espérance de vie des hommes à la naissance 2021`, code = Code)


higher_education <- read_excel("higher education rate 2019.xlsx") %>%
  rename(higher_education_rate = "Part des diplômés d'un BAC+2 dans la pop. non scolarisée de 15 ans ou + 2019", nom = department,
         code = Code)


election_result <- read_csv("election result.csv") %>% 
  select(dep_code, dep_name, cand_nom, cand_prenom, cand_nb_voix, inscrits_nb, abstention_nb) %>% 
  rename(nom = dep_name, cand_lastname = cand_nom, cand_name = cand_prenom, cand_num_vote = cand_nb_voix, 
         registered = inscrits_nb, abstention = abstention_nb, code = dep_code)

deps <- readRDS("departments.rds")
deps = st_as_sf(deps)
# ========================================================================================================
  # data wrangling
result <- election_result %>% pivot_wider(names_from = cand_lastname, values_from = cand_num_vote, 
                                          id = c(code, nom))

df_merged <- list(result, white_collar, unemployment, poverty, life_expectancy, higher_education, deps) %>% 
  reduce(left_join) %>% na.omit()

## to find the number of NAs in each column
#apply(df_merged, 2, function(x) sum(is.na(x)))

# save the prepared data
saveRDS(df_merged, "df_merged.rds")
