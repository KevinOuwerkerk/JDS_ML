# Kevin Ouwerkerk
# 2020-08-18

# Loading libraries -------------------------------------------------------
library(tidyverse)
library(readxl)
library(VIM)
library(missForest)
# load data ---------------------------------------------------------------
subs <- read_excel("data/raw/MLos_all substances_calculations_v3_fromWP17.xlsx", sheet = 1, skip = 3)

mod_subs <- read_csv("data/unique_CAS.csv") %>% mutate(CAS = str_remove(CAS, "_cas"))

epa_subs <-
  list.files(path = "data/raw/usepa_subs/",
             pattern = "*.xlsx",
             full.names = TRUE) %>%
  map_df( ~ read_xlsx(.)) %>%
  select(
    CAS = Substance_CASRN,
    molw = Structure_MolWt,
    biodeg = NCCT_BIODEG,
    meltingpoint = NCCT_MP,
    halflife = NCCT_HL,
    R_biodeg  = NCCT_RBiodeg,
    logP = NCCT_LogP,
    bcf = NCCT_BCF,
    water_solubility = NCCT_WS,
    koc = NCCT_KOC,
    koa = NCCT_KOA,
    boiling_point = NCCT_BP,
    km = NCCT_KM,
    aoh = NCCT_AOH,
    vapore_pressure = NCCT_VP
  )



## impute missing data ##
# combine al substances
epa_subs <- distinct(bind_rows(epa_subs, mod_subs))

#vis missings 
epa_subs %>% aggr(combined = TRUE)

vars_by_NAs <- epa_subs %>%
  is.na() %>%
  colSums() %>%
  sort(decreasing = FALSE) %>% 
  names()

# epa_subs %>%
#   select(all_of(vars_by_NAs)) %>%
#   kNN(k = 5)
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cores = cl)
epa_subs_imp <- missForest(as.data.frame(epa_subs[, -1]), variablewise = TRUE, parallelize = "variables")


#order missing vars
subs_data <- left_join(mod_subs, epa_subs)

subs_data %>% aggr(combined = TRUE)

vars_by_NAs <- select(subs_data, -CAS) %>%
  is.na() %>%
  colSums() %>%
  sort(decreasing = FALSE) %>% 
  names()

# subs_data <- subs_data %>%
#   select(all_of(vars_by_NAs)) %>%
#   kNN(k = 5,
#       numFun = weighted.mean,
#       weightDist = TRUE) %>% 
#   select(-molw_imp:-vapore_pressure_imp)







# write data to disk ------------------------------------------------------
write_rds(subs_data, path = "data/modified/subs_data.rds")

