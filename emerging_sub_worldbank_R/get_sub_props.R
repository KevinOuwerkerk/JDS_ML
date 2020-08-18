# Kevin Ouwerkerk
# 2020-08-18

# Loading libraries -------------------------------------------------------
library(tidyverse)
library(readxl)

# load data ---------------------------------------------------------------
subs <- read_excel("data/raw/MLos_all substances_calculations_v3_fromWP17.xlsx", sheet = 1, skip = 3)

mod_subs <- read_csv("data/unique_CAS.csv")
