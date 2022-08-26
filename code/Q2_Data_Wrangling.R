library(tidyverse)
library(ipumsr)
library(rdrobust)
library(fixest)
library(vtable)

ddi <- read_ipums_ddi("../data/cps_00004.xml")
Q2_data <- read_ipums_micro(ddi)

# Base transformations in order to support Q2 analysis
Q2_data <- Q2_data %>%
  filter(YEAR>=2019 & YEAR <= 2021) %>%
  mutate(Employment = case_when(
    EMPSTAT == 10 | EMPSTAT == 12 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(date = ISOdate(YEAR,MONTH,1)) %>%
  # mutate(month_diff = difftime(date,ISOdate(2020,4,1),units="months")) %>%
  mutate(weeks = as.numeric(difftime(date,ISOdate(2020,3,1), units="weeks"), units="weeks")) %>%
  filter(MONTH != 3) %>%
  mutate(disc_id = case_when(
    weeks < 0 ~ 0,
    TRUE ~ 1
  )) %>%
  # https://cps.ipums.org/cps/codes/ind_2014_codes.shtml
  filter(IND1990 > 000 & IND1990 < 940) %>%
  mutate(Industry = as.factor(case_when(
    IND1990 > 000 & IND1990 <= 032 ~ "AGRICULTURE, FORESTRY, AND FISHERIES",
    IND1990 >= 040 & IND1990 <= 050 ~ "MINING",
    IND1990 == 060 ~ "CONSTRUCTION",
    IND1990 >= 100 & IND1990 <= 392 ~ "MANUFACTURING",
    IND1990 >= 400 & IND1990 <= 472 ~ "TRANSPORTATION, COMMUNICATIONS, AND OTHER PUBLIC UTILITIES",
    IND1990 >= 500 & IND1990 <= 571 ~ "WHOLESALE TRADE",
    IND1990 >= 580 & IND1990 <= 691 ~ "RETAIL TRADE",
    IND1990 >= 700 & IND1990 <= 712 ~ "FINANCE, INSURANCE, AND REAL ESTATE",
    IND1990 >= 721 & IND1990 <= 760 ~ "BUSINESS AND REPAIR SERVICES",
    IND1990 >= 761 & IND1990 <= 791 ~ "PERSONAL SERVICES",
    IND1990 >= 800 & IND1990 <= 810 ~ "ENTERTAINMENT AND RECREATION SERVICES",
    IND1990 >= 812 & IND1990 <= 893 ~ "PROFESSIONAL AND RELATED SERVICES",
    IND1990 >= 900 & IND1990 <= 932 ~ "PUBLIC ADMINISTRATION",
    IND1990 >= 940 & IND1990 <= 998 ~ "ACTIVE DUTY MILITARY",
    TRUE ~ "NIU"
  ))) %>%
  filter(Industry != "NIU" & Industry != "ACTIVE DUTY MILITARY") %>%
  mutate(Retail = Industry == "RETAIL TRADE")

# Aggregating monthly data and ensuring
Q2_data_mo <- Q2_data %>%
  filter(Industry != "NIU" & Industry != "ACTIVE DUTY MILITARY") %>%
  group_by(Industry, date, Retail, weeks, disc_id) %>%
  summarise(Employment = mean(Employment))

# Isolating retail
data_retail <- Q2_data %>%
  filter(Retail == 1)

# Isolating non-retail
data_non_retail <- Q2_data %>%
  filter(Retail != 1)

# Summarizing by retail[T/F] by date
data_ret_summ <- Q2_data %>%
  group_by(date, weeks, Retail) %>%
  summarise(Employment=mean(Employment))