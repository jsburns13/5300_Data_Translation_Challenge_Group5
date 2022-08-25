library(ipumsr)
library(lubridate)
library(tidyverse)
library(vtable)

ddi <- read_ipums_ddi("data/cps_00004.xml")
data <- read_ipums_micro(ddi)

data <- data %>% filter(IND1990 >= 636 & IND1990 <= 699)
data <- data %>% mutate(date = ymd(paste(YEAR, MONTH, "01")))
data <- data %>% filter(date >= ymd(20190301) & date < ymd(20210301))


data <- data %>% mutate(covid_era = case_when(date >= ymd(20200301) ~ 1,
                                              TRUE ~ 0))

data <- data %>% mutate(employed = case_when(EMPSTAT <= 12 ~ 1,
                                             TRUE ~ 0))


data <- data %>% filter(MONTH != 3)

q1_data <- data %>% group_by(date, covid_era) %>%
  summarize(monthly_employment = sum(employed))

save(q1_data, file =  "code/Q1_Data_Wrangling.RData")
