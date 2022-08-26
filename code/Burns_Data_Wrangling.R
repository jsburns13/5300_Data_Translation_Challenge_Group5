library(tidyverse)
library(ipumsr)
library(rdrobust)
library(fixest)

ddi <- read_ipums_ddi("data/cps_00004.xml")
data <- read_ipums_micro(ddi)

data <- data %>%
  filter(YEAR>=2019 & YEAR <= 2021) %>%
  mutate(Employment = case_when(
    EMPSTAT == 10 | EMPSTAT == 12 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(date = ISOdate(YEAR,MONTH,1)) %>%
  mutate(running = as.numeric(difftime(date,ISOdate(2020,4,1), units="days"), units="days")) %>%
  mutate(disc_id = case_when(
    running < 0 ~ 0,
    TRUE ~ 1
  )) %>%
  # https://cps.ipums.org/cps/codes/ind_2014_codes.shtml
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
  mutate(Retail = case_when(
    Industry == "RETAIL TRADE" ~ 1,
    TRUE ~ 0
  ))

mean_data <- data %>%
  group_by(Industry, date) %>%
  summarise(avg_employment = mean(Employment))

ggplot(data = mean_data, aes(x=date, y=avg_employment, colour=Industry, group=Industry)) +
  geom_point() + geom_line()

rdplot(data$Employment, data$running, c=0, p=1, h=185, kernel = "uniform")

rdplot(data$Employment, data$running, c=0, p=2, h=185, kernel = "uniform")

model_1 <- feols(Employment ~ running * disc_id | Industry, data=data)

etable(model_1)

data %>%
  group_by(Industry) %>%
  summarize(mean(Employment))

IndList <- split(data, f=data$Industry)
model_list <- list()
rdrobust_list <- list()

for (i in 1:length(IndList)) {
  indus <- levels(IndList[[i]]$Industry)[i]
  modelname <- paste0(indus, "_model") %>%
    replace(" ", "_")

  model_list[[paste0(indus)]] <- assign(modelname,feols(Employment ~ running * disc_id, data=IndList[[i]]))
  rdrobust_list[[paste0(indus)]] <- assign(modelname,rdrobust(IndList[[i]]$Employment,IndList[[i]]$running,c=0,p=2,h=185,kernel="uniform"))
  }

etable(model_list)
for (i in 1:length(rdrobust_list)) {
  print(names(rdrobust_list)[i])
  summary(rdrobust_list[[i]])
}

model_2 <- feols(Employment ~ running * disc_id * Retail, data=data)

etable(model_1, model_2)
