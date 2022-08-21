library(tidyverse)
library(ipumsr)

ddi <- read_ipums_ddi("data/cps_00004.xml")
data <- read_ipums_micro(ddi)

data <- data %>%
  filter(YEAR>=2019 & YEAR <= 2021) %>%
  mutate(Employment = case_when(
    EMPSTAT == 10 | EMPSTAT == 12 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(date = ISOdate(YEAR,MONTH,1)) %>%
  mutate(IND1950 = as.factor(IND1950)) %>%
  # https://cps.ipums.org/cps/codes/ind_2014_codes.shtml
  mutate(Retail = as.factor(case_when(
    IND1950 == "1190" | IND1950 == "5470" | IND1950 == "5580" | IND1950 == "5790" ~ 1,
    TRUE ~ 0
  )))

mean_data <- data %>%
  group_by(Retail, date) %>%
  summarise(avg_employment = mean(Employment))

ggplot(data = mean_data, aes(x=date, y=avg_employment, colour=Retail, group=Retail)) +
  geom_point() + geom_line()
