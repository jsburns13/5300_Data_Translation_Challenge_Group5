# install.packages("ipumsr")
library(ipumsr)
library(dplyr)
library(tidyverse)
library(vtable)
library(ggplot2)
library(lubridate)
library(fixest)
library(marginaleffects)

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

#ddi <- read_ipums_ddi('./data/cps_00003.xml')
#data <- read_ipums_micro(ddi)

ddi1 <- read_ipums_ddi('./data/cps_00006.xml')
data1 <- read_ipums_micro(ddi1)

#Read industry name file
indnames <- read_csv('./data/indnames.csv')

#labeltable(df$EMPSTAT)

#Join both dataframe [Use IND1990]
df <- data1 %>% left_join(indnames, by = c("IND1990" = "ind"))

#Check how many unique industry name in the dataframe
unique(indnames$indname)

#Check how's data look like
vtable(df)

#Filter out whoever is not in the laborforce, create yearmo variable
df <- df %>% filter(LABFORCE == 2) %>% mutate(yearmo = YEAR*100 + MONTH)

#Create COVIDSTATUS variable (pre-COVID and post-COVID variable)
df <- df %>% mutate(COVIDSTATUS = case_when(yearmo < 202003 ~ 'pre-covid', yearmo > 202003 ~ 'post-covid'))

#Create Unemployed-experienced worker, Unemployed-new worker and Employed variable from EMPSTAT
df <- df %>% mutate(Employed = case_when(EMPSTAT == 10 ~ '1', TRUE ~ '0'))
df <- df %>% mutate(Unemployed_experienced_worker = case_when(EMPSTAT == 21 ~ '1', TRUE ~ '0'))
df <- df %>% mutate(Unemployed_new_worker = case_when(EMPSTAT == 22 ~ '1', TRUE ~ '0'))

#Fix the datatype of Unemployed_experienced_worker, Unemployed-new worker from character to numeric for aggregate purpose
df$Employed <- as.numeric(df$Employed) # Convert character to numeric
df$Unemployed_experienced_worker <- as.numeric(df$Unemployed_experienced_worker) # Convert character to numeric
df$Unemployed_new_worker <- as.numeric(df$Unemployed_new_worker) # Convert character to numeric

#Aggregate the total count of unemployed_experienced_worker, unemployed-new worker
df_Employed <- df %>% drop_na(indname) %>% group_by(yearmo, indname) %>% summarise(Count = sum(Employed))
df_unemployed_experienced <- df %>% drop_na(indname) %>% group_by(yearmo, indname) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new <- df %>%  group_by(yearmo, indname) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_Employed to see how's data look like 
ggplot(df_Employed, aes(x = yearmo, y = Count, col = indname)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-Industry")

#Plot df_unemployed_experienced to see how's data look like 
ggplot(df_unemployed_experienced, aes(x = yearmo, y = Count, col = indname)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-Industry")

#Plot df_unemployed_new to see how's data look like 
ggplot(df_unemployed_new, aes(x = yearmo, y = Count)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-Industry")

#sum(is.na(df$indname))

#Divide Education attainments into broader range:high school or lower, college/associate/bachelor and doctorate.
df <- df %>% mutate(Education = case_when(EDUC == 010 ~ 'high school or lower',
                                          EDUC == 020 ~ 'high school or lower',
                                          EDUC == 030 ~ 'high school or lower',
                                          EDUC == 040 ~ 'high school or lower',
                                          EDUC == 050 ~ 'high school or lower',
                                          EDUC == 060 ~ 'high school or lower',
                                          EDUC == 071 ~ 'high school or lower',
                                          EDUC == 073 ~ 'high school or lower',
                                          EDUC == 081 ~ 'college/associate/bachelor', 
                                          EDUC == 091 ~ 'college/associate/bachelor',
                                          EDUC == 092 ~ 'college/associate/bachelor',
                                          EDUC == 111 ~ 'college/associate/bachelor',
                                          EDUC == 125 ~ 'doctorate', TRUE ~ 'NA'))

#Check the education attainments of employed and umemployed workers
df_Employed_EDUC <- df %>% filter(Education != 'NA') %>% group_by(yearmo, Education) %>% summarise(Count = sum(Employed))
df_unemployed_experienced_EDUC <- df %>% filter(Education != 'NA') %>% group_by(yearmo, Education) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new_EDUC <- df %>% filter(Education != 'NA') %>% group_by(yearmo, Education) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_unemployed_experienced_Educ to see how's data look like 
ggplot(df_Employed_EDUC, aes(x = yearmo, y = Count, col = Education)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-Education")

#Plot df_unemployed_experienced_Educ to see how's data look like 
ggplot(df_unemployed_experienced_EDUC, aes(x = yearmo, y = Count, col = Education)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-Education")

#Plot df_unemployed_new_Educ to see how's data look like 
ggplot(df_unemployed_new_EDUC, aes(x = yearmo, y = Count, col = Education)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-Education")

#Create ClassofWorker variable to turn CLASSWKR into broader range
df <- df %>% mutate(ClassofWorker = case_when(CLASSWKR == 13 ~ 'Self-employed',
                                              CLASSWKR == 14 ~ 'Self-employed',
                                              CLASSWKR == 22 ~ 'Private Profit',
                                              CLASSWKR == 23 ~ 'Private nonprofit',
                                              CLASSWKR == 25 ~ 'Federal government employee',
                                              CLASSWKR == 26 ~ 'Armed forces',
                                              CLASSWKR == 27 ~ 'State government employee',
                                              CLASSWKR == 28 ~ 'Local government employee', TRUE ~ 'NA'))

#Check the Worker Class of employed and umemployed workers
df_Employed_WorkerClass <- df %>% filter(ClassofWorker != 'NA') %>% group_by(yearmo, ClassofWorker) %>% summarise(Count = sum(Employed))
df_unemployed_experienced_WorkerClass <- df %>% filter(ClassofWorker != 'NA') %>% group_by(yearmo, ClassofWorker) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new_WorkerClass <- df %>% filter(ClassofWorker != 'NA') %>% group_by(yearmo, ClassofWorker) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_Employed_WorkerClass to see how's data look like 
ggplot(df_Employed_WorkerClass, aes(x = yearmo, y = Count, col = ClassofWorker)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-WorkerClass")

#Plot df_unemployed_experienced_WorkerClass to see how's data look like 
ggplot(df_unemployed_experienced_WorkerClass, aes(x = yearmo, y = Count, col = ClassofWorker)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-WorkerClass")

#Plot df_unemployed_new_WorkerClass to see how's data look like [All the industry name of new worker are NA]
ggplot(df_unemployed_new_WorkerClass, aes(x = yearmo, y = Count, col = ClassofWorker)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-WorkerClass")

#Create Workstatus variable to turn WKSTAT into broader range: full-time, part-time
df <- df %>% mutate(Workstatus = case_when(WKSTAT == 10 ~ 'full time',
                                           WKSTAT == 11 ~ 'full time',
                                           WKSTAT == 14 ~ 'full time',
                                           WKSTAT == 15 ~ 'full time',
                                           WKSTAT == 12 ~ 'part time',
                                           WKSTAT == 21 ~ 'part time',
                                           WKSTAT == 22 ~ 'part time',
                                           WKSTAT == 40 ~ 'part time',
                                           WKSTAT == 41 ~ 'part time', TRUE ~ 'Not at work'))

#Check the Work Status of employed and umemployed workers
df_Employed_Workstatus <- df %>% group_by(yearmo, Workstatus) %>% summarise(Count = sum(Employed))
df_unemployed_experienced_Workstatus <- df %>% group_by(yearmo, Workstatus) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new_Workstatus <- df %>% group_by(yearmo, Workstatus) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_Employed_Workstatus to see how's data look like 
ggplot(df_Employed_Workstatus, aes(x = yearmo, y = Count, col = Workstatus)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-WorkerClass")

#Plot df_unemployed_experienced_Workstatus to see how's data look like 
ggplot(df_unemployed_experienced_Workstatus, aes(x = yearmo, y = Count, col = Workstatus)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-WorkerClass")

#Plot df_unemployed_new_Workstatus to see how's data look like 
ggplot(df_unemployed_new_Workstatus, aes(x = yearmo, y = Count, col = Workstatus)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-WorkerClass")

#Convert age into numeric type
df$AGE <- as.numeric(df$AGE) # Convert to numeric

#Create Age_broad variable to turn age into broader range
df <- df %>% mutate(Age_broad = case_when(AGE >= 56 ~ '56-65',
                                          AGE >= 46 ~ '46-55',
                                          AGE >= 36 ~ '36-45',
                                          AGE >= 25 ~ '25-35', TRUE ~ 'NA'))

#Check the different Age range of employed and umemployed workers situation
df_Employed_Age <- df %>% group_by(yearmo, Age_broad) %>% summarise(Count = sum(Employed))
df_unemployed_experienced_Age <- df %>% group_by(yearmo, Age_broad) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new_Age <- df %>% group_by(yearmo, Age_broad) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_Employed_Age to see how's data look like 
ggplot(df_Employed_Age, aes(x = yearmo, y = Count, col = Age_broad)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-Age_broad")

#Plot df_unemployed_experienced_Age to see how's data look like 
ggplot(df_unemployed_experienced_Age, aes(x = yearmo, y = Count, col = Age_broad)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-Age_broad")

#Plot df_unemployed_new_Age to see how's data look like 
ggplot(df_unemployed_new_Age, aes(x = yearmo, y = Count, col = Age_broad)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-Age_broad")

#Convert SEX into character type
df$SEX <- as.character(df$SEX) # Convert to character

#Check gender of employed and umemployed workers situation 
df_Employed_Sex <- df %>% group_by(yearmo, SEX) %>% summarise(Count = sum(Employed))
df_unemployed_experienced_Sex <- df %>% group_by(yearmo, SEX) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new_Sex <- df %>% group_by(yearmo, SEX) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_Employed_Sex to see how's data look like 
ggplot(df_Employed_Sex, aes(x = yearmo, y = Count, col = SEX)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-SEX")

#Plot df_unemployed_experienced_Sex to see how's data look like 
ggplot(df_unemployed_experienced_Sex, aes(x = yearmo, y = Count, col = SEX)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-SEX")

#Plot df_unemployed_new_Sex to see how's data look like 
ggplot(df_unemployed_new_Sex, aes(x = yearmo, y = Count, col = SEX)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-SEX")

#Convert RACE into character type
df$RACE <- as.character(df$RACE) # Convert to character

#Check Race distribution of employed and umemployed workers situation
df_Employed_Race <- df %>% group_by(yearmo, RACE) %>% summarise(Count = sum(Employed))
df_unemployed_experienced_RACE <- df %>% group_by(yearmo, RACE) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new_RACE <- df %>% group_by(yearmo, RACE) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_Employed_Race to see how's data look like 
ggplot(df_Employed_Race, aes(x = yearmo, y = Count, col = RACE)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-RACE")

#Plot df_unemployed_experienced_RACE to see how's data look like 
ggplot(df_unemployed_experienced_RACE, aes(x = yearmo, y = Count, col = RACE)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-RACE")

#Plot df_unemployed_new_RACE to see how's data look like 
ggplot(df_unemployed_new_RACE, aes(x = yearmo, y = Count, col = RACE)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-RACE")

#Convert MARST into character type
df$MARST <- as.character(df$MARST) # Convert to character

#Check Marital status
df_Employed_Marital <- df %>% filter(MARST != 9) %>% group_by(yearmo, MARST) %>% summarise(Count = sum(Employed))
df_unemployed_experienced_Marital <- df %>% filter(MARST != 9) %>% group_by(yearmo, MARST) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new_Marital <- df %>% filter(MARST != 9) %>% group_by(yearmo, MARST) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_Employed_Marital to see how's data look like 
ggplot(df_Employed_Marital, aes(x = yearmo, y = Count, col = MARST)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-MARST")

#Plot df_unemployed_experienced_Marital to see how's data look like 
ggplot(df_unemployed_experienced_Marital, aes(x = yearmo, y = Count, col = MARST)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-MARST")

#Plot df_unemployed_new_Marital to see how's data look like 
ggplot(df_unemployed_new_Marital, aes(x = yearmo, y = Count, col = MARST)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-MARST")

#Create Occupation variable turn OCC1990 into broader category [OCC1990]
df <- df %>% mutate(Occupation = case_when(OCC1990 <= 200 ~ 'MANAGERIAL AND PROFESSIONAL SPECIALTY OCCUPATIONS',
                                            OCC1990 <= 391 ~ 'TECHNICAL, SALES, AND ADMINISTRATIVE SUPPORT OCCUPATIONS',
                                            OCC1990 <= 469 ~ 'SERVICE OCCUPATIONS',
                                            OCC1990 <= 498 ~ 'FARMING, FORESTRY, AND FISHING OCCUPATIONS',
                                            OCC1990 <= 699 ~ 'PRECISION PRODUCTION, CRAFT, AND REPAIR OCCUPATIONS',
                                            OCC1990 <= 890 ~ 'OPERATORS, FABRICATORS, AND LABORERS',
                                            OCC1990 == 905 ~ 'MILITARY OCCUPATIONS', TRUE ~ 'NA'))

#Check how's the employed, unemployed situation in different occupation 
df_Employed_Occupation <- df %>% filter(Occupation != 'NA') %>% group_by(yearmo, Occupation) %>% summarise(Count = sum(Employed))
df_unemployed_experienced_Occupation <- df %>% filter(Occupation != 'NA') %>% group_by(yearmo, Occupation) %>% summarise(Count = sum(Unemployed_experienced_worker))
df_unemployed_new_Occupation <- df %>%  group_by(yearmo, Occupation) %>% summarise(Count = sum(Unemployed_new_worker))

#Plot df_Employed_OCC to see how's data look like 
ggplot(df_Employed_Occupation, aes(x = yearmo, y = Count, col = Occupation)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Employed-MARST")

#Plot df_unemployed_experienced_OCC to see how's data look like 
ggplot(df_unemployed_experienced_Occupation, aes(x = yearmo, y = Count, col = Occupation)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed Experienced worker-MARST")

#Plot df_unemployed_new_OCC to see how's data look like [Occupation of all new workers are NA]
ggplot(df_unemployed_new_Occupation, aes(x = yearmo, y = Count, col = Occupation)) + geom_point() + 
  geom_smooth(method = 'lm') + geom_vline(xintercept = 202003) + ggtitle("Unemployed New worker-MARST")


## EDA

# Q: Retail needs to worry about who has money to spend - what has changed about who is working and earning money?

#record count of each employment stat
df %>% 
  group_by(EMPSTAT) %>% 
  count(rec_count = n())

# Dependent variable (Employed) Where EMPSTAT in (10, 12) vs (Unemployed) where EMPSTAT in (21,22)
df <- df %>% mutate(is_employed = case_when(EMPSTAT == 10 ~ '1', 
                                            EMPSTAT == 12 ~ '1',
                                            TRUE ~ '0')) %>% 
  mutate(is_employed = as.numeric(is_employed))


# Drop in Employment by Race plots of groups
agg <- df %>% 
  group_by(yearmo, RACE) %>% 
  summarise(employed_count = sum(as.numeric(is_employed))) %>% 
  arrange(.,RACE, yearmo)

ggplot(data=agg, aes(y = log(employed_count), x = yearmo))+
  geom_line(alpha = 0.6, aes(color = factor(RACE)))+
  geom_vline(xintercept = 202003)+
  labs(y = 'Count of Employeed', x = 'Time', color = 'RACE')

# Drop in Employment by indname
# Note that they take a dip it appears that the 
agg <- df %>% 
  group_by(yearmo, indname) %>% 
  summarise(employed_count = sum(as.numeric(is_employed))) %>% 
  arrange(.,indname, yearmo)

ggplot(data=agg, aes(y = employed_count, x = yearmo))+
  geom_line(alpha = 0.6, aes(color = factor(indname)))+
  geom_vline(xintercept = 202003)+
  labs(y = 'Count of Employeed', x = 'Time', color = 'Industry Name')

# Drop in Employment by age group
#Note: Some drop more than others
agg <- df %>% 
  group_by(yearmo,Age_broad) %>% 
  summarise(employed_count = sum(as.numeric(is_employed))) %>% 
  arrange(.,Age_broad, yearmo)

ggplot(data=agg, aes(y = employed_count, x = yearmo))+
  geom_line(alpha = 0.6, aes(color = factor(Age_broad)))+
  geom_vline(xintercept = 202003)+
  labs(y = 'Count of Employeed', x = 'Time', color = 'Age_broad')

# Drop in Employment by sex
agg <- df %>% 
  group_by(yearmo,SEX) %>% 
  summarise(employed_count = sum(as.numeric(is_employed))) %>% 
  arrange(.,SEX, yearmo)

ggplot(data=agg, aes(y = employed_count, x = yearmo))+
  geom_line(alpha = 0.6, aes(color = factor(SEX)))+
  geom_vline(xintercept = 202003)+
  labs(y = 'Count of Employeed', x = 'Time', color = 'SEX')

# Drop in Employment by maritial status
agg <- df %>% 
  group_by(yearmo, MARST) %>% 
  summarise(employed_count = sum(as.numeric(is_employed))) %>% 
  arrange(.,MARST, yearmo)

ggplot(data=agg, aes(y = employed_count, x = yearmo))+
  geom_line(alpha = 0.6, aes(color = factor(MARST)))+
  geom_vline(xintercept = 202003)+
  labs(y = 'Count of Employeed', x = 'Time', color = 'MARST')

## Frequency stats
# Majority class is employed, no large differences in employment among demographic traits

# note majority class is employed for each race
ggplot(df, aes(x= RACE, color = is_employed)) + geom_bar()

# note majority class is employed for each indname 
ggplot(df, aes(x= indname, color = is_employed)) + geom_bar()

# note majority class is employed for each age 
ggplot(df, aes(x= Age_broad, color = is_employed)) + geom_bar()

# note majority class is employed for each sex
ggplot(df, aes(x= SEX, color = is_employed)) + geom_bar()

# note majority class is employed for each MARST
ggplot(df, aes(x= MARST, color = is_employed)) + geom_bar()

##### TODO: More in depth notations

### modeling binary effects (no interaction term)

#basic linear model with factor variables
lpm1 <- feols(is_employed ~ factor(RACE) + factor(SEX) + factor(Age_broad) + factor(MARST) + factor(Education), 
              data = df, se='hetero')
#basic probit model with factor variables
prob1 <- feglm(is_employed ~ factor(RACE) + factor(SEX) + factor(Age_broad) + factor(MARST) + factor(Education),
               family = binomial(link = 'probit'), 
               se='hetero', 
               data = df)
#basic logit model with factor variables               
log1 <- feglm(is_employed ~ factor(RACE) + factor(SEX) + factor(Age_broad) + factor(MARST) + factor(Education),
               family = binomial(link = 'logit'), 
               se='hetero', 
               data = df)
etable(lpm1, prob1, log1, digits = 3)
# Notes: (this is not looking and pre and post covid simply the factors on employment as a constant)
# Some RACE Categories Coefs have a positive effect while others negative. (Which races?)
# All marital statuses have a negative coef (Interesting)
# Higher education has a positive effect on employment probability (which makes sense)

# add predicted values to df
df <- df %>% 
  mutate(lpm_fitted = predict(lpm1))

# plot fitted values for race
# Certain races are have less likely prediction of employment. 
ggplot(data = df, mapping = aes(x=RACE, y=lpm_fitted))+
  geom_point()+
  geom_smooth(color = 'red', se=FALSE)+
  theme_bw()

# plot fitted values for education
# Certain races are have less likely prediction of employment. 
ggplot(data = df, mapping = aes(x=Education, y=lpm_fitted))+
  geom_point()+
  geom_smooth(color = 'red', se=FALSE)+
  theme_bw()

### Modeling scalar (looking at prior wealth)
lpm2 <- feols(is_employed ~ HWTFINL, data = df, se='hetero')
etable(lpm2)
# Statistically significant negative coef with employment

df <- df %>% 
  mutate(lpm2_fitted = predict(lpm2))

ggplot(data = df, mapping = aes(x=HWTFINL, y=lpm2_fitted))+
  geom_point()+
  geom_smooth(color = 'red', se=FALSE)+
  geom_line(aes(x=is_employed, y=lpm2_fitted))+
  theme_bw()
# Strong negative correlation between HWTFINL and its predicted employment status


### Modeling with interaction term
lpm3 <- feols(is_employed ~ COVIDSTATUS, data = df, se='hetero')
lpm4 <- feols(is_employed ~ COVIDSTATUS + COVIDSTATUS:Education, data = df, se='hetero')
etable(lpm3,lpm4)

df %>% 
  group_by(Education) %>% 
  summarise(rec_count = n())

#lpm3 - When precovid = true there is a 4 percentage point increase on is_employed.
#lpm4 - Doctorate stat is interesting but might need help explaining importance. 

## Looking at Occupations type
lpm5 <- feols(is_employed ~ COVIDSTATUS, data = df, se='hetero')
lpm6 <- feols(is_employed ~ COVIDSTATUS + COVIDSTATUS:Occupation, data = df, se='hetero')
prob7 <- feglm(is_employed ~ COVIDSTATUS + COVIDSTATUS:Occupation, family = ('probit') , data = df, se='hetero')
etable(lpm5,lpm6, prob7)
# What is this relative too??



# make dummy for education

doctor <- ifelse(df$Education == 'doctorate', 1, 0)
bachelors <- ifelse(df$Education == 'college/associate/bachelor', 1, 0)
highschool <- ifelse(df$Education == 'high school or lower', 1, 0)

dummy_df <- data.frame(is_employed = df$is_employed,
                       COVIDSTATUS = df$COVIDSTATUS,
                       doctor = doctor,
                       bachelors = bachelors,
                       highschool= highschool)
## Diff in Diff Doc
diff_in_diff <- feols(is_employed ~ doctor * COVIDSTATUS, data =dummy_df)
etable(diff_in_diff)

## Diff in Diff bach
diff_in_diff <- feols(is_employed ~ bachelors * COVIDSTATUS, data =dummy_df)
etable(diff_in_diff)

## Diff in Diff highschool
diff_in_diff <- feols(is_employed ~ COVIDSTATUS + highschool * COVIDSTATUS, data =dummy_df)
etable(diff_in_diff)


