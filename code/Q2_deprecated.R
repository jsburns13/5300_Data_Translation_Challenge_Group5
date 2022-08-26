library(tidyverse)
library(ipumsr)
library(rdrobust)
library(fixest)
library(vtable)

ggplot(data = Q2_data_mo, aes(x=date, y=Employment, colour=Industry, group=Industry)) +
  geom_point() + geom_line()

rdplot(Q2_data_mo$Employment, Q2_data_mo$weeks, c=0, p=1, h=24, kernel = "uniform")

rdplot(Q2_data_mo$Employment, Q2_data_mo$weeks, c=0, p=2, h=25, kernel = "uniform")
rdplot(data_retail$Employment, data_retail$weeks, c=0, p=2, h=25, kernel = "uniform")
ggplot(data=data_ret_summ, aes(x=date, y=Employment, colour=Retail)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept=ISOdate(2020,3,1))

model_1 <- feols(Employment ~ date * disc_id | Industry, data=Q2_data_mo)

etable(model_1)

Q2_data_mo %>%
  group_by(Industry) %>%
  summarize("Employment Rate" = mean(Employment))

IndList <- split(Q2_data_mo, f=Q2_data_mo$Industry)
model_list <- list()
rdrobust_list <- list()

for (i in 1:length(IndList)) {
  indus <- levels(IndList[[i]]$Industry)[i]
  modelname <- paste0(indus, "_model") %>%
    replace(" ", "_")
  
  model_list[[paste0(indus)]] <- assign(modelname,feols(Employment ~ weeks * disc_id, data=IndList[[i]]))
  rdrobust_list[[paste0(indus)]] <- assign(modelname,rdrobust(IndList[[i]]$Employment,IndList[[i]]$weeks,c=0,p=2,h=185,kernel="uniform"))
}

etable(model_list)
for (i in 1:length(rdrobust_list)) {
  print(names(rdrobust_list)[i])
  summary(rdrobust_list[[i]])
}

model_2 <- feols(Employment ~ weeks * disc_id | Retail, data=Q2_data_mo, se="hetero")
model_2_rd_r <- rdrobust(data_retail$Employment, data_retail$weeks, c=0, p=2, h=25, kernel = "uniform")
model_2_rd_nr <- rdrobust(data_non_retail$Employment, data_non_retail$weeks, c=0, p=2, h=25, kernel = "uniform")

model_3 <- feols(Employment ~ disc_id * Industry, data = Q2_data_mo, se="hetero")

etable(model_1, model_2)
summary(model_2_rd_r)
summary(model_2_rd_nr)

# 1 do rdrobusts for model 1 & 2
# 2 redo everything with sum(employment) instead of %
# 3 get retail vs everything in a single rd plot/gg plot
# 4 wald?
# 5 R Markdown

wald(model_3)

