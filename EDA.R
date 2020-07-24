library(tidyverse) # Load required packages
library(gridExtra)
library(knitr)
library(Amelia)
library(nainar)
library(ggplot2)
library(reshape)
library(tidyverse)
library(readr)
library(esquisse)
library(naniar)



repurchase_training <- read_csv("repurchase_training.csv") # load/read dataset

dim(repurchase_training)



rep_eda <- repurchase_training






#Make variables as factors

rep_eda$Target <- as.factor(rep_eda$Target)
rep_eda$age_band <- as.factor(rep_eda$age_band)
rep_eda$gender <- as.factor(rep_eda$gender)
rep_eda$car_model  <- as.factor(rep_eda$car_model)
rep_eda$car_segment  <- as.factor(rep_eda$car_segment)
rep_eda$age_of_vehicle_years  <- as.factor(rep_eda$age_of_vehicle_years)
rep_eda$sched_serv_warr  <- as.factor(rep_eda$sched_serv_warr)
rep_eda$non_sched_serv_warr  <- as.factor(rep_eda$non_sched_serv_warr)
rep_eda$sched_serv_paid  <- as.factor(rep_eda$sched_serv_paid)
rep_eda$non_sched_serv_paid  <- as.factor(rep_eda$non_sched_serv_paid)
rep_eda$total_paid_services  <- as.factor(rep_eda$total_paid_services)
rep_eda$total_services  <- as.factor(rep_eda$total_services)
rep_eda$mth_since_last_serv  <- as.factor(rep_eda$mth_since_last_serv)
rep_eda$annualised_mileage  <- as.factor(rep_eda$annualised_mileage)
rep_eda$num_dealers_visited  <- as.factor(rep_eda$num_dealers_visited)
rep_eda$num_serv_dealer_purchased  <- as.factor(rep_eda$num_serv_dealer_purchased)


# Change NULL to NA
rep_eda[rep_eda == "NULL"] = NA


# EDA for missing values
missmap(rep_eda) 


missing.values <- rep_eda %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 






missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#################################################################



missing.values <- rep_eda %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <- (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

#################################################################

gg_miss_upset(rep_eda)


# EDA for class imbalance

ggplot(rep_eda) +
  aes(x = Target, fill = Target) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(y = "Number of observations", title = "Number of observations in each Target") +
  theme_gray()



table(rep_eda$Target)


