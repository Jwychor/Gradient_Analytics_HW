####Packages####
library(tidyverse)
library(caret)
library(conjoint)
library(MASS)
library(haven)
library(purrr)
library(reshape2)

setwd("C:\\Users\\jwych\\Documents\\GitHub\\Gradient_Analytics_HW")

#### Q1 ####
experiment_data <- read_sav('data/experiment_data.sav')
?melt
colnames(experiment_data)
levels(experiment_data$rtb)

ORModel <- experiment_data %>%
  mutate_all(~ as.factor(.)) %>%
  polr(answer ~ duration + offer + outcome + price + rtb + social_proof,
       data = ., Hess = T)

install.packages('data.table')

experiment_data %>%
  group_by(response_id) %>%
  summarize(GroupVariance = var(rep(answer)))

experiment_data %>%
  select_if(!str_detect(names(.),"response_id|task")) %>%
  gather() %>%
  count(.$value) %>%
  print(., n = 28)

experiment_data %>%
  mutate_all(~ as.character(.)) %>%
  melt(id = c("response_id"))

experiment_data %>%
  melt(id = c("response_id")) %>%
  group_by(value) %>%
  count(variable) %>%
  arrange(variable) %>%
  ggplot(aes(x = variable, y = n, fill = fct_rev(value))) + 
  geom_bar(fill = rep(c("#44C2BD", "#0E93DA"),20), stat = "identity", position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  theme(legend.position = "none") +
  labs(x = "Variable", y = "Count", title = "Distribution of Each Level of Each Variable") +
  theme_bw()

summary(ORModel)

ci <- confint(ORModel)
exp(coef(ORModel))
oddsRatios <- exp(cbind(OR = coef(ORModel), ci))
oddsRatios

#### Q2 ####

install.packages('ordinal')

survey_data <- read_sav('data/survey_data.sav')
