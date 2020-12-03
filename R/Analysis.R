## p {

##   font-family: Helvetica;

##   font-size: 12pt;

## }

## 
## h1, h2, h3, h4, h5, h6 {

##   font-weight: 600;

## }

## 
## .output-chunk {

##   color: #064669;

## }

## 
## .code-chunk {

##   color: rgb(41, 43, 44);

##   background: rgba(3, 221, 255, 0.15);

##   border: solid 1px rgba(3, 221, 255, 1);

## }


## ----setup, echo=FALSE-------------------------------------
library(knitr)
opts_chunk$set(
	comment = "",
	message = FALSE,
	warning = FALSE,
	tidy.opts = list(
		keep.blank.line = TRUE,
		width.cutoff = 150
		),
	options(width = 150),
	eval = TRUE,
	echo = TRUE,
	fig.height = 7,
	fig.width = 12,
	fig.align = "left",
	class.source = "code-chunk",
	class.output = "output-chunk"
)
# remove scientific notation
options(scipen = 999)

# project colors
colorSet <- c("#44C2BD", "#0E93DA")

# print head
# don't print long-lines


## ----Packages,  results='hide', message=FALSE, warning=FALSE----
library(tidyverse)
library(MASS)
library(caret)
library(haven)
library(purrr)
library(reshape2)
library(scales)
library(ordinal)
library(lattice)
library(psych)
library(fastDummies)
library(FactoMineR)
library(factoextra)
library(logisticPCA)
library(klaR)

# Ensure 'select' comes from package 'dplyr'
select <- dplyr::select

# Ensure 'alpha' comes from 'psych'
alpha <- psych::alpha


## ----Load experiment data, warning=FALSE, message=FALSE----
# Set root directory
setwd("..")

# Load the data
experiment_data <-
  read_sav('data/experiment_data.sav')

# Return the structure of our data
glimpse(experiment_data)


## ----NA check----------------------------------------------
# Count the number of NA's in each column
experiment_data %>%
  summarize_all(~ sum(is.na(.)))


## ----Distribution of responses-----------------------------
# Melt data into long format then graph percent of total for each variable
experiment_data %>%
  melt(id = c("response_id")) %>%
  group_by(value) %>%
  count(variable) %>%
  ungroup() %>% group_by(variable) %>%
  mutate(pct = round(n/sum(n),4) * 100)%>%
  arrange(variable) %>%
  ggplot(aes(x = variable, y = pct, fill = fct_rev(value))) + 
  geom_bar(fill = rep(c(colorSet),20), stat = "identity", position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = pct), position = position_stack(vjust = 0.5), size = 6) +
  labs(x = "Variable", y = "Percent of Total", title = "Distribution of Each Level of Each Variable (%)") +
  theme_bw() +
  theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_text(size = 14), title = element_text(size = 16))


## ----Distribution of answers-------------------------------
# Convert variable types to factor
experiment_data <- experiment_data %>%
  mutate_all(~ as.factor(.))

# Percentage of responses for each answer
experiment_data %>%
  histogram(~ answer,
             main = "Distribution of Answers",
             data = .)


## ----Response distributions price--------------------------
experiment_data %>%
  histogram(~ answer | price, 
            main = "Responses by Price",
            data = .)


## ----Response distributions duration-----------------------
experiment_data %>%
  histogram(~ answer | duration, 
            main = "Responses by Duration",
            data = .)


## ----Response distributions offer--------------------------
experiment_data %>%
  histogram(~ answer | offer, 
            main = "Responses by Offer",
            data = .)


## ----Response distributions outcome------------------------
experiment_data %>%
  histogram(~ answer | outcome, 
            main = "Responses by Outcome",
            data = .)


## ----Response distributions RTB----------------------------
experiment_data %>%
  histogram(~ answer | rtb, 
            main = "Responses by RTB",
            data = .)


## ----Response distributions Social Proof-------------------
experiment_data %>%
  histogram(~ answer | social_proof, 
            main = "Responses by Social Proof",
            data = .)


## ----Response distributions Task---------------------------
# Count plot of 'answer' by 'task'
experiment_data %>%
  ggplot(aes(x = task, y = answer)) +
  geom_count(color = colorSet[1]) +
  labs(title = "Answers by Task", x = "Answer", y = "Task") +
  theme_bw() +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 14), title = element_text(size = 16))

# Correlation between 'task' and 'answer'
experiment_data %>%
  select(task, answer) %>%
  mutate_all(~ as.numeric(.)) %>%
  cor(., method = "kendall") %>%
  .[1,2]


## ----Multicollinearity test--------------------------------
# Test independence of all predictors for all 2-way interactions
experiment_data %>%
  xtabs(~ duration + offer + outcome + price + rtb + social_proof, data = .) %>%
  loglm(~ duration + offer + outcome + price + rtb + social_proof, data = .)


## ----Ordinal regression model------------------------------
# Create a cumulative link mixed model including all predictors
# WARNING: This may take a very long time to compute
ORModel <- experiment_data %>%
  clm(answer ~ duration  + price + offer + outcome + rtb + social_proof,
       data = .,
       nAGQ = 10,
      Hess = TRUE)

summary(ORModel)


## ----Nominal test------------------------------------------
# Test the proportional odds assumption
nominal_test(ORModel)


## ----Ordinal regression interactions model-----------------
# Create a model testing 2-way interactions
ORModelX <- experiment_data %>%
  clm(answer ~ (price + outcome + rtb + social_proof + offer + duration)^2,
       data = .,
       nAGQ = 10,
      Hess = TRUE)

# Compare the main effects only model to the interaction model
anova(ORModel, ORModelX)


## ----Dropped terms model comparison------------------------
# Build a new model that drops 'duration' and 'price' 
ORModel2 <- experiment_data %>%
  clm(answer ~ outcome + price + rtb + social_proof,
       data = ., 
      Hess = T)

# Compare the models
anova(ORModel, ORModel2)


## ----------------------------------------------------------
# Set root directory
setwd("..")

# Load data
survey_data <-
  read_sav('data/survey_data.sav')

# Store the colnames of the survey data
SurveyColnames <- colnames(survey_data)
as.data.frame(SurveyColnames)


## ----NA count----------------------------------------------
# Store the number of NA's in each column
NAOutPut <- survey_data %>%
  summarize_all(~ sum(is.na(.)))

# Print the output in minimal form
as.numeric(NAOutPut)


## ----Complete cases philosophy-----------------------------
# Store Complete number of cases
Sections <- c("Philosophy","Attitudes", "Awareness", "Source", "Behvaior", "Demographic", "Biographic")
CompleteCases <- numeric()

# Function for appending Complete Cases
appendCases <- function(matching) {
  CompleteCases <<- append(CompleteCases,
         survey_data %>%
           select(starts_with(matching)) %>%
           complete.cases() %>%
           sum()
         )
}

# Add complete observation counts for each section of the survey
appendCases("m1_philosophy")
appendCases("m2_attitudes")
appendCases("m2_awareness")
appendCases("source")
appendCases("behavior")
appendCases("d_")
appendCases("s_")

# Combine the section labels and the number of complete cases
casesDf <- data.frame(Sections, CompleteCases)
casesDf


## ----Awareness NAs per column------------------------------
# Get the percentage of NA's per column for the 'awareness' section
survey_data %>%
  select(starts_with("m2_awareness")) %>%
  map(~ paste0(round(sum(is.na(.) / length(.)) * 100, 2), "%"))


## ----Demographics NAs per column---------------------------
# Get the percentage of NA's per column for the 'demographic' section
survey_data %>%
  select(starts_with("d_")) %>%
  map(~ paste0(round(sum(is.na(.) / length(.)) * 100, 2), "%"))


## ----Survey distributions----------------------------------
survey_data %>%
  select(starts_with(c("s_", "d_"))) %>%
  mutate_all(~ as_factor(.)) %>%
  map(~ kable(table(.)))


## ----Philosophy alpha--------------------------------------
# Alpha for the 'philosophy' scale
survey_data %>%
  select(starts_with("m1_philosophy")) %>%
  alpha()


## ----Philosophy dropped alpha------------------------------
# Alpha estimate for the 'philosophy' scale without bad questions (2 and 9)
survey_data %>%
  select(starts_with("m1_philosophy"), -contains(c("_2","_9"))) %>%
  alpha()


## ----Philosophy questions----------------------------------
# Add 'philosophy' composite scores
survey_data$philosophy_composite <- 
  survey_data %>%
  select(starts_with("m1_philosophy"), -contains(c("_2","_9"))) %>% 
  rowMeans()


## ----Attitudes alpha---------------------------------------
# Alpha estimate for the 'attitudes' scale
survey_data %>% 
  select(starts_with("m2_attitudes")) %>%
  alpha()


## ----Attitudes alpha dropped-------------------------------
survey_data %>%
  select(starts_with("m2_attitudes"), -contains(c("_1","_2"))) %>%
  alpha()


## ----Attitudes composite-----------------------------------
# Add 'attitudes' composite scores
survey_data$attitudes_composite <- survey_data %>%
  select(starts_with("m2_attitudes")) %>%
  rowMeans(.)


## ----Survey subsetting-------------------------------------
# Prefixes for each group of the survey
survey_group_prefixes <- c("s_", "d_", "interest_", "past_", "behavior_", "source_")

# Subset only variables of interest
survey_factors <- survey_data %>%
  rename(interest_cbt = interst_cbt) %>%
  select(starts_with(survey_group_prefixes), ends_with("_composite")) %>%
  map(~ as_factor(.)) %>%
  as.data.frame()

# Find variables that have 95% or more of their responses as one response
near_0_vars <- nearZeroVar(survey_factors)
near_0_vars


## ----Near-0 variance---------------------------------------
# Column names for near_0_variance predictors
survey_factors[,near_0_vars] %>%
  colnames()


## ----------------------------------------------------------
# Remove low-variance variables
survey_factors <- survey_factors[,-near_0_vars]


## ----MFA, fig.keep='none', results='hide', cache=TRUE------
# All of the sources have been removed, so remove it from the prefixes list
survey_group_prefixes <- survey_group_prefixes[!survey_group_prefixes %in% "source_"]

# Create an ordered vector of the number of questions in each group
survey_groups <- map(
  survey_group_prefixes, 
      function(x) length(
        # Match column names to the survey_group_prefixes
        grep(
          # "^" specifies "start_with" in R's regex
          paste0("^", x), 
          colnames(survey_factors)))
        )

# Add survey composites to the end of the survey groups
survey_groups <- c(survey_groups,1,1)

# Names for each group
survey_group_names <- c("Biographic", "Demographics", "Interests", "Past-Coach", "Behaviors", "Philosophy", "Attitudes")

# Run an MFA
survey_factors_MFA <- MFA(survey_factors %>% mutate_at(c("philosophy_composite", "attitudes_composite"), as.numeric), 
                          group = as.numeric(survey_groups),
                          # Specify groups (n = categorical, s = numeric)
                          type = c(rep("n",5),rep("s",2)),
                          name.group = survey_group_names,
                          ncp = 6
                          )


## ----MFA variable groups-----------------------------------
fviz_mfa_var(survey_factors_MFA, 'group')


## ----MFA scree plot----------------------------------------
fviz_screeplot(survey_factors_MFA)
paste("Number of Dimensions: ", nrow(survey_factors_MFA$eig))


## ----MFA partial axes--------------------------------------
fviz_mfa_axes(survey_factors_MFA, 
              axes = c(1,2),
              geom = c("point", "arrow"))


## ----MFA dimension 1---------------------------------------
fviz_contrib(survey_factors_MFA, 
             choice = "quali.var", 
             axes = 1, 
             top = 10)


## ----MFA dimension 2---------------------------------------
fviz_contrib(survey_factors_MFA, 
             choice = "quali.var", 
             axes = 2, 
             top = 10)


## ----Dummy encoding----------------------------------------
# Dummy code demographic, biographic, and interest variables
survey_dummy <- survey_data %>%
  rename(interest_cbt = interst_cbt) %>%
  select(starts_with(survey_group_prefixes)) %>%
  map(~ as_factor(.)) %>%
  dummy_cols() %>%
  select_if(is.numeric)

cluster_data <- data.frame(
  survey_dummy,
  survey_data %>%
    select(ends_with("_composite"))
)

ncol(cluster_data)


## ----Remove low-var columns k-modes------------------------
# Find low variance columns
near_0_vars <- nearZeroVar(cluster_data)

# Remove low variance columns
cluster_data <- cluster_data[, -near_0_vars]

# Replace NAs with 0s
cluster_data <- cluster_data %>%
  replace(is.na(.), 0)


## ----Kmodes, results='hide', warnings = FALSE, message=FALSE, cache=TRUE----
# Get within cluster variance values for k = 1:30
klist <- numeric()

map(1:30, function(x){
  # Set the seed to ensure the output is consistent
  set.seed(1073)
  k <- kmodes(cluster_data, modes = x)
  klist <<- c(klist, sum(k$withindiff))
})

kindex <- seq(1,length(klist))

k_df <- data.frame(klist, kindex)

## ----Plot Kmodes-------------------------------------------
# Plot the errors for each value of K
k_df %>%
  ggplot(aes(x = kindex, y = klist)) +
  geom_point(color = colorSet[1], size = 4) + 
  theme_bw() +
  labs(title = "Scree Plot of Total Error for K = 1:30", y = "Total Error", x = "K") +
  theme(legend.position = "none", axis.text = element_text(size = 10), axis.title = element_text(size = 14), title = element_text(size = 16))


## ----Clusters by source------------------------------------
# Set seed to ensure identical results each time
set.seed(1073)

# Kmodes on our data for K = 5
km4 <- kmodes(cluster_data, modes = 4)

source_cluster_data <- data.frame(km4$cluster, 
                                  survey_data %>% select(starts_with("source")))

# Graph the percentage of source of sleep issues by cluster
source_cluster_data %>%
  melt(id.vars = "km4.cluster") %>%
  filter(value == 1) %>%
  group_by(variable) %>%
  count(km4.cluster) %>%
  mutate(pct = round(n/sum(n),4) * 100) %>%
  arrange(variable) %>%
  ggplot(aes(x = variable, y = pct, fill = factor(km4.cluster))) + 
  geom_bar(stat = "identity", position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = pct), position = position_stack(vjust = 0.5), size = 3.5) +
  labs(x = "Source", y = "Percent of Total", title = "Distribution of Sleep Issues by Cluster (%)") +
  theme_bw() +
  theme(legend.title = element_blank(), axis.text.x = element_text(size = 10, angle = 45, hjust = 1), axis.title = element_text(size = 14), title = element_text(size = 16))


## ----Cluster 3---------------------------------------------
# Add cluster labels to our data
survey_data$cluster <- km4$cluster

# Analyze people with cluster 3
survey_data %>%
  filter(cluster == 3) %>%
  select(starts_with(c("d_"))) %>%
  mutate_all(~ as_factor(.)) %>%
  map(~ kable(table(.)))


## ----Create .R file, echo=FALSE, eval=FALSE----------------
purl("vignettes/Analysis.Rmd", documentation = 1)

