setwd("~/Desktop/Dissertation")
analysis_data <- read.csv("analysis_data copy.csv")

ep <- readRDS("Corpus_speeches_EP.RDS")

library(fixest)
library(modelsummary)
library(dplyr)
library(tibble)


#OLS With clustered SE

sci_ols_cl1 <- feols(scientific_prop ~ female, data = analysis_data, vcov = ~ speaker)

sci_ols_cl2 <- feols(
  scientific_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  vcov = ~ speaker
)

soc_ols_cl1 <- feols(social_prop ~ female, data = analysis_data, vcov = ~ speaker)

soc_ols_cl2 <- feols(
  social_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  vcov = ~ speaker
)

emo_ols_cl1 <- feols(emotion_prop ~ female, data = analysis_data, vcov = ~ speaker)

emo_ols_cl2 <- feols(
  emotion_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  vcov = ~ speaker
)

modelsummary(
  list(
    "Scientific (1)" = sci_ols_cl1,
    "Scientific (2)" = sci_ols_cl2,
    "Social (1)"     = soc_ols_cl1,
    "Social (2)"     = soc_ols_cl2,
    "Emotion (1)"    = emo_ols_cl1,
    "Emotion (2)"    = emo_ols_cl2
  ),
  stars = TRUE,
  statistic = "({std.error})",
  coef_omit = "party|factor\\(year\\)",
  coef_rename = c(
    "female" = "Female",
    "speech_length" = "Speech length"
  ),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors",
  add_rows = tibble(
    term = c("Party controls", "Year fixed effects", "SE type"),
    `Scientific (1)` = c("No",  "No",  "Clustered by MEP"),
    `Scientific (2)` = c("Yes", "Yes", "Clustered by MEP"),
    `Social (1)`     = c("No",  "No",  "Clustered by MEP"),
    `Social (2)`     = c("Yes", "Yes", "Clustered by MEP"),
    `Emotion (1)`    = c("No",  "No",  "Clustered by MEP"),
    `Emotion (2)`    = c("Yes", "Yes", "Clustered by MEP")
  ),
  title = "OLS models with clustered standard errors",
  output = "regression_table_clustered.html"
)


library(stats)
library(modelsummary)
library(tibble)
library(fixest)



#Fractional logit regression 

sci_frac1 <- feglm(scientific_prop ~ female, 
                   data = analysis_data, 
                   family = binomial(link = "logit"), 
                   cluster = ~speaker)

sci_frac2 <- feglm(scientific_prop ~ female + party + speech_length + factor(year), 
                   data = analysis_data, 
                   family = binomial(link = "logit"), 
                   cluster = ~speaker)

soc_frac1 <- feglm(social_prop ~ female, 
                   data = analysis_data, 
                   family = binomial(link = "logit"), 
                   cluster = ~speaker)

soc_frac2 <- feglm(social_prop ~ female + party + speech_length + factor(year), 
                   data = analysis_data, 
                   family = binomial(link = "logit"), 
                   cluster = ~speaker)

emo_frac1 <- feglm(emotion_prop ~ female, 
                   data = analysis_data, 
                   family = binomial(link = "logit"), 
                   cluster = ~speaker)

emo_frac2 <- feglm(emotion_prop ~ female + party + speech_length + factor(year), 
                   data = analysis_data, 
                   family = binomial(link = "logit"), 
                   cluster = ~speaker)

# Summary table for Fractional Logit
modelsummary(
  list(
    "Scientific (1)" = sci_frac1,
    "Scientific (2)" = sci_frac2,
    "Social (1)"     = soc_frac1,
    "Social (2)"     = soc_frac2,
    "Emotion (1)"    = emo_frac1,
    "Emotion (2)"    = emo_frac2
  ),
  stars = TRUE,
  # No need for manual vcov list: modelsummary detects the cluster from feglm automatically
  statistic = "({std.error})",
  coef_omit = "party|factor\\(year\\)",
  coef_rename = c("female" = "Female", "speech_length" = "Speech length"),
  gof_omit = "AIC|BIC|Log.Lik|Deviance|DF|RMSE|Std.Errors",
  add_rows = tibble(
    term = c("Party controls", "Year fixed effects", "SE type", "Model"),
    `Scientific (1)` = c("No",  "No",  "Clustered (speaker)", "Fractional logit"),
    `Scientific (2)` = c("Yes", "Yes", "Clustered (speaker)", "Fractional logit"),
    `Social (1)`     = c("No",  "No",  "Clustered (speaker)", "Fractional logit"),
    `Social (2)`     = c("Yes", "Yes", "Clustered (speaker)", "Fractional logit"),
    `Emotion (1)`    = c("No",  "No",  "Clustered (speaker)", "Fractional logit"),
    `Emotion (2)`    = c("Yes", "Yes", "Clustered (speaker)", "Fractional logit")
  ),
  title = "Fractional logit models with clustered standard errors",
  output = "regression_table_fractional.html"
)