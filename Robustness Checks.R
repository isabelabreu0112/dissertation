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

library(fixest)
library(modelsummary)
library(tibble)

#OLS With robust SE

sci_ols_rob1 <- feols(scientific_prop ~ female, data = analysis_data, vcov = "HC1")

sci_ols_rob2 <- feols(
  scientific_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  vcov = "HC1"
)

soc_ols_rob1 <- feols(social_prop ~ female, data = analysis_data, vcov = "HC1")

soc_ols_rob2 <- feols(
  social_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  vcov = "HC1"
)

emo_ols_rob1 <- feols(emotion_prop ~ female, data = analysis_data, vcov = "HC1")

emo_ols_rob2 <- feols(
  emotion_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  vcov = "HC1"
)

modelsummary(
  list(
    "Scientific (1)" = sci_ols_rob1,
    "Scientific (2)" = sci_ols_rob2,
    "Social (1)"     = soc_ols_rob1,
    "Social (2)"     = soc_ols_rob2,
    "Emotion (1)"    = emo_ols_rob1,
    "Emotion (2)"    = emo_ols_rob2
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
    `Scientific (1)` = c("No",  "No",  "Robust (HC1)"),
    `Scientific (2)` = c("Yes", "Yes", "Robust (HC1)"),
    `Social (1)`     = c("No",  "No",  "Robust (HC1)"),
    `Social (2)`     = c("Yes", "Yes", "Robust (HC1)"),
    `Emotion (1)`    = c("No",  "No",  "Robust (HC1)"),
    `Emotion (2)`    = c("Yes", "Yes", "Robust (HC1)")
  ),
  title = "OLS models with heteroskedasticity-robust standard errors",
  output = "regression_table_robust.html"
)




library(stats)
library(modelsummary)
library(sandwich)
library(lmtest)
library(tibble)


#Fractional logit regression 

sci_frac1 <- glm(
  scientific_prop ~ female,
  data = analysis_data,
  family = quasibinomial(link = "logit")
)

sci_frac2 <- glm(
  scientific_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  family = quasibinomial(link = "logit")
)

soc_frac1 <- glm(
  social_prop ~ female,
  data = analysis_data,
  family = quasibinomial(link = "logit")
)

soc_frac2 <- glm(
  social_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  family = quasibinomial(link = "logit")
)

emo_frac1 <- glm(
  emotion_prop ~ female,
  data = analysis_data,
  family = quasibinomial(link = "logit")
)

emo_frac2 <- glm(
  emotion_prop ~ female + party + speech_length + factor(year),
  data = analysis_data,
  family = quasibinomial(link = "logit")
)

# robust SE list for modelsummary
vcov_frac <- list(
  sandwich::vcovHC(sci_frac1, type = "HC1"),
  sandwich::vcovHC(sci_frac2, type = "HC1"),
  sandwich::vcovHC(soc_frac1, type = "HC1"),
  sandwich::vcovHC(soc_frac2, type = "HC1"),
  sandwich::vcovHC(emo_frac1, type = "HC1"),
  sandwich::vcovHC(emo_frac2, type = "HC1")
)

modelsummary(
  list(
    "Scientific (1)" = sci_frac1,
    "Scientific (2)" = sci_frac2,
    "Social (1)"     = soc_frac1,
    "Social (2)"     = soc_frac2,
    "Emotion (1)"    = emo_frac1,
    "Emotion (2)"    = emo_frac2
  ),
  vcov = vcov_frac,
  stars = TRUE,
  statistic = "({std.error})",
  coef_omit = "party|factor\\(year\\)",
  coef_rename = c(
    "female" = "Female",
    "speech_length" = "Speech length"
  ),
  gof_omit = "AIC|BIC|Log.Lik|Deviance|DF|RMSE",
  add_rows = tibble(
    term = c("Party controls", "Year fixed effects", "Model"),
    `Scientific (1)` = c("No",  "No",  "Fractional logit"),
    `Scientific (2)` = c("Yes", "Yes", "Fractional logit"),
    `Social (1)`     = c("No",  "No",  "Fractional logit"),
    `Social (2)`     = c("Yes", "Yes", "Fractional logit"),
    `Emotion (1)`    = c("No",  "No",  "Fractional logit"),
    `Emotion (2)`    = c("Yes", "Yes", "Fractional logit")
  ),
  title = "Fractional logit / quasi-binomial models",
  output = "regression_table_fractional.html"
)