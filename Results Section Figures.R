#Data for plotting

analysis_data$gender <- recode(analysis_data$gender,
                               "F" = "Female",
                               "M" = "Male"
)



library(tidyr)

plot_data <- analysis_data %>%
  select(gender, scientific_prop, social_prop) %>%
  pivot_longer(
    cols = c(scientific_prop, social_prop),
    names_to = "frame",
    values_to = "value"
  )

plot_data$gender <- recode(plot_data$gender,
                               "F" = "Female",
                               "M" = "Male"
)


##Table 1 results 5.1 

library(dplyr)

results_table <- analysis_data %>%
  group_by(gender) %>%
  summarise(
    n_speeches = n(),
    mean_scientific = mean(scientific_prop, na.rm = TRUE),
    mean_social = mean(social_prop, na.rm = TRUE),
    mean_emotion = mean(emotion_prop, na.rm = TRUE)
  )

results_table <- results_table %>%
  mutate(
    mean_scientific = round(mean_scientific, 4),
    mean_social = round(mean_social, 4),
    mean_emotion = round(mean_emotion, 4)
  )

results_table

library(knitr)
library(kableExtra)

results_table %>%
  kable(
    caption = "Average use of different climate frames by gender",
    col.names = c("Gender", "Number of Speeches", "Scientific Frame", "Social Frame", "Emotive Language"),
    align = "lcccc"
  ) %>%
  kable_styling(full_width = FALSE, position = "center")




##Scientific and Social frame Boxplot

plot_data <- analysis_data %>%
  select(gender, scientific_prop, social_prop) %>%
  pivot_longer(
    cols = c(scientific_prop, social_prop),
    names_to = "frame",
    values_to = "value"
  )

plot_data$frame <- recode(plot_data$frame,
                          scientific_prop = "Scientific frame",
                          social_prop = "Social frame"
)

library(ggplot2)

ggplot(plot_data, aes(x = gender, y = value, fill = gender)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 1) +
  facet_wrap(~ frame) +
  theme_bw() +
  labs(
    x = "Gender",
    y = "Proportion of speech devoted to frame words",
    title = "Scientific and social framing in Green Deal speeches by gender",
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )





##Regression Table 1 (scientific and social frame)

sci_model1 <- lm(scientific_prop ~ female, data = analysis_data)

sci_model2 <- lm(
  scientific_prop ~ female + party + speech_length + factor(year),
  data = analysis_data
)

soc_model1 <- lm(social_prop ~ female, data = analysis_data)

soc_model2 <- lm(
  social_prop ~ female + party + speech_length + factor(year),
  data = analysis_data
)


library(modelsummary)

modelsummary(
  list(
    "Scientific (1)" = sci_model1,
    "Scientific (2)" = sci_model2,
    "Social (1)" = soc_model1,
    "Social (2)" = soc_model2
  ),
  stars = TRUE,
  statistic = "({std.error})",
  coef_omit = "party|factor\\(year\\)",
  coef_rename = c(
    "female" = "Female",
    "speech_length" = "Speech length"
  ),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
  add_rows = tibble(
    term = c("Party controls", "Year fixed effects"),
    `Scientific (1)` = c("No", "No"),
    `Scientific (2)` = c("Yes", "Yes"),
    `Social (1)` = c("No", "No"),
    `Social (2)` = c("Yes", "Yes")
  ),
  title = "OLS regression results for scientific and social climate framing",
  output = "regression_table_frames.html"
)






#Regression Table 2 (emotive langauge)
emo_model1 <- lm(emotion_prop ~ female, data = analysis_data)

emo_model2 <- lm(
  emotion_prop ~ female + party + speech_length + factor(year),
  data = analysis_data
)

modelsummary(
  list(
    "Emotion (1)" = emo_model1,
    "Emotion (2)" = emo_model2
  ),
  stars = TRUE,
  statistic = "({std.error})",
  coef_omit = "party|factor\\(year\\)",
  coef_rename = c(
    "female" = "Female",
    "speech_length" = "Speech length"
  ),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
  add_rows = tibble(
    term = c("Party controls", "Year fixed effects"),
    `Emotion (1)` = c("No", "No"),
    `Emotion (2)` = c("Yes", "Yes")
  ),
  title = "OLS regression results for emotive language",
  output = "regression_table_emotion.html"
)






#Emotion boxplot

ggplot(analysis_data, aes(x = gender, y = emotion_prop, fill = gender)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 1) +
  theme_bw() +
  labs(
    x = "Gender",
    y = "Proportion of speech devoted to emotive language",
    title = "Emotive language by gender in Green Deal speeches",
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )
