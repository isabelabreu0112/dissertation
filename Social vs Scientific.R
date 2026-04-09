library(dplyr)
library(quanteda)

#create corpus
corp <- corpus(analysis_data, text_field = "translated_text")

#tokenize speeches 
toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE
)

toks <- tokens_remove(toks, stopwords("en"))



#build dfm
dfm_all <- dfm(toks)



###CREATE DICTIONARIES 
frame_dictionary <- 
  dictionary(list( 
    scientific = c( 
      "technolog*", 
      "innovation*", 
      "energy",
      "efficien*", 
      "industry", 
      "research", 
      "scientific*", 
      "emission", 
      "solution",
      "strategy",
      "develop*",
      "decarbon*",
      "tool",
      "security",
      "companies"
    ), 
    social = c( 
      "justice", 
      "fair*",
      "responsib*", 
      "commun*", 
      "people", 
      "worker*", 
      "health*", 
      "citizen",
      "social",
      "vulnerable",
      "safe*",
      "nature",
      "just*",
      "poverty",
      "planet"
    ) 
))

#apply dictionary 
frame_dfm <- dfm_lookup(dfm_all, frame_dictionary)


#convert into speech proportions
analysis_data$scientific_prop <- as.numeric(frame_dfm[, "scientific"]) / ntoken(dfm_all)
analysis_data$social_prop     <- as.numeric(frame_dfm[, "social"]) / ntoken(dfm_all)

analysis_data$scientific_count <- as.numeric(frame_dfm[, "scientific"])
analysis_data$social_count     <- as.numeric(frame_dfm[, "social"]) 


#difference in frames 
analysis_data$frame_balance <- analysis_data$social_prop - analysis_data$scientific_prop

ggplot(analysis_data, aes(x = gender, y = frame_balance)) +
  geom_boxplot() +
  theme_bw()

#check results of frames 

summary(analysis_data$scientific_prop)
summary(analysis_data$social_prop)

summary(analysis_data$scientific_count)
summary(analysis_data$social_count)


#Average frame use by gender

library(dplyr)

frame_summary <- analysis_data %>%
  group_by(gender) %>%
  summarise(
    n_speeches = n(),
    scientific_mean = mean(scientific_prop, na.rm = TRUE),
    social_mean = mean(social_prop, na.rm = TRUE),
    scientific_median = median(scientific_prop, na.rm = TRUE),
    social_median = median(social_prop, na.rm = TRUE)
  )

frame_summary


#Statistical comparison
t.test(scientific_prop ~ gender, data = analysis_data)
t.test(social_prop ~ gender, data = analysis_data)

