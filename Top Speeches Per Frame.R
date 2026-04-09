top_scientific <- analysis_data %>%
  arrange(desc(scientific_prop)) %>%
  select(
    speaker,
    gender,
    date,
    agenda,
    scientific_prop,
    translated_text
  ) %>%
  slice(1:10) 

top_scientific


top_social <- analysis_data %>%
  arrange(desc(social_prop)) %>%
  select(
    speaker,
    gender,
    date,
    agenda,
    social_prop,
    translated_text
  ) %>%
  slice(1:10)

top_social

top_emotion <- analysis_data %>%
  arrange(desc(emotion_prop)) %>%
  select(
    speaker,
    gender,
    date,
    agenda,
    emotion_prop,
    translated_text
  ) %>%
  slice(1:10)

top_emotion

