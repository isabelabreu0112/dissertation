
library(dplyr)
library(stringr)
library(stringi)
library(gender)
library(cld3)
library(deeplr)


setwd("~/Desktop/Dissertation")
ep <- readRDS("Corpus_speeches_EP.RDS")

ep$date <- as.Date(ep$date)





# 1) Filter green deal speeches
green_speeches <- ep %>%
  filter(!is.na(agenda)) %>%
  filter(str_detect(str_to_lower(agenda), "green deal"))

# Quick checks
nrow(green_speeches)
length(unique(green_speeches$agenda))
sort(table(green_speeches$agenda), decreasing = TRUE)






# 2) Keep only MEP speeches

clean_speeches <- green_speeches %>%
  filter(MEP == TRUE)






# 3) Build speaker-level gender table

speaker_gender <- clean_speeches %>%
  select(speaker) %>%
  distinct() %>%
  mutate(
    first_name = word(speaker, 1),
    first_name_ascii = stri_trans_general(first_name, "Latin-ASCII"),
    first_name_ascii = gsub("[^A-Za-z]", "", first_name_ascii)
  )

# Automatic gender coding from first names
gender_data <- gender(unique(speaker_gender$first_name_ascii))

speaker_gender <- speaker_gender %>%
  left_join(gender_data, by = c("first_name_ascii" = "name")) %>%
  mutate(
    gender_simple = case_when(
      gender == "female" ~ "F",
      gender == "male" ~ "M",
      TRUE ~ NA_character_
    )
  )





# 4) Manual fixes

manual_fix <- data.frame(
  speaker = c(
    "Tiemo Wölken",
    "François-Xavier Bellamy",
    "Marie-Pierre Vedrenne",
    "Πέτρος Κόκκαλης",
    "Izabela-Helena Kloc",
    "Sunčana Glavak",
    "Mazaly Aguilar",
    "Ljudmila Novak",
    "Цветелина Пенкова",
    "Zdzisław Krasnodębski",
    "Domènec Ruiz Devesa",
    "Iratxe García Pérez",
    "Geert Bourgeois",
    "Enikő Győri",
    "Henrike Hahn",
    "Μαρία Σπυράκη",
    "Miapetra Kumpula-Natri",
    "Ангел Джамбазки",
    "Jaak Madison",
    "Jytte Guteland",
    "Bert-Jan Ruissen",
    "Teuvo Hakkarainen",
    "Izaskun Bilbao Barandica",
    "Urmas Paet",
    "Geoffroy Didier",
    "Inmaculada Rodríguez-Piñero Fernández",
    "Sirpa Pietikäinen",
    "Marian-Jean Marinescu",
    "Λευτέρης Νικολάου-Αλαβάνος",
    "Dolors Montserrat",
    "Cristian-Silviu Buşoi",
    "Constanze Angela Krehl",
    "Ева Майдел",
    "Leszek Miller",
    "Rovana Plumb",
    "Dan-Ştefan Motreanu"
  ),
  gender_simple_manual = c(
    "M","M","F","M","F","F","F","F","F","M","M","F","M","F","F","F","F",
    "M","M","F","M","M","F","M","M","F","F","M","M","F","M","F","F","M","F","M"
  ),
  stringsAsFactors = FALSE
)

speaker_gender <- speaker_gender %>%
  left_join(manual_fix, by = "speaker") %>%
  mutate(
    gender_simple = coalesce(gender_simple_manual, gender_simple)
  ) %>%
  select(-gender_simple_manual)

# inspect any unresolved speakers
speaker_gender %>%
  filter(is.na(gender_simple)) %>%
  select(speaker, first_name, first_name_ascii)





# 5) merge corrected gender back into speeches

clean_speeches <- clean_speeches %>%
  select(-any_of(c(
    "first_name", "first_name_ascii", "gender", "gender_simple",
    "proportion_male", "proportion_female", "year_min", "year_max"
  ))) %>%
  left_join(
    speaker_gender %>% select(speaker, gender_simple),
    by = "speaker"
  )





# 6) Final cleaned analysis dataset

analysis_data <- clean_speeches %>%
  select(
    speaker,
    party,
    gender = gender_simple,
    date,
    agenda,
    speechnumber,
    text
  ) %>%
  mutate(
    translated_text = NA_character_,
    language = cld3::detect_language(text)
  )

# Quick checks
str(analysis_data)
table(analysis_data$gender, useNA = "ifany")
table(analysis_data$language, useNA = "ifany")






# 7) deepl translation

DEEPL_AUTH_KEY <- (API KEY)


# Keep English speeches as-is
analysis_data <- analysis_data %>%
  mutate(
    translated_text = ifelse(language == "en", text, translated_text)
  )

# Translate only non-English speeches with non-missing text
needs_translation <- which(
  analysis_data$language != "en" &
    !is.na(analysis_data$text) &
    analysis_data$text != ""
)

analysis_data$translated_text[needs_translation] <- sapply(
  analysis_data$text[needs_translation],
  function(x) {
    tryCatch(
      translate2(
        text = x,
        auth_key = DEEPL_AUTH_KEY,
        target_lang = "EN"
      ),
      error = function(e) NA_character_
    )
  }
)






# 8) Variables - eg. speech length

analysis_data$speech_length <- str_count(analysis_data$translated_text, "\\S+")

analysis_data$year <- format(as.Date(analysis_data$date), "%Y")

analysis_data$female <- ifelse(analysis_data$gender == "F", 1, 0)





# 9) Save final dataset

write.csv(analysis_data, "green_deal_analysis_data.csv", row.names = FALSE)
write.csv(analysis_data_share, "analysis_data_share.csv", row.names = FALSE)





