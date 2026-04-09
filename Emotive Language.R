
emotion_dictionary<-dictionary(list(
  emotive=c(
    "hope*",
    "fear*",
    "crisis",
    "emergency",
    "threat*",
    "future",
    "danger*",
    "protect*",
    "concern",
    "save",
    "action",
    "listen",
    "care*",
    "important",
    "justice"
  )
))


emotion_dfm <- dfm_lookup(dfm_all,emotion_dictionary)

analysis_data$emotion_prop <- 
  as.numeric(emotion_dfm[,"emotive"])/ntoken(dfm_all)


analysis_data%>%
  group_by(gender)%>%
  summarise(
    mean_emotion=mean(emotion_prop,na.rm=TRUE)
  )

