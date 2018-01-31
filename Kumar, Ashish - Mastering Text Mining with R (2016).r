### Kumar book notes ###

# Chapter 7

library(rJava)
library(NLP)
library(openNLP)

install.packages('openNLPmodels.en', repos = "http://datacube.wu.ac.at/", type = "source")

simpleText <- "Text Mining is a interesting field. Mr. Paul is a good
  programmer. He loves watching football. He lives in U.S.A."

simpleText_str <- as.String(simpleText)

annotated_sentence <- annotate(simpleText_str, Maxent_Sent_Token_Annotator())

annotated_word <- annotate (simpleText_str,
                            Maxent_Word_Token_Annotator(probs=TRUE),
                            annotated_sentence)

annotated_str <- annotate(simpleText_str, list(Maxent_Sent_Token_Annotator(),
                                               Maxent_Word_Token_Annotator()))

entity_annotator_loc <-Maxent_Entity_Annotator(language = "en",
                                               kind = "location",
                                               probs = FALSE,
                                               model = NULL)

annotated_loc_val <- annotate(simpleText_str, entity_annotator_org, annotated_str)

simpleText_str[annotated_loc_val][annotated_loc_val$type='entity']

