### Kumar book notes ###

# Chapter 7

library(rJava)
library(NLP)
library(openNLP)

if(!"openNLPmodels.en" %in% installed.packages()[,"Package"]){
 install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
 library(openNLPmodels.en)
} else library(openNLPmodels.en)


simpleText <- "Text Mining is a interesting field in Canada. Mr. Paul is a good
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

annotated_loc_val <- annotate(simpleText_str, entity_annotator_loc, annotated_str)

simpleText_str[annotated_loc_val][annotated_loc_val$type=='entity']

entity_annotator <- function(entity) {
	Maxent_Entity_Annotator(language = "en",
                            kind = entity,
                            probs = FALSE,
                            model = NULL)}

entity_extractor <- function(text, annotator) {
	text_str <- as.String(text)
	annotated_sentence <- annotate(text_str, Maxent_Sent_Token_Annotator())
	annotated_word <- annotate(text_str, Maxent_Word_Token_Annotator(probs=T),
                               annotated_sentence)
	annotated_val <- annotate(text_str, annotator, annotated_word)
	text_str[annotated_val][annotated_val$type=='entity']
}

#ex
annotator <- entity_annotator('location')
entity_extractor(simpleText, annotator)

#ex2
sapply(c(simpleText,simpleText,"hello, there!"), entity_extractor, entity_annotator('location'), USE.NAMES = F)

