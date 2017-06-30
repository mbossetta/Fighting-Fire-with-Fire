#Load Packages
library(devtools)
library(qdap)
library(XML)
library(tm)
library(rgdal)
library(rJava)
library(SnowballC)
library(pvclust)
library(ggplot2)
library(ggfortify)

#Load Transcripts
LBC <- read.transcript("THE LBC DEBATE FINAL.docx", col.names = c("Person", "Dialogue"), merge.broke.tot = "TRUE", quote2bracket = "TRUE", rm.empty.rows = "TRUE", header = "FALSE")
BBC <- read.transcript("BBC DEBATE FINAL.docx", col.names = c("Person", "Dialogue"), merge.broke.tot = "TRUE", quote2bracket = "TRUE", rm.empty.rows = "TRUE")

#Add Debate Variable
LBC$debate <- "LBC"
BBC$debate <- "BBC"

# Pre-process/clean the texts #

# LBC #
LBC$Dialogue <- replace_symbol(LBC$Dialogue)
LBC$Dialogue <- replace_ordinal(LBC$Dialogue)
LBC$Dialogue <- scrubber(LBC$Dialogue, num2word = TRUE, rm.quote = TRUE, fix.comma = TRUE,
                         fix.space = TRUE)
LBC$Dialogue <- incomplete_replace(LBC$Dialogue)
add_incomplete(LBC$Dialogue, endmarks = "[.?|!]+$", silent = FALSE)
comma_spacer(LBC$Dialogue)
clean(LBC$Dialogue)

# BBC #
BBC$Dialogue <- replace_number(BBC$Dialogue)
BBC$Dialogue <- replace_symbol(BBC$Dialogue)
BBC$Dialogue <- replace_ordinal(BBC$Dialogue)
BBC$Dialogue <- scrubber(BBC$Dialogue, num2word = TRUE, rm.quote = TRUE, fix.comma = TRUE,
                         fix.space = TRUE)
BBC$Dialogue <- incomplete_replace(BBC$Dialogue)
add_incomplete(BBC$Dialogue, endmarks = "[.?|!]+$", silent = FALSE)
comma_spacer(BBC$Dialogue)
clean(BBC$Dialogue)

##Check Text for errors
check_text(LBC$Dialogue)
check_text(BBC$Dialogue)

##SentSplit, create stemmed column
LBC2 <- sentSplit(LBC, "Dialogue", stem = "TRUE")
BBC2 <- sentSplit(BBC, "Dialogue", stem = "TRUE")

##Split Texts by person
LBCNF <- subset(LBC2, LBC2$Person=="NIGEL FARAGE")
LBCNC <- subset(LBC2, LBC2$Person=="NICK CLEGG")

BBCNF <- subset(BBC2, BBC2$Person=="NIGEL FARAGE")
BBCNC <- subset(BBC2, BBC2$Person=="NICK CLEGG")

##bind back together (to cut out moderators)
NC <- rbind(BBCNC, LBCNC)
NF <- rbind(BBCNF, LBCNF)
NCNF <- rbind(NC, NF)

##including moderators (for F-score)
ALL <- rbind(LBC2, BBC2)

##Group Proper Names and create columns by non-stemmed ($dia) and stemmed ($dia2)
keepsprop <- c("european union", "nigel farage", "nick clegg", "mister farage", "mister clegg", "nigel farag", 
               "vladimir putin", "mister cameron", "david cameron", "mister miliband", "ed miliband")

LBC2$dia <- space_fill(LBC2$Dialogue, keepsprop)
BBC2$dia <- space_fill(BBC2$Dialogue, keepsprop)
LBCNF$dia <- space_fill(LBCNF$Dialogue, keepsprop)
LBCNC$dia <- space_fill(LBCNC$Dialogue, keepsprop)
BBCNF$dia <- space_fill(BBCNF$Dialogue, keepsprop)
BBCNC$dia <- space_fill(BBCNC$Dialogue, keepsprop)
LBC2$dia2 <- space_fill(LBC2$stem.text, keepsprop)
BBC2$dia2 <- space_fill(BBC2$stem.text, keepsprop)
LBCNF$dia2 <- space_fill(LBCNF$stem.text, keepsprop)
LBCNC$dia2 <- space_fill(LBCNC$stem.text, keepsprop)
BBCNF$dia2 <- space_fill(BBCNF$stem.text, keepsprop)
BBCNC$dia2 <- space_fill(BBCNC$stem.text, keepsprop)
NC$dia2 <- space_fill(NC$stem.text, keepsprop)
NC$dia <- space_fill(NC$Dialogue, keepsprop)
NF$dia2 <- space_fill(NF$stem.text, keepsprop)
NF$dia <- space_fill(NF$Dialogue, keepsprop)
NCNF$dia <- space_fill(NCNF$Dialogue, keepsprop)
NCNF$dia2 <- space_fill(NCNF$stem.text, keepsprop)
ALL$dia2 <- space_fill(ALL$stem.text, keepsprop)
ALL$dia <- space_fill(ALL$Dialogue, keepsprop)

## stopwords
SW <- exclude(Top200Words, "people", "work", 
              "great", "old", "change", "different", "world", "America", 
              "answer")

##extra pronoun/contraction stopword list

SW2 <- c("i", "i'm", "i'll", "i've", "it's", "you're", "you've", "we've", 
         "that's", "those", "we're", "he'll")

##Word Frequencies Per Actor Per Debate, non-stemmed
##Note: for stemmed lists sub $dia for $dia2

LBCNFfreq <- freq_terms(LBCNF$dia, top = 15, at.least = 2, stopwords = c(SW, SW2), extend = TRUE, char.keep = NULL)
print(LBCNFfreq)

LBCNCfreq <- freq_terms(LBCNC$dia, top = 15, at.least = 2, stopwords = c(SW, SW2), extend = TRUE, char.keep = NULL)
print(LBCNCfreq)

BBCNFfreq <- freq_terms(BBCNF$dia, top = 15, at.least = 2, stopwords = c(SW, SW2), extend = TRUE, char.keep = NULL)
print(BBCNFfreq)

BBCNCfreq <- freq_terms(BBCNC$dia, top = 15, at.least = 2, stopwords = c(SW, SW2), extend = TRUE, char.keep = NULL)
print(BBCNCfreq)

##word difference list on non-stemmed lists

LBCworddiff <- word_diff_list(LBC2$dia, grouping.var = LBC2$Person, vs.all = FALSE, vs.all.cut = 1, 
                              stopwords = SW,
                              alphabetical = FALSE, digits = 2)
print(LBCworddiff$`NICK CLEGG_vs_NIGEL FARAGE`$`unique_to_NICK CLEGG`)
print(LBCworddiff$`NICK CLEGG_vs_NIGEL FARAGE`$`unique_to_NIGEL FARAGE`)


BBCworddiff <- word_diff_list(BBC2$dia2, grouping.var = BBC2$Person, vs.all = FALSE, vs.all.cut = 1, 
                              stopwords = SW,
                              alphabetical = FALSE, digits = 2)
print(BBCworddiff$`NICK CLEGG_vs_NIGEL FARAGE`$`unique_to_NICK CLEGG`)
print(BBCworddiff$`NICK CLEGG_vs_NIGEL FARAGE`$`unique_to_NIGEL FARAGE`)

##Formality Scores (using original Dialogue, i.e without proper nouns grouped)

ALLFORM <- with(ALL, formality(Dialogue, list(Person, debate), order.by.formality = TRUE, digits = 2))
ALLFORM2 <- scores(ALLFORM)
plot(ALLFORM2, bar.colors = c("Dark2"))

###MDS + 2-D plot (using stemmed text)
(diss <- with(NCNF, Dissimilarity(dia2, list(Person, debate),
                                 method = "euclidean")))

autoplot(cmdscale(diss, eig = TRUE), label = TRUE, label.size = 3)

###Dendrogram w/ pvclust (using stemmed text)
##Note: gives slightly different results each time, so run multiple times) 
wfm.mod <- with(NCNF, wfm(dia2, list(Person, debate)))
fit2 <- suppressMessages(pvclust(wfm.mod, method.hclust="ward",
                                method.dist="euclidean"))

plot(fit2, print.pv = TRUE, print.num = TRUE, float = .022, cex.pv = 1.2, cex = 1.1, cex.main = 1.4)

pvrect(fit2, alpha=.95)

