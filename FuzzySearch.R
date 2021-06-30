#####Keyword search#########
#Read me:  
#This file allows you to custom search terms in CSAT Data
#Please check the file °CSATkeywords.csv° to see the full list of keywords available in the column "Comments". 

#Record running time
sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()
sleep_for_a_minute()

#############Package Installation##################
#You will need to install the packages only the first time you use your R platform (R Studio recommended).
#You can put "#" in front of each line from the second time you use the script to avoid installing again.
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("gridExtra")
#install.packages("tm")
#install.packages("stringr")
#install.packages("tidyverse")
#install.packages(RODBC)
#install.packages("stringi")
#install.packages("naniar")

# Loadlibrary
library(dplyr)
library(tidyr)
library(plyr)
library(lubridate, warn.conflicts = FALSE)
library(gridExtra)
library(tm)
library(stringr)
library(stringi)
library(naniar)


#################### Data Upload ################
##Change the directory as necessary
setwd("C:/Users/darroyob/OneDrive - Intel Corporation/CSAT/")
df <- read.csv("2020CSATAnalysis.csv")
keywords <- read.csv("CSAT Tool Listtest.csv")



#####################Filters#####################
names(df)

## Filter data by date (last year) if needed ##
#df$Intel.Date2 <-(ymd(df$Intel.Date))
#max_date <- max(df$Intel.Date2, na.rm = TRUE)
#min_date <- max_date - years(1)
#dfsubset<- df %>% filter(Intel.Date2 >= min_date, Intel.Date2 <= max_date)
#unique(dfsubset$Intel.Date2)

#Select columns for analysis
dfsubset <- df %>%
  select(Incident.Number, Comments, Survey.Feedback, Sub.Category, Tools.Business.Process)
dfsubset <- dfsubset[!grepl("NA", dfsubset$Incident.Number),]




###########Text Pre-processing##############
dfsubset$Comments <- str_to_lower(dfsubset$Comments)
# Replaces ASCII characters
dfsubset$Comments <- str_replace_all(dfsubset$Comments, "Â", "")
dfsubset$Comments = str_replace_all(dfsubset$Comments, "Â", "")
dfsubset$Comments = str_replace_all(dfsubset$Comments,"â???T", "'")
dfsubset$Comments = str_replace_all(dfsubset$Comments,"â???T", "'")
dfsubset$Comments = str_replace_all(dfsubset$Comments,"â???o", '"')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'â???"', '-')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'â???', '"')

##Eliminates special characters##
dfsubset$Comments = str_replace_all(dfsubset$Comments,'/', '')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'\\', '')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'@', '')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'\n', ' ')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'#', '')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'?', '')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'!', '')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'-', '')
dfsubset$Comments = str_replace_all(dfsubset$Comments,'  ', '')


### Creates a single text with all data for word count###
text<-paste(dfsubset$Comments, collapse =" ")

##Creates a corpus of words##
docs <- Corpus(VectorSource(text))
tospace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))


##NLP Functions##
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove English common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove Selected stop words
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("Intel", "why")) 
# Remove punctuation
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#Creates Word frequency list
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)
d

## Prints wordcount to csv ##
write.csv(d, file = "wordlistCSAT.csv")
remove(d)


###Search n.1: Tool Keywords
##################Keyword Search###################################
## Choose the corpus from the keywords ##
keywords$Tool.Keyword <- str_to_lower(keywords$Tool.Keyword)
toolkeywords <- keywords[!(is.na(keywords$Tool.Keyword) | keywords$Tool.Keyword==""), ]

## Search for words in the desired column of the CSAT Data.  In this case, the comment column is the second (dfsubset[,2])##
dfmatch <- sapply(stri_extract_all_regex(dfsubset[,2], 
                              paste(toolkeywords[,2], collapse = '|')), toString)
dfmatch <- as.data.frame(matrix(unlist(dfmatch),nrow=length(dfmatch),byrow=TRUE))

##Merges results with the original file
df1 <- cbind(df, dfmatch)
df1$V1 <- sapply(df1$V1, function(x) paste(unique(unlist(str_split(x,", "))), collapse = ", "))

##ELiminates repeated keywords
df1$V1 <- sapply(strsplit(as.character(df1$V1), split=","), function(x) {
  paste(unique(trimws(x)), collapse = ', ') } )

## Substitutes for Category names
df1$Tool.Category <- as.character(df1$V1);
for (sri in seq_len(nrow(toolkeywords))) {
  pat <- paste0('\\b',toolkeywords$Tool.Keyword[sri],'\\b');
  lris <- grep(pat,df1$Tool.Category);
  df1$Tool.Category[lris] <- sub(pat,toolkeywords$Category[sri],df1$Tool.Category[lris]);
};

##Eliminates spaces and unused commas
df1$Tool.Category <- trimws(df1$Tool.Category, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
df1$Tool.Category = str_replace_all(df1$Tool.Category,'  ', ' ')
df1$Tool.Category = str_replace_all(df1$Tool.Category,' ,', '')
df1$Tool.Category <- trimws(df1$Tool.Category, whitespace = ",")
df1$Tool.Category <- trimws(df1$Tool.Category, which = c("both", "left", "right"), whitespace = " ")

# Renames columns with keywords
names(df1)[names(df1) == 'V1'] <- 'Tool.Keywords'

###Search n.2: Business Process Keywords
##################Keyword Search###################################
## Choose the corpus from the keywords ##
keywords$Process.Keyword <- str_to_lower(keywords$Process.Keyword)
processkeywords <- keywords[!(is.na(keywords$Process.Keyword) | keywords$Process.Keyword==""), ]

## Search for words in the desired column of the CSAT Data.  In this case, the comment column is the second (dfsubset[,2]), and third column for processkeywords##
dfmatch2 <- sapply(stri_extract_all_regex(dfsubset[,2], 
                                         paste(processkeywords[,3], collapse = '|')), toString)
dfmatch2 <- as.data.frame(matrix(unlist(dfmatch2),nrow=length(dfmatch2),byrow=TRUE))

##Merges results with the original file
df2 <- cbind(df1, dfmatch2)
df2$V1 <- sapply(df2$V1, function(x) paste(unique(unlist(str_split(x,", "))), collapse = ", "))

##ELiminates repeated keywords
df2$V1 <- sapply(strsplit(as.character(df2$V1), split=","), function(x) {
  paste(unique(trimws(x)), collapse = ', ') } )

## Substitutes for Category names
df2$Process.Category <- as.character(df2$V1);
for (sri in seq_len(nrow(processkeywords))) {
  pat <- paste0('\\b',processkeywords$Process.Keyword[sri],'\\b');
  lris <- grep(pat,df2$Process.Category);
  df2$Process.Category[lris] <- sub(pat,processkeywords$Category[sri],df2$Process.Category[lris]);
};

# Renames column with keywords
names(df2)[names(df2) == 'V1'] <- 'Process.Keywords'

##Eliminates spaces and unused commas
df2$Process.Category <- trimws(df2$Process.Category, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
df2$Process.Category = str_replace_all(df2$Process.Category,'  ', ' ')
df2$Process.Category = str_replace_all(df2$Process.Category,' ,', '')
df2$Process.Category <- trimws(df2$Process.Category, whitespace = ",")
df2$Process.Category <- trimws(df2$Process.Category, which = c("both", "left", "right"), whitespace = " ")

####Final Dataframe
csatfinal = subset(df2, select = -c(Process.Keywords,Tool.Keywords, Tools.Business.Process))

### Count Tools and Processes
df3 <- separate_rows(df1, Tool.Category, sep = ",")
df3$Tool.Category <- trimws(df3$Tool.Category, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
toolfreq <- count(df3, 'Tool.Category')
toolfreq <- arrange(toolfreq, desc(freq))
toolfreq

#################Results###########################
## Prints results to csv ##
write.csv(csatfinal, file = "CSATkeywordmatch.csv")

###Prints running time
end_time <- Sys.time()
end_time - start_time

