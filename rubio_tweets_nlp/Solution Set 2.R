###################################################
setwd("YOUR DIRECTORY HERE")

#Load in Libraries & Read in Data
install.packages("tm", "readxl", "dplyr", "tidytext", "stringr", "topicmodels", "wordcloud")
library(readxl)
library(dplyr)
library(tidytext)
library(tm)
library(stringr)
library(topicmodels)
library(wordcloud)
#data <- read_excel("GOP Debate.xlsx")
data <- read_excel("ProblemSet2_Data.xlsx")
##Explore Data & Select Candidate For Assignment
##Feel Free to explore polling data (aggregated on realclearpolitics)
#( https://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html#polls )


head(data)
#datakasich<-data %>% filter(candidate== "John Kasich")
#datatrump<-data %>% filter(candidate== "Donald Trump")
datarubio<-data %>% filter(Candidate== "Marco Rubio")

######################################################
##SENTIMENT ANALYSIS
######################################################
# Convert "sentiment" to a binary variable (0 for negative, 1 for positive)
table(data$Sentiment)
data$sentiment_binarypositive <- ifelse(data$Sentiment == "Positive", "Positive", "Negative & Neutral")
data$sentiment_binarynegative <- ifelse(data$Sentiment == "Negative", "Negative", "Positive & Neutral")
table(data$sentiment_binarynegative)
table(data$sentiment_binarypositive)   

##Explore Proportion Differences in Tweet Sentiment For Selected Candidate
#Calculate the proportion of each response OF FULL DEBATE
#response_counts <- table(data$Sentiment)
response_counts <- table(data$Sentiment)
response_proportions <- prop.table(response_counts)
result_table <- data.frame(response = names(response_proportions), 
                  frequency = response_counts, proportion = response_proportions)
print(result_table)

#Calculate the proportion of each response OF JUST Marco Rubio
response_counts <- table(datarubio$Sentiment)
response_proportions <- prop.table(response_counts)
result_table <- data.frame(response = names(response_proportions), frequency = response_counts, proportion = response_proportions)
print(result_table)

### filter for confidence in sentiment
# Filter data for observations with sentiment confidence >= 0.6
filtered_data <- datarubio[datarubio$"Sentiment Confidence" >= 0.6,]

# Generate result table
response_counts <- table(filtered_data$Sentiment)
response_proportions <- prop.table(response_counts)
result_table <- data.frame(response = names(response_proportions), 
                           frequency = response_counts, 
                           proportion = response_proportions)

# Print the result table
print(result_table)

# filter data rubio down to observations where the sentiment confidence score is .4 or higher
datarubio <- filtered_data[filtered_data$"Subject Confidence" >= 0.4, ]


#Consider key topics & sentiment by topic
table(datarubio$Subject, datarubio$Sentiment)
table(datarubio$Sentiment, datarubio$`Retweet Count`)
datarubio$TopicSentiment<- paste(datarubio$Subject, datarubio$Sentiment)
#Incorporate Retweets to adjust for frequency of Topics & Sentiment
Subject_Retweet <- datarubio %>%
  group_by(Subject) %>%
  summarize(total_retweets = sum(`Retweet Count`)) %>%
  arrange(desc(total_retweets))
print(Subject_Retweet)

Sentiment_Retweet <- datarubio %>%
  group_by(Sentiment) %>%
  summarize(total_retweets = sum(`Retweet Count`)) %>%
  arrange(desc(total_retweets))
print(Sentiment_Retweet)

SentimentSubject_Retweet<- datarubio %>%
  group_by(TopicSentiment) %>%
  summarize(total_retweets = sum(`Retweet Count`)) %>%
  arrange(desc(total_retweets))
print(SentimentSubject_Retweet)

#########################################
#####Frequency Analysis for retweets about immigration
#########################################
#word_counts <- data %>%
#  unnest_tokens(word, text) %>%
#  anti_join(stop_words) %>%  # Remove stop words
#  count(word, sort = TRUE)
#test<-head(word_counts, 200)

# Filter data for observations where Subject = "None of the above" 
# Define additional words and numbers to filter out
words_to_filter <- c("rubio","carlyfiorina", "marco", "hilary", "hillary", "carson", "marcorubio", "gopdebate", "gopdebates", "trump", "carly", "fiorina", "christie", "rubios", "kasich", "huckabee", "walker", "cruz", "johnkasich", "christie")

# filter for rubio tweets that are both positive and in the none of the above subject category
filtered_data <- datarubio %>%
  filter(Sentiment == "positive", Subject == "None of the above")

# Tokenize and count words, excluding stop words, specified words, and numbers
word_counts <- datarubio %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% words_to_filter & !grepl("^\\d+$", word)) %>%  # Remove specified words and numbers
  anti_join(stop_words) %>%  
  count(word, sort = TRUE)

# Get the top 100 words
test2 <- head(word_counts, 100)

# Create a word cloud
wordcloud(words = word_counts$word, freq = word_counts$n, scale = c(3, 0.5), 
          colors = brewer.pal(8, "Dark2"))

#########################################
### TOPIC MODELING
table(datarubio$Subject)
#########################################
datarubiofox<-datarubio %>% filter(datarubio$Subject == "Religion")
# Step 1: Create a corpus
corpus <- Corpus(VectorSource(data$text))
# Step 2: Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Step 3: Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)
# Step 4: Convert the document-term matrix to a matrix
mat <- as.matrix(dtm)
##Potentially Could Run PCA here to inform number of groups
# Step 5: Create an LDA model
lda_model <- LDA(mat, k = 4)  # 'k' is the number of topics, adjust as needed
# Step 6: Print the top terms for each topic
terms(lda_model, 6)  # Change '10' to the desired number of top terms per topic


##################################################
#Explore the shift in poll approval of your candidate
#Polling data collected from  Public Policy Polling
table(data$Candidate, data$Poll_9.1.15)
table(data$Candidate, data$Poll_7.22.15)
table(data$Candidate, data$`Shift in Poll`)
