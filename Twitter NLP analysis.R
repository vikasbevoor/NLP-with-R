#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

twitteR:::setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                              "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                              "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                              "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#registerTwitterOAuth(cred)
Tweets <-userTimeline('imVkohli', n=500,includeRts = F)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
head(TweetsDF)

write.csv(TweetsDF, "Tweets2.csv",row.names = F)

getwd()


####################### Emotion mining ##############################
library("syuzhet")

my_example_text <- readLines(file.choose())
s_v <- get_sentences(my_example_text)
class(s_v)
str(s_v)
head(s_v)


sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)

afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)

sentiment_vector
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# To extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

# more depth analysis
poa_v<-my_example_text
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_dct_transform(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)

# subset
sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

# subset
joy_items <- which(nrc_data$joy > 0)
head(s_v[joy_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)
# 3 median=2 mean+mode"TefalHandleTweets.csv")
# 
library(rtweet)


