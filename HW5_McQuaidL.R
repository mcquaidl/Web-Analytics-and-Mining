
#professor Joners code to install the packages
## You will need to install these packages if you don't have them. (Run these lines of code)
packages = c('dplyr','rtweet','tidyr', 'tidytext', 'RedditExtractoR')
new.packs = packages[!(packages%in%installed.packages())] 
# installs packages if you don't have them!
if(length(new.packs)>0){
  install.packages(new.packs)
}


#library
library(rtweet)
library(dplyr) # we will be using the dplyr package (install first if necessary)
library(tidyr)
library(ggplot2)
library(SnowballC)
library(tidytext)
library(wordcloud)
library(RedditExtractoR)


#Part 1
#a. pick 2 countries and search for the tweets associated with these two terms
ireland <- search_tweets(           #did not include exact number to be returned
  "ireland", include_rts = FALSE
)
dim(ireland) #check the dimensions on ireland

scotland <- search_tweets(        #did not include exact number to be returned
  "scotland", include_rts = FALSE
)
dim(scotland) #check the dimensions on scotland

##Combining b & c as professor Joner does in the lectures and in the sample code
#b. process each set of tweets into tidy text
#&
#c. use some of the pre-processing transformations described in the lecture
#we are going to remove the http elements off of the text in the tweets (as professor Joner does in lecture  of module 5)
#this processes the tweets into tidy text and cleans our text
#first we load stop words
data("stop_words")

ireland$stripped_text <- gsub("http\\S+","",ireland$text) #remove http
ireland$stripped_text <- gsub("[^\u0020-\u007F]+","",ireland$stripped_text) #remove unicode
ireland$stripped_text <- gsub("'|’","",ireland$stripped_text) #remove apostraphe & angled apostrophe
ireland$stripped_text <- gsub("&lt","",ireland$stripped_text) #remove the commonly occurring sequence &lt
ireland$stripped_text <- gsub("\n","",ireland$stripped_text) #remove the commonly occurring sequence \n
ireland_clean <- ireland %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%    #convert to lowercase, remove punctuation and add id
  anti_join(stop_words)   #remove stop words
#mutate(word = wordStem(word))    #did not choose to stem for this data as "e's" can be dropped and that is not ideal for a country that ends in 'e'

head(ireland_clean)    #check

scotland$stripped_text <- gsub("http\\S+","",scotland$text) #remove http
scotland$stripped_text <- gsub("[^\u0020-\u007F]+","",scotland$stripped_text) #remove unicode
scotland$stripped_text <- gsub("'|’","",scotland$stripped_text) #remove apostraphe & angled apostrophe
scotland$stripped_text <- gsub("&lt","",scotland$stripped_text) #remove the commonly occurring sequence &lt
scotland$stripped_text <- gsub("\n","",scotland$stripped_text) #remove the commonly occurring sequence \n
scotland_clean <- scotland %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%    #convert to lowercase, remove punctuation and add id
  anti_join(stop_words) %>%    #remove stop words
  mutate(word = wordStem(word))    #stem words in stripped text

head(scotland_clean)

#d. get a list of the most frequent terms from each country's tweets
#put the cleaned data through ordering process
ireland_clean %>%
  count(word, sort = TRUE) %>%    #count the frequency of the words
  top_n(10) %>%    #list the top 10, or more if there is a tie
  mutate(word = reorder(word, n))    #reorder the list by descending

scotland_clean %>%
  count(word, sort = TRUE) %>%    #count the frequency of the words
  top_n(10) %>%    #list the top 10, or more based on if there is a tie
  mutate(word = reorder(word, n))    #reorder the list by descending

#e. show the wordcloud for each country
ireland.counts <- ireland_clean %>%   #create variable to store counts
  count(word, sort = TRUE)   #count words

wordcloud(words = ireland.counts$word, freq =ireland.counts$n, min.freq = 2,   #create wordcloud
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

scotland.counts <- scotland_clean %>%   #create variable to store counts
  count(word, sort = TRUE)   #count words

wordcloud(words = scotland.counts$word, freq =scotland.counts$n, min.freq = 2,   #create wordcloud
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#f. show top word pairs (bigrams) for each country
ireland.pairs = ireland %>%   #taking the pairs of the words
  select(stripped_text) %>%   #looking at the preprocessed stripped text
  unnest_tokens(pairs, stripped_text,token = "ngrams", n = 2)   #convert to lowercase, remove punctuation and add id
# ireland.pairs.separate = ireland.pairs %>%   #new variable to split the contents of the column 'pairs'
#   separate(pairs, c("Word1", "Word2"), sep = " ")   #using the separate function, creating new columns
# ireland.pairs.clean <- ireland.pairs.separate %>%   #create new variable to clean out stop words for both columns
#   filter(!Word1 %in% stop_words$word) %>%   #clear out stop words in column 1
#   filter(!Word2 %in% stop_words$word)   #clear out stop words in column 2
ireland.pairs.counts <- ireland.pairs %>%   #create separate variable which as the pairs of words together, and the count
  count(pairs, sort = TRUE)   #count pairs
head(ireland.pairs.counts)   #display top pairs descending


scotland.pairs = scotland %>%   #taking the pairs of the words
  select(stripped_text) %>%   #looking at the preprocessed stripped text
  unnest_tokens(pairs, stripped_text,token = "ngrams", n = 2)   #convert to lowercase, remove punctuation and add id
# scotland.pairs.separate = scotland.pairs %>%   #new variable to split the contents of the column 'pairs'
#   separate(pairs, c("Word1", "Word2"), sep = " ")   #using the separate function, creating new columns
# scotland.pairs.clean <- scotland.pairs.separate %>%   #create new variable to clean out stop words for both columns
#   filter(!Word1 %in% stop_words$word) %>%   #clear out stop words in column 1
#   filter(!Word2 %in% stop_words$word)   #clear out stop words in column 2
scotland.pairs.counts <- scotland.pairs %>%   #create separate variable which as the pairs of words together, and the count
  count(pairs, sort = TRUE)   #count pairs
head(scotland.pairs.counts)   #display top pairs descending

#g. compute the sentiment score for all the tweets for each country
get_sentiments('bing')

bing.ireland = ireland_clean %>%   #create variable to show the top sentiments for ireland
  inner_join(get_sentiments("bing")) %>%   #get sentiments from bing and join those with the words
  count(word, sentiment, sort = TRUE) %>%   #count the number of words
  ungroup()   #ungroup the words, sentiment count and number of times the words occurred
bing.ireland

bing.scotland = scotland_clean %>%   #create variable to show the top sentiments for scotland
  inner_join(get_sentiments("bing")) %>%   #get sentiments from bing and join those with the words
  count(word, sentiment, sort = TRUE) %>%   #count the number of words
  ungroup()   #ungroup the words, sentiment count and number of times the words occurred
bing.scotland


#leverage the code that professor Joner provided for calculating the sentiment score
## Calculate score for each tweet
sentiment_bing = function(twt){
  #Step 1;  perform basic text cleaning (on the tweet), as seen earlier
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      # Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>% 
    unnest_tokens(word,stripped_text) %>% 
    anti_join(stop_words, by="word") %>%  #remove stop words
    inner_join(get_sentiments("bing"), by="word") %>% # merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    ## Create a column "score", that assigns a -1 one to all negative words, and 1 to positive words. 
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment == 'positive'~ n*1)
    )
  ## Calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, # if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  ## This is to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", # Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" # Type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}


ireland.sent <- lapply(ireland$text,function(x){sentiment_bing(x)})   #get sentiment analysis on each tweet
#notice that we are using the tweets that have not been preprocessed, as this is not necessary
#given that the function 'sentiment_bing' does the preprocessing for us
ireland.sent   #displays ~100 tweets

ireland.sentiment <- tibble(
  country = 'ireland',
  score = unlist(map(ireland.sent,'score')),
  type = unlist(map(ireland.sent,'type'))
  
)

ireland.sentiment   #list of ireland sentiment
ireland.sentiment$score   #scores for all of the tweets
sum(ireland.sentiment$score)   #the total score for the ireland tweets (includes type 1 and type 2)
ggplot(ireland.sentiment,aes(x=score)) +   #plotting the ireland sentiment score (includes type 1 and type 2)
  geom_histogram(bins = 15, alpha = .6) + theme_bw()  
ggplot(ireland.sentiment %>% filter(type != "Type 1"),aes(x=score)) +   #plotting the ireland sentiment score DOES NOT include Type 1
  geom_histogram(bins = 15, alpha = .6) + theme_bw()


scotland.sent <- lapply(scotland$text,function(x){sentiment_bing(x)})   #get sentiment analysis on each tweet
#notice that we are using the tweets that have not been preprocessed, as this is not necessary
#given that the function 'sentiment_bing' does the preprocessing for us
scotland.sent   #displays ~100 tweets

scotland.sentiment <- tibble(
  country = 'scotland',
  score = unlist(map(scotland.sent,'score')),
  type = unlist(map(scotland.sent,'type'))
  
)

scotland.sentiment   #list of scotland sentiment
scotland.sentiment$score   #scores for all of the tweets
sum(scotland.sentiment$score)   #the total score for the scotland tweets (includes type 1 and type 2)
ggplot(scotland.sentiment,aes(x=score)) +   #plotting the scotland sentiment score (includes type  and type 2)
  geom_histogram(bins = 15, alpha = .6) + theme_bw()  
ggplot(scotland.sentiment %>% filter(type != "Type 1"),aes(x=score)) +   #plotting the scotland sentiment score DOES NOT include Type 1
  geom_histogram(bins = 15, alpha = .6) + theme_bw()


ireland.sent <- lapply(ireland$text,function(x){sentiment_bing(x)})
scotland.sent <- lapply(scotland$text,function(x){sentiment_bing(x)})


library(purrr)

overall.sentiment = bind_rows(    #creating tibbles for the 2 countries
  tibble(
    country = 'ireland',
    score = unlist(map(ireland.sent,'score')),
    type = unlist(map(ireland.sent,'type'))
  ),
  tibble(
    country = 'scotland',
    score = unlist(map(scotland.sent,'score')),
    type = unlist(map(scotland.sent,'type'))
  )
)
overall.sentiment

ggplot(overall.sentiment,aes(x=score, fill = country)) + geom_histogram(bins = 15, alpha = .6) +   #plotting the countries sentiments with both type 1 & type 2
  facet_grid(~country) + theme_bw()

overall.sentiment %>% group_by(country) %>%  #calculating the score including both type 1 & type 2
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

#plotting the scores excluding type 1
ggplot(overall.sentiment %>% filter(type != "Type 1"),aes(x=score, fill = country)) + geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~country) + theme_bw()

#calculating the score excluding type 1
overall.sentiment %>% filter(type != "Type 1") %>% group_by(country) %>% 
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

#Part 2
#a. Pick two countries and search for the Reddit comments on the Subreddit “World News” for these two countries.
subreddit.topics = 'World News'   #subreddit topics in world news
russia.topics = 'russia'   #setting the search for sri lanka
russia.links = reddit_urls(   #pull the links from reddit for sri lanka
  search_terms = subreddit.topics,
  subreddit = russia.topics,
  sort_by = 'new',
  page_threshold = 3   #load 3 pages
)
russia.links$title#check

## Grab Reddit comments for the first title for russia
russia.url = russia.links$URL[1]

russia.thread = reddit_content(russia.url)
russia.thread  # check that we got content - most recent post may not have content


japan.topics ='japan'   #setting the search for japan
japan.links = reddit_urls(   #pull the links from reddit for Japan
  search_terms = subreddit.topics,
  subreddit = japan.topics,
  sort_by = 'new',
  page_threshold = 3   #load 3 pages
)
japan.links$title  #check

## Grab Reddit comments for the third title for japan
japan.url = japan.links$URL[7]

japan.thread = reddit_content(japan.url)
japan.thread  # check that we got content - most recent post may not have content

##Combining b & c as professor Joner does in the lecture
#b. process each set of comments into a tidy text
#c. use some of the pre-processing transformations described in the lecture
russia.comments =  russia.thread %>% mutate(   #russia
  # Remove http elements manually
  stripped_text = gsub("http\\S+","",comment),
  stripped_text <- gsub("[^\u0020-\u007F]+","",stripped_text), #remove unicode
  stripped_text <- gsub("'|’","",stripped_text), #remove apostraphe & angled apostrophe
  stripped_text <- gsub("&lt","",stripped_text), #remove the commonly occurring sequence &lt
  stripped_text <- gsub("\n","",stripped_text) #remove the commonly occurring sequence \n
) %>% 
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%   #creating the tidy text
  mutate(word = wordStem(word)) %>%   #stemming the words
  anti_join(stop_words)   #removing stop words
russia.comments   #check

japan.comments =  japan.thread %>% mutate(   #japan
  # Remove http elements manually
  stripped_text = gsub("http\\S+","",comment),
  stripped_text <- gsub("[^\u0020-\u007F]+","",stripped_text), #remove unicode
  stripped_text <- gsub("'|’","",stripped_text), #remove apostraphe & angled apostrophe
  stripped_text <- gsub("&lt","",stripped_text), #remove the commonly occurring sequence &lt
  stripped_text <- gsub("\n","",stripped_text) #remove the commonly occurring sequence \n
) %>% 
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%   #creating the tidy text
  mutate(word = wordStem(word)) %>%   #stemming the words
  anti_join(stop_words)   #removing stop words
japan.comments   #check

#d. get a list of the most frequent terms from each country's comments
russia.comments %>%   #creating the ggplot for russia
  count(word, sort = TRUE) %>%   #count and sort
  top_n(10) %>%   #display the top 10
  mutate(word = reorder(word, n)) %>%   #reorder the words
  ggplot(aes(x = word, y = n)) +   #create plot
  geom_col() +   #column labels entered below
  xlab(NULL) +   #x labels entered below
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in Russia's Reddit comments")

russia.comments %>%   #specifically displaying the top 10 words
  count(word, sort = TRUE) %>%   #count and sort
  top_n(10)   #display the top 10

japan.comments %>%   #creating the ggplot for japan
  count(word, sort = TRUE) %>%   #count and sort
  top_n(10) %>%   #display the top 10
  mutate(word = reorder(word, n)) %>%   #reorder the words
  ggplot(aes(x = word, y = n)) +   #create plot
  geom_col() +   #column labels entered below
  xlab(NULL) +   #x labels entered below
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in Japan's Reddit comments")

japan.comments %>%   #specifically displaying the top 10 words
  count(word, sort = TRUE) %>%   #count and sort
  top_n(10)   #display the top 10

#e. show the wordcloud for each country
russia.counts <- russia.comments%>%   #create variable to store counts
  count(word, sort = TRUE)   #count words

wordcloud(words = russia.counts$word, freq =russia.counts$n, min.freq = 2,   #create wordcloud
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

japan.counts <- japan.comments%>%   #create variable to store counts
  count(word, sort = TRUE)   #count words

wordcloud(words = japan.counts$word, freq =japan.counts$n, min.freq = 2,   #create wordcloud
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#f. compute the sentiment score for all the comments for each country
bing.russia = russia.comments %>%   #create variable to show the top sentiments for russia
  inner_join(get_sentiments("bing")) %>%   #get sentiments from bing and join those with the words
  count(word, sentiment, sort = TRUE) %>%   #count the number of words
  ungroup()   #ungroup the words, sentiment count and number of times the words occurred
bing.russia

bing.japan = japan.comments %>%   #create variable to show the top sentiments for japan
  inner_join(get_sentiments("bing")) %>%   #get sentiments from bing and join those with the words
  count(word, sentiment, sort = TRUE) %>%   #count the number of words
  ungroup()   #ungroup the words, sentiment count and number of times the words occurred
bing.japan


#leverage the code that professor Joner provided for calculating the sentiment score
## Calculate score for each comment
sentiment_bing = function(twt){
  #Step 1;  perform basic text cleaning (on the comment), as seen earlier
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      # Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>% 
    unnest_tokens(word,stripped_text) %>% 
    anti_join(stop_words, by="word") %>%  #remove stop words
    inner_join(get_sentiments("bing"), by="word") %>% # merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    ## Create a column "score", that assigns a -1 one to all negative words, and 1 to positive words. 
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment == 'positive'~ n*1)
    )
  ## Calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, # if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  ## This is to keep track of which comments contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", # Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" # Type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}


russia.sent <- lapply(russia.thread$comment,function(x){sentiment_bing(x)})   #get sentiment analysis on each comment
#notice that we are using the comments that have not been preprocessed, as this is not necessary
#given that the function 'sentiment_bing' does the preprocessing for us
russia.sent   #displays ~100 comments

russia.sentiment <- tibble(
  country = 'russia',
  score = unlist(map(russia.sent,'score')),
  type = unlist(map(russia.sent,'type'))
  
)

russia.sentiment   #list of russia sentiment
russia.sentiment$score   #scores for all of the comments
sum(russia.sentiment$score)   #the total score for the russia comments (includes type 1 and type 2)
ggplot(russia.sentiment,aes(x=score)) +   #plotting the russia sentiment score (includes type 1 and type 2)
  geom_histogram(bins = 15, alpha = .6) + theme_bw()  
ggplot(russia.sentiment %>% filter(type != "Type 1"),aes(x=score)) +   #plotting the russia sentiment score DOES NOT include Type 1
  geom_histogram(bins = 15, alpha = .6) + theme_bw()


japan.sent <- lapply(japan.thread$comment,function(x){sentiment_bing(x)})   #get sentiment analysis on each comment
#notice that we are using the comments that have not been preprocessed, as this is not necessary
#given that the function 'sentiment_bing' does the preprocessing for us
japan.sent   #displays ~100 comments

japan.sentiment <- tibble(
  country = 'japan',
  score = unlist(map(japan.sent,'score')),
  type = unlist(map(japan.sent,'type'))
  
)

japan.sentiment   #list of japan sentiment
japan.sentiment$score   #scores for all of the comments
sum(japan.sentiment$score)   #the total score for the japan comments (includes type 1 and type 2)
ggplot(japan.sentiment,aes(x=score)) +   #plotting the japan sentiment score (includes type  and type 2)
  geom_histogram(bins = 15, alpha = .6) + theme_bw()  
ggplot(japan.sentiment %>% filter(type != "Type 1"),aes(x=score)) +   #plotting the japan sentiment score DOES NOT include Type 1
  geom_histogram(bins = 15, alpha = .6) + theme_bw()


russia.sent <- lapply(russia.thread$comment,function(x){sentiment_bing(x)})
japan.sent <- lapply(japan.thread$comment,function(x){sentiment_bing(x)})

overall.sentiment = bind_rows(    #creating tibbles for the 2 countries
  tibble(
    country = 'russia',
    score = unlist(map(russia.sent,'score')),
    type = unlist(map(russia.sent,'type'))
  ),
  tibble(
    country = 'japan',
    score = unlist(map(japan.sent,'score')),
    type = unlist(map(japan.sent,'type'))
  )
)
overall.sentiment

ggplot(overall.sentiment,aes(x=score, fill = country)) + geom_histogram(bins = 15, alpha = .6) +   #plotting the countries sentiments with both type 1 & type 2
  facet_grid(~country) + theme_bw()

overall.sentiment %>% group_by(country) %>%  #calculating the score including both type 1 & type 2
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

#plotting the scores excluding type 1
ggplot(overall.sentiment %>% filter(type != "Type 1"),aes(x=score, fill = country)) + geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~country) + theme_bw()

#calculating the score excluding type 1
overall.sentiment %>% filter(type != "Type 1") %>% group_by(country) %>% 
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

