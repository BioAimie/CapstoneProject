setwd('~/Coursera/CapstoneProject/')

# load the libraries that are needed
library(tm)
library(ggplot2)

# # ----------------------------- THIS TAKES AN ERNORMOUS AMMOUNT OF TIME TO RUN, SO SAVE THE OUTPUT SO THIS NEVER HAS TO BE DONE AGAIN!!! ----------------------
# # read in the data:
# en.blogs <- readLines('en_US/en_US.blogs.txt', encoding = 'UTF-8')
# en.news <- readLines('en_US/en_US.news.txt', encoding = 'UTF-8')
# en.tweets <- readLines('en_US/en_US.twitter.txt', encoding = 'UTF-8')
# 
# # find a random sample from each source...
# # Natural language is more likely to be found in complete sentences (in blogs or news), but the length of each line (in words) for news vs. blogs
# # is greater in the blogs data set. Therefore, the largest sampling is taken from blogs, the second from news, and the third from tweets. The break
# # down is: 5% of blogs, 3% of news, 1% of tweets.
# words.blogs <- length(strsplit(paste(en.blogs, collapse = ' '), ' ')[[1]])
# words.news <- length(strsplit(paste(en.news, collapse = ' '), ' ')[[1]])
# words.tweets <- length(strsplit(paste(en.tweets, collapse = ' '), ' ')[[1]])
# 
# set.seed(24)
# lines.blogs <- runif(ceiling(0.05*length(en.blogs)), 1, length(en.blogs))
# lines.news <- runif(ceiling(0.35*length(en.news)), 1, length(en.news))
# lines.tweets <- runif(ceiling(0.01*length(en.tweets)), 1, length(en.tweets))
# 
# sample.txt <- c(en.blogs[lines.blogs], en.news[lines.news], en.tweets[lines.tweets])
# 
# # sample the words in en.blogs and correct spelling mistakes using Peter Norvig algorithm
# sorted_words <- names(sort(table(strsplit(tolower(paste(en.blogs[lines.blogs], collapse = " ")), "[^a-z]+")), decreasing = TRUE))
# correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }

# With sample.txt set, do some preliminary cleaning, including replacing special characters, and using the spell checker. Also, remove
# profanity and replace instances with the word 'swearjar' because that's sort of fun and will preserve the tendancy to swear in natural
# langauge without actually suggesting naughty words.
swearJar <- c('cunt','ass','shit','fuck','tit','bitch','whore','slut')
nLines <- length(sample.txt)
sample.clean <- c()
t <- proc.time()
for (i in 1:nLines) {

  print(i) # so I can see the progress
  # clean the line and reformat into character vector, but catch any errors
  possibleErrors <- tryCatch(
    clean.charVec <- tolower(strsplit(sample.txt[i], '')[[1]])[grep(paste(' ',paste(letters, collapse='|'),sep='|'), gsub('\\.|\\?|\\!|\\(|\\)|\\:','', tolower(strsplit(sample.txt[i], '')[[1]])))],
    error = function(e) e
  )
  if(!inherits(possibleErrors, 'error')) {

    clean.charVec <- tolower(strsplit(sample.txt[i], '')[[1]])[grep(paste(' ',paste(letters, collapse='|'),sep='|'), gsub('\\.|\\?|\\!|\\(|\\)|\\:','', tolower(strsplit(sample.txt[i], '')[[1]])))]
    clean.chunk <- strsplit(paste(clean.charVec, collapse=''), ' ')[[1]]
    # spelling takes WAYYYYY TOO LONG!! Using the timer, it takes 0.2s to run 5 lines without spelling and > 49s with spelling, so don't use spell check
    # clean.chunk <- sapply(1:length(clean.chunk), function(x) correct(clean.chunk[x])) # spelling takes WAYYYYY TOO LONG!! Using the timer, it takes 0.2s to run
    clean.chunk <- paste(clean.chunk, collapse=' ')
    clean.chunk[grep(paste(swearJar, collapse='|'), clean.chunk)] <- 'swearjar'
    sample.clean <- c(sample.clean, clean.chunk)
  }
}
proc.time() - t
# write(sample.clean, 'cleanSample.txt', sep = '\n')
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------
sample.clean <- readLines('cleanSample.txt')
# Use the tm package to create a corpus using sample.clean, then clean the corpus further by removing punctuation, numbers, whitespace, stopwords,and 
# converting to lower case. Tokenize the corpus and stem complete each vector, then concatenate tokens and convert back to a data frame for the 
# predictive algorithm.
sampleCorpus <- Corpus(VectorSource(sample.clean))
sampleCorpus <- tm_map(sampleCorpus, removePunctuation)
sampleCorpus <- tm_map(sampleCorpus, removeNumbers)
sampleCorpus <- tm_map(sampleCorpus, stripWhitespace)
sampleCorpus <- tm_map(sampleCorpus, removeWords, stopwords('english'))
sampleCorpus <- tm_map(sampleCorpus, content_transformer(tolower))



sampleCorpus <- tm_map(sampleCorpus, stemDocument)

# # This takes longer than 12 hours, so I stopped the process... I will proceed without completing this I guess...
# dict <- sampleCorpus
# sampleCorpus.token <- lapply(sampleCorpus, scan_tokenizer)
# sampleToken.stem <- lapply(sampleCorpus.token, stemCompletion, dict) #this takes a REALLLLLLY LONG TIME, JUST FORGET IT AND DO IT THE WAY I DID BEFORE FOR FUCKS SAKE!!!!
# sample.df <- data.frame(Text = sapply(sampleToken.stem, paste, collapse=' '), stringsAsFactors = FALSE)

# perform n-gram analysis of the corpus

