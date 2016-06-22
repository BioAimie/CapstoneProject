setwd('~/Coursera/CapstoneProject/')

# load the libraries that are needed
library(tm)
library(ggplot2)

# read in the data:
en.blogs <- readLines('en_US/en_US.blogs.txt', encoding = 'UTF-8')
en.news <- readLines('en_US/en_US.news.txt', encoding = 'UTF-8')
en.tweets <- readLines('en_US/en_US.twitter.txt', encoding = 'UTF-8')

# find a random sample from each source... 
# Natural language is more likely to be found in complete sentences (in blogs or news), but the length of each line (in words) for news vs. blogs
# is greater in the blogs data set. Therefore, the largest sampling is taken from blogs, the second from news, and the third from tweets. The break
# down is: 10% of blogs, 5% of news, 2% of tweets.
words.blogs <- length(strsplit(paste(en.blogs, collapse = ' '), ' ')[[1]])
words.news <- length(strsplit(paste(en.news, collapse = ' '), ' ')[[1]])
words.tweets <- length(strsplit(paste(en.tweets, collapse = ' '), ' ')[[1]])

set.seed(24)
lines.blogs <- runif(ceiling(0.1*length(en.blogs)), 1, length(en.blogs))
lines.news <- runif(ceiling(0.05*length(en.news)), 1, length(en.news))
lines.tweets <- runif(ceiling(0.02*length(en.tweets)), 1, length(en.tweets))

sample.txt <- c(en.blogs[lines.blogs], en.news[lines.news], en.tweets[lines.tweets])

# sample the words in en.blogs and correct spelling mistakes using Peter Norvig algorithm
sorted_words <- names(sort(table(strsplit(tolower(paste(en.blogs[lines.blogs], collapse = " ")), "[^a-z]+")), decreasing = TRUE))
correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }

# With sample.txt set, do some preliminary cleaning, including replacing special characters, and using the spell checker. Also, remove
# profanity and replace instances with the word 'swearjar' because that's sort of fun and will preserve the tendancy to swear in natural
# langauge without actually suggesting naughty words.
swearJar <- c('cunt','ass','shit','fuck','tit','bitch','whore','slut')
nLines <- length(sample.txt)
sample.clean <- c()
for (i in 1:nLines) { 

    # clean the line and reformat into character vector
    clean.charVec <- tolower(strsplit(sample.txt[i], '')[[1]])[grep(paste(' ',paste(letters, collapse='|'),sep='|'), gsub('\\.|\\?|\\!|\\(|\\)','', tolower(strsplit(en.blogs[i], '')[[1]])))]
    clean.chunk <- strsplit(paste(clean.charVec, collapse=''), ' ')[[1]]
    clean.chunk <- sapply(1:length(clean.chunk), function(x) correct(clean.chunk[x]))
    clean.chunk <- paste(clean.chunk, collapse=' ')
    clean.chunk[grep(paste(swearJar, collapse='|'), clean.chunk)] <- 'swearjar'
    sample.clean <- c(sample.clean, clean.chunk)
}

# Use the tm package to create a corpus using sample.clean, then clean the corpus further by removing punctuation, numbers, whitespace, stopwords,and 
# converting to lower case. Tokenize the corpus and stem complete each vector, then concatenate tokens and convert back to a data frame for the 
# predictive algorithm.
sampleCorpus <- Corpus(VectorSource(sample.clean))
sampleCorpus <- tm_map(sampleCorpus, removePunctuation)
sampleCorpus <- tm_map(sampleCorpus, removeNumbers)
sampleCorpus <- tm_map(sampleCorpus, stripWhitespace)
sampleCorpus <- tm_map(sampleCorpus, removeWords, stopwords('english'))
sampleCorpus <- tm_map(sampleCorpus, content_transformer(tolower))
dict <- sampleCorpus
sampleCorpus <- tm_map(sampleCorpus, stemDocument)
sampleCorpus.token <- lapply(sampleCorpus, scan_tokenizer)
sampleToken.stem <- lapply(sampleCorpus.token, stemCompletion, dict)
sample.df <- data.frame(Text = sapply(sampleToken.stem, paste, collapse=' '), stringsAsFactors = FALSE)


# dataSets <- c('Blogs','News','Twitter')
# 
# size.blogs <- object.size(en.blogs)/1000000
# size.news <- object.size(en.news)/1000000
# size.tweets <- object.size(en.tweets)/1000000
# 
# data.frame(DataSets = dataSets, Size_MB = c(size.blogs, size.news, size.tweets), Lines = c(lines.blogs, lines.news, lines.tweets), Words = c(words.blogs, words.news, words.tweets))
# 
# # 
# nLines <- 1000
# en.blogs <- readLines('en_US/en_US.blogs.txt', n = nLines)
# 
# # sample the words in en.blogs and correct spelling mistakes using Peter Norvig algorithm
# sorted_words <- names(sort(table(strsplit(tolower(paste(en.blogs, collapse = " ")), "[^a-z]+")), decreasing = TRUE))
# correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }
# 
# gram.1.all <- c()
# gram.2.all <- c()
# gram.3.all <- c()
# # THIS TAKES A LONG TIME TO RUN - AVOID RERUNNING IF POSSIBLE!!!
# for (i in 1:nLines) { 
# 
#   # clean the line and reformat into character vector
#   clean.charVec <- tolower(strsplit(en.blogs[i], '')[[1]])[grep(paste(' ',paste(letters, collapse='|'),sep='|'), gsub('\\.|\\?|\\!|\\(|\\)','', tolower(strsplit(en.blogs[i], '')[[1]])))]
#   clean.chunk <- strsplit(paste(clean.charVec, collapse=''), ' ')[[1]]
#   clean.chunk <- sapply(1:length(clean.chunk), function(x) correct(clean.chunk[x]))
#   
#   # perform an n-gram analysis... only keep lines where there are at least 3 words
#   if(length(clean.chunk < 3)) { 
#     
#     next() 
#   } else {
#     
#     gram.1 <- with(data.frame(Combo = clean.chunk, Count = 1), aggregate(Count~Combo, FUN=sum))
#     gram.2 <- with(data.frame(Combo = do.call(rbind, lapply(2:length(clean.chunk), function(x) paste(clean.chunk[(x-1):x], collapse=' '))), Count = 1), aggregate(Count~Combo, FUN=sum))
#     gram.3 <- with(data.frame(Combo = do.call(rbind, lapply(3:length(clean.chunk), function(x) paste(clean.chunk[(x-2):x], collapse=' '))), Count = 1), aggregate(Count~Combo, FUN=sum))
#     gram.1.all <- rbind(gram.1.all, gram.1)
#     gram.2.all <- rbind(gram.2.all, gram.2)
#     gram.3.all <- rbind(gram.3.all, gram.3)
#   }
# }
# 
# # with all the lines bound, aggregate the gram.n.all by each combo and make some summary statistics
# gram.1.agg <- with(gram.1.all, aggregate(Count~Combo, FUN=sum))
# gram.2.agg <- with(gram.2.all, aggregate(Count~Combo, FUN=sum))
# gram.3.agg <- with(gram.3.all, aggregate(Count~Combo, FUN=sum))
# 
# write.csv(gram.1.agg, 'gram1Cheat.csv')
# write.csv(gram.2.agg, 'gram2Cheat.csv')
# write.csv(gram.3.agg, 'gram3Cheat.csv')
# 
# gram.1.agg$Combo <- factor(gram.1.agg$Combo, levels = gram.1.agg[with(gram.1.agg, order(Count, decreasing = TRUE)), 'Combo'])
# gram.2.agg$Combo <- factor(gram.2.agg$Combo, levels = gram.2.agg[with(gram.2.agg, order(Count, decreasing = TRUE)), 'Combo'])
# gram.3.agg$Combo <- factor(gram.3.agg$Combo, levels = gram.3.agg[with(gram.3.agg, order(Count, decreasing = TRUE)), 'Combo'])
# 
# # make a ggplot Pareto for each n-gram that shows the top 100 combos by frequency
# ggplot(gram.1.agg[with(gram.1.agg, order(Count, decreasing = TRUE)), ][1:100, ], aes(x=Combo, y=Count)) + geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=90, hjust=1))
# ggplot(gram.2.agg[with(gram.2.agg, order(Count, decreasing = TRUE)), ][1:100, ], aes(x=Combo, y=Count)) + geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=90, hjust=1))
# ggplot(gram.3.agg[with(gram.3.agg, order(Count, decreasing = TRUE)), ][1:100, ], aes(x=Combo, y=Count)) + geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=90, hjust=1))
# 
# # make a histogram of the frequencies of words in each n-gram (log-y scale)
# plot(gram.1.agg$Count, log='y', type='h', lwd=10, lend=2)
# plot(gram.2.agg$Count, log='y', type='h', lwd=10, lend=2)
# plot(gram.3.agg$Count, log='y', type='h', lwd=10, lend=2)