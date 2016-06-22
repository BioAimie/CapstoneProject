setwd('~/Coursera/CapstoneProject/')

# load the libraries that are needed
library(ggplot2)

# read in the data, but exclude tweets because they are not typical sentence structure
en.blogs <- readLines('en_US/en_US.blogs.txt', encoding = 'UTF-8')
en.news <- readLines('en_US/en_US.news.txt', encoding = 'UTF-8')

# loop through all the lines in these corpora and find the ones with the most words as these are likely to contain the most full sentences
# and are therefore the most useful samples for NLP analysis
line.size.blogs <- sapply(1:length(en.blogs), function(x) length(strsplit(paste(en.blogs[x], collapse=' '), ' ')[[1]]))
line.size.news <- sapply(1:length(en.news), function(x) length(strsplit(paste(en.news[x], collapse=' '), ' ')[[1]]))
lines.blogs <- which(line.size.blogs > quantile(line.size.blogs, probs=0.97))
lines.news <- which(line.size.news > quantile(line.size.news, probs=0.97))
corpus <- c(en.blogs[lines.blogs], en.news[lines.news])

# with the sample corpus, go through each line and break up the sentences.... If there are no sentences, then that is gibberish and should be ignored
punctuation <- c('?','.','!',';')
corpus.df <- c()
for(i in 10798:length(corpus)) {
  
  breaks <- grep(paste(paste('\\', punctuation, sep=''), collapse='|'), strsplit(corpus[i], ' ')[[1]])
  
   if(length(breaks) == 0) {
    
    next()
  } else if(breaks == 0) { 
    
    next() 
  } else {
    
    breaks <- c(0, breaks)
    
    ith.df <- do.call(rbind, lapply(1:(length(breaks)-1) , function(x) data.frame(Sentence = x, Text = paste(lapply(1:(length(breaks)-1), function(y) strsplit(corpus[i],' ')[[1]][(breaks[y]+1):(breaks[(y+1)])])[[x]], collapse=' '))))
    corpus.df <- rbind(corpus.df, ith.df)
  }
}
# write.csv(corpus.df, 'corpusDataFrame.csv') # save the data frame because this takes an f-load of time to run

# with the corpus.df, go through each sentence do some cleaning...
corpus.clean <- corpus.df

  # convert all the text to lower case
corpus.clean[,'Text'] <- sapply(1:length(corpus.df$Sentence), function(x) tolower(as.character(corpus.df[x,'Text']))) 

  # only keep characters that are alphabetical... this removes punctuation and also numbers from the sentences
corpus.clean[,'Text'] <- sapply(1:length(corpus.clean$Sentence), function(x) paste(strsplit(as.character(corpus.clean[x,'Text']),'')[[1]][grep(paste(paste(letters, collapse='|'),' ',sep='|'), strsplit(as.character(corpus.clean[x,'Text']),'')[[1]])], collapse=''))

  # with the clean corpus, count the words in each sentence
corpus.clean[,'WordCount'] <- sapply(1:length(corpus.clean$Sentence), function(x) length(strsplit(as.character(corpus.clean[x,'Text']), ' ')[[1]]))

  # keep only sentences that have more than one word (i.e. remove any entries that were just characters or numbers)
corpus.clean <- corpus.clean[corpus.clean[,'WordCount'] > 0, ]

  # find any swear words and replace them with the word 'swearjar'... this maintains the language structure... 
  # also, clean up an extra whitespaces that were left when numbers and punctuations was removed.
swearJar <- c('cunt','ass','shit','fuck','tit','bitch','whore','slut')
corpus.clean[,'Text'] <- sapply(1:length(corpus.clean$Sentence), function(x) paste(gsub(paste(swearJar, collapse='|'), 'swearjar', strsplit(as.character(corpus.clean[x,'Text']), ' ')[[1]])[grep(paste(letters, collapse='|'), gsub(paste(swearJar, collapse='|'), 'swearjar', strsplit(as.character(corpus.clean[x,'Text']), ' ')[[1]]))], collapse=' '))

  # sweep back through now that everything is clean and get rid of entries that have just white space
corpus.clean[,'WordCount'] <- sapply(1:length(corpus.clean$Sentence), function(x) length(strsplit(as.character(corpus.clean[x,'Text']), ' ')[[1]]))
corpus.clean <- corpus.clean[corpus.clean[,'WordCount'] > 0, ]

# write this nice clean corpus into a .csv file so that work can be picked up from this point at any time in the future
write.csv(corpus.clean, 'cleanCorpus.csv')

# 1-gram
one.gram <- do.call(rbind, lapply(1:length(corpus.clean$Sentence), function(x) data.frame(Combo = sapply(1:length(strsplit(as.character(corpus.clean[x,'Text']), ' ')[[1]]), function(y) strsplit(as.character(corpus.clean[x,'Text']), ' ')[[1]][y]), Record = 1)))

# 2-gram
do.call(rbind, lapply(1:length(corpus.clean[corpus.clean$WordCount >= 2, 'Sentence']), function(x) data.frame(Combo = sapply(2:length(strsplit(as.character(corpus.clean[corpus.clean$WordCount >= 2, ][x, 'Text']), ' ')[[1]]), function(y) paste(strsplit(as.character(corpus.clean[corpus.clean$WordCount >= 2, ][x, 'Text']), ' ')[[1]][(y-1):y], collapse=' ')), Record = 1)))

# 3-gram
do.call(rbind, lapply(1:length(corpus.clean[corpus.clean$WordCount >= 3, 'Sentence']), function(x) data.frame(Combo = sapply(3:length(strsplit(as.character(corpus.clean[corpus.clean$WordCount >= 3, ][x, 'Text']), ' ')[[1]]), function(y) paste(strsplit(as.character(corpus.clean[corpus.clean$WordCount >= 3, ][x, 'Text']), ' ')[[1]][(y-2):y], collapse=' ')), Record = 1)))

# 4-gram
do.call(rbind, lapply(1:length(corpus.clean[corpus.clean$WordCount >= 4, 'Sentence']), function(x) data.frame(Combo = sapply(4:length(strsplit(as.character(corpus.clean[corpus.clean$WordCount >= 4, ][x, 'Text']), ' ')[[1]]), function(y) paste(strsplit(as.character(corpus.clean[corpus.clean$WordCount >= 4, ][x, 'Text']), ' ')[[1]][(y-3):y], collapse=' ')), Record = 1)))
