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

# one.gram <- do.call(rbind, lapply(1:length(corpus.clean$Sentence), function(x) data.frame(Combo = sapply(1:length(strsplit(as.character(corpus.clean[x,'Text']), ' ')[[1]]), function(y) strsplit(as.character(corpus.clean[x,'Text']), ' ')[[1]][y]), Record = 1)))

# because the corpus is enormous and it takes over 40 hours to run two.gram on it (without completing...), the corpus must be trimmed so that it has a smaller number of entries
# think this through in terms of the predictive algorithm...
# for the algorithm, I think that 4-gram should be highest-rated, then 3-gram, then 2-gram, then 1-gram should be used if the other grams do not suggest a next word
  # if there are 3 words or more words, then 4 gram
  # if there are 2 words, then 3 gram
  # if there is only 1 word, then 2 gram
  # if there are zero words, do not suggest a next word (let the user first input a word)
  # if the algorithm cannot guess a word based on grams, then suggest a word based on frequency of one-grams (throw in a randomizer so that sometimes it selects a funny word)
  #   for example, if the random number between 0 and 1 is between certain ranges then make an assigned random guess
# based on this algorithm, any sentences with fewer than 4 words can be eliminated from the set
# also, using 
  # https://strainindex.wordpress.com/2008/07/28/the-average-sentence-length/ (17 average, 21 fairly difficult)
  # http://countwordsworth.com/blog/what-is-a-good-average-sentence-length/ (15-20 words rule)
  # http://www.aje.com/en/arc/editing-tip-sentence-length/ (12-17 scientific, 20 or fewer words as general rule... Harry Potter books have an average of 12 words)
# this can be futher refined... based on this, let's see how we can trim the set
summary(corpus.clean$WordCount) # indicates that ~25% have less than 10 words, and ~25% have 25 or more words
sd(corpus.clean$WordCount) # there is a large standard deviation
# since 10-25 is still 50% of the data, and the set is so large, let's further trim to between 12 and 20 as 20 is recommended as the max
percent.twleve.twenty <- length(corpus.clean[corpus.clean$WordCount >= 12 & corpus.clean$WordCount <= 20, 'Sentence'])/length(corpus.clean$Sentence)
corpus.clean.trim <- corpus.clean[corpus.clean$WordCount >= 12 & corpus.clean$WordCount <= 20, ]

# 1-gram
one.gram.alt <- do.call(rbind, lapply(1:length(corpus.clean.trim$Sentence), function(x) data.frame(Combo = sapply(1:length(strsplit(as.character(corpus.clean.trim[x,'Text']), ' ')[[1]]), function(y) strsplit(as.character(corpus.clean.trim[x,'Text']), ' ')[[1]][y]), Record = 1)))
one.gram.alt.freq <- with(one.gram.alt, aggregate(Record~Combo, FUN=sum))
one.gram.alt.freq <- one.gram.alt.freq[with(one.gram.alt.freq, order(Record, decreasing = TRUE)), ]

# 2-gram
two.gram <- do.call(rbind, lapply(1:length(corpus.clean.trim[corpus.clean.trim$WordCount >= 2, 'Sentence']), function(x) data.frame(Combo = sapply(2:length(strsplit(as.character(corpus.clean.trim[corpus.clean.trim$WordCount >= 2, ][x, 'Text']), ' ')[[1]]), function(y) paste(strsplit(as.character(corpus.clean.trim[corpus.clean.trim$WordCount >= 2, ][x, 'Text']), ' ')[[1]][(y-1):y], collapse=' ')), Record = 1)))
two.gram.freq <- with(two.gram, aggregate(Record~Combo, FUN=sum))
two.gram.freq <- two.gram.freq[with(two.gram.freq, order(Record, decreasing = TRUE)), ]
  
# 3-gram
three.gram <- do.call(rbind, lapply(1:length(corpus.clean.trim[corpus.clean.trim$WordCount >= 3, 'Sentence']), function(x) data.frame(Combo = sapply(3:length(strsplit(as.character(corpus.clean.trim[corpus.clean.trim$WordCount >= 3, ][x, 'Text']), ' ')[[1]]), function(y) paste(strsplit(as.character(corpus.clean.trim[corpus.clean.trim$WordCount >= 3, ][x, 'Text']), ' ')[[1]][(y-2):y], collapse=' ')), Record = 1)))
three.gram.freq <- with(three.gram, aggregate(Record~Combo, FUN=sum))
three.gram.freq <- three.gram.freq[with(three.gram.freq, order(Record, decreasing = TRUE)), ]

# 4-gram
four.gram <- do.call(rbind, lapply(1:length(corpus.clean.trim[corpus.clean.trim$WordCount >= 4, 'Sentence']), function(x) data.frame(Combo = sapply(4:length(strsplit(as.character(corpus.clean.trim[corpus.clean.trim$WordCount >= 4, ][x, 'Text']), ' ')[[1]]), function(y) paste(strsplit(as.character(corpus.clean.trim[corpus.clean.trim$WordCount >= 4, ][x, 'Text']), ' ')[[1]][(y-3):y], collapse=' ')), Record = 1)))
four.gram.freq <- with(four.gram, aggregate(Record~Combo, FUN=sum))
four.gram.freq <- four.gram.freq[with(four.gram.freq, order(Record, decreasing = TRUE)), ]

# create a new column called leadGram that has the lead in for the n-gram
two.gram.freq[,'leadGram'] <- sapply(1:length(two.gram.freq[,'Record']), function(x) strsplit(as.character(two.gram.freq[x,'Combo']),' ')[[1]][1])
two.gram.freq[,'gram'] <- sapply(1:length(two.gram.freq[,'Record']), function(x) strsplit(as.character(two.gram.freq[x,'Combo']), ' ')[[1]][2])
three.gram.freq[,'leadGram'] <- sapply(1:length(three.gram.freq[,'Record']), function(x) paste(strsplit(as.character(three.gram.freq[x,'Combo']),' ')[[1]][1:2], collapse=' '))
three.gram.freq[,'gram'] <- sapply(1:length(three.gram.freq[,'Record']), function(x) strsplit(as.character(three.gram.freq[x,'Combo']),' ')[[1]][3])
four.gram.freq[,'leadGram'] <- sapply(1:length(four.gram.freq[,'Record']), function(x) paste(strsplit(as.character(four.gram.freq[x,'Combo']),' ')[[1]][1:3], collapse=' '))
four.gram.freq[,'gram'] <- sapply(1:length(four.gram.freq[,'Record']), function(x) strsplit(as.character(four.gram.freq[x,'Combo']),' ')[[1]][4])

# trim it down....
four.gram.trim <- four.gram.freq[four.gram.freq$Record > 1, c('leadGram','gram','Record')]
three.gram.trim <- three.gram.freq[three.gram.freq$Record > 1, c('leadGram','gram','Record')]
two.gram.trim <- two.gram.freq[two.gram.freq$Record > 1, c('leadGram','gram','Record')]
one.gram.trim <- one.gram.freq[one.gram.freq$Record > 100, c('Combo','Record')]
write.csv(one.gram.trim, 'oneGramTrim.csv')

