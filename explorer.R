setwd('~/Coursera/CapstoneProject/')

en.blogs <- readLines('en_US/en_US.blogs.txt', n = 1000)

a <- en.blogs[1]
b <- tolower(strsplit(a, '')[[1]])[grep(paste(' ',paste(letters, collapse='|'),sep='|'), gsub('\\.|\\?|\\!|\\(|\\)','', tolower(strsplit(a, '')[[1]])))]
c <- strsplit(paste(b, collapse=''), ' ')[[1]]
d <- data.frame(Words = c, Record = 1)

hist(with(d, aggregate(Record~Words, FUN=sum))$Record)