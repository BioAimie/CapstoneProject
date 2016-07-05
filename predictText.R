predictText <- function(characterString) {
  
  text <- tolower(strsplit(characterString, ' ')[[1]])
  l <- length(text)
  funWords <- c('homeboy', 'taco', 'snaggletooth', 'sombrero', 'blankie', 'kittens','tequila', 'moose', 'spongebob', 'fireball')
  randomWords <- c(funWords, as.character(one.gram.freq[one.gram.freq$Record > 2000, 'Combo']))
  n <- length(randomWords)
  
  # if the supplied string is greater than or equal to three words in length, use the four-gram data
  if(l >= 3) {
    
    possible <- four.gram.freq[grep(paste('^',paste(text, collapse=' '),'$', sep=''), four.gram.freq$leadGram), ]
    if(length(possible[,1]) > 0) {
      
      index <- which(possible$Record == max(possible$Record))
      
      if(length(index) == 1) {
        
        return(possible[index, 'gram'])
      } else {
        
        index <- index[round(runif(1, 1, length(index)),0)]
        return(possible[index, 'gram'])
      }
    }
    
    # if the string cannot be found in the four-gram data, then look in the three-gram
    if(length(possible[,1]) == 0) {
      
      possible <- three.gram.freq[grep(paste('^',paste(text[2:3], collapse=' '),'$', sep=''), three.gram.freq$leadGram), ]
      if(length(possible[,1]) > 0) {
        
        index <- which(possible$Record == max(possible$Record))
        
        if(length(index) == 1) {
          
          return(possible[index, 'gram'])
        } else {
          
          index <- index[round(runif(1, 1, length(index)),0)]
          return(possible[index, 'gram'])
        }
      } 
      
      # if the string can't be found in the three-gram, then dive into the two-gram
      else {
        
        possible <- two.gram.freq[grep(paste('^',paste(text[3], collapse=' '),'$', sep=''), two.gram.freq$leadGram), ]
        if(length(possible[,1]) > 0) {
          
          index <- which(possible$Record == max(possible$Record))
          
          if(length(index) == 1) {
            
            return(possible[index, 'gram'])
          } else {
            
            index <- index[round(runif(1, 1, length(index)),0)]
            return(possible[index, 'gram'])
          }
        } 
        
        # if the string still cannot be found, generate a random fun word
        else {
          
          return(randomWords[round(runif(1,1,n),0)])
        }
      }
    }
  }
  
  # if the supplied string is two words in length, check the three-gram, then go to the two-gram, and finally make up a fun word if the pattern isn't matched
  if(l == 2) {
    
    possible <- three.gram.freq[grep(paste('^',paste(text, collapse=' '),'$', sep=''), three.gram.freq$leadGram), ]
    if(length(possible[,1]) > 0) {
      
      index <- which(possible$Record == max(possible$Record))
      
      if(length(index) == 1) {
        
        return(possible[index, 'gram'])
      } else {
        
        index <- index[round(runif(1, 1, length(index)),0)]
        return(possible[index, 'gram'])
      }
    } 
    
    # if the string can't be found in the three-gram, then dive into the two-gram
    else {
      
      possible <- two.gram.freq[grep(paste('^',paste(text[2], collapse=' '),'$', sep=''), two.gram.freq$leadGram), ]
      if(length(possible[,1]) > 0) {
        
        index <- which(possible$Record == max(possible$Record))
        
        if(length(index) == 1) {
          
          return(possible[index, 'gram'])
        } else {
          
          index <- index[round(runif(1, 1, length(index)),0)]
          return(possible[index, 'gram'])
        }
      } 
      
      # if the string still cannot be found, generate a random fun word
      else {
        
        return(randomWords[round(runif(1,1,n),0)])
      }
    }
  }
  
  # if the test is only one word long, then use two-gram to find the next word and default to a fun word if the pattern is not matched
  if(l == 1) {
    
    possible <- two.gram.freq[grep(paste('^', paste(text, collapse=' '),'$', sep=''), two.gram.freq$leadGram), ]
    if(length(possible[,1]) == 0 ) {
      
      return(randomWords[round(runif(1,1,n),0)])
    } else {
      
      index <- which(possible$Record == max(possible$Record))
      
      if(length(index) == 1) {
        
        return(possible[index, 'gram'])
      } else {

        index <- index[round(runif(1, 1, length(index)),0)]
        return(possible[index, 'gram'])
      }
    }
  }

  else { stop('This input is not valid. Please type something in to predict text.') }
}