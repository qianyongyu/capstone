# this module contains text prediction and evaluation routines
library(data.table)

# load the 3 D values in the algorithm
load('ds')
# load all the n-grams and pre-calculated data
load('dict')
# pre-discount the frequency of each n-gram using the D values
for(n in 2:5) {
  dict[[n]][,Freq:=ifelse(Freq == 1, Freq - ds[1],
                          ifelse(Freq == 2, Freq - ds[2], Freq - ds[3]))]
}
# pick the most popular words as candidates for prediction
common <- dict[[1]][Freq > 100]$w1
# some initial guess for first word in sentence
init <- copy(dict[[1]][common])
init[,c("p", "d", "total") := NULL]
setnames(init, "Freq", "p")

# this looks up the ngram data using the dictionary
lookupNGram <- function(words) {
  for(i in 1:length(words)) {
    # translate to word index
    ng <- dict[[1]][words[i]]
    vv <- ng$idx
    if(i == 1) {
      # use first word's index as parent
      p <- vv
    } else {
      # use parent + index to lookup ngram
      ng <- dict[[i]][.(p, vv)]
      # use the new ngram's index as parent
      p <- ng$idx
    }
  }
  return(ng)
}

# use modified Kneser-Ney smoothing to calculate the probability of candidate words given a history
pKNm <- function(candidates, history, profile = FALSE) {
  m <- data.table(w1 = candidates)
  start <- proc.time()
  
  # joins with the 1-gram table to get base pKN
  m <- dict[[1]][m]
  if(profile) {
    print("Word lookup")
    print(proc.time() - start)
  }
  m[,c("Freq", "total", "d") := NULL]
  setnames(m, "idx", "v")
  n <- length(history)
  
  # recursively calculate the pKN using increasingly long prefixes
  for(i in n:1) {
    ngramStart <- proc.time()
    
    # find the prefix
    prefix <- lookupNGram(history[i:n])
    if(profile) {
      print(paste0("Looking up prefix '", paste(history[i:n], collapse = " "), "'"))
      print(proc.time() - ngramStart)
    }
    if(!is.na(prefix$d)) {
      # prefix found
      mergeStart <- proc.time()
      
      # store the previous pKN value for reference
      m[, paste0("p", n - i + 1) := p]
      
      # multiply by (pre-calculated) prefix gamma
      m[, p := p * prefix$d]
      
      # join with N-gram table to get adjusted N-gram frequency
      m <- merge(m, dict[[n - i + 2]][.(prefix$idx)], by = c("v"), all.x = TRUE)
      
      # for all N-grams with prefix, add the modified ML value
      m[, p := p + ifelse(is.na(Freq), 0, Freq / prefix$total)]
      
      # some cleanup
      m[, c("parent", "idx", "Freq") := NULL]
      if(n - i + 2 < 5) {
        m[, c("total", "d") := NULL]
      }
      if(profile) {
        print(paste("Merging with n -", n - i + 1))
        print(proc.time() - mergeStart)
      }
    } else {
      # no more prefix is possible, so we can stop here 
      break
    }
  }
  if(profile) {
    print('Total Time')
    print(proc.time() - start)
  }
  return(m)
}

# given a word and a prediction m
# consisting of words and pKN probabilities
# returns whether the word is successfully predicted by m
# when *required* number of words are allowed on each guess
# and up to *prefix* number of letters of the word are known
# the result is filled into the *ctx* object
checkPrediction <- function(word, m, required, prefix, ctx) {
  predictions <- m[order(-p)][1:required,]$w1
  result <- 'Miss'
  if(word %in% predictions) {
    result <- 'Hit'
  }
  ctx$detail <- rbind(ctx$detail, data.frame(Word = word, 
                                       Predictions = paste0(predictions, collapse = ', '), 
                                       Result = result))
  if(result != 'Hit' && prefix > 0) {
    # for the first (up to) prefix letters
    for(i in 1:min(prefix, nchar(word))) {
      
      # find top words with the prefix
      predictions <- m[startsWith(w1, substr(word, 1, i))][order(-p)][1:required,]$w1
      if(word %in% predictions) {
        result <- 'Hit'
      }
      ctx$detail <- rbind(ctx$detail, data.frame(Word = paste0('(', substr(word, 1, i), ')', substr(word, i + 1, nchar(word))), 
                                           Predictions = paste0(predictions, collapse = ', '), 
                                           Result = result))
      if(result == 'Hit') {
        # not need to go on
        break
      }
    }
  }
  if(result == 'Hit') {
    ctx$hits <- ctx$hits + 1
  } else{
    ctx$misses <- ctx$misses + 1
  }
  return(ctx)
}

# given a sentence, try to predict each word in sequence based on the previous words
predictSentence <- function(sentence, required = 3, prefix = 2, maxN = 4) {
  # start with base single word frequency
  m <- init
  cleaned = gsub("[^a-z']+", ' ', tolower(sentence))
  words <- strsplit(cleaned, ' ')[[1]]
  history <- c()
  ctx <- list(detail = data.frame(),
                 hits = 0,
                 misses = 0)
  for(word in words) {
    # check if the prediction is correct
    ctx <- checkPrediction(word, m, required, prefix, ctx)
    
    # truncate history 
    history <- append(history, word)
    len <- length(history)
    if(len > maxN) {
      history <- history[(len - maxN + 1):len]
    }
    
    # predict the next word
    m <- pKNm(common, history)
  }
  
  return(ctx)
}