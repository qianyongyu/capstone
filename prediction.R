# this module contains text prediction and evaluation routines
library(data.table)

# # load the 3 D values in the algorithm
# load('ds')
# # load all the n-grams and pre-calculated data
# load('dict')
# # pre-discount the frequency of each n-gram using the D values
# for(n in 2:5) {
#   dict[[n]][,Freq:=ifelse(Freq == 1, Freq - ds[1],
#                           ifelse(Freq == 2, Freq - ds[2], Freq - ds[3]))]
# }
# # pick the most popular words as candidates for prediction
# common <- dict[[1]][Freq > 100]$w1
# # some initial guess for first word in sentence
# init <- copy(dict[[1]][common])
# init[,c("p", "d", "total") := NULL]
# setnames(init, "Freq", "p")

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
  m <- dict[[1]][m]
  if(profile) {
    print("Word lookup")
    print(proc.time() - start)
  }
  m[,c("Freq", "total", "d") := NULL]
  setnames(m, "idx", "v")
  n <- length(history)
  for(i in n:1) {
    ngramStart <- proc.time()
    prefix <- lookupNGram(history[i:n])
    if(profile) {
      print(paste0("Looking up prefix '", paste(history[i:n], collapse = " "), "'"))
      print(proc.time() - ngramStart)
    }
    if(!is.na(prefix$d)) {
      mergeStart <- proc.time()
      m[, paste0("p", n - i + 1) := p]
      m[, p := p * prefix$d]
      m <- merge(m, dict[[n - i + 2]][.(prefix$idx)], by = c("v"), all.x = TRUE)
      m[, p := p + ifelse(is.na(Freq), 0, Freq / prefix$total)]
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

predicted <- function(word, m, required, prefix, detail) {
  predictions <- m[order(-p)][1:required,]$w1
  result <- 'Miss'
  if(word %in% predictions) {
    result <- 'Hit'
  }
  if(detail) {
    cat(paste(word, '------>', paste0(predictions, collapse = ', '), "------>", result, '\n'))
  }
  if(result != 'Hit' && prefix > 0) {
    for(i in 1:min(prefix, nchar(word))) {
      predictions <- m[startsWith(w1, substr(word, 1, i))][order(-p)][1:required,]$w1
      if(word %in% predictions) {
        result <- 'Hit'
      }
      if(detail) {
        cat(paste(paste0('(', substr(word, 1, i), ')', substr(word, i + 1, nchar(word))), '------>', paste0(predictions, collapse = ', '), "------>", result, '\n'))
      }
      if(result == 'Hit') {
        break
      }
    }
  }
  return(result == 'Hit')
}

predictSentence <- function(sentence, required = 3, prefix = 2, detail = FALSE) {
  m <- init
  cleaned = gsub("[^a-z']+", ' ', tolower(sentence))
  words <- strsplit(cleaned, ' ')[[1]]
  hits <- 0
  misses <- 0
  history <- c()
  maxN <- length(dict) - 1
  for(word in words) {
    if(predicted(word, m, required, prefix, detail)) {
      hits <- hits + 1
    } else {
      misses <- misses + 1
    }
    history <- append(history, word)
    len <- length(history)
    if(len > maxN) {
      history <- history[(len - maxN + 1):len]
    }
    m <- pKNm(common, history)
  }
  cat(paste("\nHits:", hits, "Misses:", misses, "Precision:", round(hits / (hits + misses), 3)))
}