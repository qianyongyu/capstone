# this file contains utility functions

# reads the file fn in chunks of size lines, and pass each chunk to the callback
# function cb with a persistent ctx context.
chunked <- function(fn, size, cb, ctx) {
  f <- file(fn, 'r')
  buf <- readLines(f, size)
  while(length(buf) > 0) {
    ctx <- cb(buf, ctx)
    if(length(buf) == size) {
      buf <- readLines(f, size)
    } else {
      break
    }
  }
  close(f)
  return(ctx)
}

# lowercases input and removes all characters except a-z and those considered to be 
# delimiters by Weka (so that NGramTokenizer can remove them later)
textClean <- function(txt) {
  return(gsub('[^a-z \r\n\t.,;:\'\"()?!]+', '', tolower(txt)))
}

# write the lines in a chunk evenly into a vector of file connections
# ctx$fs, while keeping track of number of lines in ctx$lines,
# max line length in ctx$maxLine and number of words in ctx$words
splitAndSummarize <- function(buf, ctx) {
  buf <- sapply(buf, textClean)
  lens <- sapply(buf, nchar, USE.NAMES = FALSE)
  words <- sapply(buf, function(x) {
    return(length(strsplit(x, ' ')[[1]]))
  }, USE.NAMES = FALSE)
  curMax <- max(lens)
  if(curMax > ctx$maxLine) {
    ctx$maxLine <- curMax
  }
  ctx$lines <- ctx$lines + length(buf)
  ctx$words <- ctx$words + sum(words)
  
  num_files = length(ctx$fs)
  for(line in buf) {
    writeLines(line, ctx$fs[[ctx$cur_file + 1]])
    ctx$cur_file <- ((ctx$cur_file + 1) %% num_files)
  }
  return(ctx)
}

# for each s in src, for each part in <s>/split/<part>, for each n in ns
# writes n-grams as Term/Freq tsv to <s>/<n>/<part>
writeNGrams <- function(src, parts, ns) {
  for(s in src) {
    for(part in parts) {
      ds <- DirSource(dir = file.path(s, "split"), pattern = paste0('^', part, '\\.gz$'))
      c <- VCorpus(ds)
      for(n in ns) {
        tdm <- TermDocumentMatrix(c, 
            control = list(wordLengths = c(1, Inf), 
                tokenize = function(x) NGramTokenizer(x, 
                            Weka_control(min = n, max = n, delimiters = ' \r\n\t.,;:"()?!'))))
        dir.create(file.path(s, n), FALSE)
        wf <- gzfile(file.path(s, n, paste0(part, ".gz")))
        tf <- as.matrix(tdm)
        write.table(tf, wf, sep = '\t')
      }
      c <- NULL
      gc()
    }
  }
}

# for each s in src, for each n in ns, merge all parts under <s>/<n>/<parts>.gz
# sum Freq by Term, filter out Terms with Freq <= cutoff[[n]],
# and write the result to <s>/<n>/<name>.gz
mergeNGrams <- function(src, parts, ns, name, cutoff) {
  for(s in src) {
    for(n in ns) {
      data <- list()
      for(part in parts) {
        df <- read.table(file.path(s, n, paste0(part, '.gz')), skip = 1, col.names = c("Term", "Freq"), colClasses = c("character", "integer"))
        data[[part]] <- data.table(df)
      }
      m <- rbindlist(data)
      m <- m[, list(Freq = sum(Freq)), by=Term]
      m <- m[Freq > cutoff[[n]]]
      wf <- gzfile(file.path(s, n, paste0(name, '.gz')))
      write.table(m, wf, sep = '\t', row.names = FALSE)
      m <- NULL
      data <- NULL
      gc()
    }
  }
}

# for each n in ns, merge all <src>/<n>/<name>.gz files
# and write the result as <name>.<n>.tsv
mergeAll <- function(src, ns, name) {
  for(n in ns) {
    data <- list()
    for(s in src) {
      df <- read.table(file.path(s, n, paste0(name, '.gz')), header = TRUE)
      data[[s]] <- data.table(df)
    }
    m <- rbindlist(data)
    m <- m[, list(Freq = sum(Freq)), by=Term]
    m[, paste0("w", 1:n) := tstrsplit(Term, " ", fixed = TRUE)][]
    m[, Term:=NULL]
    write.table(m, paste(name, n, 'tsv', sep = '.'), sep = '\t', row.names = FALSE)
    m <- NULL
    data <- NULL
    gc()
  }
}

calculateDs <- function(name) {
  n1 <- nrow(fread(paste(name, 1, 'tsv', sep = '.')))
  n2 <- nrow(fread(paste(name, 2, 'tsv', sep = '.')))
  n3 <- nrow(fread(paste(name, 3, 'tsv', sep = '.')))
  n4 <- nrow(fread(paste(name, 4, 'tsv', sep = '.')))
  y <- n1 / (n1 + n2 * 2)
  d1 <- 1 - 2 * y * n2 / n1
  d2 <- 2 - 3 * y * n3 / n2
  d3 <- 3 - 4 * y * n4 / n3
  return(c(d1, d2, d3))
}

buildDict <- function(maxN, name, ds) {
  ngrams <- list()
  prefixes <- list()
  V <- NULL
  prev <- NULL
  for(n in 1:maxN) {
    d <- fread(paste(name, n, 'tsv', sep = '.'))
    d[,idx := 1:nrow(d)]
    if(n > 1) {
      prefixColumns <- paste0("w", 1:(n - 1))
      
      # lookup prefix idx
      d <- merge(d, prev, by = prefixColumns)
      if(n > 2) {
        d[,c("parent", "v") := NULL]
      }
      d[,c("Freq.y") := NULL]
      setnames(d, c("idx.x", "idx.y", "Freq.x"), c("idx", "parent", "Freq"))
      
      # lookup word idx
      d <- merge(d, V, by.x = paste0("w", n), by.y = "w1")
      setnames(d, c("idx.x", "idx.y", "Freq.x"), c("idx", "v", "Freq"))
      d[,c("Freq.y") := NULL]

      setkey(d, parent, v)
    } else {
      setkey(d, "w1")
      V <- d
    }
    prev <- d
    ngrams[[n]] <- d
  }
  for(n in 2:maxN) {
    ngrams[[n]][, paste0("w", 1:n) := NULL]
  }
  for(n in 2:maxN) {
    prefix <- ngrams[[n]][, .(total = sum(Freq)), by = "parent"]
    
    temp <- ngrams[[n]][Freq == 1, .(ones = .N), by = "parent"]
    prefix <- merge(prefix, temp, all.x = TRUE, by = "parent")
    prefix[is.na(ones), ones := 0]
    
    temp <- ngrams[[n]][Freq == 2, .(twos = .N ), by = "parent"]
    prefix <- merge(prefix, temp, all.x = TRUE, by = "parent")
    prefix[is.na(twos), twos := 0]
    
    temp <- ngrams[[n]][Freq > 2, .(threes = .N), by = "parent"]
    prefix <- merge(prefix, temp, all.x = TRUE, by = "parent")
    prefix[is.na(threes), threes := 0]
    
    prefix[, d := (ones * ds[1] + twos * ds[2] + threes * ds[3]) / total]
    prefix[,c("ones", "twos", "threes") := NULL]
    ngrams[[n-1]] <- merge(ngrams[[n-1]], prefix, by.x = "idx", by.y = "parent", all.x = TRUE)
    
    if (n == 2) {
      n2 <- nrow(ngrams[[2]])
      finishes <- ngrams[[2]][, .(p = .N / n2), by = v]
      ngrams[[1]] <- merge(ngrams[[1]], finishes, by.x = "idx", by.y = "v", all.x = TRUE)
      setkey(ngrams[[1]], "w1")
    } else {
      setkey(ngrams[[n-1]], "parent", "v")
    }
  }
  return(ngrams)
}

predictWord <- function(word, ctx, required = 3) {
  if(is.null(word)) {
    return(list(w1 = NULL, w2 = NULL, w3 = NULL, w4 = NULL, predictions = list(
      a = 1, the = 1, and = 1, of = 1, to = 1)))
  }
  predictions <- list()
  ctx$w1 <- ctx$w2
  ctx$w2 <- ctx$w3
  ctx$w3 <- ctx$w4
  ctx$w4 <- word
  if(!is.null(ctx$w1)) {
    candiates <- dict[[5]][.(ctx$w1, ctx$w2, ctx$w3, ctx$w4), .(w5, Freq)][order(-Freq)]
    if(!is.na(candiates$w5[1])) {
      for(t in candiates$w5) {
        predictions[[t]] <- 4
        if(length(predictions) >= required) {
          break
        }
      }
    }
  }
  if(length(predictions) < required && !is.null(ctx$w2)) {
    candiates <- dict[[4]][.(ctx$w2, ctx$w3, ctx$w4), .(w4, Freq)][order(-Freq)]
    if(!is.na(candiates$w4[1])) {
      for(t in candiates$w5) {
        predictions[[t]] <- 4
        if(length(predictions) >= required) {
          break
        }
      }
    }
  }
  if (length(predictions) < required && !is.null(ctx$w3)) {
    candiates <- dict[[3]][.(ctx$w3, ctx$w4), .(w3, Freq)][order(-Freq)]
    if(!is.na(candiates$w3[1])) {
      for(t in candiates$w3) {
        predictions[[t]] <- 3
        if(length(predictions) >= required) {
          break
        }
      }
    }
  }
  if (length(predictions) < required) {
    candiates <- dict[[2]][.(ctx$w4), .(w2, Freq)][order(-Freq)]
    if(!is.na(candiates$w2[1])) {
      for(t in candiates$w2) {
        predictions[[t]] <- 2
        if(length(predictions) >= required) {
          break
        }
      }
    }
  }
  ctx$predictions <- predictions
  return(ctx)
}
