# this file contains utility functions required for model building

# for each s in src, split the large corpus file en_US.<s>.txt into 100 parts as <s>/split/<1..100>
# and write a summary file as summary.tsv
prepareCorpus <- function(src) {
  summary <- data.frame(row.names = c("Number of Lines", "Max Line Length", "Number of Words"))
  for(s in src) {
    dir.create(file.path(s, "split"), FALSE, recursive = TRUE)
    fn <- paste("en_US", s, "txt", sep = '.')
    # open 100 split files for writing
    files <- lapply(seq(1, 100), function(i) {
      gzfile(file.path(s, 'split', paste0(i,".gz")), "w")
    })
    ctx <- chunked(fn, 10000, splitAndSummarize, list(lines = 0, 
                                                      maxLine = 0, 
                                                      words = 0, 
                                                      fs = files,
                                                      cur_file = 0))
    # close split files
    lapply(files, function(f) {
      close(f)
    })
    summary[[s]] <- c(ctx$lines, ctx$maxLine, ctx$words)
  }
  write.table(summary, "summary.tsv", sep = "\t")
}

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

# for each n in ns and for each s in src, merge all <s>/<n>/<name>.gz files
# by summing the Freq by Term
# and write the result as <name>.<n>.tsv
mergeAll <- function(src, ns, name) {
  for(n in ns) {
    data <- list()
    for(s in src) {
      # data.table doesn't support reading gzipped file directly
      df <- read.table(file.path(s, n, paste0(name, '.gz')), header = TRUE)
      data[[s]] <- data.table(df)
    }
    m <- rbindlist(data)
    m <- m[, list(Freq = sum(Freq)), by=Term]
    # splits Term into words and put them into separate columns
    m[, paste0("w", 1:n) := tstrsplit(Term, " ", fixed = TRUE)][]
    m[, Term:=NULL]
    write.table(m, paste(name, n, 'tsv', sep = '.'), sep = '\t', row.names = FALSE)
    m <- NULL
    data <- NULL
    gc()
  }
}

# calculates the 3 D values used by modified Kneser-Ney smoothing
# this is typically calculated on a holdout corpus instead of the training one
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

# this builds the core data structure for the modified Kneser-Ney smoothing
# it contains a list of data.tables, with the following common columns:
# idx - index of the N-gram within the current N-gram table
# Freq - total frequency of this N-gram (after cutoff)
# total - total frequency of (N+1)-grams starting with this N-gram
# d - pre-calculated gamma value
# when N = 1, the data.table also contains:
# w1 - the actual word
# p - base pKN for the word as the starting point of the recursion
# when N > 1, the data.table also contains:
# parent - the index of the prefix of the N-gram in the (N-1)-gram table
# v - the index of the last word of the N-gram in the 1-gram table
buildDict <- function(maxN, name, ds) {
  # list of N-Grams for N in (1..maxN)
  ngrams <- list()
  # this is a data.table for converting words in the vocabulary into an integer
  # which saves lots of storage
  V <- NULL
  # this is the placeholder for previous N-gram table
  # each N-gram table joins with the previous one to get the index of the prefixes (parents)
  prev <- NULL
  for(n in 1:maxN) {
    d <- fread(paste(name, n, 'tsv', sep = '.'))
    d[,idx := 1:nrow(d)]
    if(n > 1) {
      prefixColumns <- paste0("w", 1:(n - 1))
      
      # lookup prefix idx
      d <- merge(d, prev, by = prefixColumns)
      if(n > 2) {
        # gets rid of the parent's parent and index from the joined table
        d[,c("parent", "v") := NULL]
      }
      # and the parent's frequency as well
      d[,c("Freq.y") := NULL]
      # the parent's index gets stored in the parent column
      setnames(d, c("idx.x", "idx.y", "Freq.x"), c("idx", "parent", "Freq"))
      
      # lookup word idx
      d <- merge(d, V, by.x = paste0("w", n), by.y = "w1")
      setnames(d, c("idx.x", "idx.y", "Freq.x"), c("idx", "v", "Freq"))
      # remove the word's frequency from the joined table
      d[,c("Freq.y") := NULL]

      # we always lookup by parent and then index
      setkey(d, parent, v)
    } else {
      # 1-Gram is always looked up with the word
      setkey(d, "w1")
      V <- d
    }
    prev <- d
    ngrams[[n]] <- d
  }
  for(n in 2:maxN) {
    # now that we've found all parents, it's time to remove the actual words from all (N > 1)-grams.
    ngrams[[n]][, paste0("w", 1:n) := NULL]
  }
  for(n in 2:maxN) {
    # pre-calculate the gamma value for each prefix (parent)
    
    # the denominator is the total Freq of the prefix
    prefix <- ngrams[[n]][, .(total = sum(Freq)), by = "parent"]
    
    # count N-grams with 1, 2, or 3+ Freq for each prefix
    temp <- ngrams[[n]][Freq == 1, .(ones = .N), by = "parent"]
    prefix <- merge(prefix, temp, all.x = TRUE, by = "parent")
    prefix[is.na(ones), ones := 0]
    
    temp <- ngrams[[n]][Freq == 2, .(twos = .N ), by = "parent"]
    prefix <- merge(prefix, temp, all.x = TRUE, by = "parent")
    prefix[is.na(twos), twos := 0]
    
    temp <- ngrams[[n]][Freq > 2, .(threes = .N), by = "parent"]
    prefix <- merge(prefix, temp, all.x = TRUE, by = "parent")
    prefix[is.na(threes), threes := 0]
    
    # multiply with the 3 separate D values and divide by total
    # this is the gamma value of the prefix
    prefix[, d := (ones * ds[1] + twos * ds[2] + threes * ds[3]) / total]
    prefix[,c("ones", "twos", "threes") := NULL]
    
    # store the prefix gamma with the correponding N-grams
    ngrams[[n-1]] <- merge(ngrams[[n-1]], prefix, by.x = "idx", by.y = "parent", all.x = TRUE)
    
    if (n == 2) {
      # calculate the base pKN value for individual words
      n2 <- nrow(ngrams[[2]])
      finishes <- ngrams[[2]][, .(p = .N / n2), by = v]
      
      # and store pKN with 1-Gram
      ngrams[[1]] <- merge(ngrams[[1]], finishes, by.x = "idx", by.y = "v", all.x = TRUE)
      setkey(ngrams[[1]], "w1")
    } else {
      setkey(ngrams[[n-1]], "parent", "v")
    }
  }
  return(ngrams)
}
