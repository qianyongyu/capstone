# this module contains the model building routines

library(tm)
library(RWeka)
library(data.table)
# avoids Weka library multi-core issue
options(mc.cores=1)
source("utils.R")

# assuming we're in the directory of the three en_US text files
src <- c('blogs', 'news', 'twitter')

# summary <- data.frame(row.names = c("Number of Lines", "Max Line Length", "Number of Words"))
# for(s in src) {
#   dir.create(file.path(s, "split"), FALSE, recursive = TRUE)
#   fn <- paste("en_US", s, "txt", sep = '.')
#   # open 100 split files for writing
#   files <- lapply(seq(1, 100), function(i) {
#     gzfile(file.path(s, 'split', paste0(i,".gz")), "w")
#   })
#   ctx <- chunked(fn, 10000, splitAndSummarize, list(lines = 0, 
#                                                maxLine = 0, 
#                                                words = 0, 
#                                                fs = files,
#                                                cur_file = 0))
#   # close split files
#   lapply(files, function(f) {
#     close(f)
#   })
#   summary[[s]] <- c(ctx$lines, ctx$maxLine, ctx$words)
# }
# 
# write.table(summary, "summary.tsv", sep = "\t")

# writeNGrams(src, 1:100, 1:5)
# mergeNGrams(src, 1:50, 1:5, "train", list("1" = 0, "2" = 0, "3" = 1, "4" = 1, "5" = 1))
# mergeAll(src, 1:5, 'train')
# mergeNGrams(src, 51:70, 5, "holdout", list("1" = 0, "2" = 0, "3" = 1, "4" = 1, "5" = 1))
# mergeAll(src, 5, 'holdout')

# ds <- calculateDs('holdout')
# save(ds, 'ds')
# dict <- buildDict(5, 'train', ds)
# save(dict, 'dict')
