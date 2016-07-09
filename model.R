# this module contains the model building routines
# actual functions used are in utils.R in order to keep this file concise

library(tm)
library(RWeka)
library(data.table)
# avoids Weka library multi-core issue
options(mc.cores=1)
source("utils.R")

# assuming we're in the directory of the three en_US text files
src <- c('blogs', 'news', 'twitter')

# summarize and split the corpus into 100 parts
prepareCorpus(src);

# parse each split into 1-5 grams
writeNGrams(src, 1:100, 1:5)

# merge the first 50 splits of n-gram frequencies using a 0-0-1-1-1 cutoff
mergeNGrams(src, 1:50, 1:5, "train", list("1" = 0, "2" = 0, "3" = 1, "4" = 1, "5" = 1))

# merge across sources for the training set
mergeAll(src, 1:5, 'train')

# merge the another 20 splits of n-gram frequencies using a 0-0-1-1-1 cutoff
mergeNGrams(src, 51:70, 1:5, "holdout", list("1" = 0, "2" = 0, "3" = 1, "4" = 1, "5" = 1))

# and this will be the holdout set
mergeAll(src, 1:5, 'holdout')

# the 3 D values are calculated on the holdout set
ds <- calculateDs('holdout')
save(ds, file = 'ds')

# and the model is built on the training set
dict <- buildDict(5, 'train', ds)
save(dict, file = 'dict')
