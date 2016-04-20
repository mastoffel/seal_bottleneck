# 
library(readxl)
library(strataG)

# sheet numbers to load
dataset_names <- excel_sheets("data/seal_data_largest_clust_and_pop_all_hw.xlsx")

load_dataset <- function(dataset_names) {
    read_excel("data/seal_data_largest_clust_and_pop_all_hw.xlsx", sheet = dataset_names)
}
# load all datasets
all_seals <- lapply(dataset_names, load_dataset)
names(all_seals) <- dataset_names

afs <- all_seals[[1]]
msats <- new("gtypes", gen.data = afs[4:ncol(afs)], ind.names = afs$id, ploidy = 2)
mRatio(msats)



data(dolph.msats)
data(dolph.strata)
strata.schemes <- dolph.strata[, c("broad", "fine")]
rownames(strata.schemes) <- dolph.strata$id


msats.g <- new("gtypes", gen.data = dolph.msats[, -1], ploidy = 2,
    ind.names = dolph.msats[, 1], schemes = strata.schemes)

library(strataG)
browseVignettes("strataG")

mRatio()