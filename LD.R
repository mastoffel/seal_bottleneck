# linkage disequilibrium
library(readxl)

# sheet numbers to load
dataset_names <- excel_sheets("data/seal_data_largest_clust_and_pop_all_hw.xlsx")

load_dataset <- function(dataset_names) {
    read_excel("data/seal_data_largest_clust_and_pop.xlsx", sheet = dataset_names)
}
# load all datasets
all_seals <- lapply(dataset_names, load_dataset)
names(all_seals) <- dataset_names


library(genetics)

transform_genotypes <- function(x){
    ind_geno <- c(4:ncol(x))
    name_pairs <- sapply(ind_geno, function(y) if(even(y)) out <- names(x)[y:(y+1)])
    name_pairs <- name_pairs[!sapply(name_pairs, is.null)]
    out <- makeGenotypes(x, name_pairs)  
    out[, 4:ncol(out)] <- lapply(out[, 4:ncol(out)], as.genotype)
    new <- LD(out[, 4:5])
}


# try with strataG
library(strataG)
test <- all_seals[[1]]
msats <- new("gtypes", gen.data = test[4:ncol(test)], ind.names = test$id, ploidy = 2)
# using modified version of LDgenepop
ld <- LDgenepop_mod(msats, exec = "/Users/martin/programs/Genepop", show.output = TRUE)
