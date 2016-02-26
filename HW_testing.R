# Hardy Weinberg
library(pegas)
library("ape")
library("pegas")
library("seqinr")
library("ggplot2")
library("adegenet")
library(stringr)
library(dplyr)

# load cleaned seal data
load("seal_data.RData")

# create genind files where genotypes are stored as "134/140" instead of two column format and save
# them in folder ../data/genind_formatted as tab seperated text files, including id and population--------

# create file with genotypes stored as "134/140" from two-column per locus format
change_geno_format <- function(seal_data_df) {
    loci_names <- names(seal_data_df[3:ncol(seal_data_df)])
    # filter loci names and remove the allel add-on
    loci_names <- str_replace_all(loci_names[seq(from = 1, to = length(loci_names), by = 2)], "_a", "")
    short_geno <- data.frame(matrix(nrow = nrow(seal_data_df), ncol = length(loci_names)))
    names(short_geno) <- loci_names
    
    genotypes <- seal_data_df[, 3:ncol(seal_data_df)]
    length_data <- ncol(genotypes)
    
    col_num <- 1
    for (i in seq(from = 1, to = length_data, by = 2)) {
        short_geno[col_num] <- paste(genotypes[[i]], genotypes[[i+1]], sep = "/")
        col_num <- col_num + 1
    }
    
    short_geno[short_geno == "NA/NA"] <- NA
    geno_out <- cbind(seal_data_df[c(1,2)], short_geno)
    # format pop and ID to be one string
    geno_out$pop <- str_replace_all(as.character(geno_out$pop), ",", "")
    geno_out$pop <- str_replace_all(as.character(geno_out$pop), " ", "_")
    geno_out$id <- str_replace_all(as.character(geno_out$id), " ", "_")
    geno_out
}

# format all seal datasets
seal_data_locus_format <- lapply(seal_data, change_geno_format)

check_na_genotypes <- function(df) {
                                df[3:ncol(df)] <- lapply(df[3:ncol(df)], function(x) {
                                    if (sum(grepl("NA", x))>0) {
                                        x[which(grepl("NA", x))] <- NA
                                    }
                                    x
                                })
                        df
}

# MAIN FORMAT to convert to genind and loci classes
seal_data_locus_format <- lapply(seal_data_locus_format, check_na_genotypes)


# write to txt files
for (i in 1:length(seal_data_locus_format)) {
        write.table(seal_data_locus_format[[i]], file = paste("../data/genind_formatted/", names(seal_data_locus_format[i]), ".txt", sep = ""
                                                        ), sep = " ", quote = FALSE, row.names = FALSE)
}

seal_data_pegas <- list()
for (i in 1:length(seal_data_locus_format)) {
seal_data_pegas[[names(seal_data_locus_format)[i]]] <- read.loci(paste("../data/genind_formatted/", names(seal_data_locus_format)[i], ".txt", sep = ""),
               header = TRUE, loci.sep = " ", allele.sep = "/", col.pop = 2, col.loci = c(3:ncol(seal_data_locus_format[[i]])))
}

save(seal_data_pegas, file = "seal_data_pegas.RData")


# create data.frame with all descriptive variables
sample_size <- as.numeric(lapply(seal_data_pegas, function(x) nrow(x)))
loci_full <- as.numeric(as.numeric(lapply(seal_data_pegas, function(x) ncol(x)-2)))
total_genotypes <- sample_size * loci_all

# Exact test of Hardy-Weinberg equilibrium by Markov chain Monte Carlo----------------
load("seal_data_pegas.RData")

# hardy weinberg tests on everything
all_hw <- lapply(seal_data_pegas, hw.test)
all_non_hw <- lapply(all_hw, function(x) {
                                    x <- as.data.frame(x)
                                    x[which(x["Pr.exact"] < 0.05), ]
                                    })
all_non_hw_number <- unlist(lapply(all_non_hw, nrow))

# Chisqu test of Hardy-Weinberg equilibrium by Markov chain Monte Carlo----------------
all_non_hw_chi <- lapply(all_hw, function(x) {
    x <- as.data.frame(x)
    x[which(x["Pr(chi^2 >)"] < 0.05), ]
})
all_non_hw_number_chi <- unlist(lapply(all_non_hw_chi, nrow))

# number of populations
num_pop <- unlist(lapply(seal_data_locus_format, function(x) out <- length(levels(as.factor(x$pop)))))


seal_data_descr <- data.frame("sample_size" = sample_size,
                              "loci_full" = loci_full ,
                              "total_genotypes" = total_genotypes,
                              "number_populations" = num_pop,
                              "non_hw_exact_test" = all_non_hw_number,
                              "non_hw_chisqu_test" = all_non_hw_number_chi)
                              

write.csv(seal_data_descr, "seal_data_descriptives.csv")


# NULL allele test
library(PopGenReport)
test <- loci2genind(seal_data_pegas[[1]])
seal_data_genind <- lapply(seal_data_pegas, loci2genind) 
?null.all
