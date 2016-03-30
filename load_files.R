library(readxl)

# sheet numbers to load
dataset_names <- c(1:29) 

load_dataset <- function(dataset_number) {
    read_excel("../data/all_data.xlsx", sheet = paste("dataset", dataset_number), na = "x")
}

# load all datasets
all_data <- lapply(dataset_names, load_dataset)
all_data[[12]] <-  read_excel("../data/all_data_HW.xlsx", sheet = "dataset 12-2", na = "x")

# extract all_species names
all_species_names <- unlist(lapply(all_data, function(x) out <- names(x)[1]))
all_species_names[12] <- "Crabeater seal recoded"
all_species_names[23] <- "South American (Falklands) sea lion"

# name all datasheets in the list with the appropriate species names
names(all_data) <- all_species_names

extract_genotypes <- function(data_sheet) {
    data_sheet_2 <- data_sheet[3:nrow(data_sheet), 3:ncol(data_sheet)]
    names(data_sheet_2) <- 1:ncol(data_sheet_2)
    data_sheet_2 <- data_sheet_2[rowSums(is.na(data_sheet_2)) != ncol(data_sheet_2), ]
    # row.names(data_sheet_2) <- data_sheet_2[[1]]
    out <- data_sheet_2
}


# just pure genotypes
all_genotypes <- lapply(all_data, extract_genotypes)
names(all_genotypes) <- all_species_names

library(inbreedR)
all_sMLH_var <- as.numeric(lapply(all_genotypes, function(x) out <- var(sMLH(convert_raw(x)), na.rm = TRUE)))
all_g2_vals <- as.numeric(lapply(all_genotypes, function(x) out <- g2_microsats(convert_raw(x))$g2))

plot(all_g2_vals)
text(1:28, all_g2_vals, all_species_names, cex = 0.7, pos = 4)
?convert_raw
