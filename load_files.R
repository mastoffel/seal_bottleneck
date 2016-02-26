
# load and clean --------------
library(readxl)
library(stringr)
dataset_names <- c(1:22, 51:56) #see overview_data.xslx file in main folder

load_dataset <- function(dataset_number) {
    read_excel("../data/all_data.xlsx", sheet = paste("dataset", dataset_number), na = "x")
}

# load all datasets
all_data <- lapply(dataset_names, load_dataset)
all_data[[12]] <-  read_excel("../data/all_data.xlsx", sheet = "dataset 12-2", na = "x")

# extract all_species names
all_species_names <- unlist(lapply(all_data, function(x) out <- names(x)[1]))
all_species_names[12] <- "Crabeater seal recoded"
all_species_names[23] <- "South American (Falklands) sea lion"

# change names of datasets to avoid spaces, brackets and other weird symbols
all_species_names <- str_replace_all(all_species_names, "dataset", "")
all_species_names <- str_replace_all(all_species_names, "data", "")
all_species_names <- str_replace_all(all_species_names, "  ", "_")
all_species_names <- str_trim(all_species_names)
all_species_names <- str_replace_all(all_species_names, " ", "_")
all_species_names <- str_replace_all(all_species_names, "'", "")
all_species_names<- str_replace_all(all_species_names, "\\(", "")
all_species_names <- str_replace_all(all_species_names, "\\)", "")

# name all datasheets in the list with the appropriate species names
names(all_data) <- all_species_names

# clean sheets --------------------------------------------------------------------------
# function to create a vector of loci names with _a and _b for the alleles
format_names <- function(data_sheet)  {
    # delete_NA cols
    data_sheet <- Filter(function(x)!all(is.na(x)), data_sheet)
    # clean names
    loc_names <- as.character(data_sheet[1, 3:ncol(data_sheet)]) %>%
        str_replace_all(" ", "_") %>%
        str_replace_all( "\\." , "") 
         for (i in seq(from = 2, to = length(loc_names), by = 2)) {
                 loc_names[i] <- paste(loc_names[i-1], "_b", sep = "")
                 loc_names[i-1] <- paste(loc_names[i-1], "_a", sep = "")
         }
    # loc_names <- loc_names[seq(from = 1, to = length(loc_names), by = 2)]
    loc_names
}

all_names <- lapply(all_data, format_names)

# extract the correct rows and assign the locus names
format_geno <- function(data_sheet) {
    loc_names <- format_names(data_sheet)
    if (any(loc_names == "NA"))  {
        loc_names <- loc_names[-which((loc_names == "NA_a") | (loc_names == "NA_b") | (loc_names == "NA"))]
    }
    # delete headers
    data_sheet <- data_sheet[3:nrow(data_sheet), ]
    # delete_NA cols
    data_sheet <- Filter(function(x)!all(is.na(x)), data_sheet)
    # names
    names(data_sheet) <- c("id", "pop", loc_names)
    # delete NA rows
    data_sheet <- data_sheet[rowSums(is.na(data_sheet))!=ncol(data_sheet), ]
    # make pop numeric
    # data_sheet$pop <- as.numeric(as.factor(data_sheet$pop))
    data_sheet
}

library(dplyr)

# format everything
seal_data <- lapply(all_data, format_geno)

# tbl_df is a better data.frame
seal_data <- lapply(seal_data, tbl_df)

# change pop to factor
seal_data <- lapply(seal_data, function(x) {
                                x$pop <- as.factor(x$pop)
                                return(x)})

# how many locations? --not necessarily right, to check
number_of_populations <- (lapply(seal_data, function(x) length(levels(x$pop))))
names(number_of_populations) <- names(seal_data)


# correction
seal_data[[3]][469, 3] <- NA
seal_data[[3]][469, 4] <- NA

# save
save(seal_data, file = "seal_data.RData")








