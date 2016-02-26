# extract files for structure analysis  
library(readxl)

dataset_names <- c(1:22, 51:56) #see overview_data.xslx file in main folder

load_dataset <- function(dataset_number) {
    read_excel("../data/all_data_clean.xlsx", sheet = paste("dataset", dataset_number), na = "x")
}

# load all datasets
all_data <- lapply(dataset_names, load_dataset)
all_data[[12]] <-  read_excel("../data/all_data_clean.xlsx", sheet = "dataset 12-2", na = "x")

# extract all_species names
all_species_names <- unlist(lapply(all_data, function(x) out <- names(x)[1]))
all_species_names[12] <- "Crabeater seal recoded"
all_species_names[23] <- "South American (Falklands) sea lion"

# name all datasheets in the list with the appropriate species names
names(all_data) <- all_species_names

# last 6 datasets for structure analysis
structure_data_all <- all_data[23:28]

library(stringr)
library(magrittr)

# function to create a vector of loci names with _a and _b for the alleles
format_structure <- function(data_sheet)  {
    loc_names <- as.character(data_sheet[1, 3:ncol(data_sheet)]) %>%
                                   str_replace_all(" ", "_") %>%
                                   str_replace_all( "\\." , "") 
#     for (i in seq(from = 2, to = length(loc_names), by = 2)) {
#             loc_names[i] <- paste(loc_names[i-1], "_b", sep = "")
#             loc_names[i-1] <- paste(loc_names[i-1], "_a", sep = "")
#     }
    loc_names <- loc_names[seq(from = 1, to = length(loc_names), by = 2)]
    loc_names
}

# extract the correct rows and assign the locus names
# also changes missing values 
format_files_structure <- function(data_sheet, na_val) {
    loc_names <- format_structure(data_sheet)
    if (any(loc_names == "NA"))  loc_names <- loc_names[-which(loc_names == "NA")]
    
    # delete headers
    data_sheet <- data_sheet[3:nrow(data_sheet), ]
    # delete_NA cols
    data_sheet <- Filter(function(x)!all(is.na(x)), data_sheet)
    # names
    names(data_sheet) <- c("id", "pop", loc_names)
    # delete NA rows
    data_sheet <- data_sheet[rowSums(is.na(data_sheet))!=ncol(data_sheet), ]
    # data_sheet_2 <- data_sheet_2[-which(is.na(data_sheet_2$id)), ] 
    if (any(is.na(data_sheet))) data_sheet[is.na(data_sheet)] <- na_val
    # make pop numeric
    data_sheet$pop <- as.numeric(as.factor(data_sheet$pop))
    
    data_sheet
}

structure_data <- lapply(structure_data_all, format_files_structure, na_val = -9)

names(structure_data)
for (i in 1:length(structure_data)) {
    write.table(structure_data[[i]], paste(names(structure_data)[[i]], ".txt", sep = ""), 
                quote = FALSE, sep = "\t", row.names = FALSE)
}

#### TO DO THE OUTCOME FILES: DELETE ID,POP, AND NA´s IN THE HEADER #####
