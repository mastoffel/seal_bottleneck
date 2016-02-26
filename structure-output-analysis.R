# summarising output with pophelper package from github
load("seal_bottleneck_data_simple.RData")
library(devtools)
library(stringr)
library(pophelper)
library(magrittr)
# preparation

# folder that contains output from STRUCTURE, whereby each seal species has its own folder
path_to_structure_out <- "structure_results/"

# get names for all structure files that end with f
seal_species_names <- names(all_seals)

# create folder for STRUCTURE summary plots
system("mkdir cluster_summary_plots")

seals_structure_summary <- list()
for (i in 1:length(seal_species_names)) {
    # lists all files in structure output folder 
    all_files <- list.files(paste0(path_to_structure_out, seal_species_names[i]))
    # lists relevant files
    struc_out_names <- all_files[str_detect(all_files, "results_job_T.+f")]
    # creates character vector containing path to relevant files
    struc_out_paths <- str_c(paste0(path_to_structure_out, seal_species_names[i], "/"), struc_out_names)
    
    # pophelper summary functions
    structure_summary <- tabulateRunsStructure(files=struc_out_paths) %>%
        summariseRunsStructure() %>%
        evannoMethodStructure(exportplot = T, imgtype = "pdf")
    # move plot into plot folder and rename it
    system(paste0("mv evannoMethodStructure.pdf cluster_summary_plots/", seal_species_names[i], ".pdf"))
    
    seals_structure_summary[[i]] <- structure_summary
    names(seals_structure_summary[i]) <- seal_species_names[i]
}


# rule: if mean estimated ln probability of data is highest at K=1, decide for K=1
#       if not, use evanno-method (delta K) to decide which k

rtd <- runsToDfStructure(files = struc_out_paths)

clumppExportStructure(files = struc_out_paths)

