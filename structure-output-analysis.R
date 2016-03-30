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
system("mkdir structure_result_tables")

seals_structure_summary <- list()
runs_to_df <- list()
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
        evannoMethodStructure(exportplot = T, imgtype = "pdf", writetable = TRUE)
    # move plot into plot folder and rename it
    system(paste0("mv evannoMethodStructure.pdf cluster_summary_plots/", seal_species_names[i], ".pdf"))
    system(paste0("mv evannoMethodStructure.txt structure_result_tables/", seal_species_names[i], ".txt"))

    runs_to_df[[i]] <- runsToDfStructure(struc_out_paths)
    seals_structure_summary[[i]] <- structure_summary
    names(seals_structure_summary)[i] <- seal_species_names[i]
}
names(seals_structure_summary)



system("mkdir clumpp")
for (i in 1:length(seal_species_names)) {
    # lists all files in structure output folder 
    all_files <- list.files(paste0(path_to_structure_out, seal_species_names[i]))
    # lists relevant files
    struc_out_names <- all_files[str_detect(all_files, "results_job_T.+f")]
    # creates character vector containing path to relevant files
    struc_out_paths <- str_c(paste0(path_to_structure_out, seal_species_names[i], "/"), struc_out_names)
    # basic usage
    clumppExportStructure(files=struc_out_paths, prefix = seal_species_names[i])
    # create species directory in clumpp
    system(paste0("mkdir clumpp/", seal_species_names[i]))
    # copy all pophelper-prepared structure folders into clumpp/species
    system(paste0("mv ", seal_species_names[i], "_K* clumpp/", seal_species_names[i]))
    # copy CLUMPP executable into every folder
    system(paste0("for i in clumpp/", seal_species_names[i], "/", seal_species_names[i], "_K*; do cp /Users/martin/programs/CLUMPP $i; done"))
    # for computation time reasons remove K > 6
    system(paste0("rm -r clumpp/", seal_species_names[i], "/*_K10"))
    system(paste0("rm -r clumpp/", seal_species_names[i], "/*_K[7-9]"))
    # execute CLUMPP (the brackets activate a secondary shell)
    # system(paste0('for i in clumpp/', seal_species_names[i], '/*/ ; do (cd "$i" && /Users/martin/program/CLUMPP); done'))
    system(paste0('for i in clumpp/', seal_species_names[i], '/*/ ; do (cd "$i" && ./CLUMPP); done'))
} 


# collectCLumppOutput and plot
system("mkdir structure_assignment_plots")
for (i in 1:length(seal_species_names)) {
    current_dir <- getwd()
    setwd(paste0("/Users/martin/Dropbox/projects/current/bottleneck/seal_bottleneck_analysis/clumpp/",
                 seal_species_names[i]))
    # function to summarize CLUMPP output
    collectClumppOutput(prefix = seal_species_names[i], filetype = "aligned")
    # make list of all files
    slist <- list.files(paste0(seal_species_names[i], "-aligned/"))
    # create paths to all files
    path_to_slist <- str_c(paste0(seal_species_names[i], "-aligned/"), slist)
    # plot pdfs
    plotRuns(files=path_to_slist, imgoutput = "tab", imgtype = "pdf", sortind = "all")
    # create folder for pdfs
    system(paste0("mkdir ../../structure_assignment_plots/", seal_species_names[i]))
    # move plots to folders
    system(paste0("mv *pdf ../../structure_assignment_plots/", seal_species_names[i]))
    # go back to working directory
    setwd(current_dir)
}
    
    
# rule: if mean estimated ln probability of data is highest at K=1, decide for K=1
#       if not, use evanno-method (delta K) to decide which k

optimal_k <- function(x) {
    elpd <- which.max(x$elpdmean)
    if (elpd == 1) {
        return(1)
    } else {
    delta_k <- which.max(x$deltaK)
    delta_k
    }
}

clustering <- lapply(seals_structure_summary, optimal_k)
names(clustering) <- names(all_seals)
best_clustering <- unlist(clustering)

# output best k's
both_k <- function(x) {
    elpd <- which.max(x$elpdmean)
    deltaK <- which.max(x$deltaK)
    ks <- c(lnk = x$k[elpd], deltak = x$k[deltaK])
}

ks <- do.call(rbind, lapply(seals_structure_summary, both_k))
# write.table(ks, "best_k.txt", sep = "\t")


# extract_largest_cluster and name clusters by K (e.g. K_10)
names(runs_to_df) <- seal_species_names
first_run_each_k <- seq(from = 1, to = 50, by = 5)

extract_and_name_runs <- function(x) {
    runs <- x[first_run_each_k]
    ks <- unlist(lapply(runs, ncol))
    names(runs) <- paste0("K_", ks)
    runs
}
runs_to_df_reduced <- lapply(runs_to_df, extract_and_name_runs)

# create data.frame with optimal K´s 
clusters <- as.data.frame(best_clustering) 
names(runs_to_df_reduced)

get_cluster_info <- function(species, runs_to_df_reduced, clusters) {
    species_runs <- runs_to_df_reduced[[species]]
    num_clust <- clusters[species, ]
    if (num_clust > 1) {
        best_clust <- species_runs[[paste0("K_", num_clust)]]
        cluster <- apply(best_clust, 1, which.max)
        assign_to_cluster <- data.frame(cluster)
    } else {
        cluster <- rep(1, nrow(species_runs[[1]]))
        assign_to_cluster <- data.frame(cluster)
    }
    assign_to_cluster
}

lapply(seal_species_names[1:6], get_cluster_info, runs_to_df_reduced, clusters)
# check seal_species_names[7] --> weird species runs

which(cluster > 1)
names(runs_to_df_simple)
