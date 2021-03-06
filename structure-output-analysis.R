
## script to analyse structure output for 29 seal species

#(1) summarise structure output files with tables / graphs
#(2) add cluster membership 
#(3) create new data.frames with largest cluster
#(4) create new data.frames with single largest (geographical) populations
#(5) put all genotypes into a large list



# summarising output with pophelper package from github

library(devtools)
# load data with original population names ---------------------------------------------------------
library(readxl)
# install.packages("gdata")
library(gdata)
# sheet numbers to load
dataset_names <- excel_sheets("data/all_seals_full_final.xls")


load_dataset <- function(dataset_names) {
    read_excel("data/all_seals_full_final.xls", sheet = dataset_names)
}

# load_dataset <- function(dataset_names) {
#     read.xls("data/all_seals_basic.xls", sheet = dataset_names, stringsAsFactors = FALSE)
# }

# load all datasets
all_seals <- lapply(dataset_names, load_dataset)
names(all_seals) <- dataset_names

# delete anderson walrus
all_seals$atlantic_walrus_andersen <- NULL
dataset_names <- dataset_names[-19]

# process structure results ------------------------------------------------------------------------
library(devtools)
library(stringr)
library(pophelper)
library(magrittr)
# preparation

# folder that contains output from STRUCTURE, whereby each seal species has its own folder
path_to_structure_out <- "data/structure_results/"

# get names for all structure files that end with f
seal_species_names <- names(all_seals)

# create folder for STRUCTURE summary plots
system("mkdir data/cluster_summary_plots")
system("mkdir data/structure_result_tables")

# structure output summary and lnK plots -----------------------------------------------------------
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
    system(paste0("mv evannoMethodStructure.pdf data/cluster_summary_plots/", seal_species_names[i], ".pdf"))
    system(paste0("mv evannoMethodStructure.txt data/structure_result_tables/", seal_species_names[i], ".txt"))

    runs_to_df[[i]] <- runsToDfStructure(struc_out_paths)
    seals_structure_summary[[i]] <- structure_summary
    names(seals_structure_summary)[i] <- seal_species_names[i]
}
names(seals_structure_summary)



system("mkdir data/clumpp")
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
    system(paste0("mkdir data/clumpp/", seal_species_names[i]))
    # copy all pophelper-prepared structure folders into clumpp/species
    system(paste0("mv ", seal_species_names[i], "_K* data/clumpp/", seal_species_names[i]))
    # copy CLUMPP executable into every folder
    system(paste0("for i in data/clumpp/", seal_species_names[i], "/", seal_species_names[i], "_K*; do cp /Users/martin/programs/CLUMPP $i; done"))
    # for computation time reasons remove K > 6
    system(paste0("rm -r data/clumpp/", seal_species_names[i], "/*_K10"))
    system(paste0("rm -r data/clumpp/", seal_species_names[i], "/*_K[7-9]"))
    # execute CLUMPP (the brackets activate a secondary shell)
    # system(paste0('for i in clumpp/', seal_species_names[i], '/*/ ; do (cd "$i" && /Users/martin/program/CLUMPP); done'))
    system(paste0('for i in data/clumpp/', seal_species_names[i], '/*/ ; do (cd "$i" && ./CLUMPP); done'))
} 


# collectCLumppOutput and plot
system("mkdir data/structure_assignment_plots")
for (i in 1:length(seal_species_names)) {
    current_dir <- getwd()
    setwd(paste0("/Users/martin/Dropbox/projects/current/bottleneck/seal_bottleneck_analysis/data/clumpp/",
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
    system(paste0("mkdir /Users/martin/Dropbox/projects/current/bottleneck/seal_bottleneck_analysis/data/structure_assignment_plots/", seal_species_names[i]))
    # move plots to folders
    system(paste0("mv *pdf /Users/martin/Dropbox/projects/current/bottleneck/seal_bottleneck_analysis/data/structure_assignment_plots/", seal_species_names[i]))
    # go back to working directory
    setwd(current_dir)
}
    


# optimal K ---------------------------------------------------------------------------------------
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


# extract_largest_cluster and name clusters by K (e.g. K_10) ---------------------------------------
# create reduced "runs" dataset with just the first of each k run
names(runs_to_df) <- seal_species_names
first_run_each_k <- seq(from = 1, to = 50, by = 5)

extract_and_name_runs <- function(x) {
    runs <- x[first_run_each_k]
    runs[[1]] <- data.frame(cluster = rep(1, nrow(runs[[2]]))) # delete the one cluster matrix due to problems with NaN
    ks <- unlist(lapply(runs, ncol))
    names(runs) <- paste0("K_", ks)
    runs
}
runs_to_df_reduced <- lapply(runs_to_df, extract_and_name_runs)

# create data.frame with optimal K�s 
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

all_clusters <- lapply(seal_species_names, get_cluster_info, runs_to_df_reduced, clusters)
# check seal_species_names[7] --> weird species runs


# add cluster membership to genotypes
names(all_clusters) <- dataset_names
add_cluster_to_geno <- function(species, all_clusters, all_seals){
    out <- cbind(all_seals[[species]][1:2], all_clusters[species], all_seals[[species]][3:ncol(all_seals[[species]])])
}

all_seals_clusters <- lapply(dataset_names, add_cluster_to_geno, all_clusters, all_seals)

## ensure correct column types
all_seals_clusters_final <- lapply(all_seals_clusters, function(x){
                                                            x$id <- as.character(x$id) 
                                                            x
                                                            })
names(all_seals_clusters_final) <- dataset_names

# delete first two loci of crabeater_seal
all_seals_clusters_final$crabeater_seal_recoded <- all_seals_clusters_final$crabeater_seal_recoded[, -c(4:7)]

# write excel file with each dataset plus clusters
library(WriteXLS)
envir <- environment()
list_to_df <- function(species, dfs, envir){
    assign(species, dfs[[species]], envir)
}
lapply(dataset_names, list_to_df, all_seals_clusters_final, envir)
WriteXLS(dataset_names, ExcelFileName = "all_seals_clusters.xls")


# make new dataframes
cluster_df <- function(species, all_seals_clusters_final){
    seal_df <- all_seals_clusters_final[[species]]
    largest_cluster <- unname(which.max(table(seal_df$cluster)))
    seal_df_largest_cluster <- seal_df[seal_df$cluster == largest_cluster , ]
    if (nrow(seal_df) != nrow(seal_df_largest_cluster)) {
        df_name <- paste0(species, "_cl_", largest_cluster)
        return(list(df_name, seal_df_largest_cluster))
    }
}

largest_clusters <- lapply(dataset_names, cluster_df, all_seals_clusters_final)
# names(largest_clusters) <- dataset_names

# get rid of non-cluster populations (NULL anyway) and keep new largest cluster data.frames
clustered_pop <- largest_clusters[!sapply(largest_clusters, is.null)]
names_clusters <- unlist(lapply(clustered_pop, function(x) x[1]))
cluster_geno <- lapply(clustered_pop, function(x) x[[2]])
names(cluster_geno) <- names_clusters

# append to all_seals
all_seals_extended <- append(all_seals_clusters_final, cluster_geno)


# extract largest single population and add to genotypes list -------------------------------------

# make new dataframes
pop_df <- function(species, all_seals_clusters_final){
    seal_df <- all_seals_clusters_final[[species]]
    if (any(seal_df$cluster != 1)) {
        largest_pop <- names(which.max(table(seal_df$pop)))
        seal_df_largest_pop <- seal_df[seal_df$pop == largest_pop, ]
    if (nrow(seal_df) != nrow(seal_df_largest_pop)) {
        df_name <- paste0(species, "_pop")
        return(list(df_name, seal_df_largest_pop))
    }
    }
}

largest_pop <- lapply(dataset_names, pop_df, all_seals_clusters_final)

# get rid of non-cluster populations (NULL anyway) and keep new largest cluster data.frames
largest_pops_for_clustered <- largest_pop[!sapply(largest_pop, is.null)]
names_pops <- unlist(lapply(largest_pops_for_clustered , function(x) x[1]))
largest_pops_geno <- lapply(largest_pops_for_clustered, function(x) x[[2]])
names(largest_pops_geno) <- names_pops 

# append to all_seals_extended
all_seals_clusts_pops <- append(all_seals_extended, largest_pops_geno)
names(all_seals_clusts_pops)

# write excel file with each dataset 
library(WriteXLS)
envir <- environment()
list_to_df <- function(species, dfs, envir){
    assign(species, dfs[[species]], envir)
}
lapply(names(all_seals_clusts_pops), list_to_df, all_seals_clusts_pops, envir)
# WriteXLS(names(all_seals_clusts_pops), ExcelFileName = "seal_data_largest_clust_and_pop.xls")


# extract nes k = 2 and add data.frame to file --> decision for k = 2 due to assignment plot
nes_k2 <- runs_to_df_reduced[["nes"]]$K_2
cluster <- apply(nes_k2, 1, which.max)
table(nes$pop)
nes <- all_seals[["nes"]]
nes <- cbind(nes[1:2], cluster, nes[3:ncol(nes)])
all_seals_clusts_pops$nes_cl_2 <- nes[nes$cluster==2, ]
names(all_seals_clusts_pops)
nes_cl_2 <- all_seals_clusts_pops$nes_cl_2
WriteXLS(names(all_seals_clusts_pops), ExcelFileName = "seal_data_largest_clust_and_pop.xls")


save(all_seals_clusts_pops, file = "seals_full.RData")


