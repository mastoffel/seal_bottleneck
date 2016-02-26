# all genotypes
load("all_seal_data.Rdata")

# check if all have id and pop column
lapply(lapply(all_seal_data, function(x) names(x) %in% c("id", "pop")), sum)
# number of markers
lapply(all_seal_data, function(x) (dim(x)[2]-2)/2)
# number of individuals
lapply(all_seal_data, function(x) (dim(x)[1]))

# format all as numeric
turn_into_num <- function(x){
    x[, 3:ncol(x)] <- as.data.frame(apply( x[, 3:ncol(x)], 2, as.numeric))
    x
}
all_seals <- lapply(all_seal_data, turn_into_num)
load("seal_bottleneck_data.RData")

for (i in 1:28){
    assign(names(all_seals[i]), all_seals[[i]])
}
names(all_seals)[23] <- "south_american_sea_lion"


# change all ids --> get rid of empty spaces and exchange weird characters with _
## nice function --> save for later use
library(stringr)
simplify_names <- function(df){
    # replaces all non-alphanumeric characters including empty
    df$id <- str_replace_all(df$id, "[^[:alnum:]]", "_")
    df$id <- tolower(df$id)
    df
}
all_seals_new <- lapply(all_seals, simplify_names)

# check for duplicates 
lapply(all_seals_new, function(x) sum(duplicated(x$id)))
#

# simplify all IDs by just given them numbers
all_seals_simple_id <- lapply(all_seals, function(x){
                                        x$id <- c(1:nrow(x))
                                        return(x)
})  

save(all_seals_simple_id, file = "seal_bottleneck_data.RData")
all_seals <- all_seals_simple_id

#check if all populations are factors
lapply(all_seals, function(x) (x$pop))

all_seals_simple_pop <- lapply(all_seals, function(x) {
                        x$pop <- as.numeric(x$pop)
                        x
})

all_seals <- all_seals_simple_pop
save(all_seals, file = "seal_bottleneck_data_simple.RData")

for (i in 1:28){
    assign(names(all_seals[i]), all_seals[[i]])
}
library(WriteXLS)
WriteXLS(x = names(all_seals), ExcelFileName = "all_seals_simple.xls", SheetNames = names(all_seals))
