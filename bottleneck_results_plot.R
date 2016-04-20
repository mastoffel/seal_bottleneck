# plotting bottleneck

library(data.table)  # faster fread() and better weekdays()
library(dplyr)       # consistent data.frame operations
library(purrr)       # consistent & safe list/vector munging
library(tidyr)       # consistent data.frame cleaning
library(lubridate)   # date manipulation
library(ggplot2)     # base plots are for Coursera professors
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)       # kable : prettier data.frame output
library(stringr)
# load data with original population names ---------------------------------------------------------
library(readxl)
# sheet numbers to load
bottleneck_out <- read_excel("data/out_bottleneck_stats.xls")
bottleneck_out_hw <- read_excel("data/out_bottleneck_stats_HW.xls")

bottleneck_out <- rbind(bottleneck_out, bottleneck_out_hw)
names(bottleneck_out)[1] <- "id"
# extract pure names

bottleneck_out$id <- sapply(strsplit(bottleneck_out$id, "_genepop"), `[[`, 1)

# numeric
charcols <- str_detect(names(bottleneck_out), "Def.Exc") | str_detect(names(bottleneck_out), "id")
bottleneck_out[!charcols] <- lapply(bottleneck_out[!charcols], as.numeric)

# split up column with number of loci in het excess
str(bottleneck_out)
exc_cols <- str_detect(names(bottleneck_out), "Def.Exc")

split_up <- function(x){
    df <- apply(data.frame(str_split_fixed(x, "vs", 2)), 2, as.numeric)
    out <- as.data.frame(df)
}

sep_cols <- do.call(cbind, lapply(bottleneck_out[exc_cols], split_up))
names(sep_cols) <- str_replace(names(sep_cols), "X1", "het_def")
names(sep_cols) <- str_replace(names(sep_cols), "X2", "het_exc")
names(sep_cols) <- str_replace(names(sep_cols), "Def.Exc.", "")

# add to original data.frame
bottleneck <- cbind(bottleneck_out[!exc_cols], sep_cols)

# add factor for full / pop / cl
bottleneck$dataset[str_detect(bottleneck$id, "_pop")] <- "pop"
bottleneck$dataset[str_detect(bottleneck$id, "_cl")] <- "cl"
bottleneck$dataset[is.na(bottleneck$dataset)] <- "full"

bottle_tests <- bottleneck %>%
                    mutate(IAM_het_exc_ratio = IAM_Heq / IAM_het_def) %>%
                    mutate(TPM70_het_exc_ratio = TPM70_Heq / TPM70_het_def) %>%
                    mutate(TPM95_het_exc_ratio = TPM95_Heq / TPM95_het_def) %>%
                    mutate(SMM_het_exc_ratio = SMM_Heq / SMM_het_def) 
# extract sign tests for all models

sign_tests <- str_detect(names(bottleneck_out), "Sign")

bottle_tests <- bottleneck_out[, sign_tests]
bottle_tests$id <- bottleneck_out$id

library(reshape2)
library(ggplot2)
library(dplyr)
bot <- melt(bottle_tests, id.vars = "id")
bot$value <- as.numeric(bot$value)
# bot$dataset <- NA
# str_detect(bot$id, "_pop")
# bot$dataset[str_detect(bot$id, "_pop")] <- "pop"
# bot$dataset[str_detect(bot$id, "_cl")] <- "cl"
# bot$dataset[is.na(bot$dataset)] <- "full"
# bot$dataset <- as.factor(bot$dataset)

ggplot(bot, aes(x= variable, y = id, fill = value)) + 
            #facet_grid(.~dataset) + 
            geom_tile(color = "white", size = 0.1) +
            scale_fill_viridis(name = "p-value", label = comma) +
            theme_tufte(base_family="Helvetica") +
            #coord_equal() +
            theme(plot.title=element_text(hjust=0),
                  axis.ticks=element_blank(),
                  axis.text=element_text(size=10),
                  legend.title=element_text(size=10),
                  legend.text=element_text(size=9)) 
             # scale_fill_gradientn(colors =  breaks=c(0.01, 0.05, 0.1, 1))
   
            #coord_equal()


