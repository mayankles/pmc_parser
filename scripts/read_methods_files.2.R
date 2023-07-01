rm(list=ls())

setwd("~/Desktop/{{Documents}}/my_tools/PMC_parser")
data_dir="data"
library(data.table)
library(stringr)
library(stringi)
library(parallel)


# system.time(list.files(data_dir, pattern = "methods.txt", recursive = T, full.names = T))
# system.time(system(paste0("ls -f -R ",data_dir,"/*/methods.txt"), intern = TRUE))
# meth_files <- list.files(data_dir, pattern = "methods.txt", recursive = T, full.names = T)

meth_files <- system(paste0("ls -f -R ",data_dir,"/*/methods.txt"), intern = TRUE)

methods_filelist_file <- "methods_files.txt"
system(paste0("find ",data_dir," -name \"methods.txt\" > ", methods_filelist_file))
meth_files <- fread(methods_filelist_file)[[1]]




greplist <- list("R" =" R programming| R version",
                 "CRAN" =" CRAN",
                 "Bioconductor" =" Bioconductor| bioconductor",
                 "Python2" = " Python.{0,9}2",
                 "Python3" = " Python.{0,9}3",
                 "Python" = " Python "
                 )

# meth_text <- lapply(meth_files, function(meth_file) {
#   currmeth <- read.table(meth_file, sep="\t")
#   currmeth[,3]
# })
# sum(is.na(meth_text))
# meth_text[[which(!is.na(meth_text))[100]]]

library(data.table)
combined_meth_data <- rbindlist(lapply(meth_files, fread))
colnames(combined_meth_data) <- c("PMCID","year","methods_str")

grep_results <- lapply(combined_meth_data$methods_str, function(method_str) {
  # currmeth <- read.table(meth_file, sep="\t")
  match_results <- unlist(lapply(greplist, grepl, method_str))
  # if (length(match_results) < 1) {
  #   browser()
  # }
  # returnval <- data.frame(pmc=currmeth[,1],
  #                         year=currmeth[,2],
  #                         isNA=is.na(currmeth[,3]),
  #                         t(match_results)
  #                         )
  return(match_results)
})

# grep_results <- as.list(rep(NA, length(meth_files)))
# names(grep_results) <- meth_files
# for (meth_file in meth_files) {
#   currmeth <- read.table(meth_file, sep="\t",allowEscapes=T)
#   grep_results[[meth_file]] <- unlist(lapply(greplist, grepl, currmeth[,3]))
# }

# this <- do.call(rbind, grep_results)
# colSums(this)


library(dplyr)
match_data <- cbind(combined_meth_data[, 1:2],
                    do.call(rbind, grep_results)
)

saveRDS(match_data, file="match_data.Rds")

grep_names=names(greplist)
yearly_counts <- match_data %>% group_by(year) %>% summarise_at(sum, .vars=grep_names)
yearly_totals <- match_data %>% group_by(year) %>% summarise(n_papers=n())

papers_scale=1000
yearly_counts[,names(greplist)] <- yearly_counts[,names(greplist)]/(yearly_totals$n_papers/papers_scale)

# which(rowSums(is.na(match_data[,..grep_names])) !=0 )

yearly_counts <- yearly_counts[yearly_counts$year > 1995, ]

library(ggplot2)
library(reshape2)
plotdata <- reshape2::melt(yearly_counts, id.vars="year", value.name="count",variable.name="Match")

grepcolors <- c("R" ="chartreuse",
                 "CRAN" =" chartreuse3",
                 "Bioconductor" ="forestgreen",
                 "Python2" = "gold4",
                 "Python3" = "gold2",
                 "Python" = "gold"
)

n_pmcs <- length(meth_files)
# n_valid <- sum(!match_data$isNA)
# title_text <- paste0("Articles parsed: ", n_pmcs,"\nMethods found: ",n_valid)
title_text <- paste0("Articles parsed: ", n_pmcs)
y_label_text <- paste0("Usage (per 1000 papers)")

myplot <- ggplot(plotdata) + 
  # geom_col(aes(x=year, y=count, fill=Match))
  geom_point(aes(x=year, y=count, color=Match)) +
  geom_line(aes(x=year, y=count, color=Match)) +
  scale_color_manual(values=grepcolors) +
  ggtitle(title_text) +
  ylab(y_label_text) +
  theme_minimal()

ggsave("yearly_count_plot.pdf",plot=myplot, width=6, height=4)

