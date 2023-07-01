rm(list=ls())

setwd("~/Desktop/{{Documents}}/my_tools/PMC_parser")
data_dir="data"
library(data.table)
library(dplyr)
library(stringr)
library(stringi)
#library(parallel)


# system.time(list.files(data_dir, pattern = "methods.txt", recursive = T, full.names = T))
# system.time(system(paste0("ls -f -R ",data_dir,"/*/methods.txt"), intern = TRUE))
# meth_files <- list.files(data_dir, pattern = "methods.txt", recursive = T, full.names = T)

print(paste0("[",date(),"] Listing methods files..."))
#meth_files <- system(paste0("ls -f -R ",data_dir,"/*/methods.txt"), intern = TRUE)
methods_filelist_file <- "methods_files.txt"
# system(paste0("find ",data_dir," -name \"methods.txt\" > ", methods_filelist_file))
meth_files <- fread(methods_filelist_file)[[1]]


greplist <- list("R" =" R programming| R version",
                 "CRAN" =" CRAN",
                 "Bioconductor" =" Bioconductor| bioconductor",
                 "Python2" = " Python.{0,9}2",
                 "Python3" = " Python.{0,9}3",
                 "Python" = " Python ",
                 "Perl" = " Perl ",
                 "Fortran" = " fortran| Fortran",
                 "ggplot" = " ggplot",
                 "matplotlib" = " matplotlib"
)

match_data <- readRDS(file="match_data.Rds")

grep_names=names(greplist)
yearly_counts_sum <- match_data %>% group_by(year) %>% summarise_at(sum, .vars=grep_names)
yearly_totals <- match_data %>% group_by(year) %>% summarise(n_papers=n())

papers_scale=1000
yearly_counts <- yearly_counts_sum
yearly_counts[,names(greplist)] <- yearly_counts[,names(greplist)]/(yearly_totals$n_papers/papers_scale)

# which(rowSums(is.na(match_data[,..grep_names])) !=0 )
min_year=1995
yearly_counts <- yearly_counts[yearly_counts$year > min_year, ]
yearly_counts <- yearly_counts[yearly_counts$year < 2022, ]

library(ggplot2)
library(reshape2)
plotdata <- reshape2::melt(yearly_counts, id.vars="year", value.name="count",variable.name="Match")

grepcolors <- c("R" ="chartreuse",
                "CRAN" =" chartreuse3",
                "Bioconductor" ="forestgreen",
                "Python2" = "gold4",
                "Python3" = "gold2",
                "Python" = "gold",
                "Perl" = "purple",
                "Fortran" = " cornflowerblue",
                "ggplot" = " forestgreen",
                "matplotlib" = "wheat1"
)

events <- c("Bioconductor" = 2001.75,
            "RStudio beta" = 2011.2,
            "RStudio 1.0" = 2016.9,
            "R Shiny" = 2012.9,
            "Python 2.0" = 2000,
            # "IPython" = 2001,
            "Python 3.0" = 2008,
            "Jupyter" = 2014)
event_plotdata <- data.frame(event=names(events),year=events)
event_plotdata$lang <- ifelse(grepl("Python|Jupyter",event_plotdata$event, ignore.case = T),"Python","R")
event_plotdata$label_x <- event_plotdata$year-0.75
event_plotdata$label_y <- diff(range(plotdata$count, na.rm=T))*0.9

n_pmcs <- length(meth_files)
# n_pmcs <- 152536
# n_valid <- sum(!match_data$isNA)
# title_text <- paste0("Articles parsed: ", n_pmcs,"\nMethods found: ",n_valid)
title_text <- paste0("Articles parsed: ", prettyNum(n_pmcs, big.mark=",", digits=0))
y_label_text <- paste0("Usage (per 1000 papers)")

myplot <- ggplot(plotdata) + 
  # geom_col(aes(x=year, y=count, fill=Match))
  geom_point(aes(x=year, y=count, color=Match)) +
  geom_line(aes(x=year, y=count, color=Match)) +
  geom_vline(data=event_plotdata, aes(xintercept=year, color=lang), linetype=2, size=0.8, alpha=0.4) +
  geom_text(data=event_plotdata, aes(x=label_x, y=label_y, label=event, color=lang), angle=90, size=5, alpha=0.8) +
  scale_color_manual(values=grepcolors) +
  ggtitle(title_text) +
  ylab(y_label_text) +
  # theme_minimal()
  theme_classic()
myplot

ggsave("yearly_count_plot.pdf",plot=myplot, width=6, height=6)  




library(ComplexHeatmap)

pmcid_lists <- lapply(grep_names, function(col_name) {
  # col_name=grep_names[1]
  match_data$PMCID[match_data[[col_name]]]
})
names(pmcid_lists) <- grep_names


mylist <- pmcid_lists[c("R","Python","Python2","Python3","Perl")]
# m1 <- make_comb_mat(mylist)
# UpSet(m1)

library(VennDiagram)
mylist <- pmcid_lists[c("Python","Python2","Python3")]
m1 <- make_comb_mat(mylist)
UpSet(m1)
VennDiagram::venn.diagram(mylist,"venn.python.png")
mylist <- pmcid_lists[c("Python","R","Perl","fortran")]
m1 <- make_comb_mat(mylist)
UpSet(m1)
VennDiagram::venn.diagram(mylist,"venn.across.png")
system("rm venn*.log")

print(paste0("[",date(),"] Done!"))







