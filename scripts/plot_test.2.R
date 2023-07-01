rm(list=ls())

setwd("~/Desktop/{{Documents}}/my_tools/PMC_parser")
data_dir="data"
library(data.table)
library(dplyr)
library(stringr)
library(stringi)

all_plots_file="all_plots.pdf"
pdf(all_plots_file, width = 8, height=8)


methods_filelist_file <- "methods_files.txt"
# system(paste0("find ",data_dir," -name \"methods.txt\" > ", methods_filelist_file))
meth_files <- fread(methods_filelist_file)[[1]]

match_data <- readRDS(file="match_data.Rds")
grepcolors <- c("R" ="chartreuse",
                # "CRAN" =" chartreuse3",
                "Bioconductor" ="forestgreen",
                # "Python2" = "gold4",
                # "Python3" = "gold2",
                "Python" = "gold",
                "Perl" = "purple",
                "Fortran" = " cornflowerblue"#,
                # "ggplot" = " forestgreen",
                # "matplotlib" = "wheat1"
)

events <- c("Bioconductor" = 2001.75,
            "RStudio beta" = 2011.2,
            "RStudio 1.0" = 2016.9,
            "R Shiny" = 2012.9,
            "Python 2.0" = 2000,
            # "IPython" = 2001,
            "Python 3.0" = 2008,
            "Jupyter" = 2014)


# match_data_2022 <- match_data[match_data$year==2022,]
# match_data_2020 <- match_data[match_data$year==2020,]

grep_names=names(grepcolors)
yearly_counts <- match_data %>% group_by(year) %>% summarise_at(sum, .vars=grep_names)
yearly_totals <- match_data %>% group_by(year) %>% summarise(n_papers=n())

papers_scale=1000
minyear=1994
maxyear=2022

yearly_plotdata <- yearly_counts
yearly_plotdata[,grep_names] <- yearly_plotdata[,grep_names]/(yearly_totals$n_papers/papers_scale)

# which(rowSums(is.na(match_data[,..grep_names])) !=0 )
yearly_plotdata <- yearly_plotdata[yearly_plotdata$year > minyear, ]
yearly_plotdata <- yearly_plotdata[yearly_plotdata$year < maxyear, ]

library(ggplot2)
library(reshape2)
plotdata <- reshape2::melt(yearly_plotdata, id.vars="year", value.name="count",variable.name="Match")

# grepcolors <- c("R" ="chartreuse",
#                  "CRAN" =" chartreuse3",
#                  "Bioconductor" ="forestgreen",
#                  "Python2" = "gold4",
#                  "Python3" = "gold2",
#                  "Python" = "gold"
# )

n_pmcs <- length(meth_files)
# n_pmcs <- 152536
# n_valid <- sum(!match_data$isNA)
# title_text <- paste0("Articles parsed: ", n_pmcs,"\nMethods found: ",n_valid)
title_text <- paste0("Articles parsed: ", prettyNum(n_pmcs,big.mark = ",",digits=0))
y_label_text <- paste0("Mentioned in Methods (per ",papers_scale," papers)")

myplot <- ggplot(plotdata) + 
  # geom_col(aes(x=year, y=count, fill=Match))
  geom_point(aes(x=year, y=count, color=Match)) +
  geom_line(aes(x=year, y=count, color=Match)) +
  scale_color_manual(values=grepcolors) +
  ggtitle(paste0("Language usage in Methods sections (", minyear+1, " - ", maxyear-1,")"), 
          subtitle = title_text) +
  ylab(y_label_text) +
  theme_minimal()

ggsave("yearly_count_plot.1.pdf",plot=myplot, width=6, height=4)  

papers_scale=1000
minyear=1999
maxyear=2023

yearly_plotdata <- yearly_counts
yearly_plotdata[,grep_names] <- yearly_plotdata[,grep_names]/(yearly_totals$n_papers/papers_scale)

# which(rowSums(is.na(match_data[,..grep_names])) !=0 )
yearly_plotdata <- yearly_plotdata[yearly_plotdata$year > minyear, ]
yearly_plotdata <- yearly_plotdata[yearly_plotdata$year < maxyear, ]

library(ggplot2)
library(reshape2)
plotdata <- reshape2::melt(yearly_plotdata, id.vars="year", value.name="count",variable.name="Match")

# grepcolors <- c("R" ="chartreuse",
#                  "CRAN" =" chartreuse3",
#                  "Bioconductor" ="forestgreen",
#                  "Python2" = "gold4",
#                  "Python3" = "gold2",
#                  "Python" = "gold"
# )

n_pmcs <- length(meth_files)
# n_pmcs <- 152536
# n_valid <- sum(!match_data$isNA)
# title_text <- paste0("Articles parsed: ", n_pmcs,"\nMethods found: ",n_valid)
title_text <- paste0("Articles parsed: ", prettyNum(n_pmcs,big.mark = ",",digits=0))
y_label_text <- paste0("Mentioned in Methods (per ",papers_scale," papers)")

myplot <- ggplot(plotdata) + 
  # geom_col(aes(x=year, y=count, fill=Match))
  geom_point(aes(x=year, y=count, color=Match)) +
  geom_line(aes(x=year, y=count, color=Match)) +
  scale_color_manual(values=grepcolors) +
  ggtitle(paste0("Language usage in Methods sections (", minyear+1, " - ", maxyear-1,")"), 
          subtitle = title_text) +
  ylab(y_label_text) +
  theme_minimal()

ggsave("yearly_count_plot.2.pdf",plot=myplot, width=6, height=4)  

# print(paste0("[",date(),"] Done!"))


total_plotdata <- yearly_totals[yearly_totals$year > minyear | is.na(yearly_totals$year),]
# total_plotdata <- yearly_totals
# total_plotdata$year[is.na(total_plotdata$year)] <- "NotFound"
# total_plotdata$year <- addNA(factor(total_plotdata$year))
# total_plotdata$year <- as.character(total_plotdata$year)
# 
myplot2 <- ggplot(total_plotdata)+
  geom_col(aes(x=year, y=n_papers)) +
  # geom_line(aes(x=year, y=count)) +
  # scale_color_manual(values=grepcolors) +
  ggtitle("Papers with methods succesfully extracted", subtitle = title_text) +
  ylab("Count") +
  scale_y_log10()+
  theme_minimal()
ggsave("yearly_totals_plot.1.pdf",plot=myplot2, width=6, height=4)  



library(ComplexHeatmap)

pmcid_lists <- lapply(grep_names, function(col_name) {
  # col_name=grep_names[1]
  match_data$PMCID[match_data[[col_name]]]
})
names(pmcid_lists) <- grep_names


mylist <- pmcid_lists[c("R","Python","Python2","Python3","Perl")]
# m1 <- make_comb_mat(mylist)
# UpSet(m1)



library(eulerr)
pdf("match_overlaps.venn.pdf",width=8, height=7)
subset_terms <- c("Python","Python2","Python3")
venndat <- match_data[,..subset_terms]
venncol <- grepcolors[match(subset_terms, names(grepcolors))]
mytitle <- "'Python' seems to be most inclusive among the regexes"
plot(venn(venndat), quantities = TRUE, fills=venncol, adjust_labels=F, main=mytitle)

subset_terms <- c("R","CRAN","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- grepcolors[match(subset_terms, names(grepcolors))]
mytitle <- "'Bioconductor' is often cited without R"
plot(venn(venndat), quantities = TRUE, fills=venncol, main=mytitle)

subset_terms <- c("Python","R","Perl","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- grepcolors[match(subset_terms, names(grepcolors))]
mytitle <- "Languages have limited overlaps in Methods"
plot(venn(venndat), quantities = TRUE, fills=venncol, main=mytitle)

subset_terms <- c("Python","R","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- grepcolors[match(subset_terms, names(grepcolors))]
plot(venn(venndat), quantities = TRUE, fills=venncol)
dev.off()


library(eulerr)
pdf("match_overlaps.euler.pdf",width=8, height=7)
subset_terms <- c("Python","Python2","Python3")
venndat <- match_data[,..subset_terms]
venncol <- grepcolors[match(subset_terms, names(grepcolors))]
mytitle <- "'Python' seems to be most inclusive among the regexes"
plot(euler(venndat), quantities = TRUE, fills=venncol, adjust_labels=F, main=mytitle)

subset_terms <- c("R","CRAN","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- grepcolors[match(subset_terms, names(grepcolors))]
mytitle <- "'Bioconductor' is often cited without R"
plot(euler(venndat), quantities = TRUE, fills=venncol, main=mytitle)

subset_terms <- c("Python","R","Perl","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- grepcolors[match(subset_terms, names(grepcolors))]
mytitle <- "Languages have limited overlaps in Methods"
plot(euler(venndat), quantities = TRUE, fills=venncol, main=mytitle)

subset_terms <- c("Python","R","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- grepcolors[match(subset_terms, names(grepcolors))]
plot(euler(venndat), quantities = TRUE, fills=venncol)
dev.off()


library(VennDiagram)
mylist <- pmcid_lists[c("Python","Python2","Python3")]
# m1 <- make_comb_mat(mylist)
# UpSet(m1)
VennDiagram::venn.diagram(mylist,"venn.python.png")
mylist <- pmcid_lists[c("Python","R","Perl","fortran")]
# m1 <- make_comb_mat(mylist)
# UpSet(m1)
VennDiagram::venn.diagram(mylist,"venn.across.png")
system("rm venn*.log")


this <- match_data[match_data$R & match_data$Python & match_data$Perl, ]
this <- match_data[match_data$CRAN, ]
this <- match_data[match_data$Python3 & !match_data$Python, ]










interactive_plotdata <- full_join(yearly_totals, yearly_counts)
interactive_plotdata <- data.frame(interactive_plotdata[interactive_plotdata$year > minyear, ])

source("https://raw.githubusercontent.com/mtandon09/Dynamic_Plotly/master/make_cutomizable_plotly.R")
library(plotly)
mywidget <- make_customizable_plotly(interactive_plotdata,      
                                     sort_by_cor=T,     
                                     id_var=NULL,       
                                     plot3D=T,
                                     my_title="Programming language/repository usaage in open-access papers in PMC",
                                     pointsize=10,
                                     plotwidth=800, plotheight=600)
              
  

htmlwidgets::saveWidget(widget = mywidget, file = "pmc_results.1.html")


