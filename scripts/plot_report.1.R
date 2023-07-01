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
allcolors <- c("R" ="chartreuse",
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

n_pmcs <- length(meth_files)
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

trend_plot_1 <- "yearly_count_plot.1.pdf"
ggsave(trend_plot_1,plot=myplot, width=10, height=6)  

papers_scale=1000
minyear=1999
maxyear=2023

yearly_plotdata <- yearly_counts
yearly_plotdata[,grep_names] <- yearly_plotdata[,grep_names]/(yearly_totals$n_papers/papers_scale)

yearly_plotdata <- yearly_plotdata[yearly_plotdata$year > minyear, ]
yearly_plotdata <- yearly_plotdata[yearly_plotdata$year < maxyear, ]

library(ggplot2)
library(reshape2)
plotdata <- reshape2::melt(yearly_plotdata, id.vars="year", value.name="count",variable.name="Match")

n_pmcs <- length(meth_files)
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

trend_plot_2 <- "yearly_count_plot.2.pdf"
ggsave(trend_plot_2,plot=myplot, width=10, height=6)  



total_plotdata <- yearly_totals[yearly_totals$year > minyear | is.na(yearly_totals$year),]
myplot2 <- ggplot(total_plotdata)+
  geom_col(aes(x=year, y=n_papers)) +
  # geom_line(aes(x=year, y=count)) +
  # scale_color_manual(values=grepcolors) +
  ggtitle("Papers with methods succesfully extracted", subtitle = title_text) +
  ylab("Count") +
  scale_y_log10()+
  theme_minimal()
total_plot_1 <- "yearly_totals_plot.1.pdf"
ggsave(total_plot_1,plot=myplot2, width=10, height=6)  

pmcid_lists <- lapply(grep_names, function(col_name) {
  match_data$PMCID[match_data[[col_name]]]
})
names(pmcid_lists) <- grep_names


mylist <- pmcid_lists[c("R","Python","Python2","Python3","Perl")]


library(eulerr)
venn_plots <- "match_overlaps.venn.pdf"
pdf(venn_plots,width=10, height=10)
subset_terms <- c("Python","Python2","Python3")
venndat <- match_data[,..subset_terms]
venncol <- allcolors[match(subset_terms, names(allcolors))]
mytitle <- "\n\n'Python' seems to be most inclusive among the regexes\nVenn Diagram"
plot(venn(venndat), quantities = TRUE, fills=venncol, adjust_labels=F, main=mytitle)

subset_terms <- c("R","CRAN","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- allcolors[match(subset_terms, names(allcolors))]
mytitle <- "\n\n'Bioconductor' is often cited without R\nVenn Diagram"
plot(venn(venndat), quantities = TRUE, fills=venncol, main=mytitle)

subset_terms <- c("Python","R","Perl","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- allcolors[match(subset_terms, names(allcolors))]
mytitle <- "Languages have limited overlaps in Methods\nVenn Diagram"
plot(venn(venndat), quantities = TRUE, fills=venncol, main=mytitle)

subset_terms <- c("Python","R","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- allcolors[match(subset_terms, names(allcolors))]
mytitle <- "\n\nLanguages have limited overlaps in Methods (R/Python only)\nVenn Diagram"
plot(venn(venndat), quantities = TRUE, fills=venncol, main=mytitle)

dev.off()


library(eulerr)
euler_plots <- "match_overlaps.euler.pdf"
pdf(euler_plots,width=10, height=10)
subset_terms <- c("Python","Python2","Python3")
venndat <- match_data[,..subset_terms]
venncol <- allcolors[match(subset_terms, names(allcolors))]
mytitle <- "\n\n'Python' seems to be most inclusive among the regexes\nEuler Diagram"
plot(euler(venndat), quantities = TRUE, fills=venncol, adjust_labels=F, main=mytitle)

subset_terms <- c("R","CRAN","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- allcolors[match(subset_terms, names(allcolors))]
mytitle <- "'Bioconductor' is often cited without R\nEuler Diagram"
plot(euler(venndat), quantities = TRUE, fills=venncol, main=mytitle)

subset_terms <- c("Python","R","Perl","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- allcolors[match(subset_terms, names(allcolors))]
mytitle <- "Languages have limited overlaps in Methods\nEuler Diagram"
plot(euler(venndat), quantities = TRUE, fills=venncol, main=mytitle)

subset_terms <- c("Python","R","Bioconductor")
venndat <- match_data[,..subset_terms]
venncol <- allcolors[match(subset_terms, names(allcolors))]
mytitle <- "\n\nLanguages have limited overlaps in Methods (R/Python only)\nEuler Diagram"
plot(euler(venndat), quantities = TRUE, fills=venncol, main=mytitle)
dev.off()


spot_check_df <- data.frame()

set.seed(1234)
this <- match_data[match_data$year > 2021, ]
# future_pub <- this$PMCID[sample(1:nrow(this), size = 5)]
future_pub <- sample(this$PMCID, size = 5)
spot_check_df <- rbind(spot_check_df,
                       data.frame("Match_Type"="2022 Pubs",
                                  PMCID=future_pub)
                       )

this <- match_data[match_data$R & match_data$Python & match_data$Perl, ]
R_Py_Perl <- sample(this$PMCID, size = 5)
spot_check_df <- rbind(spot_check_df,
                       data.frame("Match_Type"="R + Python + Perl",
                                  PMCID=R_Py_Perl)
)


this <- match_data[match_data$CRAN & !match_data$R, ]
CRAN_only <- sample(this$PMCID, size = 5)
spot_check_df <- rbind(spot_check_df,
                       data.frame("Match_Type"="CRAN only",
                                  PMCID=CRAN_only)
)

this <- match_data[match_data$Python3 & !match_data$Python, ]
Py3_only <- sample(this$PMCID, size = 5)
spot_check_df <- rbind(spot_check_df,
                       data.frame("Match_Type"="Python 3 only",
                                  PMCID=Py3_only)
)

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8450520/
spot_check_df$url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/", spot_check_df$PMCID,"/")


library(flextable)
myft <- flextable(spot_check_df, col_keys = c("Match_Type","PMCID"))
myft <- theme_box(myft)

ftab <- compose( x = myft, j = "PMCID",
                 value = as_paragraph(
                   hyperlink_text(x = PMCID, url = url, 
                                  fp_text_default(color = "#006699", bold = F, underlined = T)) ) )
outtable <- merge_v(ftab, j = ~ Match_Type ) %>%
  bold(j = "Match_Type", bold = TRUE)
outtable <- add_header_lines(outtable, 
                             values = c("Selected Combinations (Max 5 shown)") ) 

table_file <- "spot_check_table.pdf"
save_as_image(outtable, table_file, zoom = 1, expand =2)




library(ComplexHeatmap)
subset_terms <- c("R","CRAN","Bioconductor","Python","Python2","Python3","Perl")
combn_dat <- match_data[,..subset_terms]
comb_res <- make_comb_mat(combn_dat)


saved_comb_samples <- "comb_samples.Rds"
if (!file.exists(saved_comb_samples)) {
  set.seed(1234)
  comb_samples <- lapply(comb_name(comb_res), function(combname) {
    # combname="1000000"
    comb_idx <-extract_comb(comb_res, comb_name = combname)
    mypmcdata <- data.frame(PMCID=NA, year=NA)
    if (length(comb_idx) > 1 ) {
      mypmcdata <- match_data[sample(comb_idx, min(c(5, length(comb_idx)))),c("PMCID","year")]
    }
    data.frame(MatchTypeCode=combname,
               mypmcdata)
  })
  
  saveRDS(comb_samples, file=saved_comb_samples)
}
comb_samples <- readRDS(saved_comb_samples)
all_spot_check <- do.call(rbind, comb_samples)
all_spot_check <- all_spot_check[!is.na(all_spot_check$PMCID), ]
all_spot_check$url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/", all_spot_check$PMCID,"/")

# code=all_spot_check$MatchTypeCode[1]
# code="0000001"
all_spot_check$MatchType <- unlist(lapply(all_spot_check$MatchTypeCode, function(code) {
  match_vec=colnames(combn_dat)
  # nchar(code)==length(match_vec)
  match_names <- match_vec[as.logical(as.numeric(unlist(strsplit(code, split = ""))))]
  match_label <- paste0(match_names, collapse=" + ")
  return(match_label)  
}))


library(flextable)
myft <- flextable(all_spot_check, col_keys = c("MatchType","PMCID", "year"))
# myft <- theme_box(myft)

ftab <- compose( x = myft, j = "PMCID",
                 value = as_paragraph(
                   hyperlink_text(x = PMCID, url = url, 
                                  fp_text_default(color = "#006699", bold = F, underlined = T)) ) )
ftab <- compose( x = myft, j = "PMCID",
                 value = as_paragraph(
                   hyperlink_text(x = PMCID, url = url, 
                                  fp_text_default(color = "#006699", bold = F, underlined = T)) ) )
outtable <- merge_v(ftab, j = ~ MatchType ) %>%
  bold(j = "MatchType", bold = TRUE) %>% 
  # colformat_date(j="year", fmt_date = "%Y") %>%
  colformat_int(j="year", big.mark = "") %>%
  theme_box() %>% autofit()
outtable <- add_header_lines(outtable, 
                             values = c("All Combinations (Max 5 shown)") ) 


full_spotchecktable_file <- "spot_check_table.full.pdf"
save_as_image(outtable, full_spotchecktable_file, zoom = 1, expand =2)


library(pdftools)


report_pdfs <- c(trend_plot_1,
                 total_plot_1,
                 trend_plot_2,
                 euler_plots,
                 venn_plots,
                 table_file,
                 full_spotchecktable_file)

pdf_combine(report_pdfs, output = "combined_plots.new.pdf")






