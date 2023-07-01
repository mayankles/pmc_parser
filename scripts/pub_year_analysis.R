rm(list=ls())

setwd("~/Rprojects/PMC_parser")
data_dir="data"

pmc_files_csv=file.path(data_dir,"oa_file_list.csv")
if ( ! file.exists(pmc_files_csv)) {
  download.file("ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_file_list.csv",
                pmc_files_csv)
}


library(data.table)
file_list <- fread(pmc_files_csv)
citations=file_list$`Article Citation`
test_Vec=citations[1:200]

grep(regexp("[[alphanum:]]+\\. [[:digit:]]{4} "),file_list$`Article Citation`)
gsub("[[:alnum:]+]\\. ([[:digit:]{4}]) ","\\1",test_Vec)
gsub("([[:alnum:]]+) ","\\1",test_Vec)

library(stringr)
str_match(test_Vec,"[A-za-z ]+\\. ([0-9]{4}) ")
stri_reverse(substr(stri_reverse(str_extract(test_Vec,"[A-za-z ]+\\. ([0-9]{4}) ")),2,5))

pub_year <- str_extract(citations,"([0-9]{4})")
pub_year <- as.numeric(stri_reverse(substr(stri_reverse(str_extract(citations,"[A-za-z ]+\\. ([0-9]{4}) ")),2,5)))
hist(pub_year)
yr_counts <- sort(table(pub_year))
sort(table(pub_year))

sum(yr_counts[as.numeric(names(yr_counts)) > 2015])

