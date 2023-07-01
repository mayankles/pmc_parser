rm(list=ls())

# setwd("~/Rprojects/PMC_parser")
setwd("~/Rprojects/PMC_parser")

data_dir="data"
library(data.table)
library(stringr)
library(stringi)
# library(parallel)

get_n_methods <- function(pmc_files_csv=NULL, from_idx=1, to_idx=10) {
  require(data.table)
  require(stringr)
  require(stringi)
  require(parallel)
  require(xml2)
  
  # pmc_files_csv="data/oa_file_list.csv"
  # from_idx=5245
  # to_idx=5247
  if (is.null(pmc_files_csv)) {
    pmc_files_csv="data/oa_file_list.csv"
  }
  # browser()
  
  if (grepl(".Rdata$", pmc_files_csv)) {
    load(pmc_files_csv)
  } else {
    # browser()
    # pmc_files_csv=file.path(data_dir,"oa_file_list.csv")
    if ( ! file.exists(pmc_files_csv)) {
      warning("Downloading PMC file list from NCBI...")
      options(timeout=1e6)
      download.file("ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_file_list.csv",
                    pmc_files_csv,
                    quiet = T)
    }
    
    
    file_list <- fread(pmc_files_csv)
    save(file_list, file = gsub(".csv",".csv.Rdata",pmc_files_csv))
  }
  
  # browser()
  if (!exists("file_list")) {
    stop("File List not found")
  }
  pmc_ftp_base="ftp://ftp.ncbi.nlm.nih.gov/pub/pmc"
  sorted_idx <- sort(as.numeric(c(from_idx, to_idx)))
  from_idx <- sorted_idx[1]
  to_idx <- sorted_idx[2]
  # print(from_idx)
  # print(to_idx)
  if (from_idx > nrow(file_list)){
    from_idx=nrow(file_list)
  }
  if (to_idx > nrow(file_list)){
    to_idx=nrow(file_list)
  }
  print(paste0("Fetching from index ", from_idx, " to ", to_idx, "..."))
  # file_ind=3
  batch_size=to_idx-from_idx+1
  for (file_ind in from_idx:to_idx) {
    file_tar_url <- paste0(pmc_ftp_base, "/", file_list[file_ind, 1])
    
    curr_pmc_id <- gsub(".tar.gz" ,"", basename(file_tar_url))
    print(paste0("[ ", file_ind-from_idx+1," of ", batch_size," ] Fetching ", curr_pmc_id, " (", file_ind, " of ", nrow(file_list),")..."))
    
    # if (! dir.exists(curr_pmc_id)) {
    #   dir.create(curr_pmc_id)
    # }
    
    tar_dest <- file.path(data_dir,curr_pmc_id,basename(file_tar_url))
    if (! dir.exists(dirname(tar_dest))) {
      dir.create(dirname(tar_dest))
    }
    
    nxml_file <- list.files(dirname(tar_dest),pattern = "*.nxml", full.names = T)
    
    if (length(nxml_file) != 1) {
      tryCatch({
        download.file(file_tar_url, tar_dest, quiet = T)
      }, error = function(e) {
        tryCatch({
          download.file(gsub("^ftp","http",file_tar_url), tar_dest, quiet = F)
        }, error = function(e) {
          message(paste0("Download failed for ",file_tar_url," ..."))
          message(paste0("Error: ",e))
          next()
        })
      })
      
      if (! file.exists(tar_dest)) {
        next()
      }
      
      tryCatch({
        nxml_file <- grep("*.nxml", untar(tar_dest, list=T), value=T)
        untar(tar_dest, files=grep("*.nxml", untar(tar_dest, list=T), value=T), exdir=data_dir)
      }, error =function(e) {
        message(paste0("No nxml file found in ",file_tar_url," ..."))
        message(paste0("Error: ",e))
        next()
      })
        
      nxml_file <- file.path(dirname(tar_dest),basename(nxml_file))
    }
    
    full_xml <- read_xml(nxml_file)
    methods_text <- NA
    tryCatch({
      methods_nodes <- xml_find_all(full_xml, "/article/body/sec[@sec-type='methods']")
      methods_nodes <- grep("method",xml_text(xml_find_all(full_xml, 
                                  "/article/body/sec/title")),
                          ignore.case = T)
                                  # "/article/body/sec/title[contains(@title, 'Method')]")
    
      methods_text <- xml_text(xml_find_all(full_xml, 
                                          paste0("/article/body/sec[",methods_nodes,"]")))
    }, error = function(e) {
      print(paste0("XML parsing failed for ", curr_pmc_id))
    })
    
    # if (length(methods_text)>0) {
      methods_table_file <- file.path(dirname(tar_dest),"methods.txt")
      if (! file.exists(methods_table_file)) {
        methods_text <- gsub("\\n|\\t","",methods_text)
        methods_text <- gsub("\"","'",methods_text)
        pub_year <- as.numeric(stri_reverse(substr(stri_reverse(str_extract(file_list$`Article Citation`[file_ind],"[A-za-z ]+\\. ([0-9]{4}) ")),2,5)))
        out_data <- data.table(PMCID=curr_pmc_id, year=pub_year, methods_text=methods_text)
        write.table(out_data, methods_table_file, sep="\t", quote=T, col.names = F,row.names = F)
      }
    # }
    
    if (file.exists(tar_dest)) {
      file.remove(tar_dest)
    }
  }

}


# 
# args = commandArgs(trailingOnly=TRUE)
# if (length(args) < 2) {
#   stop("Must provide at least two arguments: start and end index of PMC file list.")
# }
# 
# fr=args[1]
# to=args[2]

# fr=1e4
fr=100250
to=100250
pmc_list_file="data/oa_file_list.csv.Rdata"
get_n_methods(pmc_list_file,from_idx=fr, to_idx=to)


 
# 
# # problem <- "data/PMC102764/methods.txt"
# problem <- "data/PMC102764/1472-6947-2-4.nxml"
# full_xml <- read_xml(problem)
# methods_text <- NA
# methods_nodes <- xml_find_all(full_xml, "/article/body/sec[@sec-type='methods']")
# methods_nodes <- grep("method",xml_text(xml_find_all(full_xml, 
#                                                      "/article/body/sec/title")),
#                       ignore.case = T)
# # "/article/body/sec/title[contains(@title, 'Method')]")
# 
# methods_text <- xml_text(xml_find_all(full_xml, 
#                                       paste0("/article/body/sec[",methods_nodes,"]")))
# methods_text <- gsub("\"","'",methods_text)
# this <- fread(pmc_list_file)
# # idx_list <- list(c(1,500),
# #                  c(501,1000),
# #                  c(1001,1500),
# #                  c())
# 
# 
# # pmc_list_file="data/oa_file_list.csv"
# # all_idx <- seq(1,3e6)
# # chunk_size = 500
# # numcores=detectCores()-1
# # idx_list <- split(all_idx, ceiling(seq_along(all_idx)/chunk_size))
# 
# # idx_list <- idx_list[1:20]
# # mclapply(idx_list,function(idx_range, list_file) {
# #   get_n_methods(list_file, from_idx=idx_range[1], to_idx=idx_range[length(idx_range)])
# # }, pmc_list_file, mc.cores = numcores)
# 



