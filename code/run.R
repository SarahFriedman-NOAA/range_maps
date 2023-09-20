
## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", 
         "RODBC", 
         "here", 
         "janitor",
         "getPass", 
         "reshape",
         "ridigbio"
         )

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}
rm(p, pkg)


load_cached_data <- FALSE


# load necessary functions
source("code/functions.R")


## Download and Wrangle Oracle Data ---------------------------------------------------
if(!file.exists("data/") | load_cached_data){
  source("code/connect_to_oracle.R")
  source("code/00_download_data.R")
} 

source("code/01_clean_data.R")



## Generate Range Maps ---------------------------------------------------
save_output <- TRUE # saves pdfs of ranges to output folder
source("code/02_plot_ranges.R")

