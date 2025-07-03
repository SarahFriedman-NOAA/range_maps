
## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", 
         "RODBC", 
         "here", 
         "janitor",
         "getPass", 
         "reshape",
         "ridigbio",
         "sf",
         "dbscan",
         "maps",
         "mapproj"
         )

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}
rm(p, pkg)


load_cached_data <- TRUE


# load necessary functions
source("code/functions.R")


## Download and Wrangle Oracle Data ---------------------------------------------------
if(!file.exists("data/") | !load_cached_data){
  if (file.exists("Z:/Projects/ConnectToOracle.R")) {
    source("Z:/Projects/ConnectToOracle.R")
  } else {
    gapindex::get_connected()
  }
  source("code/00_download_data.R")
} 

source("code/01_clean_data.R")



## Generate Range Maps ---------------------------------------------------
save_output <- TRUE # saves pdfs of ranges to output folder
source("code/plot_ranges_beta.R")

