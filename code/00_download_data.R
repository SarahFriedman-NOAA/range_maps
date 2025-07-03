## Download data sets to local machine -------------------------------------------------------

# RACEBASE tables to query
locations <- c(
  ## General Tables of data (racebase)
  "RACEBASE.HAUL",
  "RACEBASE.CATCH",
  #"RACEBASE.SPECIES_CLASSIFICATION",
  
  "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION",
  "GAP_PRODUCTS.TAXONOMIC_CHANGES",
  
  ## Race Data tables
  #"RACE_DATA.RACE_SPECIES_CODES",
  "RACE_DATA.V_CRUISES"
)



# Download tables from Oracle to data folder on local machine
if (!file.exists("data/oracle")) dir.create("data/oracle", recursive = TRUE)

for (i in 1:length(locations)) {
  print(locations[i])
  filename <- tolower(gsub("\\.", "-", locations[i]))
  a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
  readr::write_csv(
    x = a,
    here("data", "oracle", paste0(filename, ".csv"))
  )
  remove(a)
}



# read Oracle tables into R environment
a <- list.files(
  path = here::here("data", "oracle"),
  pattern = "\\.csv"
)

for (i in 1:length(a)) {
  b <- readr::read_csv(file = here::here("data", "oracle", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(x = paste0(str_extract(a[i], "[^-]*(?=\\.)"), "0"), value = b)
  rm(b)
}
