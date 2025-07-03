# function to get species codes for everything within group
get_group_codes <- function(group, rank, tax_table) {
  if(is.na(group)) return(NA)
  tt <- tax_table
  
  rank <- ifelse(rank == "subspecies", "species", rank)
  if (rank != "species" | is.na(rank)) {
    if(grepl(" ", group)) group <- stringr::str_extract(group, "^\\w+")
    col <- paste0(rank, "_taxon")
    
    if(group == "Limpet"){
      out <- tt$species_code[tt$infraorder_taxon == "Patellogastropoda" |
                               tt$family_taxon == "Fissurellidae"]
      
    } else {
      if(is.na(rank)) return(NA)
      out <- tt$species_code[tt[eval(col)] == eval(group) & tt$survey_species == 1]
    }
    
  } else {
    out <- tt$species_code[tt$species_name == eval(group) & tt$survey_species == 1]
  }
  out <- sort(unique(out[!is.na(out)]))
  out
}


# function to estimate break point for different data densities
getLevel <- function(x, y, prob) {
  kk <- MASS::kde2d(x, y)
  dx <- diff(kk$x[1:2])
  dy <- diff(kk$y[1:2])
  sz <- sort(kk$z)
  c1 <- cumsum(sz) * dx * dy
  approx(c1, sz, xout = 1 - prob)$y
}


# remove outliers from dataset
remove_outliers <- function(data) {
  n <- nrow(data)
  if (n > 3) {
    eps <- case_when(
      n >= 1000 ~ 2,
      n >= 150 & n < 1000 ~ 3,
      TRUE ~ 6
    )

    mpts <- case_when(
      n >= 1000 ~ 20,
      n >= 150 & n < 1000 ~ 10,
      TRUE ~ 5
    )

    clustering <- dbscan::dbscan(data[, c("lon", "lat")],
      eps = eps, minPts = mpts, borderPoints = FALSE
    )

    out <- data %>%
      dplyr::mutate(
        cluster = clustering$cluster,
        outlier = ifelse(cluster == 0, T, F)
      ) %>%
      dplyr::filter(!outlier)
    if (nrow(out) == 0) {
      out <- data
    }
  } else {
    out <- data
  }
  out
}


# function to generate species contours for map
make_contours <- function(data, ff = 10) {
  kk2 <- MASS::kde2d(data$lon, data$lat,
    lims = c(
      c(min(data$lon) - ff, max(data$lon) + ff),
      c(min(data$lat) - ff, max(data$lat) + ff)
    )
  )
  dimnames(kk2$z) <- list(kk2$x, kk2$y)
  dd <- reshape::melt(kk2$z)

  # get species contour boundaries for each region
  perc <- case_when(
    nrow(data) < 10 ~ 0.8,
    nrow(data) >= 100 & nrow(data) <= 1000 ~ 0.95,
    nrow(data) > 1000 ~ 0.98,
    nrow(data) < 100 ~ 0.92
  )
  br <- getLevel(data$lon, data$lat, perc)

  return(list(dd, br))
}






# getting map elements and shape files
# world map to use for range plots
world2 <- ggplot2::map_data("world2", wrap = c(40, 400)) %>%
  dplyr::filter(region %in% c("Russia", "USA", "Canada"))
wrld2 <- sf::st_as_sf(map('world2', plot=F, fill=T)) 


# getting shape files for AK regions
# made shapes using https://geojson.io/#new&map=3.63/56.05/-166.44

# raw_shps <- sf::read_sf("data/AK_regional_maps/")
# ak_shp <- st_cast(st_sf(raw_shps), "POLYGON") %>%
#   dplyr::mutate(region = c("ebs", "nbs", "ai", "se_goa", "goa", "slope")) %>%
#   sf::st_transform(crs = "EPSG:3338")

# raw_shps <- sf::read_sf("data/AK_regional_maps2/")
# ak_shp <- sf::st_cast(sf::st_sf(raw_shps), "POLYGON") %>%
#   dplyr::mutate(region = c("n_ebs", "central_ai", "se_ai", "slope", "w_ebs",
#                            "nbs", "se_goa", "goa", "w_ai", "s_ebs")) %>%
#   sf::st_transform(crs = "EPSG:3338")


# raw_shps <- sf::read_sf("data/AK_regional_maps3/")
# ak_shp <- sf::st_cast(sf::st_sf(raw_shps), "POLYGON") %>%
#   dplyr::mutate(region = c("central_ai", "s_ebs", "se_ai", "slope", "w_ebs",
#                            "nbs", "se_goa", "goa", "w_ai", "n_ebs", "s_slope")) %>%
#   sf::st_transform(crs = "EPSG:3338")

# raw_shps <- sf::read_sf("data/AK_regional_maps4/")
# ak_shp <- sf::st_cast(sf::st_sf(raw_shps), "POLYGON") %>%
#   dplyr::mutate(region = c("central_ai", "sw_goa", "n_ebs", "slope", "nbs",
#                            "se_goa", "goa", "w_ai", "s_ebs", "s_slope", "w_slope")) %>%
#   sf::st_transform(crs = "EPSG:3338")

raw_shps <- sf::read_sf("data/AK_regional_maps5/")
ak_shp <- sf::st_cast(sf::st_sf(raw_shps), "POLYGON") %>%
  dplyr::mutate(region = c("n_ebs", "ai3", "se_goa", "goa", "kodiak",
                           "ai5", "ai4", "ai2", "ai1", "stlaw", "nbs",
                           "s_slope", "n_slope", "s_ebs", "pribs", "stmat")) %>%
  sf::st_transform(crs = "EPSG:3338")


# assign each observation to a region in AK
get_region <- function(x) {
  tmp <- x %>%
    dplyr::select(lat, lon) %>%
    dplyr::mutate(long = ifelse(lon > 180, lon - 360, lon)) %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = "WGS84") %>%
    sf::st_transform(crs = "EPSG:3338")

  res <- c()
  for (j in 1:nrow(ak_shp)) {
    p <- as.numeric(sf::st_intersects(tmp, ak_shp[j, ]))
    res[p == 1] <- ak_shp$region[j]
  }
  return(res)
}
