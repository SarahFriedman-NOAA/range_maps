
# sp <- "Enophrys lucasi"

# data frame of species to create range maps for
# g <- catch %>%
#   dplyr::filter(species_code %in% guide_species$species_code) %>% # only fish for now
#   # filter(species_name == sp) %>%
#   dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
#   tidyr::nest()


g <- guide_species %>%
  dplyr::select(species:family) %>%
  dplyr::left_join(catch, by = "species_code") %>%
  dplyr::select(species_name = species, common_name:species_code, 
                family = family.x, year:n_records) %>%
  # filter(species_name == "Stenobrachius nannochir") %>%
  dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
  dplyr::filter(!is.na(lat) & !is.na(lon)) %>%
  group_by(species_code) %>%
  tidyr::nest()


# looping over each species to create plot
for (i in 1:nrow(g)) {

  # removing outlier points based on clustering algorithm
  gg <- remove_outliers(g$data[[i]])


  # idigbio records of complete range
  idig <- idig_search_records(rq = list(scientificname = gg$species_name[[1]])) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = as.numeric(str_extract(datecollected, "^[0-9]{4}"))) %>%
   # dplyr::filter(year > 2000) %>% # only newer records
    dplyr::mutate(lon = round(geopoint.lon, 2), 
                  lat = round(geopoint.lat, 2)) %>%
    dplyr::select(lon, lat) %>%
    dplyr::filter(complete.cases(.)) %>%
    unique() %>%
    dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
    dplyr::filter(lat < 70 & lat > 50 & lon > 170 & lon < 230) # fitting points to plot
    
  idig <- remove_outliers(idig)


  # Find idig polygon for each region
  if (!is.na(idig) && nrow(idig) > 1) {

    idig_reg <- unique(idig$region)
    if (length(idig_reg) > 1) f <- 10 else f <- 20
    
    for (j in 1:length(idig_reg)) {
      idigs <- idig %>%
        split(.$region)
      
      kk2 <- MASS::kde2d(idigs[[j]]$lon, idigs[[j]]$lat,
                         lims = c(
                           c(min(s[[j]]$lon) - f, max(s[[j]]$lon) + f),
                           c(min(s[[j]]$lat) - f, max(s[[j]]$lat) + f)
                         )
      )
      dimnames(kk2$z) <- list(kk2$x, kk2$y)
      dd <- reshape::melt(kk2$z)
      assign(x = paste0("dd", j), value = dd)
      
      # get species contour boundaries for each region
      out <- getLevel(idigs[[j]]$lon, idigs[[j]]$lat, 0.9999)
      assign(x = paste0("h", j), value = out)
    }
  } else {
    idig <- h <- dc2 <- kk2 <- NA
  }
  
  if(all(is.na(gg$lat)) && is.na(idig) ) no_data <- TRUE else no_data <- FALSE


  # plot data
  if (nrow(gg) < 20 & !no_data) {
    p <- ggplot2::ggplot()

    # add layer if idig data present
    if (!is.na(idig) && nrow(idig) > 5) {
      p <- p + ggplot2::stat_contour(
        data = dc2, aes(x = X1, y = X2, z = value),
        breaks = h, fill = "#96c2db",
        col = "#96c2db", alpha = 0.7, lwd = 0.3, geom = "polygon"
      ) 
    }

    p <- p +
      ggplot2::geom_polygon(
        data = world2, aes(x = long, y = lat, group = group),
        col = "grey70", fill = "grey93", lwd = 0.3
      )  + 
      ggplot2::geom_point(
        data = gg, aes(x = lon, y = lat),
        cex = 2.5, col = "grey10", alpha = 0.8
      )  +
      ggplot2::coord_map(ylim = c(50, 70), xlim = c(170, 230)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks.length = unit(0, "pt")
      ) 
      # ggplot2::ggtitle(label = gg$species_name[1])
  } else {
    if(!no_data){
      
    # kernal density estimator of dataset by region
      regions <- unique(gg$region)

    kk <- MASS::kde2d(gg$lon, gg$lat,
      lims = c(
        c(min(gg$lon) - f, max(gg$lon) + f),
        c(min(gg$lat) - f, max(gg$lat) + f)
      )
    )
    dimnames(kk$z) <- list(kk$x, kk$y)
    dc <- reshape::melt(kk$z)
    

    # 5 contours
    #if (nrow(gg) > 500) {
      perc <- seq(0.1, 0.99, length.out = 5)
    # } else {
    #   if (nrow(gg) < 40) {
    #     perc <- seq(0.05, 0.9, length.out = 5)
    #   } else {
    #     perc <- seq(0.05, 0.95, length.out = 5)
    #   }
    # }

    # get species contour boundaries at each percent
    for (i in 1:length(perc)) {
      out <- getLevel(gg$lon, gg$lat, perc[[i]])
      assign(x = paste0("L", i), value = out)
    }

    # plotting data
    p <- ggplot2::ggplot(data = dc, aes(x = X1, y = X2))

    # add layer if idig data present
    if (!is.na(idig) && nrow(idig) > 3) {
      p <- p + ggplot2::stat_contour(
        data = dc2, aes(z = value),
        breaks = h, fill = "#96c2db",
        col = "#96c2db", alpha = 0.7, lwd = 0.3, geom = "polygon"
      )
    }

    p <- p +
      ggplot2::geom_polygon(
        data = world2, aes(x = long, y = lat, group = group),
        col = "grey70", fill = "grey93", lwd = 0.3
      ) +
      ggplot2::coord_map(ylim = c(50, 70), xlim = c(170, 230)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"), 
        axis.ticks.length = unit(0, "pt")
      ) 
      # ggplot2::ggtitle(label = gg$species_name[[1]])
    }
  }


  # create family-level folders for range map pdfs
  if (!file.exists(paste0("output/", gg$family[1], "/"))) {
    dir.create(paste0("output/", gg$family[1], "/"), recursive = TRUE)
  }

  if (save_output) {
    pdf(paste0("output/", gg$family[1], "/", gg$species_name[1], ".pdf"))
    # par(mar = c(0,0,0,0), oma = c(0,0,0,0), xaxs="i", yaxs="i")
    # png(filename = paste0("output/", gg$family[1], "/", gg$species_name[1], ".png"),
    #     width=4, height=3, units="in", bg = "transparent", res = 1200)
    print(p)
    dev.off()
  } else {
    print(p)
  }
}
