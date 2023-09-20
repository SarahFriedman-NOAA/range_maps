# world map to use for range plots
world2 <- ggplot2::map_data("world2", wrap = c(40, 400)) %>%
  dplyr::filter(region %in% c("Russia", "USA", "Canada"))

# sp <- "Myoxocephalus polyacanthocephalus"

# data frame of species to create range maps for
g <- catch %>%
  dplyr::filter(species_code <= 31550) %>% # only fish for now
 # filter(species_name == sp) %>%
  dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
  tidyr::nest()


# looping over each species to create plot
for (i in 1:nrow(g)) {
  # print(g$data[[i]]$species_name[[1]])
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

  # Find idig polygon
  if (!is.na(idig) && nrow(idig) > 1) {
    f <- 20 # scaling factor so contours close properly
    kk2 <- MASS::kde2d(idig$lon, idig$lat,
      lims = c(
        c(min(idig$lon) - f, max(idig$lon) + f),
        c(min(idig$lat) - f, max(idig$lat) + f)
      )
    )
    dimnames(kk2$z) <- list(kk2$x, kk2$y)
    dc2 <- reshape::melt(kk2$z)
    h <- getLevel(idig$lon, idig$lat, 0.99)
  } else {
    idig <- h <- dc2 <- kk2 <- NA
  }


  # plot data
  if (nrow(gg) < 20) {
    p <- ggplot2::ggplot()

    # add layer if idig data present
    if (!is.na(idig) && nrow(idig) > 5) {
      p <- p + ggplot2::stat_contour(
        data = dc2, aes(x = X1, y = X2, z = value),
        breaks = h, fill = "#FFFFBF",
        col = "#FFFFBF", alpha = 0.7, lwd = 0.3, geom = "polygon"
      ) 
    }

    p <- p +
      ggplot2::geom_polygon(
        data = world2, aes(x = long, y = lat, group = group),
        col = "grey60", fill = "grey80", lwd = 0.3
      ) +
      ggplot2::geom_point(
        data = gg, aes(x = lon, y = lat),
        cex = 1.5, col = "grey10", alpha = 0.8
      ) +
      ggplot2::coord_map(ylim = c(50, 70), xlim = c(170, 230)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggplot2::ggtitle(label = gg$species_name[1])
  } else {
    # kernal density estimator of dataset
    kk <- MASS::kde2d(gg$lon, gg$lat,
      lims = c(
        c(min(gg$lon) - f, max(gg$lon) + f),
        c(min(gg$lat) - f, max(gg$lat) + f)
      )
    )
    dimnames(kk$z) <- list(kk$x, kk$y)
    dc <- reshape::melt(kk$z)

    # 5 contours
    if (nrow(gg) > 500) {
      perc <- seq(0.1, 0.99, length.out = 5)
    } else {
      if (nrow(gg) < 40) {
        perc <- seq(0.05, 0.9, length.out = 5)
      } else {
        perc <- seq(0.05, 0.95, length.out = 5)
      }
    }

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
        breaks = h, fill = "#FFFFBF",
        col = "#FFFFBF", alpha = 0.7, lwd = 0.3, geom = "polygon"
      )
    }

    p <- p +
      ggplot2::stat_contour(aes(z = value),
        breaks = L5, fill = "#FFFFBF",
        col = "#FFFFBF", alpha = 0.7, lwd = 0.3, geom = "polygon"
      ) +
      ggplot2::stat_contour(aes(z = value),
        breaks = L4, fill = "#FEE08B",
        col = "#FEE08B", alpha = 0.7, lwd = 0.3, geom = "polygon"
      ) +
      ggplot2::stat_contour(aes(z = value),
        breaks = L3, fill = "#FDAE61",
        col = "#FDAE61", alpha = 0.7, lwd = 0.3, geom = "polygon"
      ) +
      ggplot2::stat_contour(aes(z = value),
        breaks = L2, fill = "#F46D43",
        col = "#F46D43", alpha = 0.7, lwd = 0.3, geom = "polygon"
      ) +
      ggplot2::stat_contour(aes(z = value),
        breaks = L1, fill = "#D53E4F",
        col = "#D53E4F", alpha = 0.7, lwd = 0.3, geom = "polygon"
      ) +
      #  ggplot2::geom_point(
      #   data = gg, aes(x = lon, y = lat),
      #   cex = 0.5, col = "blue", alpha = 0.4
      # ) +
      ggplot2::geom_polygon(
        data = world2, aes(x = long, y = lat, group = group),
        col = "grey60", fill = "grey80", lwd = 0.3
      ) +
      ggplot2::coord_map(ylim = c(50, 70), xlim = c(170, 230)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggplot2::ggtitle(label = gg$species_name[[1]])
  }


  # create family-level folders for range map pdfs
  if (!file.exists(paste0("output/", gg$family[1], "/"))) {
    dir.create(paste0("output/", gg$family[1], "/"), recursive = TRUE)
  }

  if (save_output) {
    pdf(paste0("output/", gg$family[1], "/", gg$species_name[1], ".pdf"))
    print(p)
    dev.off()
  } else {
    print(p)
  }
}
