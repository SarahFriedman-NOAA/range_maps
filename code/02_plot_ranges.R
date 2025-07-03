# sp <- "Sebastolobus macrochir"

# # data frame of species to create range maps for
# g <- catch %>%
#   #filter(species_name %in% sp) %>%
#   filter(grepl(sp, species_name)) %>%
#   dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
#   ungroup() %>%
#   tidyr::nest()


# prepping dataset for plotting
g <- guide_species %>%
  #  filter(species == "Lebbeus groenlandicus") %>%
  filter(tax_group == "shrimp") %>%
  dplyr::select(phylum:species_code) %>%
  dplyr::left_join(catch, by = "species_code") %>%
  dplyr::select(
    species_name = species, species_code,
    tax_group, lat, lon, region
  ) %>%
  dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
  dplyr::filter(!is.na(lat) & !is.na(lon)) %>%
  unique() %>%
  group_by(species_code) %>%
  tidyr::nest()


# looping over each species to create plot
for (i in 1:nrow(g)) {
 # print(g$data[[i]]$species_name[1])

  # removing outlier points based on clustering algorithm
  gg <- remove_outliers(g$data[[i]]) %>%
    dplyr::mutate(region = get_region(.))

  # sample down the number of points so distribution isn't as biased towards densly sampled regions
  if (nrow(gg) > 5000) {
    gg <- gg %>%
      sample_n(3000, replace = FALSE)
  }

  # idigbio records of complete range
  idig <- idig_search_records(rq = list(scientificname = gg$species_name[[1]])) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::mutate(year = as.numeric(data_dwc_year)) %>%
    dplyr::filter(year > 1900) %>% # only newer records
    dplyr::mutate(
      lon = round(geopoint_lon, 2),
      lat = round(geopoint_lat, 2)
    ) %>%
    dplyr::select(lon, lat) %>%
    dplyr::filter(complete.cases(.)) %>%
    unique() %>%
    dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
    dplyr::filter(lat < 70 & lat > 50 & lon > 170 & lon < 230) # fitting points to plot

  
  idig <- remove_outliers(idig) %>%
    dplyr::mutate(region = get_region(.)) %>%
    dplyr::filter(!is.na(region)) %>%
    dplyr::group_by(region) %>%
    dplyr::filter(n() >= 3)


  # Find idig polygon for each region
  if (any(!is.na(idig)) && nrow(idig) > 3) {
    idig_reg <- names(table(idig$region))

    idigs <- idig %>%
      dplyr::filter(region %in% idig_reg) %>%
      split(.$region)

    # remove outliers by region
    idigs <- lapply(idigs, remove_outliers)

    for (j in 1:length(idig_reg)) {
      nd <- nrow(idigs[[j]])
      if (nd > 5) {
        ff <- case_when(
          nd <= 200 & nd >= 40 ~ 15,
          nd < 40 ~ 10,
          nd > 200 ~ 20
        )

        out <- make_contours(idigs[[j]], ff = ff)
        
        assign(x = paste0("dd", j), value = out[[1]])
        assign(x = paste0("h", j), value = out[[2]])
        rm(f)
      }
    }
  }
  
  if (all(is.na(gg$lat)) && is.na(idig)) no_data <- TRUE else no_data <- FALSE


  # Find GAP polygon for each region
  if (!no_data & nrow(gg) > 20) {
    s <- gg %>%
      split(.$region)
    regions <- names(s)

    # remove outliers by region
    s <- lapply(s, remove_outliers)

    for (j in 1:length(regions)) {
      if (nrow(s[[j]]) > 3) {
        out <- make_contours(s[[j]], ff = 10)
        assign(x = paste0("dc", j), value = out[[1]])
        assign(x = paste0("L", j), value = out[[2]])
      }
    }
  }



  # plotting data
  p <- ggplot2::ggplot()

  # plot idig polygons for each region
  if (any(!is.na(idig)) && nrow(idig) > 5) {
    for (j in 1:length(idig_reg)) {
      idigr <- idigs[[idig_reg[j]]]
      da <- paste0("dd", j)
      br <- paste0("h", j)
      
      if (nrow(idigr) > 5) {
        p <- p + ggplot2::stat_contour(
          data = eval(as.name(da)), aes(x = X1, y = X2, z = value),
          breaks = eval(as.name(br)), fill = "#96c2db",
          col = "#96c2db", lwd = 0.3, geom = "polygon"
        )
      } 
      }
    }
  

  # plot GAP polygons for each region
  if (!no_data & nrow(gg) > 20) {
    for (j in 1:length(regions)) {
      gr <- s[[regions[j]]]

      if (nrow(gr) > 5) {
        if(regions[j] == "slope"){
          p <- p + ggforce::geom_mark_hull(
            data = gr, aes(x = lon, y = lat),
            fill = "#1c62a5", col = NA, alpha = 1,
            expand = 0.02)
        } else {
        da <- paste0("dc", j)
        br <- paste0("L", j)
        p <- p + ggplot2::stat_contour(
          data = eval(as.name(da)), aes(x = X1, y = X2, z = value),
          breaks = eval(as.name(br)), fill = "#1c62a5",
          col = "#1c62a5", lwd = 0.3, geom = "polygon"
        )
        }
      
      } else {
        p <- p + ggplot2::geom_point(
          data = gr, aes(x = lon, y = lat),
          cex = 4, col = "grey10", alpha = 0.8
        )
      }
    }
  }

  # universal plotting aesthetics
  p <- p +
    ggplot2::geom_polygon(
      data = world2, aes(x = long, y = lat, group = group),
      col = "grey70", fill = "grey93", lwd = 0.3
    )

  # plot GAP data as points if too few obs for a polygon
  if (!no_data && nrow(gg) < 20) {
    p <- p +
      ggplot2::geom_point(
        data = gg, aes(x = lon, y = lat),
        cex = 4, col = "grey10", alpha = 0.8
      )
  }

  p <- p +
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



  # create family-level folders for range map pdfs
  if (!file.exists(paste0("output/", gg$tax_group[1], "/"))) {
    dir.create(paste0("output/old_", gg$tax_group[1], "/"), recursive = TRUE)
  }

  if (save_output) {
    pdf(paste0("output/", gg$tax_group[1], "_old/", gg$species_name[1], ".pdf"))
    print(p) 
    dev.off()
    
    x <- c(paste0(rep(c("dd", "dc", "h", "L"), each = 5), 1:5), "f", "s", "idigs", "idig", "gg")
    rm(list = x)
  } else {
    print(p)
  }
}

