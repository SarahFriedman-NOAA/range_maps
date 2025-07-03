# prepping dataset for plotting
g <- guide_species %>%
  dplyr::filter(!grepl("remove", audit) & volume != "shelled") %>%
  # dplyr::select(tax_group:species_code) %>%
  dplyr::left_join(select(classy, species_code, id_rank), by = join_by(species_code)) %>%
  dplyr::mutate(group_codes = purrr::map2(species, id_rank, ~get_group_codes(.x, .y, classy))) %>%
  tidyr::unnest(cols = c(group_codes)) %>%
  dplyr::left_join(catch, by = c("group_codes" = "species_code")) %>% 
  dplyr::select(
    species_name = species, species_code, volume,
    tax_group, lat, lon, region
  ) %>%
  dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon)) %>%
  dplyr::filter(!is.na(lat) & !is.na(lon)) %>%
  unique() %>% 
  dplyr::group_by(species_code) %>%
  tidyr::nest() %>%
  arrange(species_code) 


# looping over each species to create plot
for (i in 1:nrow(g)) {
  print(g$species_code[i])
  
  # removing outlier points based on clustering algorithm
  gg <- remove_outliers(g$data[[i]]) %>%
    dplyr::mutate(
      region = get_region(.),
      source = "gap"
    )

  if (!all(gg$species_name == gg$species_name[1])) stop("Some data are not from the same species. Check the species codes in guide_species")


  # sample down the number of points so distribution isn't as biased towards densly sampled regions
  if (nrow(gg) > 5000) {
    gg <- gg %>%
      sample_n(3000, replace = FALSE)
  }

  # if(nrow(gg) < 20) next()

  # supplement GAP data if too few points
  if (nrow(gg) < 20 & !grepl(paste(rm_bits, "| sp\\. "), gg$species_name[1])) {
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
      dplyr::mutate(
        region = get_region(.),
        source = "idigbio"
      ) %>%
      dplyr::filter(!is.na(region))

    gg <- bind_rows(gg, idig)
  }

  if (all(is.na(gg$lat)) && nrow(gg) == 0) next()


  # plotting data
  p <- ggplot2::ggplot()

  # Find GAP polygon for each region
  if (nrow(gg) > 15) {
    s <- gg %>%
      dplyr::group_by(region) %>%
      dplyr::mutate(perc = n() / nrow(.)) %>%
      split(.$region)

    # remove outliers by region
    s <- lapply(s, remove_outliers)

    # clunky way to ensure regional continuity
    reg_plot <- do.call(rbind, s) %>%
      dplyr::add_count() %>%
      dplyr::select(region, perc, n) %>%
      unique() %>%
      dplyr::mutate(plot = case_when(
        n > 5 & perc > 0.1 ~ T,
        n > 10 ~ T,
        TRUE ~ F
      )) %>%
      dplyr::full_join(
        data.frame(region = c(
          "n_ebs", "ai3", "se_goa", "goa", "kodiak",
          "ai5", "ai4", "ai2", "ai1", "stlaw", "nbs",
          "s_slope", "n_slope", "s_ebs", "pribs", "stmat"
        )),
        by = join_by(region)
      )



    if (sum(reg_plot$plot[reg_plot$region == "s_slope"], na.rm = T) == 1) {
      reg_plot$plot[reg_plot$region == "pribs"] <- TRUE
    }
    if (sum(reg_plot$plot[reg_plot$region == "n_ebs"], na.rm = T) == 1) {
      reg_plot$plot[reg_plot$region == "stmat"] <- TRUE
    }
    if (sum(reg_plot$plot[reg_plot$region %in% c("nbs", "n_ebs")], na.rm = T) == 2) {
      reg_plot$plot[reg_plot$region == "stlaw"] <- TRUE
    }

    if (sum(reg_plot$plot[reg_plot$region %in% c("ai1", "ai3")], na.rm = T) == 2) {
      reg_plot$plot[reg_plot$region == "ai2"] <- TRUE
    }

    if (sum(reg_plot$plot[reg_plot$region %in% c("ai2", "ai5")], na.rm = T) == 2) {
      reg_plot$plot[reg_plot$region == "ai3"] <- TRUE
      reg_plot$plot[reg_plot$region == "ai4"] <- TRUE
    }

    if (sum(reg_plot$plot[reg_plot$region %in% c("ai2", "ai4")], na.rm = T) == 2) {
      reg_plot$plot[reg_plot$region == "ai3"] <- TRUE
    }

    if (sum(reg_plot$plot[reg_plot$region %in% c("ai3", "ai5", "s_slope")], na.rm = T) >= 2) {
      reg_plot$plot[reg_plot$region == "ai4"] <- TRUE
    }
    if (sum(reg_plot$plot[reg_plot$region %in% c("ai1", "s_slope")], na.rm = T) == 2) {
      reg_plot$plot[reg_plot$region == "ai2"] <- TRUE
      reg_plot$plot[reg_plot$region == "ai3"] <- TRUE
      reg_plot$plot[reg_plot$region == "ai4"] <- TRUE
    }

    if (sum(reg_plot$plot[reg_plot$region %in% c("ai1", "goa")], na.rm = T) == 2) {
      reg_plot$plot[reg_plot$region == "ai2"] <- TRUE
      reg_plot$plot[reg_plot$region == "ai3"] <- TRUE
      reg_plot$plot[reg_plot$region == "ai4"] <- TRUE
      reg_plot$plot[reg_plot$region == "ai5"] <- TRUE
      reg_plot$plot[reg_plot$region == "kodiak"] <- TRUE
    }

    if (sum(reg_plot$plot[reg_plot$region %in% c("s_slope", "goa", "ai3")], na.rm = T) >= 2) {
      reg_plot$plot[reg_plot$region == "ai4"] <- TRUE
    }

    if (sum(reg_plot$plot[reg_plot$region %in% c("s_ebs", "kodiak", "ai4")], na.rm = T) >= 2) {
      reg_plot$plot[reg_plot$region == "ai5"] <- TRUE
    }
    if (sum(reg_plot$plot[reg_plot$region %in% c("se_goa", "kodiak")], na.rm = T) >= 2) {
      reg_plot$plot[reg_plot$region == "goa"] <- TRUE
    }


    regions <- setNames(reg_plot$plot, reg_plot$region)
    regions <- regions[!is.na(regions)]


    for (j in 1:length(regions)) {
      gr <- s[[names(regions)[j]]]

      if (regions[j]) {
        reg_shp <- dplyr::filter(ak_shp, region == names(regions)[j]) %>%
          sf::st_transform(crs = sf::st_crs(wrld2)) %>%
          dplyr::mutate(
            lon = list(sf::st_coordinates(.)[, 1]),
            lat = list(sf::st_coordinates(.)[, 2])
          ) %>%
          tidyr::unnest(c(lon, lat)) %>%
          sf::st_drop_geometry() %>%
          dplyr::mutate(lon = ifelse(lon < 0, 360 + lon, lon))


        p <- p +
          ggplot2::geom_polygon(
            data = reg_shp, aes(x = lon, y = lat),
            fill = "#1c62a5", col = "#1c62a5", lwd = 0.5
          )
      } else {
        p <- p + ggplot2::geom_point(
          data = dplyr::filter(gr, source == "gap"), aes(x = lon, y = lat),
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
  if (nrow(gg) < 15) {
    p <- p +
      ggplot2::geom_point(
        data = dplyr::filter(gg, source == "gap"), aes(x = lon, y = lat),
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


  if (save_output) {
    # create family-level folders for range map pdfs
    if (!file.exists(paste0("output/", gg$volume[1], "/", gg$tax_group[1], "/"))) {
      dir.create(paste0("output/", gg$volume[1], "/", gg$tax_group[1], "/"), recursive = TRUE)
    }

    ggsave(filename = paste0("output/", gg$volume[1], "/", gg$tax_group[1], "/", g$species_code[[i]], ".png"),
           plot = p,
           dpi = 600,
           bg = "transparent")
    # print(p +
    # ggplot2::geom_point(
    #   data = dplyr::filter(gg, source == "gap"), aes(x = lon, y = lat),
    #   cex = 1, col = "grey10", alpha = 0.8
    # ))
    # print(p)
    # 
    # dev.off()

    x <- c(
      "s", "idig", "gg", "reg_plot", "regions", "gr", "p"
    )
    rm(list = x)
  } else {
    print(p)
  }
}
