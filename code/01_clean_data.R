# get all catch and taxonomy info together and filtering to just groundfish surveys after 2000
survey_def_ids <- c("AI" = 52, "GOA" = 47, "EBS" = 98, 
                    "BSS" = 78, "NBS" = 143)

# indicator strings that record is not strictly a species
rm_bits <- paste0(c(
  "\\(adult\\)", "\\(juvenile\\)", "egg", "egg case",
  "hybrid", "larva", "larvae", "tubes", "group", "unid\\."
), collapse = "| ")


cruise <- read_csv("data/oracle/race_data-v_cruises.csv") %>%
  janitor::clean_names() %>%
  dplyr::filter(year >= 2000 & survey_definition_id %in% survey_def_ids) %>%
  dplyr::select(year, survey_definition_id, cruisejoin, region, cruise, cruise_id, vessel_id)

haul <- read_csv("data/oracle/racebase-haul.csv") %>%
  janitor::clean_names() %>%
  dplyr::filter(abundance_haul == "Y") %>%
  dplyr::select(cruisejoin:haul, start_latitude, start_longitude, depth = bottom_depth)


species_codes <- read_csv("data/oracle/gap_products-test_species_classification.csv") %>%
  janitor::clean_names() %>%
  dplyr::filter(survey_species == 1 & id_rank == "species" & !grepl(rm_bits, species_name)) %>%
  dplyr::select(species_code, species_name, 
                family = family_taxon, order = order_taxon, class = class_taxon)


catch <- read_csv("data/oracle/racebase-catch.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-c(weight, number_fish, subsample_code)) %>%
  dplyr::right_join(cruise, by = join_by(cruisejoin, region, cruise)) %>%
  dplyr::right_join(haul, by = join_by(cruisejoin, hauljoin, region, vessel, cruise, haul)) %>%
  dplyr::left_join(species_codes, by = join_by(species_code)) %>%
  dplyr::select(species_name, species_code, family:class, year, cruise, vessel, haul, 
         region, lat = start_latitude, lon = start_longitude, depth) %>% 
  dplyr::arrange(order, family, species_name) %>%
  dplyr::group_by(species_code) %>% 
  dplyr::add_count(name = "n_records") %>%
  dplyr::filter(n_records > 0 & !is.na(species_name))

