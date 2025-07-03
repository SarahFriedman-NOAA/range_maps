# get all catch and taxonomy info together and filtering to just groundfish surveys after 2000
survey_def_ids <- c("AI" = 52, "GOA" = 47, "EBS" = 98, 
                    "BSS" = 78, "NBS" = 143)

# indicator strings that record is not strictly a species
rm_bits <- paste0(c(
  "\\(adult\\)", "\\(juvenile\\)", "egg", "egg case",
  "hybrid", "larva", "larvae", "tubes", "group", "unid\\."
), collapse = "| ")


cruise <- readr::read_csv("data/oracle/race_data-v_cruises.csv") %>%
  janitor::clean_names() %>%
  dplyr::filter(year >= 2000 & survey_definition_id %in% survey_def_ids) %>%
  dplyr::select(year, survey_definition_id, cruisejoin, region, cruise, cruise_id, vessel_id)

haul <- readr::read_csv("data/oracle/racebase-haul.csv") %>%
  janitor::clean_names() %>%
  dplyr::filter(abundance_haul == "Y") %>%
  dplyr::select(cruisejoin:haul, start_latitude, start_longitude, depth = bottom_depth)

# guide_species <- readxl::read_excel("../invert_species_info_2020.xlsx") %>%
#   janitor::clean_names()
guide_species <- readxl::read_excel("../invert_species_info_2020.xlsx") %>%
  janitor::clean_names() 

taxonomic_classification <- readr::read_csv("data/oracle/gap_products-taxonomic_classification.csv") %>%
  janitor::clean_names() %>%
  dplyr::filter(survey_species == 1)

species_codes <- taxonomic_classification %>%
  dplyr::filter(id_rank == "species" & !grepl(rm_bits, species_name) | species_code %in% guide_species$species_code) %>%
  dplyr::select(species_code, species_name, id_rank,
                family = family_taxon, order = order_taxon, class = class_taxon)


taxonomic_changes <- readr::read_csv("data/oracle/gap_products-taxonomic_changes.csv") %>%
  janitor::clean_names()


# including taxonomic changes table so species codes aren't dropped through time due to taxonomic reshuffling
code_changes <- taxonomic_changes %>%
  # dplyr::filter(grepl("change taxon code", action)) %>%
  dplyr::select(old_species_name:new_species_code) %>%
  dplyr::left_join(taxonomic_classification, by = c("new_species_code" = "species_code")) %>%
  dplyr::select(species_name = old_species_name, everything(), -new_species_name, -species_name) %>%
  tidyr::pivot_longer(old_species_code:new_species_code, values_to = "species_code") %>%
  dplyr::select(-name) 
  
classy <- unique(rbind(taxonomic_classification, code_changes)) %>%
  mutate(species_name = str_remove(species_name, " \\(.+$"))


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

