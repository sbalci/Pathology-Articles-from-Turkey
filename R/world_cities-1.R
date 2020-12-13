# load("~/psychiatry/.RData")

PsychArticles_Affiliation2 <- readRDS("~/psychiatry/data/pubmed/PsychArticles_Affiliation2.RDS")


PsychArticles_Affiliation3 <- PsychArticles_Affiliation2 %>%
  dplyr::filter(country == "") %>%
  dplyr::filter(Aff_number == "1" | Aff_number == lastlevel)


world_cities <- maps::world.cities %>%
  dplyr::pull(name) %>%
  stringr::str_c(., collapse = "|")



PsychArticles_Affiliation3$world_cities <- sapply(
  stringr::str_extract_all(
    string = PsychArticles_Affiliation3$Affiliation,
    pattern = world_cities
  ),
  toString
)

saveRDS(
  object = PsychArticles_Affiliation3,
  file = paste0("~/psychiatry/data/pubmed/PsychArticles_Affiliation3-", as.character(Sys.time()), ".RDS")
)
