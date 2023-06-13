# get entities from relevant sections
# note this code uses an internal package for biodiversity entities.
# add taxonomy (from GBIF)

library(tidyverse)
library(workflow)

model = model
taxonomy_path = taxonomy_path # (join is on lowercase entities)

subject_matter %>%
  select(text, id, lang) %>%
  write_csv("data/subject_matter.csv")

subject_ents <- get_entities(path = "data/subject_matter.csv", model = model, dest = "data/subject_matter_ents.csv")

mat_information %>%
  select(text, id, lang) %>%
  write_csv("data/mat_information.csv")

mat_information_ents <- get_entities(path = "data/mat_information.csv", model = model, dest = "data/mat_information_ents.csv")

usages_description %>%
  select(text, id, lang) %>%
  write_csv("data/usages_description.csv")

usages_description_ents <- get_entities(path = "data/usages_description.csv", model = model, dest = "data/usages_description_ents.csv")

# Add taxonomy ------------------------------------------------------------

subject_ents_taxa <- lensr::lens_taxonomy(subject_ents, taxonomy_path = taxonomy_path)
mat_information_taxa <- lensr::lens_taxonomy(mat_information_ents, taxonomy_path = taxonomy_path)
usages_description_taxa <- lensr::lens_taxonomy


