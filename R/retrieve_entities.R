# get entities from relevant sections

library(tidyverse)
library(workflow)

subject_matter %>%
  select(text, id, lang) %>%
  write_csv("data/subject_matter.csv")

subject_ents <- get_entities(path = "data/subject_matter.csv", model = "en_spacytaxa", dest = "data/subject_matter_ents.csv")

mat_information %>%
  select(text, id, lang) %>%
  write_csv("data/mat_information.csv")

mat_information_ents <- get_entities(path = "data/mat_information.csv", model = "en_spacytaxa", dest = "data/mat_information_ents.csv")

usages_description %>%
  select(text, id, lang) %>%
  write_csv("data/usages_description.csv")

usages_description_ents <- get_entities(path = "data/usages_description.csv", model = "en_spacytaxa", dest = "data/usages_description_ents.csv")

# compile entities (source col)
# join to taxonomy
# Create tableau workbook
# Create dashboard views

subject_ents_taxa <- lensr::lens_taxonomy(subject_ents, taxonomy_path = "/Volumes/Public/Documents/workflowdata/data/taxonomy.qs")
mat_information_taxa <- lensr::lens_taxonomy(mat_information_ents, taxonomy_path = "/Volumes/Public/Documents/workflowdata/data/taxonomy.qs")
usages_description_taxa <- lensr::lens_taxonomy


