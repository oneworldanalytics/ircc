library(tidyverse)
library(jsonlite)
library(textclean)
library(lubridate)
library(countrycode)
library(places)
library(lensr) # not public
library(duckdb)
library(workflow) # not public

# Note on Tidy Data

# The aim is to parse the ircc json into tables following tidy data principles
# That means that data frames will typically be in long format with e.g. cols
# text, id, lang rather than in wide format with text appearing under cols for
# each language (id, en, es, fr etc.). Multiple values (e.g. multiple entities for
# the same IRCC) are not concatenated in a cell but appear on their own row with a shared id.
# This approach allows for easy summarisation and visualisation.

# Helper Functions --------------------------------------------------------

df_helper <- function(list, lang = NULL) {

  out <- tibble(text = list) %>%
    mutate(text = textclean::replace_html(text)) %>%
    mutate(text = str_squish(text)) %>%
    mutate(id = id) %>%
    mutate(lang = lang) %>%
    drop_na(text) %>%
    select(id, everything())
}

list_helper <- function(list) {

  out <- list %>%
    set_names(nm = id) %>%
    bind_rows(.id = "id")
}

# Read in Raw, Create Index -----------------------------------------------

# read in raw data
ircc <- jsonlite::fromJSON("/Users/pauloldham/Documents/workflow/data/all_govs_ircc.json", flatten = FALSE)

# establish an index to use with naming lists

id <- paste0("ircc_", 1:nrow(ircc))


# Create Character Col and List Col Sets ----------------------------------
# character/logical cols can be joined at the end
# list cols parsed below by funs

ircc_main <- ircc %>%
  select(!where(is.list)) %>%
  mutate(id = id) %>%
  janitor::clean_names() %>%
  mutate(year_of_issuance = lubridate::year(date_of_issuance)) %>%
  mutate(year_of_expiry = lubridate::year(date_of_expiry))

ircc_list <- ircc %>%
  select(where(is.list))

# Government --------------------------------------------------------------

# add country name and codes using the country code package
# add un regions on iso3 using the places package
# note the use of an internal package.

government <- tibble(government = ircc_list$government$identifier) %>%
  mutate(id = id) %>%
  mutate(country_code = str_to_upper(government)) %>%
  left_join(lensr::countrycode, by = c("country_code" = "iso2c")) %>%
  left_join(places::unregions, by = c("iso3c" = "iso3")) # 27 countries have issued irccs


# Title -------------------------------------------------------------------

# df stored in long format with text and lang.
# filter using lang

title_en <- tibble(title = ircc_list$title$en) %>%
  mutate(id = id) %>%
  mutate(lang = "en") %>%
  drop_na(title)

title_es <- tibble(title = ircc_list$title$es) %>%
  mutate(id = id) %>%
  mutate(lang = "es") %>%
  drop_na(title)

title_fr <- tibble(title = ircc_list$title$fr) %>%
  mutate(id = id) %>%
  mutate(lang = "fr") %>%
  drop_na(title)

title_ru <- tibble(title = ircc_list$title$ru) %>%
  mutate(id = id) %>%
  mutate(lang = "ru") %>%
  drop_na(title)

title <- bind_rows(title_en, title_es, title_fr, title_ru) %>%
  select(id, everything())

rm(title_en, title_es, title_fr, title_ru)

# referenceToNationalPermit -----------------------------------------------

# remove html junk
# replace curly quotes
# replace non-ascii (note: may affect different languages by editing words)
# ui_todo("review ampersands")

reference_permit_en <- textclean::replace_html(ircc_list$referenceToNationalPermit$en) %>%
  tibble(reference_to_national_permit = .) %>%
  mutate(lang = "en") %>%
  mutate(id = id) %>%
  drop_na(reference_to_national_permit) %>%
  mutate(reference_to_national_permit = str_squish(reference_to_national_permit)) %>%
  mutate(reference_to_national_permit = textclean::replace_curly_quote(reference_to_national_permit)) %>%
  mutate(reference_to_national_permit = textclean::replace_non_ascii(reference_to_national_permit)) %>%
  select(id, everything())
   # 3600 of 4816 are in English

# other langs go here

# entitiesToWhomPICGranted ------------------------------------------------

# There may be more than one entity per ircc id.
# How to obtain the named entity for the ircc entity id

pic_entities <- list_helper(ircc_list$entitiesToWhomPICGranted)

# subjectMatter -----------------------------------------------------------

# some languages spill into different cols (or input data entry errors?), could
# use detect lang fun in future
# for NER will need to drop_na and send to get entities

subject_matter_en <- df_helper(list = ircc_list$subjectMatter$en, lang = "en")
subject_matter_es <- df_helper(list = ircc_list$subjectMatter$es, lang = "es")
subject_matter_fr <- df_helper(list = ircc_list$subjectMatter$fr, lang = "fr")
subject_matter_ru <- df_helper(list = ircc_list$subjectMatter$ru, lang = "ru")

subject_matter <- bind_rows(subject_matter_en, subject_matter_es,
                            subject_matter_fr, subject_matter_ru)

rm(subject_matter_en, subject_matter_es,
   subject_matter_fr, subject_matter_ru)

# keywords ----------------------------------------------------------------
# may be more than one keywords per record
# this field is not uniform as identifiers
# in some cases it has gbif urls in it.

keywords <- list_helper(ircc_list$keywords)

# usages ------------------------------------------------------------------

usages <- list_helper(ircc_list$usages)


# usagesDescription -------------------------------------------------------

usages_description_en <- df_helper(list = ircc_list$usagesDescription$en, lang = "en")
usages_description_en <- df_helper(list = ircc_list$usagesDescription$en, lang = "en")
usages_description_es <- df_helper(list = ircc_list$usagesDescription$es, lang = "es")
usages_description_fr <- df_helper(list = ircc_list$usagesDescription$fr, lang = "fr")
usages_description_ru <- df_helper(list = ircc_list$usagesDescription$ru, lang = "ru")

usages_description <- bind_rows(usages_description_en, usages_description_es, usages_description_fr,
                                usages_description_ru)

rm(usages_description_en, usages_description_es, usages_description_fr,
   usages_description_ru)


# matInformation ----------------------------------------------------------

matinformation_en <- df_helper(list = ircc_list$matInformation$en, lang = "en")
matinformation_es <- df_helper(list = ircc_list$matInformation$es, lang = "es")
matinformation_fr <- df_helper(list = ircc_list$matInformation$fr, lang = "fr")
matinformation_ru <- df_helper(list = ircc_list$matInformation$ru, lang = "ru")

mat_information <- bind_rows(matinformation_en, matinformation_es,
                             matinformation_fr, matinformation_ru)

rm(matinformation_en, matinformation_es,
   matinformation_fr, matinformation_ru)

# providers ---------------------------------------------------------------

providers <- list_helper(list = ircc$providers)

# picInformation ----------------------------------------------------------

picinformation_en <- df_helper(list = ircc_list$picInformation$en, lang = "en")
picinformation_es <- df_helper(list = ircc_list$picInformation$es, lang = "es")
picinformation_fr <- df_helper(list = ircc_list$picInformation$fr, lang = "fr")
picinformation_ru <- df_helper(list = ircc_list$picInformation$ru, lang = "ru")

pic_information <- bind_rows(picinformation_en, picinformation_es,
                             picinformation_fr, picinformation_ru)

rm(picinformation_en, picinformation_es,
   picinformation_fr, picinformation_ru)

# taxonomies --------------------------------------------------------------

taxonomies <- list_helper(list = ircc_list$taxonomies)

# thirdPartyTransferCondition ---------------------------------------------

thirdparty_en <- df_helper(list = ircc_list$thirdPartyTransferCondition$en, lang = "en")
thirdparty_es <- df_helper(list = ircc_list$thirdPartyTransferCondition$es, lang = "es")
thirdparty_fr <- df_helper(list = ircc_list$thirdPartyTransferCondition$fr, lang = "fr")
thirdparty_ru <- df_helper(list = ircc_list$thirdPartyTransferCondition$ru, lang = "ru")

thirdparty <- bind_rows(thirdparty_en, thirdparty_es,
                         thirdparty_fr, thirdparty_ru)

rm(thirdparty_en, thirdparty_es,
   thirdparty_fr, thirdparty_ru)

# relevantInformation -----------------------------------------------------
# add test for exists/NULL to fun

relevantinfo_en <- df_helper(list = ircc_list$relevantInformation$en, lang = "en")
relevantinfo_es <- df_helper(list = ircc_list$relevantInformation$es, lang = "es")
relevantinfo_fr <- df_helper(list = ircc_list$relevantInformation$fr, lang = "fr")
#relevantinfo_ru <- subj(list = ircc_list$relevantInformation$ru, lang = "ru") # does not exist

relevant_information <- bind_rows(relevantinfo_en, relevantinfo_es,
                                  relevantinfo_fr)

rm(relevantinfo_en, relevantinfo_es,
   relevantinfo_fr)

# absCNA ------------------------------------------------------------------

abscna <- list_helper(list = ircc_list$absCNA$identifier) %>%
  select(-id) %>%
  pivot_longer(cols = starts_with("ircc"), names_to = "id", values_to = "abscna")

# picDocuments ------------------------------------------------------------
pic_documents <- list_helper(list = ircc_list$picDocuments)

# matDocuments ------------------------------------------------------------

mat_documents <- list_helper(list = ircc_list$matDocuments)

# specimens ---------------------------------------------------------------

specimens <- list_helper(list = ircc_list$specimens)

# permitFiles -------------------------------------------------------------

permit_files <- list_helper(list = ircc_list$permitFiles)

# amendmentsDescription ---------------------------------------------------
# presumably this was added in error as it has 1 entry.
# cross check to join to the amendments table
# need to trycatch to see if the lang elements exist or not
# then process the ones that do.

#amenddesc_en <- df_helper(list = ircc_list$amendmentsDescription$en, lang = "en") # NULL
amenddescs_es <- df_helper(list = ircc_list$amendmentsDescription$es, lang = "es")
#amenddesc_fr <- df_helper(list = ircc_list$amendmentsDescription$fr, lang = "fr") # NULL
#amenddesc_ru <- df_helper(list = ircc_list$amendmentsDescription$ru, lang = "ru") # NULL

amendments_description <- bind_rows(amenddescs_es)

rm(amenddescs_es)
# amendmentDescription ----------------------------------------------------

amenddesc_en <- df_helper(list = ircc_list$amendmentDescription$en, lang = "en") # NULL
amenddesc_es <- df_helper(list = ircc_list$amendmentDescription$es, lang = "es")
amenddesc_fr <- df_helper(list = ircc_list$amendmentDescription$fr, lang = "fr") # NULL
amenddesc_ru <- df_helper(list = ircc_list$amendmentDescription$ru, lang = "ru") # NULL

amendment_description <- bind_rows(amenddesc_en, amenddesc_es, amenddesc_fr,
                                   amenddesc_ru, amendments_description)

rm(amenddesc_en, amenddesc_es, amenddesc_fr,
   amenddesc_ru, amendments_description)

# relatedIRCCs ------------------------------------------------------------

related_irccs <- list_helper(list = ircc_list$relatedIRCCs)

# permitDescription -------------------------------------------------------

permitdesc_en <- df_helper(list = ircc_list$permitDescription$en, lang = "en")
permitdesc_es <- df_helper(list = ircc_list$permitDescription$es, lang = "es")
#permitdesc_fr <- subj(list = ircc_list$permitDescription$fr, lang = "fr") # NULL
permitdesc_ru <- df_helper(list = ircc_list$permitDescription$ru, lang = "ru")

permit_description <- bind_rows(permitdesc_en, permitdesc_es,
                                permitdesc_ru)

rm(permitdesc_en, permitdesc_es, permitdesc_ru)

# keywordOther ------------------------------------------------------------

keywordother_en <- df_helper(list = ircc_list$keywordOther$en, lang = "en")
keywordother_es <- df_helper(list = ircc_list$keywordOther$es, lang = "es")
keywordother_fr <- df_helper(list = ircc_list$keywordOther$fr, lang = "fr")
#keywordother_ru <- df_helper(list = ircc_list$keywordOther$ru, lang = "ru") # NULL

keyword_other <- bind_rows(keywordother_en, keywordother_es, keywordother_fr)

rm(keywordother_en, keywordother_es, keywordother_fr)

# relevantDocuments -------------------------------------------------------

relevant_documents <- list_helper(list = ircc_list$relevantDocuments)

# add entities file with taxonomy

# Extract Entities --------------------------------------------------------

# entity extraction is performed with a specialised ML model
# taxonomy matching is performed with a non public package

subject_matter_ents <- read_csv("data_raw/subject_matter_ents.csv") %>%
  lensr::lens_taxonomy(., taxonomy_path = "/Volumes/Public/Documents/workflowdata/data/taxonomy.qs")
usages_description_ents <- read_csv("data_raw/usages_description_ents.csv") %>%
  lensr::lens_taxonomy(., taxonomy_path = "/Volumes/Public/Documents/workflowdata/data/taxonomy.qs")


# Write to Duckdb ---------------------------------------------------------
# Duckdb is a very fast and popular database similar to postgres
# but column rather than row focused.
# For visualisation in tableau or similar
# follow the instructions for installing drivers here
# https://duckdb.org/docs/guides/data_viewers/tableau
# Note that the duckdb versions are not backwards compatible and
# the jdbc driver for macosx should match the duckdb version.

con <- dbconnect("ircc.db")

dbWriteTable(con, "abscna", abscna)
dbWriteTable(con, "amendment_description", amendment_description)
dbWriteTable(con, "government", government)
dbWriteTable(con, "ircc_main", ircc_main)
dbWriteTable(con, "keyword_other", keyword_other)
dbWriteTable(con, "keywords", keywords)
dbWriteTable(con, "mat_documents", mat_documents)
dbWriteTable(con, "mat_information", mat_information)
dbWriteTable(con, "permit_description", permit_description)
dbWriteTable(con, "permit_files", permit_files)
dbWriteTable(con, "pic_documents", pic_documents)
dbWriteTable(con, "pic_entities", pic_entities)
dbWriteTable(con, "pic_information", pic_information)
dbWriteTable(con, "providers", providers)
dbWriteTable(con, "reference_permit_en", reference_permit_en)
dbWriteTable(con, "related_irccs", related_irccs)
dbWriteTable(con, "relevant_documents", relevant_documents)
dbWriteTable(con, "relevant_information", relevant_information)
dbWriteTable(con, "specimens", specimens)
dbWriteTable(con, "subject_matter", subject_matter)
dbWriteTable(con, "subject_matter_ents", subject_matter_ents)
dbWriteTable(con, "taxonomies", taxonomies)
dbWriteTable(con, "thirdparty", thirdparty)
dbWriteTable(con, "title", title)
dbWriteTable(con, "usages", usages)
dbWriteTable(con, "usages_description", usages_description)
dbWriteTable(con, "usages_description_ents", usages_description_ents)

dbclose(con)
