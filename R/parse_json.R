library(tidyverse)


# Helper Functions --------------------------------------------------------

df_helper   <- function(list, lang = NULL) {

  out <- tibble(text = list) %>%
    mutate(text = textclean::replace_html(text)) %>%
    mutate(text = str_squish(text)) %>%
    mutate(ircc_index = ircc_index) %>%
    mutate(lang = lang) %>%
    drop_na(text) %>%
    select(ircc_index, everything())
}

list_helper <- function(list) {

  out <- list %>%
    set_names(nm = ircc_index) %>%
    bind_rows(.id = "ircc_index")
}


# read in raw data
ircc <- jsonlite::fromJSON("/Users/pauloldham/Documents/workflow/data/all_govs_ircc.json", flatten = FALSE)

# establish an index to use with naming lists

ircc_index <- paste0("ircc_", 1:nrow(ircc))

# cols that are not list objects

ircc_chr <- ircc %>%
  select(!where(is.list)) %>%
  mutate(ircc_index = ircc_index)

ircc_list <- ircc %>%
  select(where(is.list))


# Government --------------------------------------------------------------

government <- tibble(government = ircc_list$government$identifier) %>%
  mutate(ircc_index = ircc_index) %>%
  mutate(country_code = str_to_upper(government)) %>%
  left_join(lensr::countrycode, by = c("country_code" = "iso2c")) %>%
  left_join(places::unregions, by = c("iso3c" = "iso3")) # 27 countries have issued irccs


# Title -------------------------------------------------------------------

title_en <- tibble(title = ircc_list$title$en) %>%
  mutate(ircc_index = ircc_index) %>%
  mutate(lang = "en") %>%
  drop_na(title)

title_es <- tibble(title = ircc_list$title$es) %>%
  mutate(ircc_index = ircc_index) %>%
  mutate(lang = "es") %>%
  drop_na(title)

title_fr <- tibble(title = ircc_list$title$fr) %>%
  mutate(ircc_index = ircc_index) %>%
  mutate(lang = "fr") %>%
  drop_na(title)

title_ru <- tibble(title = ircc_list$title$ru) %>%
  mutate(ircc_index = ircc_index) %>%
  mutate(lang = "ru") %>%
  drop_na(title)

title <- bind_rows(title_en, title_es, title_fr, title_ru) %>%
  select(ircc_index, everything())

rm(title_en, title_es, title_fr, title_ru)

# referenceToNationalPermit -----------------------------------------------

reference_permit_en <- textclean::replace_html(ircc_list$referenceToNationalPermit$en) %>%
  tibble(reference_to_national_permit = .) %>%
  mutate(lang = "en") %>%
  mutate(ircc_index = ircc_index) %>%
  drop_na(reference_to_national_permit) %>%
  mutate(reference_to_national_permit = str_squish(reference_to_national_permit)) %>%
  mutate(reference_to_national_permit = textclean::replace_curly_quote(reference_to_national_permit)) %>%
  mutate(reference_to_national_permit = textclean::replace_non_ascii(reference_to_national_permit)) %>%
  select(ircc_index, everything())
   # 3600 of 4816 are in English

# other langs go here

# may need more cleaning for ampersands. Create a clean up fun once established

# entitiesToWhomPICGranted ------------------------------------------------

# there is an @1 sign at the end. Is that supposed to be there?
# note there may be more than one entity per record. Preserved
# through the ircc_index below.

pic_entities <- ircc_list$entitiesToWhomPICGranted %>%
  set_names(nm = ircc_index) %>%
  bind_rows(.id = "ircc_index")


# subjectMatter -----------------------------------------------------------
# some languages spill into different cols (or data entry error?)
# for NER will need to drop_na and send to get entities

subj   <- function(list, lang = NULL) {

  out <- tibble(text = list) %>%
    mutate(text = textclean::replace_html(text)) %>%
    mutate(text = str_squish(text)) %>%
    mutate(ircc_index = ircc_index) %>%
    mutate(lang = lang) %>%
    drop_na(text) %>%
    select(ircc_index, everything())
}

subject_matter_en <- subj(list = ircc_list$subjectMatter$en, lang = "en")
subject_matter_es <- subj(list = ircc_list$subjectMatter$es, lang = "es")
subject_matter_fr <- subj(list = ircc_list$subjectMatter$fr, lang = "fr")
subject_matter_ru <- subj(list = ircc_list$subjectMatter$ru, lang = "ru")

subject_matter <- bind_rows(subject_matter_en, subject_matter_es,
                            subject_matter_fr, subject_matter_ru)

rm(subject_matter_en, subject_matter_es,
   subject_matter_fr, subject_matter_ru)

# keywords ----------------------------------------------------------------
# may be more than one keywords per record
# this field is not uniform as identifiers
# in some cases it has gbif urls in it.

keywords <- ircc_list$keywords %>%
  set_names(nm = ircc_index) %>%
  bind_rows(.id = "ircc_index")


# usages ------------------------------------------------------------------

usages <- ircc_list$usages %>%
  set_names(nm = ircc_index) %>%
  bind_rows(.id = "ircc_index")


# usagesDescription -------------------------------------------------------

usages_description_en <- subj(list = ircc_list$usagesDescription$en, lang = "en")
usages_description_es <- subj(list = ircc_list$usagesDescription$es, lang = "es")
usages_description_fr <- subj(list = ircc_list$usagesDescription$fr, lang = "fr")
usages_description_ru <- subj(list = ircc_list$usagesDescription$ru, lang = "ru")

usages_description <- bind_rows(usages_description_en, usages_description_es, usages_description_fr,
                                usages_description_ru)

rm(usages_description_en, usages_description_es, usages_description_fr,
   usages_description_ru)


# matInformation ----------------------------------------------------------

matinformation_en <- subj(list = ircc_list$matInformation$en, lang = "en")
matinformation_es <- subj(list = ircc_list$matInformation$es, lang = "es")
matinformation_fr <- subj(list = ircc_list$matInformation$fr, lang = "fr")
matinformation_ru <- subj(list = ircc_list$matInformation$ru, lang = "ru")

mat_information <- bind_rows(matinformation_en, matinformation_es,
                             matinformation_fr, matinformation_ru)

rm(matinformation_en, matinformation_es,
   matinformation_fr, matinformation_ru)

# providers ---------------------------------------------------------------

providers <- list_helper(list = ircc$providers)

# picInformation ----------------------------------------------------------

picinformation_en <- subj(list = ircc_list$picInformation$en, lang = "en")
picinformation_es <- subj(list = ircc_list$picInformation$es, lang = "es")
picinformation_fr <- subj(list = ircc_list$picInformation$fr, lang = "fr")
picinformation_ru <- subj(list = ircc_list$picInformation$ru, lang = "ru")

pic_information <- bind_rows(picinformation_en, picinformation_es,
                             picinformation_fr, picinformation_ru)

rm(picinformation_en, picinformation_es,
   picinformation_fr, picinformation_ru)

# taxonomies --------------------------------------------------------------

taxonomies <- list_helper(list = ircc_list$taxonomies)

# thirdPartyTransferCondition ---------------------------------------------

thirdparty_en <- subj(list = ircc_list$thirdPartyTransferCondition$en, lang = "en")
thirdparty_es <- subj(list = ircc_list$thirdPartyTransferCondition$es, lang = "es")
thirdparty_fr <- subj(list = ircc_list$thirdPartyTransferCondition$fr, lang = "fr")
thirdparty_ru <- subj(list = ircc_list$thirdPartyTransferCondition$ru, lang = "ru")

thirdparty <- bind_rows(thirdparty_en, thirdparty_es,
                         thirdparty_fr, thirdparty_ru)

rm(thirdparty_en, thirdparty_es,
   thirdparty_fr, thirdparty_ru)

# relevantInformation -----------------------------------------------------

relevantinfo_en <- subj(list = ircc_list$relevantInformation$en, lang = "en")
relevantinfo_es <- subj(list = ircc_list$relevantInformation$es, lang = "es")
relevantinfo_fr <- subj(list = ircc_list$relevantInformation$fr, lang = "fr")
#relevantinfo_ru <- subj(list = ircc_list$relevantInformation$ru, lang = "ru") # does not exist

relevant_information <- bind_rows(relevantinfo_en, relevantinfo_es,
                                  relevantinfo_fr)

rm(relevantinfo_en, relevantinfo_es,
   relevantinfo_fr)

# absCNA ------------------------------------------------------------------

abscna <- list_helper(list = ircc_list$absCNA$identifier) %>%
  t() %>%
  rownames_to_column()


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

#amenddesc_en <- subj(list = ircc_list$amendmentsDescription$en, lang = "en") # NULL
amenddescs_es <- subj(list = ircc_list$amendmentsDescription$es, lang = "es")
#amenddesc_fr <- subj(list = ircc_list$amendmentsDescription$fr, lang = "fr") # NULL
#amenddesc_ru <- subj(list = ircc_list$amendmentsDescription$ru, lang = "ru") # NULL

amendments_description <- bind_rows(amenddescs_es)

rm(amenddescs_es)
# amendmentDescription ----------------------------------------------------

amenddesc_en <- subj(list = ircc_list$amendmentDescription$en, lang = "en") # NULL
amenddesc_es <- subj(list = ircc_list$amendmentDescription$es, lang = "es")
amenddesc_fr <- subj(list = ircc_list$amendmentDescription$fr, lang = "fr") # NULL
amenddesc_ru <- subj(list = ircc_list$amendmentDescription$ru, lang = "ru") # NULL

amendment_description <- bind_rows(amenddesc_en, amenddesc_es, amenddesc_fr,
                                   amenddesc_ru, amendments_description)

rm(amenddesc_en, amenddesc_es, amenddesc_fr,
   amenddesc_ru, amendments_description)

# relatedIRCCs ------------------------------------------------------------

related_irccs <- list_helper(list = ircc_list$relatedIRCCs)

# permitDescription -------------------------------------------------------

permitdesc_en <- subj(list = ircc_list$permitDescription$en, lang = "en")
permitdesc_es <- subj(list = ircc_list$permitDescription$es, lang = "es")
#permitdesc_fr <- subj(list = ircc_list$permitDescription$fr, lang = "fr") # NULL
permitdesc_ru <- subj(list = ircc_list$permitDescription$ru, lang = "ru")

permit_description <- bind_rows(permitdesc_en, permitdesc_es,
                                permitdesc_ru)

rm(permitdesc_en, permitdesc_es, permitdesc_ru)

# keywordOther ------------------------------------------------------------

keywordother_en <- subj(list = ircc_list$keywordOther$en, lang = "en")
keywordother_es <- subj(list = ircc_list$keywordOther$es, lang = "es")
keywordother_fr <- subj(list = ircc_list$keywordOther$fr, lang = "fr")
#keywordother_ru <- subj(list = ircc_list$keywordOther$ru, lang = "ru") # NULL

keyword_other <- bind_rows(keywordother_en, keywordother_es, keywordother_fr)

rm(keywordother_en, keywordother_es, keywordother_fr)

# relevantDocuments -------------------------------------------------------

relevant_documents <- list_helper(list = ircc_list$relevantDocuments)


# Compile

government
title
reference_permit_en
pic_entities
subject_matter
keywords
usages
usages_description
mat_information
providers
pic_information
taxonomies
thirdparty
relevant_information
abscna
pic_documents
mat_documents
specimens
permit_files
#amendments_description
amendment_description
related_irccs
permit_description
keyword_other
relevant_documents


