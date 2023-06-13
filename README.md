## IRCC Test Set

The code in this repository is experimental. It reads in a json file of data on Internationally Recognised Certificates of Compliance (IRCC) from [the ABS Clearing House Mechanism](https://www.cbd.int/abs/theabsch.shtml#:~:text=The%20ABS%20Clearing%2DHouse%20is,resources%20along%20the%20value%20chain.) of the [United Nations Convention on Biological Diversity](https://www.cbd.int/). The IRCC section of the ABS Clearing House Mechanism is located [here](https://absch.cbd.int/en/search?schema=absPermit&currentPage=1) and lists 4,816 IRCCs as of the 12/06/2023.

Find a working visualisation of the data [here](https://public.tableau.com/app/profile/one.world.analytics/viz/ircc_testview/Overview?publish=yes). 
### Import Data by Country

The python import script is located in `scripts`. It retrieves all IRCC records issued by any government. 

Note that the script can take some time to run.

### Parse the return

There are a large number of tables created when the json is parsed. 

- abscna
- amendement description
- government
- ircc_main
- keyword_other
- keywords
- mat_documents
- mat_information
- permit_description
- permit_files
- pic_documents
- pic_entities
- pic_information
- providers
- reference_permit_en
- relevant_documents
- relevant_information
- specimens
- subject_matter
- subject_matter_ents
- taxonomies
- thirdparty
- title
- usages
- usages_description

### Duckdb

Datasets created by parsing are stored in duckdb (accessible through Python or R) as a lightweight but very powerful and fast db. You can learn more about duckdb [https://duckdb.org/](https://duckdb.org/). Note that to read the db in this repo (ircc.db) you must use the same version of the database (0.8.0) as they are not backward compatible (which is painful).
