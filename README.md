## IRCC Test Set

The code in this repository is experimental. It reads in a json file of data on Internationally Recognised Certificates of Compliance (IRCC) from [the ABS Clearing House Mechanism](https://www.cbd.int/abs/theabsch.shtml#:~:text=The%20ABS%20Clearing%2DHouse%20is,resources%20along%20the%20value%20chain.) of the [United Nations Convention on Biological Diversity](https://www.cbd.int/). The IRCC section of the ABS Clearing House Mechanism is located [here](https://absch.cbd.int/en/search?schema=absPermit&currentPage=1) and lists 4,816 IRCCs as of the 12/06/2023.

You can find an interactive working visualisation of the data using Tableau [here](https://public.tableau.com/app/profile/one.world.analytics/viz/ircc_testview/Overview?publish=yes). 

### Get the Data

Data is obtained through a Python version of a script originally written by Blaise Fonesca and adapted for Python by Reuben Nyaribari. That script is found in `scripts/fetch_all_ircc.py`). The script can be modified in line 15 to fetch data for an individual country by replacing the wildcard * with a two letter country code e.g. za. 

### Parse the return

There are a large number of tables created when the json is parsed. The json is parsed in R using `R/parse_json.R`. Note that some of the functions rely on internal packages (e.g. for named entity recognition and taxonomy matching) and so are not fully reproducbile. 

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

### CSV

csv versions of the main files are located in the `data` folder. 

### Raw Json

The raw json return from the 7th of June 2023 is found in `data_raw/all_govs.json`.

### Known Issues

The CBD Secretariat system returns identifiers for some fields (such as organisations who have received the IRCC) and it is unclear at present how to resolve those identifiers. That will be a focus of future work. 
