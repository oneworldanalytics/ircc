import requests
import json

URL = {
    "document": "https://api.cbd.int/api/v2013/documents",
    "index": "https://api.cbd.int/api/v2013/index/select",
}
HEADERS = {
    "Accept": "application/json",
    "Content-Type": "application/json",
    "Charset": "UTF-8",
}

PARAMETERS = {
    "q": "realm_ss:abs AND schema_s:absPermit AND government_s:*",  # za for south africa, can add more filters like regions_ss, usages_ss dateOfExpiry_dt etc,
    "fl": "identifier_s,government_s",  # can add more fields if required, if the fl field is skipped, all fields will be returned
    "rows": "10000",  # the result set contains number of records found if pagination is required.
    "start": "0",
}
DOCUMENTS = []

def load_records():
    """
    Loads index and documents from the API, uses the index to get the documents.
    """
    response = requests.get(URL["index"], params=PARAMETERS, headers=HEADERS)
    if response.status_code != 200:
        print("Error getting the index", response.status_code)
    else:
        data = json.loads(response.text)
        index_documents = data["response"].get("docs")
        for element in index_documents:
            document_response = requests.get(URL["document"] + "/" + element["identifier_s"] + "/info")

            if document_response.status_code != 200:
                print("Error getting the document", document_response.status_code)
            else:
                data = json.loads(document_response.text)
                document = data["body"]
                DOCUMENTS.append(document)
        # This part saves the records to a json file all_ircc.json
        with open("./all_ircc.json", "w", encoding='utf-8') as outfile:
            json.dump(DOCUMENTS, outfile, indent=4)


load_records()
