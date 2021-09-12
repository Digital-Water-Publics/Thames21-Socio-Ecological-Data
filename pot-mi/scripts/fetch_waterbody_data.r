##### DOWNLOAD WATERBODY DATA HOSTED ON LONDON DATASTORE

ea_zip = "https://data.london.gov.uk/download/water-quality-london-rivers-other-waterbodies/ac8f6c22-f742-40a1-bad7-1040bec661bb/WFD_London.zip"
download.file(ea_zip, "data/ea_waterbody_data")
unzip("data/ea_waterbody_data",exdir = "data/waterbody")
