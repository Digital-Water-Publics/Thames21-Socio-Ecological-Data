####
####
#### AIM: fetch waterbody classificatoin for different waterbodies
####
####

#### READ DATA
wbids = read.csv("data/ea_wbids.csv")

#### FUNCTION TO GET SF FEATURES OF WATERBODY
get_wb_classification = function(string, #### STRING = NAME OF CLASSFICATION AREA
                     column)
  #### COLUMN  = CLASSIFICATION TYPE E.G. OC | MC | RBD
{
  #### LOGICAL OPERATOR FOR RIVER MINE
  if (column == "OC") {
    wb = wbids %>% filter(OC == string)
  } # OPERATIONAL CATCHMENT
  if (column == "MC") {
    wb = wbids %>% filter(MC == string)
  } # MANAGMENT CATCHMENT
  if (column == "RBD") {
    wb = wbids %>% filter(RBD == string)
  } # RIVER BASIN DISTRICT

  #### LOOP THROUGH GEOJSON DOWNLOAD
  suppressWarnings(for (i in 1:nrow(wb)) {
    ##### EA CATCHMNET API CALL
    url = "https://environment.data.gov.uk/catchment-planning/WaterBody/"
    notation = wb$WBID[i]
    download_url = paste0(url, notation, "/classifications.csv")

    #### SET OUTPUT PATH
    river_wbid = wb$WBID[i]
    path = "data/river_sf/"
    river_output = paste0(path, river_wbid, ".csv")

    #### DOWNLOOAD FILE, AT LEAST TRY TO
    tryCatch(
      expr = {
        download.file(download_url, river_output)
      },
      error = function(e) {
        message("Unable to download River Please check column and string are correct")
      }
    )

    river_sf = read.csv(river_output) %>%  filter(Classification.Item == "Ecological") %>% filter(Year == 2019)

    wb$status[i] = river_sf$Status[1]
    wb$year[i] = river_sf$Year[1]

    #### REMOPVE DOWNLOADED FILE
    file.remove(river_output)
  })
  return(wb)
}

# test function
wb_class = get_wb_classification(string = "Thames",column = "RBD")
