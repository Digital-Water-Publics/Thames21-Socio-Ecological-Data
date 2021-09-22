##### DOWNLOAD WATERBODY DATA HOSTED ON VIA CDE package

library(dplyr)
library(tidyr)
library(stringr)


aa = read.csv("../../../../Downloads/200921_rivers_list.csv")
aa = as.data.frame(str_split_fixed(aa$Name.Query., ";", 2)) # split into two columns
aa$V1 = gsub('[[:digit:]]+', '', aa$V1) # remove digits
aa$V1 = aa$V1 %>% str_remove_all("[[:punct:]]") %>% str_trim("both") # remove punct and trim

aa$V2 = sub(";x","",aa$V2)
aa$V2 = aa$V2 %>% str_remove_all("[[:punct:]]") %>% str_trim("both")

aa = aa %>%
  rename(label = V1) %>%
  rename(mine_query = V2)

al = left_join(thames,aa)

tt = as.data.frame(unique(subset(aa$mine_query, str_count(aa$mine_query,"\\S+") == 1)))


#OPTIONS
geo = read_sf("../../../../Downloads/reasons_for_not_achieving_good/reasons_for_not_achieving_good_RBD_6.csv")
colnames(geo)[which(names(geo) == "water body")] = "label"
colnames(geo)[which(names(geo) == "Water body id")] = "wbid"
geo = geo %>% select(wbid,label)
geo = unique(geo)

waterbodies = merge(geo,aa)

select(label,wbid,mine_query)





#get thames river catchment data

thames = search_names(string="Thames", column="RBD") %>% rename(label = name)
waterbodies= waterbodies %>% rename(label = name)

thames_spatial = left_join(waterbodies,geo) %>% st_as_sf()

options(al=1)
plot(al)

tmap_mode("view")
tm_shape(al) + tm_fill("MC", style = "cat") + tmap_options(check.and.fix = TRUE)

# use old sf file
mapbox = read_sf("mapbox/mapbox_primary.shp")

mapbox_min = mapbox %>% distinct(watrbdy,WtrbdID)

unique(sf::st_geometry_type(thames_spatial))
