##### DOWNLOAD WATERBODY DATA HOSTED ON LONDON DATASTORE

thames = search_names(string="London", column="MC")

lark_hawstead<-get_status(ea_name="GB106039023260", column="WBID")

lark_OC_rivers<-get_status(ea_name="London", column="MC", startyr=2019, endyr=2019, type="River")
