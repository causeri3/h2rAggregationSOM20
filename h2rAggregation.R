                                                                    ######################
                                                                    ########README########
                                                                    ######################

                      #If the survey tool changes (names of variables, or new variables or choices), one will have to double-check:
                                                      ### New variables without skip-logic in "no_skip_list"
                                                      ### New & missing variables in "select_single" list

########LOAD PACKAGES#####################################################################################################################################################################################################################

#remotes::install_github("elliottmess/butter")
#!!!#fix for butteR:
#devtools::install_version("srvyr", version = "0.3.8", repos = "http://cran.us.r-project.org")

library(dplyr)
library(butteR)
library(sf)
library(srvyr)

########IMPORTS###########################################################################################################################################################################################################################
setwd("C:/Users/Vanessa Causemann/Desktop/REACH/RStuff/Github/h2rAggregationSOM20")

#import data set
df<-read.csv("h2r_Oct_2020_consolidated_mog_baidoa_clean.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," "))              #import with blanks being NA's

#spatial data folder_path
admin_gdb<- "inputs/gis_data/boundaries"

#area data
itemset<-read.csv("inputs/2020_01/itemsets.csv", stringsAsFactors = FALSE)
colnames(itemset)<-paste0("calc.",colnames(itemset))

#import csv with NC dependencies defined
NC_depend<-read.csv("inputs/h2r_NC_dep_Oct_2020.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," "))                       #import with blanks being NA's
NC_depend<-NC_depend[!is.na(NC_depend[,1]),]                                                                                                       #remove rows that sneaked in by Excel

#import csv with SL dependencies defined
SL_depend<-read.csv("inputs/h2r_SL_dep_Oct_2020.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," "))                       #import with blanks being NA's
SL_depend<-SL_depend[!is.na(SL_depend[,1]),]                                                                                                       #remove rows that sneaked in by Excel

##########################################################################################################################################################################################################################################

#remove funky duplicates
doublicates <- grep("[.]1", names(df), value = TRUE)
df<-df[-which(names(df)%in%doublicates)]

########SKIP LOGIC########################################################################################################################################################################################################################

#fill in variables that have no skiplogic
no_skip_list<-c('base',
                'enum_code',
                'idp_site',
                'consent',
                'covid_note',
                'h2r_notice',
                'info_reg',
                'when_left_prev',
                'how_long_stay',
                'still_inhabited',
                'visit_lastmonth',
                'district_info',
                'info_settlement')

#get index for no skip logic variables
index_no_skip<-which(names(df)%in%no_skip_list)   

#make all NA's to "SL", except variables without skip logic
df[-index_no_skip][is.na(df[-index_no_skip])] <- "SL"

########RECODE VALUES IN DATA SET#########################################################################################################################################################################################################

df[ df == "dontknow" ] <- NA

#create variables for analysis

df <- df  %>%  
  mutate(health_workers_available = case_when((how_often_provide_health =="once_a_week") ~ "yes",
                                              how_often_provide_health == "2_3_times_month" ~ "yes",
                                              how_often_provide_health == "once_a_month" ~ "yes",
                                              how_often_provide_health == "less_frequently" ~ "yes",
                                              TRUE ~ "no")) %>%
  mutate(dam_shelter = case_when((dam_shelters_reason == "flooding") ~ "yes",
                                 dam_shelters_reason == "conflict_looting" ~ "yes",
                                 dam_shelters_reason == "fire" ~ "yes",
                                 TRUE ~ "no")) %>%
  
  mutate(education_bar = ifelse(education_bar_boys == education_bar_girls,education_bar_boys, "NC"))

########GEOSPATIAL PREPARATION############################################################################################################################################################################################################

#spatial files regional, district, 10km hex, 6.7km hex and settlements files

adm1<- st_read(admin_gdb,"Regional_boundary")
adm1<-st_transform(adm1,crs=4326)

adm2<- st_read(admin_gdb,"District_boundary" )
adm2<-st_transform(adm2,crs=4326)

#hex_10km <- st_read(admin_gdb,"SOM_H2R_Hex_Grid" )
#hex_10km <-st_transform(hex_10km,crs=4326)

#hex_6.7km <- st_read(admin_gdb,"Somalia_Hexagons" )
#hex_6.7km <-st_transform(hex_6.7km,crs=4326)

hex_400km <- st_read(admin_gdb,"Somalia_Hexagons_400" )
hex_400km <-st_transform(hex_400km,crs=4326)

som_settlements <- st_read(admin_gdb,"Somalia_Setlements_Update_2702" )
som_settlements <-st_transform(som_settlements,crs=4326)

#create a new column that combines what was mapped as other and has nearest settlement given, keep only data set where both columns and records have values
df <- df %>% filter(!is.na(info_settlement)) %>%  mutate(finalsettlement= ifelse(info_settlement=="other",info_set_oth_near,info_settlement))

#join with the settlement data as some districts are blank if chosen near settlement
names(itemset)[names(itemset) == "calc.name"] <- "finalsettlement"
item_geo <- itemset %>%  select(finalsettlement,calc.district,calc.region)
item_geo <- distinct(item_geo,finalsettlement, .keep_all= TRUE)
df <- left_join(df,item_geo, by = "finalsettlement")

#########AGGREGATE FUNCTION###############################################################################################################################################################################################################

#calculate mode, while outputting NC (No consensus) if we don't have a clear winner. While excluding and keeping distinction between SL (skip-logic) and NA

AoK <- function(x) {
  ux <- unique(x[x!=is.na(x) & x!="SL"])                                                           #Exclude SL and NA
  #if (length(x[x!=is.na(x) & x!="SL"])== 1)                                                       #approach to check which variables have only 1 answer
  #{return("only_1_answer")}
  #else{
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {                  #if more than one mode -> NC
    return("NC")
  }
  else {                  
    if (length(ux)!=0){                                                                            #otherwise and if not only NA or SL -> return the mode
      ux[which.max(tabulate(match(x, ux)))]                                                     
    }
    else {if ("SL" %in%  x){                                                                       #otherwise if containing SL -> SL , if not -> NA
      return("SL")
    }
      else{
        return(NA)
      }
    }
 # }
  }
}

#########SORT VARIABLES INTO DUMMIES FROM MULTIPLE REPONSES / SINGLE RESPONSES / NOT NEEDED ONES######################################################################################################################################

essential_col <- c("calc.region","calc.district","finalsettlement")

#select multiple variables columns and change the 0 to "no" and 1 to "yes" to be used in the non-consensus stage later
select_multiple <- grep("[.]", names(df), value = TRUE)
select_multiple<-select_multiple[select_multiple!="calc.region"& select_multiple!="calc.district"]
select_multiple_df<- select(df,calc.region,calc.district,finalsettlement, contains(select_multiple))
select_multiple_df[select_multiple_df==0]<- "no"
select_multiple_df[select_multiple_df==1]<- "yes"

#list with single choice variables (add on here, if check_these in the main script gives you variables which should not be missing)
select_single <- c("consent", "base","ppl_no_land_tenure", "depart_return_safe", "freedommov_day","freedommov_night","info_settlement","idp_proportion_settlem","idp_arrived_from", 
                   "idp_arrived_from_reg", "idp_arrived_from_district","hc_push_main", "hc_push_second", "access_market", "market_region", "market_district", "market_settlement",
                   "distance_to_market", "food_situation", "food_source", "health_issues", "distance_clinic","region_clinic", "district_clinic", "settlement_clinic",
                   "idp_host_relationships","ppl_no_land_tenure","access_healthservices", "land_tenure_form", "depart_return_safe", "freedommov_day", "freedommov_night",
                   "shelter_type", "dam_shelters_reason", "shelters_not_rebuilt", "shelt_not_rebuilt_why", "mainsource_water", "gettingwater_time", "people_using_latrines",
                   "waste_disposal", "time_to_school", "education_bar_girls", "education_bar_boys","info_personsource", "road_connection_y_n", "food_price_changed", "nfi_price_changed", 
                   "soap_price_changed","how_often_provide_health", "idp_new_arrivals","skip_meals","unaccompanied_child_y_n", "cases_eviction", "ppl_no_shelter", "surfacewater_drinking",
                   "water_sufficient_lastmonth","water_seasonal", "stagnant_water_near", "info_ngo_y_n", "ngo_support_y_n", "plane_connection_y_n","particip_again", "handwashing_access",  
                   "covid_information", "health_workers_available", "dam_shelter", "education_bar","missing_children","unaccompanied_child_y_n", "covid_measures", "uac_where_live",
                   "women_unsafeplaces","women_services", "women_protincid", "caretaker_who","primary_reason_moved","secondary_reason_moved","pwd_left_behind","visit_lastmonth"
)

#get all other variables to exclude them
select_other <- grep("_oth", names(df), value = TRUE)

#fix wrongly selected ones
select_other_wrong <- grep("[.]", select_other, value = TRUE)
select_other<-select_other[select_other %in% select_other_wrong ==FALSE]

#variables to ignore in check_these list
not_needed_columns <- c( "start", "end","today", "deviceid","available_health_services","barriers_health", "barriers_usetoilets","conflict_causes","conflict_mediators",
                         "coping_food_strat","education_available", "idp_arrived_reason", "idp_pull_factors", "incidents_wh_leaving","info_barriers", "info_mainsource", 
                         "lack_food_reasons","left_behind_who", "livelihood_activ", "main_radios", "market_goods", "ngo_support_type","noaccess_health","nomarket_why",
                         "protection_inc_location", "protection_incidents","contact_again", "X__version__", "X_id", "X_uuid","X_submission_time", "X_index", "idp_site", 
                         "info_reg", "district_info", "gender_ki", "hc_push_second_other", "info_personsource_other","sources_covid_informaiton", "other_covid_info_sources",
                         "settlement_info_001","X_validation_status", "enum_code",
                         select_other
)


#########AGGREGATE BY MODE OR DISPLAY TIE ("NC")######################################################################################################################################################################################

settlement_single_choice <- df %>%
  select(essential_col, select_single) %>%
  group_by_(.dots = c( "calc.region","calc.district","finalsettlement")) %>%
  summarise_all(funs(AoK))

settlement_multiple_choice <- select_multiple_df %>%
  select(essential_col, all_of(select_multiple)) %>%
  group_by_(.dots = c( "calc.region","calc.district","finalsettlement")) %>%
  summarise_all(funs(AoK))

#########JOIN GEOSPATIAL AND SURVEY DATA#####################################################################################################################################################################################################

ki_coverage <- df %>%
  select(calc.region,calc.district,finalsettlement, particip_again) %>%
  group_by(calc.region,calc.district,finalsettlement) %>%
  summarise(ki_num = length(particip_again))

analysis_df_list<-list(settlement_single_choice, settlement_multiple_choice)

settlement_data <-purrr::reduce(analysis_df_list, left_join)#, by= c("calc.district","calc.region", "finalsettlement"))

#combining settlement and county name for ArcGIS, also adding in a column for KI coverage
settlement_data <- settlement_data %>%
  ungroup() %>%
  mutate(D.ki_coverage = as.numeric(ki_coverage$ki_num))

#rearranging of columns in our new data set to be in the same order as in the original one
settlement_data <- settlement_data %>% select(order(match(names(settlement_data), names(df))))

##########NC- & SL-DEPENDENCY LOOPS################################################################################################################################################################################################################################

for (i in 1:dim(NC_depend)[1]){
  index_NC_depend<-which(names(settlement_data)%in%NC_depend[i,2:dim(NC_depend)[2]])
  index_NC_rows<- which(settlement_data[NC_depend[i,1]]=="NC")
  settlement_data[index_NC_rows,index_NC_depend]<-"NC"
}

for (i in 1:dim(SL_depend)[1]){
  index_SL_depend<-which(names(settlement_data)%in%SL_depend[i,3:dim(SL_depend)[2]])
  index_SL_rows<- which(settlement_data[SL_depend[i,1]]==SL_depend[i,2])
  settlement_data[index_SL_rows,index_SL_depend]<-"SL"
}

#########FURTHER GEOSPATIAL PREPARATION#################################################################################################################################################################################################

#only take areas with more than one KI reporting
settlement_data <- settlement_data %>% 
  select(base:consent,calc.region, calc.district,finalsettlement,D.ki_coverage,info_settlement:names(settlement_data)[length(settlement_data)-4])%>% 
  filter(D.ki_coverage > 1)

#check for missing columns
missing_columns<-colnames(df)[colnames(df)%in% colnames(settlement_data)==FALSE]
check_these<-missing_columns[missing_columns %in% not_needed_columns ==FALSE]

if(length(check_these>0)){
  print("WARNING you missed these:")
  check_these %>% dput()
}

#date to label export
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

write.csv(
  settlement_data,
  file = paste0("outputs/som_H2r__clean_data_20200901",today,".csv"),
  na = "",
  row.names = FALSE)

#join data to settlement shapefile
settlement_data$P_CODE <- settlement_data$finalsettlement
settlement_data$month <- "20200901"
som_settlements_data <- inner_join(som_settlements,settlement_data )
som_settlements_data <-st_join( som_settlements_data, hex_400km)
names(som_settlements_data)[names(som_settlements_data) == "GRID_ID"] <- "hex_4000km"

#settlement data with hexagons information
som_settlements_data <- som_settlements_data %>%
  select(OBJECTID_1,name,ADM1_NAME,ADM2_NAME,hex_4000km,base,consent,finalsettlement:names(settlement_data)[length(settlement_data)-2],geometry)

settlement_level <- som_settlements_data %>%   filter(!is.na(D.ki_coverage))

#########REFORMATTING FOR butteR::mean_proportion_table##################################################################################################################################################################################

#adding on "expand_fix" level in order to pass through butteR::mean_proportion_table (below). Some variables have only one level, for those an artificial one needs to be added; after passing through the function it will get removed
settlement_level<-as.data.frame(settlement_level)

index_multiple<-which(names(settlement_level)%in%select_multiple)
index_single<-which(names(settlement_level)%in%select_single)
index_loop<-c(index_multiple, index_single)

for (i in index_loop){
  settlement_level[,i] <- forcats::fct_expand(settlement_level[,i],c("expand_fix"))
}

#########GEOSPATIAL AND MEAN PROPORTION AGGREGATION##########################################################################################################################################################################################################

dfsvy_h2r_district <-srvyr::as_survey(settlement_level)

h2r_columns <- settlement_level %>% 
  select(visit_lastmonth:names(settlement_level)[length(settlement_level)-1], - contains(c(".other",".dontknow",".noresponse"))) %>% 
  colnames() %>% 
  dput()

#Region level aggregation-----------
# <-butteR::mean_proportion_table(design = dfsvy_h2r_district,                    
#                                           list_of_variables = h2r_columns,
#                                           aggregation_level = "ADM1_NAME",
#                                           round_to = 1,
#                                           return_confidence = FALSE,
#                                           na_replace = FALSE)

#District level aggregation ----------
district_h2r <-butteR::mean_proportion_table(design = dfsvy_h2r_district,
                                             list_of_variables = h2r_columns,
                                             aggregation_level = "ADM2_NAME",
                                             round_to = 2,
                                             return_confidence = FALSE,
                                             na_replace = FALSE)

District_summary <- settlement_level %>%  select(ADM2_NAME,ADM1_NAME,D.ki_coverage) %>%
  group_by(ADM2_NAME) %>%
  summarise(assessed_num = n(), ki_num=sum(D.ki_coverage) )

District_summary <- data.frame(District_summary) %>%  select("ADM2_NAME", "assessed_num" ,  "ki_num" )

som_settlements_summary <- som_settlements %>% select(ADM2_NAME,ADM1_NAME) %>%
  group_by(ADM2_NAME) %>%
  summarise(settlem_num = n())

som_settlements_summary <- data.frame(som_settlements_summary) %>%  select("ADM2_NAME", "settlem_num")

#join into one data set
analysis_df_list<-list(District_summary, som_settlements_summary,district_h2r)
district_level <-purrr::reduce(analysis_df_list, left_join)

#Grid level aggregation---------
hex_400_h2r <-butteR::mean_proportion_table(design = dfsvy_h2r_district,
                                            list_of_variables = h2r_columns,
                                            aggregation_level = "hex_4000km",
                                            round_to = 2,
                                            return_confidence = FALSE,
                                            na_replace = FALSE)


grid_summary_400km <- settlement_level %>%  select(hex_4000km,ADM1_NAME,D.ki_coverage) %>%
  group_by(hex_4000km) %>%
  summarise(assessed_num = n(), ki_num=sum(D.ki_coverage) )

grid_summary_400km <- data.frame(grid_summary_400km) %>%  select("hex_4000km", "assessed_num" ,  "ki_num" )
grid_400km <- inner_join(grid_summary_400km,hex_400_h2r, by="hex_4000km")

#Remove "." for use in ArcGIS -----
#Better to export these directly as shapefile but the data can be used in other platforms and a simple join with existing shapefiles will do

#Settlement level
settlement_level <- settlement_level %>% select(everything(), - contains(c(".other",".dontknow",".noresponse")))
names(settlement_level) <- gsub("\\.", "_", names(settlement_level))

#grid_level
grid_level <- grid_400km %>% select(everything(), - contains(c(".other",".dontknow",".noresponse", "expand_fix")))
names(grid_level) <- gsub("\\.", "_", names(grid_level))

#district_level
district_level <- district_level %>% select(everything(), - contains(c(".other",".dontknow",".noresponse", "expand_fix")))
names(district_level) <- gsub("\\.", "_", names(district_level))

#########EXPORT##########################################################################################################################################################################################################################
#date to label export
today <- Sys.Date()
today<-format(today, format="_%Y_%b_%d")

#export data sets on different area levels
write.csv(grid_level, paste0("outputs/Aggregation_hex_400km",today,".csv"), row.names=FALSE)
write.csv(district_level,paste0("outputs/Aggregation_district",today,".csv"), row.names=FALSE)
write.csv(settlement_level,paste0("outputs/Aggregation_settlement",today,".csv"), row.names=FALSE)

#export FS columns
grid_level_fs <- grid_level %>% 
  select(c( "hex_4000km" ,"ki_num","assessed_num", "food_price_changed_prices_increased", "education_bar_cost_stud",
            "access_healthservices_no", "health_workers_available_yes", "protection_incidents_none_no", "dam_shelter_yes", 
            "handwashing_access_no", "sources_covid_informaiton_mobile_network_operator_yes"))                                      
write.csv(grid_level_fs,paste0("outputs/fs_Aggreg_by_hex_400km",today,".csv"), row.names=FALSE)
