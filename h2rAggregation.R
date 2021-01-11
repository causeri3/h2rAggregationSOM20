
#_______________________________________________________________________________________________________________________________________________________________#
#_______________________________________________________________________________________________________________________________________________________________#
#                                                                                                                                                               #
#                                                                      ######################                                                                   #
#                                                                      ########README########                                                                   #
#                                                                      ######################                                                                   #
#_______________________________________________________________________________________________________________________________________________________________#
#                                                                                                                                                               #
#_________INPUTS:                                                                                                                                               #
#                                                                                                                                                               #
#               °Folder 'inputs':                                                                                                                               #
#                  - Clean data sets for month to be aggregated                                                                                                 #
#                   * e.g. 'SOM1901_H2R_Baidoa_Clean Data_November.csv' & 'SOM1901_H2R_Mogadishu_Clean Data_November.csv'                                       #
#                                                                                                                                                               #
#               °Folder 'inputs/dependencies_SL_NC':                                                                                                            #
#                  -  'h2r_NC_dep_Oct_2020.csv'                               [Non-Consensus dependencies]                                                      #
#                  -  'h2r_SL_dep_Oct_2020.csv'                               [Skip-Logic dependencies]                                                         #
#                                                                                                                                                               #
#                °Folder 'inputs/gis_data':                                                                                                                     #
#                   - 'itemsets.csv'                                          [geographical lists]                                                              #
#                   -  Folder: 'inputs/gis_data/boundaries'                   [various shapefiles with geographical boundaries]                                 #
#_______________________________________________________________________________________________________________________________________________________________#
#                                                                                                                                                               #
#________OUTPUTS:                                                                                                                                               #
#                                                                                                                                                               #
#               ° Folder "outputs"                                                                                                                              #
#                   - 'SOM1901_H2R__clean_data_agg_date01.csv'                [Back-up of clean and by mode aggregated data]                                    #                
#                                                                                                                                                               #
#                   - 'SOM1901_H2R_settlement_aggregation_agg_month.csv'      [Dataset aggregated to the settlement level]                                      #
#                     * Which is based on no less than 2 respondents per settlement,and includes settlements that are located ONLY within the target regions:   #
#                       Bay, Bakool, Gedo, Middle Shabelle, Lower Shabelle, Middle Juba and Lower Juba                                                          #
#                                                                                                                                                               #
#                   - 'SOM1901_H2R_hex_400km_agg_month.csv                    [Data set aggregated to the hexagon level that is used for producing the maps]    #
#                                                                                                                                                               #
#                   - 'SOM1901_H2R_hex_400km_market_locations_agg_month.csv'  [Market location at the hexagon level that is used for producing the maps]        #
#                                                                                                                                                               #
#                   [- 'SOM1901_aggregation_district_agg_month.csv'           [Data set aggregated to the district level, * at the moment hashtagged/inactive]] #
#                                                                                                                                                               #
#                   - SOM1901_H2R_hex_400km_FS_agg_month.csv                  [Specific variables aggregated to the hexagon level for Fact Sheet [FS] outputs]  #
#_______________________________________________________________________________________________________________________________________________________________#
#                                                                                                                                                               #
#_______INSTRUCTIONS:                                                                                                                                           #
#                                                                                                                                                               #
#               ° Check that you have the right version of the package 'srvyr' installed, see under LOAD PACKAGES                                               #
#                                                                                                                                                               #               
#               ° Change working directory (setwd) and add file names for clean data Baidoa and Mogadishu under header IMPORTS                                  #  
#                   [both need to be saved as csv-file in the inputs folder]                                                                                    #
#                                                                                                                                                               #                 
#               ° If the survey tool changes (names of variables, or new variables), one will have to double-check:                                             #        
#                  -> In case those variables are without skip-logic, put in 'no_skip_list'                                                                     #
#                  -> In case those variables need to be excluded from aggregation, put in 'not_needed_columns' list                                            #
#                  -> Check if NC and SL dependency csv-files in "inputs/dependencies_SL_NC" are defined accordingly to the methodology                         #
#                       [The lists contains main and dependent questions:                                                                                       #
#                         Whenever the main question results in SL/NC during aggregation, the dependent questions become SL/NC.]                                #
#                                                                                                                                                               #
#               ° If more variables for FS outputs are needed: Add on in list "FS_vars" in the end of the script                                                #
#                                                                                                                                                               #
#               ° Output-naming assumes aggregation of clean data from one month before                                                                         #
#                  -> Change values "agg_date" and "agg_month" accordingly, i.e. change -1 to -2 if two months ago or -0 if same month etc.                     #
#                                                                                                                                                               #
#               ° In case target regions change, update files in 'inputs/gis_data' with help from GIS-Officer accordingly                                       #            
#_______________________________________________________________________________________________________________________________________________________________#
#_______________________________________________________________________________________________________________________________________________________________#


########LOAD PACKAGES###############################################################################################################################################################################

#remotes::install_github("elliottmess/butter")
#!!!#fix for butteR:
#devtools::install_version("srvyr", version = "0.3.8", repos = "http://cran.us.r-project.org")

library(dplyr)
library(butteR)
library(sf)
library(srvyr)
library(lubridate)

########FUNCTIONS###################################################################################################################################################################################

###########################
#####CHECK BEFORE JOIN#####
###########################

#check that columns are in line to guarantee flawless join
join_check <- function(x,y) {
  for (i in names(x)) {
    if (length(names(x))!=length(names(y))){
      print('WARNING: Different number of columns')
      break
    }
      if (!(i %in% names(y))) {
        print('WARNING: Column names are not identical')
        print('Check column number(s): ')
        print(which(!(names(x) %in% names(y))))
        print(which(!(names(y) %in% names(x)))) 
        break
      }  
      else if(i==tail(names(y),n=1)) {
        print('Check passed: Column names are identical')
      }
  }
}

#############################
#####DUPLICATES FUNCTION#####
#############################

#check for and remove duplicates
deal_with_dup<- function(x){
  doublicates <- grep("[.]1", names(x), value = TRUE)
  if (length(doublicates)==0)
  {print('No doublicates')}
  else{
    print('WARNING: Doublicates found and deleted: ')
    print(names(x)[which(names(x)%in%doublicates)])
    df<<-x[-which(names(x)%in%doublicates)]
  }
}

###############################
#####AGGREAGATION FUNCTION#####
###############################

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

########IMPORTS#####################################################################################################################################################################################

#import data set with blanks being NA's
df_baidoa<-read.csv("inputs/SOM1901_H2R_Baidoa_Clean Data_November.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," "))              
df_mogadishu<-read.csv("inputs/SOM1901_H2R_Mogadishu_Clean Data_November.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," "))        
#df<-read.csv("inputs/h2r_Oct_2020_consolidated_mog_baidoa_clean.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," "))              

#see if columns are identical
join_check(df_baidoa,df_mogadishu)

#if all good, join data sets by adding rows on
df<-rbind(df_baidoa,df_mogadishu)

#check for and remove duplicates
deal_with_dup(df)

#only keep observations from target regions
df<-df[df$info_reg=="bakool"|df$info_reg=="bay"|df$info_reg=="gedo"|df$info_reg=="lower_juba"|df$info_reg=="middle_juba"|df$info_reg=="lower_shabelle"|df$info_reg=="middle_shabelle",]

#spatial data folder_path
admin_gdb<- "inputs/gis_data/boundaries"

#area data
itemset<-read.csv("inputs/gis_data/itemsets.csv", stringsAsFactors = FALSE)
colnames(itemset)<-paste0("calc.",colnames(itemset))

#import csv with NC dependencies defined
NC_depend<-read.csv("inputs/dependencies_SL_NC/h2r_NC_dep_Oct_2020.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," "))               #import with blanks being NA's
NC_depend<-NC_depend[!is.na(NC_depend[,1]),]                                                                                                                  #remove rows that sneaked in by Excel

#import csv with SL dependencies defined
SL_depend<-read.csv("inputs/dependencies_SL_NC/h2r_SL_dep_Oct_2020.csv", stringsAsFactors = FALSE, dec=".", sep=",", na.strings=c("NA",""," "))               #import with blanks being NA's
SL_depend<-SL_depend[!is.na(SL_depend[,1]),]                                                                                                                  #remove rows that sneaked in by Excel

########SKIP LOGIC##################################################################################################################################################################################

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

########RECODE VALUES IN DATA SET###################################################################################################################################################################

df[ df == "dontknow" ] <- NA
df[ df == "cleaned" ] <- NA

#create variables for analysis

df <- df  %>%  
  mutate(health_workers_available = case_when(how_often_provide_health =="once_a_week" ~ "yes",
                                              how_often_provide_health == "2_3_times_month" ~ "yes",
                                              how_often_provide_health == "once_a_month" ~ "yes",
                                              how_often_provide_health == "less_frequently" ~ "yes",
                                              TRUE ~ "no")) %>%
  
  mutate(dam_shelter = case_when(dam_shelters_reason == "flooding" ~ "yes",
                                 dam_shelters_reason == "conflict_looting" ~ "yes",
                                 dam_shelters_reason == "fire" ~ "yes",
                                 TRUE ~ "no")) %>%
  
  mutate(education_bar = ifelse(education_bar_boys == education_bar_girls,education_bar_boys, "NC")) %>%
  
  mutate(shelters_not_rebuilt_dummy = case_when(shelters_not_rebuilt == "around_half" ~ "yes",
                                                       shelters_not_rebuilt == "less_half" ~ "no",
                                                       shelters_not_rebuilt == "more_half" ~ "yes",
                                                       shelters_not_rebuilt == "all" ~ "yes",
                                                       shelters_not_rebuilt == "SL" ~ "SL"
                                                       ))

########GEOSPATIAL PREPARATION######################################################################################################################################################################

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

#########SORT VARIABLES INTO DUMMIES FROM MULTIPLE REPONSES / SINGLE RESPONSES / NOT NEEDED ONES####################################################################################################

essential_col <- c("calc.region","calc.district","finalsettlement")

#select multiple variables columns and change the 0 to "no" and 1 to "yes" to be used in the non-consensus stage later
select_multiple <- grep("[.]", names(df), value = TRUE)
select_multiple<-select_multiple[select_multiple!="calc.region"& select_multiple!="calc.district"]
select_multiple_df<- select(df,calc.region,calc.district,finalsettlement, contains(select_multiple))
select_multiple_df[select_multiple_df==0]<- "no"
select_multiple_df[select_multiple_df==1]<- "yes"

#get all "other" variables to exclude them
select_other <- grep("_oth", names(df), value = TRUE)

#fix wrongly selected ones
select_other_wrong <- grep("[.]", select_other, value = TRUE)
select_other<-select_other[select_other %in% select_other_wrong ==FALSE]

#get all "note" variables to exclude them
select_note <- grep("_note", names(df), value = TRUE)

#variables to ignore in check_these list
not_needed_columns <- c( "start", "end","today", "deviceid","available_health_services","barriers_health", "barriers_usetoilets","conflict_causes","conflict_mediators",
                         "coping_food_strat","education_available", "idp_arrived_reason", "idp_pull_factors", "incidents_wh_leaving","info_barriers", "info_mainsource", 
                         "lack_food_reasons","left_behind_who", "livelihood_activ", "main_radios", "market_goods", "ngo_support_type","noaccess_health","nomarket_why",
                         "protection_inc_location", "protection_incidents","contact_again", "X__version__", "X_id", "X_uuid","X_submission_time", "X_index", "idp_site", 
                         "info_reg", "district_info", "gender_ki", "sources_covid_informaiton", "other_covid_info_sources", "settlement_info_001","X_validation_status", 
                         "enum_code", "declined_consent", "h2r_notice", "when_left_prev", "how_long_stay", "still_inhabited", "not_ki_req", "age_ki", "left_behind_y_n", 
                         "market_settlement_close", "people_malnourished", "child_services", "other_covid_measures", "planning2visit_settelment", "new_ki_referral",
                         select_other, select_note
)

#get select single through subset (minus multiple and not needed and essential_col)
select_single<-names(df)[names(df) %in% select_multiple ==FALSE]
select_single<-select_single[select_single %in% not_needed_columns ==FALSE]
select_single<-select_single[select_single %in% essential_col ==FALSE]

#########AGGREGATE BY MODE OR DISPLAY TIE ("NC")####################################################################################################################################################

settlement_single_choice <- df %>%
  select(all_of(essential_col), all_of(select_single)) %>%
  group_by(.dots = c( "calc.region","calc.district","finalsettlement")) %>%
  summarise_all(list(AoK))

settlement_multiple_choice <- select_multiple_df %>%
  select(all_of(essential_col), all_of(select_multiple)) %>%
  group_by(.dots = c( "calc.region","calc.district","finalsettlement")) %>%
  summarise_all(list(AoK))

#########JOIN GEOSPATIAL AND SURVEY DATA############################################################################################################################################################

ki_coverage <- df %>%
  select(calc.region,calc.district,finalsettlement, particip_again) %>%
  group_by(calc.region,calc.district,finalsettlement) %>%
  summarise(ki_num = length(particip_again))

analysis_df_list<-list(settlement_single_choice, settlement_multiple_choice)

settlement_data <-purrr::reduce(analysis_df_list, left_join, by= c("calc.district","calc.region", "finalsettlement"))

#combining settlement and county name for ArcGIS, also adding in a column for KI coverage
settlement_data <- settlement_data %>%
  ungroup() %>%
  mutate(D.ki_coverage = as.numeric(ki_coverage$ki_num))

#rearranging of columns in our new data set to be in the same order as in the original one
settlement_data <- settlement_data %>% select(order(match(names(settlement_data), names(df))))

##########NC- & SL-DEPENDENCY LOOPS#################################################################################################################################################################

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

#########FURTHER GEOSPATIAL PREPARATION#############################################################################################################################################################

#only take areas with more than one KI reporting
settlement_data <- settlement_data %>% 
  select(base:consent,calc.region, calc.district,finalsettlement,D.ki_coverage,info_settlement:names(settlement_data)[length(settlement_data)-4])%>% 
  filter(D.ki_coverage > 1)

#date to label export
today <- Sys.Date()
agg_date<-today %m+% months(-1)
agg_date<-format(agg_date, format="%Y%m")

write.csv(settlement_data, file = paste0("outputs/SOM1901_H2R__clean_data_",agg_date,"01.csv"), na = "", row.names = FALSE)

#join data to settlement shapefile
settlement_data$P_CODE <- settlement_data$finalsettlement
settlement_data$month <- paste0(agg_date,"01")
som_settlements_data <- inner_join(som_settlements,settlement_data, by = "P_CODE")
som_settlements_data <-st_join(som_settlements_data, hex_400km)
names(som_settlements_data)[names(som_settlements_data) == "GRID_ID"] <- "hex_4000km"

#join data to settlement shapefile for market settlement locations
market_settlement<-settlement_data["market_settlement"]
market_settlement$P_CODE <- market_settlement$market_settlement
market_settlement <- inner_join(som_settlements,market_settlement,  by = "P_CODE")
market_settlement <-st_join(market_settlement, hex_400km)
names(market_settlement)[names(market_settlement) == "GRID_ID"] <- "hex_4000km_market"


#settlement data with hexagons information
som_settlements_data <- som_settlements_data %>%
  select(OBJECTID_1,name,ADM1_NAME,ADM2_NAME,hex_4000km,base,consent,finalsettlement:names(settlement_data)[length(settlement_data)-2],geometry)

settlement_level <- som_settlements_data %>%   filter(!is.na(D.ki_coverage))

#########REFORMATTING FOR butteR::mean_proportion_table#############################################################################################################################################

#adding on "expand_fix" level in order to pass through butteR::mean_proportion_table (below). 
#Some variables have only one level, for those an artificial one needs to be added; after passing through the function it will get removed

settlement_level<-as.data.frame(settlement_level)
index_multiple<-which(names(settlement_level)%in%select_multiple)
index_single<-which(names(settlement_level)%in%select_single)
index_loop<-c(index_multiple, index_single)

for (i in index_loop){
  settlement_level[,i] <- forcats::fct_expand(settlement_level[,i],c("expand_fix"))
}

#########GEOSPATIAL AND MEAN PROPORTION AGGREGATION#################################################################################################################################################

dfsvy_h2r_district <-srvyr::as_survey(settlement_level)

h2r_columns <- settlement_level %>% 
  select(visit_lastmonth:names(settlement_level)[length(settlement_level)-1], - contains(c(".other",".dontknow",".noresponse"))) %>% 
  colnames()

#Region level aggregation-----------
# <-butteR::mean_proportion_table(design = dfsvy_h2r_district,                    
#                                           list_of_variables = h2r_columns,
#                                           aggregation_level = "ADM1_NAME",
#                                           round_to = 1,
#                                           return_confidence = FALSE,
#                                           na_replace = FALSE)

#District level aggregation ----------
#district_h2r <-butteR::mean_proportion_table(design = dfsvy_h2r_district,
#                                             list_of_variables = h2r_columns,
#                                             aggregation_level = "ADM2_NAME",
#                                             round_to = 2,
#                                             return_confidence = FALSE,
#                                             na_replace = FALSE)

#District_summary <- settlement_level %>%  select(ADM2_NAME,ADM1_NAME,D.ki_coverage) %>%
#  group_by(ADM2_NAME) %>%
#  summarise(assessed_num = n(), ki_num=sum(D.ki_coverage) )

#District_summary <- data.frame(District_summary) %>%  select("ADM2_NAME", "assessed_num" ,  "ki_num" )

#som_settlements_summary <- som_settlements %>% select(ADM2_NAME,ADM1_NAME) %>%
#  group_by(ADM2_NAME) %>%
#  summarise(settlem_num = n())

#som_settlements_summary <- data.frame(som_settlements_summary) %>%  select("ADM2_NAME", "settlem_num")

#join into one data set
#analysis_df_list<-list(District_summary, som_settlements_summary,district_h2r)
#district_level <-purrr::reduce(analysis_df_list, left_join)

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
#district_level <- district_level %>% select(everything(), - contains(c(".other",".dontknow",".noresponse", "expand_fix")))
#names(district_level) <- gsub("\\.", "_", names(district_level))

#########EXPORT#####################################################################################################################################################################################
#date to label export
today <- Sys.Date()
agg_month<-today %m+% months(-1)
agg_month<-format(agg_month, format="%b_%Y")

write.csv(grid_level, paste0("outputs/SOM1901_H2R_hex_400km_", agg_month,".csv"), row.names=FALSE)
#write.csv(district_level,paste0("outputs/SOM1901_aggregation_district_", agg_month,".csv"), row.names=FALSE)
write.csv(settlement_level,paste0("outputs/SOM1901_H2R_settlement_aggregation_", agg_month,".csv"), row.names=FALSE)
write.csv(market_settlement,paste0("outputs/SOM1901_H2R_hex_400km_market_locations_", agg_month,".csv"), row.names=FALSE)

#select FS variables by which answers of the following list are still there after the aggregation
FS_vars<- (c( "hex_4000km" ,"ki_num","assessed_num", "food_price_changed_prices_increased", "education_bar_cost_stud",
    "access_healthservices_no", "health_workers_available_yes", "protection_incidents_none_no", "dam_shelter_yes", 
    "handwashing_access_no", "sources_covid_informaiton_mobile_network_operator_yes", "shelters_not_rebuilt_around_half",
    "shelters_not_rebuilt_less_half","shelters_not_rebuilt_more_half", "shelters_not_rebuilt_dummy_NC", "shelters_not_rebuilt_dummy_SL", 
    "shelters_not_rebuilt_dummy_yes", "shelters_not_rebuilt_dummy_no", "dam_shelters_reason_conflict_looting","dam_shelters_reason_fire", 
    "dam_shelters_reason_flooding", "food_situation_worse", "food_source_bought_cash", "food_source_given_someone",
    "food_situation_improved", "food_situation_remained_same", "food_situation_NC" ))       
select_FS<-names(grid_level)[names(grid_level) %in% FS_vars ==TRUE]
check_not_this_month<-FS_vars[FS_vars %in% names(grid_level) ==FALSE]
print(paste0("Choice not in aggregation for this month and therefore excluded: ", check_not_this_month))
grid_level_fs <- grid_level[select_FS]

#export FS output
write.csv(grid_level_fs,paste0("outputs/SOM1901_H2R_hex_400km_FS_",agg_month,".csv"), row.names=FALSE)
