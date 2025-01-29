# Meta --------------------------------------------------------------------
# Author:        Lisbeth Vargas
# Date Created:  1/28/2025
# Date Edited:   1/28/2025
# Notes:         R file to build Medicare Advantage dataset



# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

 
 ## Basic contract/plan information
  contract.info=read_csv("ECON 470/Homework 1/1-1/data/input/CPSC_Contract_Info_2015_01.csv",
                         skip=1,
                         col_names = c("contractid","planid","org_type","plan_type",
                                       "partd","snp","eghp","org_name","org_marketing_name",
                                       "plan_name","parent_org","contract_date"),
                         col_types = cols(
                           contractid = col_character(),
                           planid = col_double(),
                           org_type = col_character(),
                           plan_type = col_character(),
                           partd = col_character(),
                           snp = col_character(),
                           eghp = col_character(),
                           org_name = col_character(),
                           org_marketing_name = col_character(),
                           plan_name = col_character(),
                           parent_org = col_character(),
                           contract_date = col_character()
                         ))
 
   ## Enrollments per plan
  enroll.info=read_csv("ECON 470/Homework 1/1-1/data/input/CPSC_Enrollment_Info_2015_01.csv",
                       skip=1,
                       col_names = c("contractid","planid","ssa","fips","state","county","enrollment"),
                       col_types = cols(
                       contractid = col_character(),
                       planid = col_double(),
                       ssa = col_double(),
                       fips = col_double(),
                       state = col_character(),
                       county = col_character(),
                       enrollment = col_double()
                       ),na="*")

 ## Merge contract info with enrollment info
  plan.data = contract.info %>%
    left_join(enroll.info, by=c("contractid", "planid")) %>%
    mutate(year=2015)
    
  ## Fill in missing fips codes (by state and county)
  plan.data = plan.data %>%
    group_by(state, county) %>%
    fill(fips)

  ## Fill in missing plan characteristics by contract and plan id
  plan.data = plan.data %>%
    group_by(contractid, planid) %>%
    fill(plan_type, partd, snp, eghp, plan_name)
  
  ## Fill in missing contract characteristics by contractid
  plan.data = plan.data %>%
    group_by(contractid) %>%
    fill(org_type,org_name,org_marketing_name,parent_org)

  ## Data build
   saveRDS(plan.data, "ECON 470/Homework 1/1-1/data/output/plan_data.rds")


    ## Pull service area data by contract/month
    service.area=read_csv("ECON 470/Homework 1/1-1/data/input/MA_Cnty_SA_2015_01.csv",skip=1,
                          col_names=c("contractid","org_name","org_type","plan_type","partial","eghp",
                                      "ssa","fips","county","state","notes"),
                          col_types = cols(
                            contractid = col_character(),
                            org_name = col_character(),
                            org_type = col_character(),
                            plan_type = col_character(),
                            partial = col_logical(),
                            eghp = col_character(),
                            ssa = col_double(),
                            fips = col_double(),
                            county = col_character(),
                            notes = col_character()
                          ), na='*')

service.area = service.area %>%
  mutate(month = 01, year = 2015)

# Fill missing fips codes (by state and county)
service.area = service.area %>%
  group_by(state, county) %>%
  fill(fips)

# Fill missing values for plan type, org info, partial status, and eghp status (by contractid)
service.area = service.area %>%
  group_by(contractid) %>%
  fill(plan_type, partial, eghp, org_type, org_name)

   ## Data build 2
   saveRDS(service.area, "ECON 470/Homework 1/1-1/data/output/service_area.rds")
   

## FINAL MERGE

final.data <- plan.data %>%
  inner_join(service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!state %in% c("VI","PR","MP","GU","AS","") &
           snp == "No" &
           (planid < 800 | planid >= 900) &
           !is.na(planid) & !is.na(fips))

saveRDS(final.data,"ECON 470/Homework 1/1-1/data/output/final_data.rds")