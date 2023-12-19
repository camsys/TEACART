#This combines all the different code from EmRate_by_Tech and VMT_Type_Tech_Base into a single big file

library(dplyr)
library(sf)

################################################################################
#Emmissions Base################################################################
################################################################################



#static inputs -----------------------------------------------------------------

Strategy_Parameters <- read.csv("Data Extracts/Strategy_Parameters.csv")

#emrate by tech inputs
electricity_emrate <- read.csv('Data Extracts/Electricity_EmRate.csv')
fuel_econ_df <- read.csv('Data Extracts/FuelEcon.csv')

fuel_factor_df <- xlsx::read.xlsx("Data Extracts/Fuel Factors_Workbook.xlsx", 3)
fuel_factor_apportionment <- xlsx::read.xlsx("Data Extracts/Fuel Factors_Workbook.xlsx", 4)

fuel_factor <- left_join(fuel_factor_df, fuel_factor_apportionment)
#end emrate by tech inputs

##VMT Type Tech Base inputs
State_Population_base_data <- read.csv('Data Extracts/State_Population.csv') # can we remove the US Total from this CSV?

AEO_VMT_Base <- read.csv('Data Extracts/AEO_VMT_Base.csv') # no calculation needed
VMT_State_Allocation_base_data <- read.csv('Data Extracts/VMT_State_Allocation.csv')

## TrchFrac branch
Stock_Type_Tech_BASE <- read.csv('Data Extracts/Stock_Type_Tech_BASE.csv')
EV_Forecast <-read.csv('Data Extracts/EV Forecast.csv')

Stock_Type_Tech_BASE_forecast <- merge(Stock_Type_Tech_BASE,EV_Forecast, by = c('vehicle_type','year'), all = TRUE)
###End VMT/Tech Frac inputs

##passenger rail inputs
input_AmTrak_EnergySource <- 'Diesel' #From Fuel Factors/Base inputs
input_AmTrak_AvgTripLength <- 198.35 #From Fuel Factors/Base inputs
input_CR_EnergySource <- 'Diesel' #From Fuel Factors/Base inputs
input_HR_EnergySource <- 'Electric'#From Fuel Factors/Base inputs
input_LR_EnergySource <- 'Electric' #From Fuel Factors/Base inputs
input_BTU_per_gallon_diesel <- 128500 #From Fuel Factors/Base inputs
input_BTU_per_kWh <- 3414 #From Fuel Factors/Base inputs
input_Diesel_CO2_kg_per_gallon <- 9.4 #From Fuel Factors
input_Locomotives_CH4_gCO2eq_per_gallon <- 0.8*25 #From Fuel Factors
input_Locomotives_N20_gCO2eq_per_gallon <- .26*298 #From Fuel Factors

Passenger_Rail_data <- read.csv("Data Extracts/Passenger_Rail_Data.csv")
PassengerRailFuelFactors <- read.csv("Data Extracts/Passenger_Rail_FuelFactors.csv") 
###End passenger rail inputs

##Freight rail inputs
input_FR_GrowthRate <- 0.006863941
input_FR_BTU_per_tonmile <- 297.5798656

Freight_Rail_data <- read.csv("Data Extracts/Freight_Rail_Data.csv")
###End Freight rail inputs

##Public Transit inputs
input_MB_app_diesel = 1 #these are apportionment for each public transit fueel type in baseline parameters
input_MB_app_CNG = 0
input_MB_app_Electric = 0

input_DR_app_gasoline = 1
input_DR_app_CNG = 0
input_DR_app_Electric = 0

input_CB_app_diesel = 1
input_CB_app_CNG = 0
input_CB_app_Electric = 0

#need to implement the custom or default code ig
MB_diesel_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "MB_diesel_mpgge"]
MB_cng_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "MB_cng_mpgge"]
MB_electric_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "MB_electric_mpgge"]
DR_gasoline_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "DR_gasoline_mpgge"]
DR_cng_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "DR_cng_mpgge"]
DR_electric_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "DR_electric_mpgge"]
CR_diesel_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "CR_diesel_mpgge"]
CR_CNG_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "CR_CNG_mpgge"]
CR_electric_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "CR_electric_mpgge"]



#
input_gal_diesel_per_gasoline_eq = 0.893 #From baseline parameters
input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile = 0.238 #From fuel Factors
input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile = 12.844


#Currently this assumes Vehicle Revenue Miles are constant year to year
Public_Transit_data <- read.csv("Data Extracts/Public_Transit_Data.csv")
###End PUblic Transit

#user inputs -------------------------------------------------------------------
input_state = "Maryland"
input_net_zero_year = 2050
#NHS INPUT
#EmRate_By_Tech ---------------------------------------------------------------

##functions

#This function calcualte the emrate for electricity based on a linear interpolation
#between base year and the user input net_zero_year
electricity_emrate_projecter <- function(eemrate_df, net_zero_year = 2100){
  
  eemrate_return <- eemrate_temp <- eemrate_df %>% select(-CO2eq_lbs_MWh) %>%
    mutate(linear_eq = (-1*CO2eq_g_kWh)/(net_zero_year-2021),
           intercept = -1*linear_eq*2021+CO2eq_g_kWh)
  
  for(n in 2022:2100){
    eemrate_return <- rbind(eemrate_return, mutate(eemrate_temp, year = n))
  }
  
  eemrate_return<-eemrate_return %>% 
    mutate(electricity_carbon_content = linear_eq*year+intercept) 
  
  eemrate_return$CO2eq[eemrate_return$year >= net_zero_year] <- 0
  
  eemrate_return_fin <- rbind(mutate(eemrate_return,fuel_type="EV100"),
                              mutate(eemrate_return,fuel_type="EV200")) %>%
    rbind(mutate(eemrate_return,fuel_type="EV300")) %>%
    select(State, year, electricity_carbon_content, fuel_type)
  
  return(eemrate_return_fin)
}


#calcualte and combine relevant data frames
eemrate <- electricity_emrate_projecter(electricity_emrate, net_zero_year = input_net_zero_year) %>% 
  filter(year <= 2050) %>%
  filter(State == input_state)

#joining dataframe together which makes it easier to calcualte things
EmRate_by_Tech <- left_join(fuel_econ_df, eemrate)
EmRate_by_Tech <- left_join(EmRate_by_Tech, fuel_factor)

#fill NAs with zero. Caused by missing factors for certain vehicles. This could 
#be skipped by using a better NA coding in the csv I think?
EmRate_by_Tech[is.na(EmRate_by_Tech)]<-0
EmRate_by_Tech[EmRate_by_Tech == 'NA'] <-0

#Calculate fuel emission rate, electircity emission rate, and combine based
#on the apportionment for each vehicle type
#most of these factors are in the fuel factors sheet
EmRate_by_Tech <- EmRate_by_Tech %>% 
  mutate(fuel_emission_rate = (1/mpg_gasoline_eq) * (1000*fuel_carbon_content * fuel_conversion) + fuel_CH4_CO2e_per_mile+fuel_N20_CO2eq_per_mile) %>%
  mutate(electricity_emission_rate = (1/mpg_gasoline_eq) * electricity_carbon_content * electricity_conversion) %>%
  mutate(emission_rate = (1-apportionment)*fuel_emission_rate+apportionment*electricity_emission_rate)


#VMT Type Tech Base -----
## State_Population tab -------

state_list <- unique(State_Population_base_data$state)
State_Population <- data.frame()

#add a 2050 year projection 
for (var in state_list){
  temp_state <- State_Population_base_data %>% 
    filter(state == var) %>%
    add_row(state = var, year = 2050, population = (.$population[.$year == 2040]/ .$population[.$year == 2030])*.$population[.$year == 2040])
  
  State_Population = rbind(State_Population,temp_state)
}

temp = data.frame()

for ( var in state_list){ # loop through states
  for (i in 2021:2029){
    temp <- State_Population %>% filter(state == var) %>%
      mutate(population = population[year == i-1] + ((population[year == 2030] -  population[year == 2020])/10),
             year = i) %>%
      distinct()
    State_Population = rbind(State_Population,temp)
    
  }
  for (i in 2031:2039){
    temp <- State_Population %>% filter(state == var) %>%
      mutate(population = population[year == i-1] + ((population[year == 2040] -  population[year == 2030])/10),
             year = i) %>%
      distinct()
    State_Population = rbind(State_Population,temp)}
  for (i in 2041:2049){
    temp <- State_Population %>% filter(state == var) %>%
      mutate(population = population[year == i-1] + ((population[year == 2050] -  population[year == 2040])/10),
             year = i) %>%
      distinct()
    State_Population = rbind(State_Population,temp)}
}

temp_total <- data.frame(state = 'U.S. Total', year = 2020, population = sum(State_Population$population[State_Population$year == 2020]))

for (i in 2021: 2050){
  temp <- data.frame(state = 'U.S. Total', year = i, population = sum(State_Population$population[State_Population$year == i]))
  temp_total = rbind(temp_total,temp)
}

State_Population <- rbind(State_Population, temp_total)
State_Population <- left_join(State_Population, 
                              add_row(
                                rename(
                                  select(
                                    filter(State_Population_base_data, year == 2020),
                                    -year),base = population), 
                                state = 'U.S. Total', 
                                base = State_Population$population[State_Population$state == 'U.S. Total'& State_Population$year == 2020]))
State_Population$poprate = (State_Population$population - State_Population$base)/State_Population$base
State_Population <- select(State_Population,-base)


# VMT_State_Allocation tab ----

VMT_State_Allocation <- VMT_State_Allocation_base_data

temp = data.frame()
for (var in state_list){
  
  for (i in 2022:2050){
    
    temp <- VMT_State_Allocation_base_data %>% filter(state == var) %>%
      mutate(stateVMT  = stateVMT[year == 2021] * (1+ State_Population$poprate[State_Population$year == i-1 & State_Population$state == var]),
             year = i) %>%
      distinct()
    
    VMT_State_Allocation = rbind(VMT_State_Allocation,temp)}
  
}

#calculate US Total
totalvmt_temp <- data.frame()

for (i in 2022: 2050){
  totalvmt_temp <- VMT_State_Allocation %>% filter(year == i) %>%
    add_row(state = 'U.S. Total', year = i, stateVMT  = sum(.$stateVMT ))
  
  VMT_State_Allocation = rbind(VMT_State_Allocation,totalvmt_temp)
}

VMT_State_Allocation <- left_join(State_Population,VMT_State_Allocation, by = c('year','state'))

#add percentage of VMT as portion of total VMT
VMT_State_Allocation<-VMT_State_Allocation %>% 
  group_by(year) %>%
  mutate(state_perc = stateVMT/(sum(stateVMT,na.rm = T)/2)) %>% #dividing by two removes US Total
  ungroup()

## join the vmt for each vehicle types
VMT_State_Allocation <- merge(VMT_State_Allocation, AEO_VMT_Base, by = 'year', all = TRUE)

VMT_VehType <- VMT_State_Allocation %>% #aka VMT_Forecast
  mutate(state_vmt_vehtype = VMT_AEO*state_perc)

#TechFrac branch -----

#calcualte miles per vehicle 
Stock_Type_Tech_BASE_forecast <- left_join(Stock_Type_Tech_BASE_forecast, AEO_VMT_Base) %>%
  group_by(vehicle_type, year) %>%
  mutate(miles_per_veh = VMT_AEO/million_vehicles)


ev_fuel_type <- c("EV100","EV200","EV300","SI PHEV 10","SI PHEV 40", "FCV", "EV", "Gasoline PHEV", "Diesel PHEV")

TechFrac <- Stock_Type_Tech_BASE_forecast %>%
  group_by(year, vehicle_type) %>%
  #Baseline vision 2022 I think aka AEO
  mutate(AEO_Tech_Frac = million_vehicles/sum(million_vehicles)) %>%
  #select(year, vehicle_type, fuel_type, AEO_Tech_Frac) %>%
  ungroup() %>%
  mutate(is_ev_type = ifelse(fuel_type %in% ev_fuel_type,1,0)) %>%
  group_by(year, vehicle_type, is_ev_type) %>%
  mutate(per_ev_nonev = AEO_Tech_Frac/sum(AEO_Tech_Frac)) %>%
  #ACC Forecasting
  ungroup() %>%
  group_by(year, vehicle_type) %>%
  mutate(ACC_Tech_Fractemp = percEVstock_ACC*per_ev_nonev*is_ev_type) %>%
  mutate(ACC_Tech_Frac = ifelse(is_ev_type == 0, per_ev_nonev*(1-sum(ACC_Tech_Fractemp)), ACC_Tech_Fractemp)) %>%
  mutate(ACC_Tech_Frac = ifelse(vehicle_type %in% c("Medium Duty Truck","Heavy Duty Truck"), AEO_Tech_Frac, ACC_Tech_Frac)) %>%
  select(-ACC_Tech_Fractemp) %>%
  ungroup() %>%
  #ACCII Version
  ungroup() %>%
  group_by(year, vehicle_type) %>%
  mutate(ACCII_Tech_Fractemp = percEVstock_ACCII*per_ev_nonev*is_ev_type) %>%
  mutate(ACCII_Tech_Frac = ifelse(is_ev_type == 0, per_ev_nonev*(1-sum(ACCII_Tech_Fractemp)), ACCII_Tech_Fractemp)) %>%
  mutate(ACCII_Tech_Frac = ifelse(vehicle_type %in% c("Medium Duty Truck","Heavy Duty Truck"), AEO_Tech_Frac, ACCII_Tech_Frac)) %>%
  select(-ACCII_Tech_Fractemp) %>%
  ungroup() %>%
  #ACC + ACT 
  group_by(year, vehicle_type) %>%   
  mutate(ACCACT_Tech_Fractemp = percEVstock_ACCACT*per_ev_nonev*is_ev_type) %>%
  mutate(ACCACT_Tech_Frac = ifelse(is_ev_type == 0, per_ev_nonev*(1-sum(ACCACT_Tech_Fractemp)), ACCACT_Tech_Fractemp)) %>%
  mutate(ACCACT_Tech_Frac = ifelse(vehicle_type %in% c("Medium Duty Truck","Heavy Duty Truck"), ACCACT_Tech_Frac, ACC_Tech_Frac)) %>%
  select(-ACCACT_Tech_Fractemp) %>%
  ungroup() %>%
  #ACCII + ACT
  mutate(ACCIIACT_Tech_Frac = ifelse(vehicle_type %in% c("Passenger Car","Light Duty Truck"), ACCII_Tech_Frac, ACCACT_Tech_Frac)) #%>%
# #Future Scenario - no numbers in the EV Forecast tab so I'm commenting it out - What's up with it?
# group_by(year, vehicle_type) %>%
# mutate(FScen_Tech_Fractemp = percEVstock_Fscen*per_ev_nonev*is_ev_type) %>%
# mutate(FScen_Tech_Frac = ifelse(is_ev_type == 0, per_ev_nonev*(1-sum(FScen_Tech_Fractemp)), FScen_Tech_Fractemp)) %>%
# select(-FScen_Tech_Fractemp) %>%
# ungroup() %>%

#Combine to make other tabs ----
#need to take a second look at this join actually is it working?
VMT_Type_Tech_Base <- TechFrac %>% 
  left_join(filter(VMT_VehType, state == input_state), by = c('year','vehicle_type')) %>%
  mutate(mmt_by_type = state_vmt_vehtype * AEO_Tech_Frac)   # the VMT_Type_Tech_BASE tab

#This is not complete not sure what we need from this bad boy quite yet
Em_OnRoad_BASE <- left_join(EmRate_by_Tech, VMT_Type_Tech_Base)

#passenger rail ----

#need to add state filter to save memory
Passenger_Rail_data <- Passenger_Rail_data %>%
  mutate(amtrak_miles = amtrak_riders*input_AmTrak_AvgTripLength)

Passenger_Rail <- Passenger_Rail_data
for(yr in 2020:2050){
  Passenger_Rail_temp = Passenger_Rail_data %>% mutate(year = yr) 
  Passenger_Rail = rbind(Passenger_Rail, Passenger_Rail_temp)
}

PassengerRailFuelFactors <- PassengerRailFuelFactors %>%
  mutate(Diesel_Amtrak_CO2eq = Amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
         Diesel_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
         Diesel_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
         Diesel_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon)) %>%
  left_join(eemrate %>% select(year, electricity_carbon_content) %>% filter(duplicated(.)))%>%
  mutate(Electric_Amtrak_CO2eq = Amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content)

#Freight Rail ----------

#need to add state filter to save memory
Freight_Rail <- Freight_Rail_data
for(yr in 2020:2050){
  fr_temp <- Freight_Rail_data %>% mutate(year= yr) %>%
    mutate(FR_million_tonmiles = FR_million_tonmiles*(1+input_FR_GrowthRate)^(yr-2019))
  Freight_Rail <- rbind(Freight_Rail, fr_temp)
}

Freight_Rail <- Freight_Rail %>% mutate(FR_Diesel_Em = input_FR_BTU_per_tonmile/input_BTU_per_gallon_diesel*input_Diesel_CO2_kg_per_gallon*1000)

#Public Transit----
Public_Transit <- Public_Transit_data %>% 
  mutate(year = 2019) %>%
  filter(State == input_state)

for(yr in 2020:2050){
  Public_Transit_temp = Public_Transit_data %>% mutate(year = yr) 
  Public_Transit = rbind(Public_Transit, Public_Transit_temp)
  remove(Public_Transit_temp)
}

Public_Transit <- Public_Transit %>%
  left_join(eemrate %>% select(year, electricity_carbon_content) %>% filter(duplicated(.))) %>% #the filter duplciated is just removing different vehicle types with the same values
  mutate(MB_diesel_emintensity = (1/MB_diesel_mpgge)*input_gal_diesel_per_gasoline_eq*input_Diesel_CO2_kg_per_gallon*1000 + input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile + input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile,
         MB_cng_emintensity = (1/MB_cng_mpgge),
         MB_electric_emintensity = (1/MB_electric_mpgge)*electricity_carbon_content, 
         DR_gasoline_emintensity = (1/DR_gasoline_mpgge), 
         DR_cng_emintensity = (1/DR_cng_mpgge),
         DR_electric_emintensity = (1/DR_electric_mpgge)*electricity_carbon_content , 
         CR_diesel_emintensity = (1/CR_diesel_mpgge),
         CR_CNG_emintensity =  (1/CR_CNG_mpgge),
         CR_electric_emintensity =  (1/CR_electric_mpgge)*electricity_carbon_content 
         )
  mutate(MB_Diesel_Emrate = MB_revenue_miles) %>%
  left_join(eemrate %>% select(year, electricity_carbon_content) %>% filter(duplicated(.)))%>%
  mutate(Electric_Amtrak_CO2eq = Amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content)
