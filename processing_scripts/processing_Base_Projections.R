################################################################################
#Emmissions Base################################################################
################################################################################

# ##Public Transit inputs
# input_MB_app_diesel = 1 #these are apportionment for each public transit fueel type in baseline parameters
# input_MB_app_CNG = 0
# input_MB_app_Electric = 0
# 
# input_DR_app_gasoline = 1
# input_DR_app_CNG = 0
# input_DR_app_Electric = 0
# 
# input_CB_app_diesel = 1
# input_CB_app_CNG = 0
# input_CB_app_Electric = 0
# 
# #need to implement the custom or default code ig
# MB_diesel_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "MB_diesel_mpgge"]
# MB_cng_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "MB_cng_mpgge"]
# MB_electric_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "MB_electric_mpgge"]
# DR_gasoline_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "DR_gasoline_mpgge"]
# DR_cng_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "DR_cng_mpgge"]
# DR_electric_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "DR_electric_mpgge"]
# CR_diesel_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "CR_diesel_mpgge"]
# CR_CNG_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "CR_CNG_mpgge"]
# CR_electric_mpgge <- Strategy_Parameters$defaul[Strategy_Parameters$variable == "CR_electric_mpgge"]
# 
# 
# 
# #
# input_gal_diesel_per_gasoline_eq = 0.893 #From baseline parameters
# input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile = 0.238 #From fuel Factors
# input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile = 12.844
# 
# 
# #Currently this assumes Vehicle Revenue Miles are constant year to year
# Public_Transit_data <- read.csv("Data Extracts/Public_Transit_Data.csv")
# ###End PUblic Transit


#EmRate_By_Tech ---------------------------------------------------------------


#This function calcualte the emrate for electricity based on a linear interpolation
#between base year and the user input net_zero_year
electricity_emrate_projecter <- function(eemrate_df, net_zero_year = 2100){

  eemrate_return <-eemrate_temp<- eemrate_df %>% select(-CO2eq_lbs_MWh) %>%
    mutate(linear_eq = (-1*CO2eq_g_kWh)/(net_zero_year-2021),
           intercept = -1*linear_eq*2021+CO2eq_g_kWh)

  for(n in 2022:2100){
    eemrate_return <- rbind(eemrate_return, mutate(eemrate_temp, year = n))
  }

  eemrate_return<-eemrate_return %>%
    mutate(electricity_carbon_content = linear_eq*year+intercept)

  eemrate_return$electricity_carbon_content[eemrate_return$year >= net_zero_year] <- 0

  eemrate_return_fin <- rbind(mutate(eemrate_return,fuel_type="EV100"),
                              mutate(eemrate_return,fuel_type="EV200")) %>%
    rbind(mutate(eemrate_return,fuel_type="EV300")) %>%
    filter(year <= 2050) %>%
    select(state, year, electricity_carbon_content, fuel_type)

  return(eemrate_return_fin)
}


#observeEvent 
eemrate <- reactive({
  #browser()
  req(rvs$Baseline$elec_grid_emissions_net_zero)
  req(rvs$Baseline$state)
  zero_em <- rvs$Baseline$elec_grid_emissions_net_zero
  state_ch <- rvs$Baseline$state
  
  eemrate<-electricity_emrate_projecter(Electricity_EmRate, net_zero_year = zero_em) %>% 
    filter(state == state_ch)

  return(eemrate)
})

#joining dataframe together which makes it easier to calcualte things

EmRate_by_Tech <- reactive({
  input_ff_factors <- rvs$Advanced[rvs$Advanced$table_no_ui == 7,] %>% 
    select(veh_type, value) %>%
    rename(apportionment = value) %>%
    mutate(apportionment = as.numeric(apportionment)) #not sure why this step was necessary
  
  eemrate <- eemrate()
  
  EmRate_by_Tech <- left_join(Fuel_Econs, eemrate) %>% select(-state)
  EmRate_by_Tech <- left_join(EmRate_by_Tech, left_join(Fuel_Factors_Revision,input_ff_factors)) %>% 
    select(-c(fuel_conversion_unit, electricity_conversion_unit,fuel_carbon_content_unit))
  
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
  
  return(EmRate_by_Tech)
  })

Tech_Frac_Vision_temp <- reactive({
  name = "AEO Baseline" #need to write map or make input value label vector and pull this from rvs$Baseline$veh_elec_baseline
  col = "AEO_Tech_Frac"
  Tech_Frac_Vision_temp <- TechFrac #dont think i need to reassign this to protect the original names
  
  names(Tech_Frac_Vision_temp)[names(Tech_Frac_Vision_temp) == col] <- "tech_frac"
  Tech_Frac_Vision_temp <- Tech_Frac_Vision_temp %>% select(veh_type, veh_subtype, year, stock_millions, tech_frac)
  return(Tech_Frac_Vision_temp)
})

#VMT_Type_Tech_Base <- reactive({ #Need to check
VMT_Type_Tech_Base<-reactive({
  state_ch <- rvs$Baseline$state
  nhs_ch <- rvs$Baseline$vmt_nhs
  #browser()
  
  nhs_vals <- filter(NHS_VMT, state == state_ch)
  
  if(nhs_ch == "Only NHS"){
  VMT_Type_Tech_Base <- Tech_Frac_Vision_temp() %>% 
    left_join(filter(VMT_VehType, state == state_ch), by = c('year','veh_type')) %>%
    
    mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>%
    mutate(mmt_by_type = ifelse(veh_supertype == "Light Duty Vehicles", 
                                nhs_vals$LDV_pct_on_NHS[1]*state_vmt_vehtype * tech_frac,
                                nhs_vals$TRK_pct_on_NHS[1]*state_vmt_vehtype * tech_frac)) 
  } else {
    VMT_Type_Tech_Base <- Tech_Frac_Vision_temp() %>% 
      left_join(filter(VMT_VehType, state == state_ch), by = c('year','veh_type')) %>%
      mutate(mmt_by_type = state_vmt_vehtype * tech_frac)
  }
  
  retuern(VMT_Type_Tech_Base)

  })


#Testing area ---
# observeEvent(eemrate_listen(),{
#   req(EmRate_by_Tech)
#   #browser()
#   print('heard')
#   print("Here's the EEMRATE")
#   print(eemrate())
#   print("Here's the by tech")
#   print(EmRate_by_Tech())
# })

# #This is not complete not sure what we need from this bad boy quite yet
# Em_OnRoad_BASE <- left_join(EmRate_by_Tech, VMT_Type_Tech_Base)
# 
#passenger rail ----
observeEvent(input$state_input,{ #not sure where we need this so I'm leaving it in this indeterminate form for now
  req('')
  state_ch <- rvs$Baseline$state
  #browser()
#passenger rail inputs
  # input_AmTrak_EnergySource <- 'Diesel' #From Fuel Factors/Base inputs #Original in Excel tool to allow custom energy source
  # input_CR_EnergySource <- 'Diesel' #From Fuel Factors/Base inputs
  # input_HR_EnergySource <- 'Electric'#From Fuel Factors/Base inputs
  # input_LR_EnergySource <- 'Electric' #From Fuel Factors/Base inputs
input_AmTrak_AvgTripLength <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & 
                                                      rvs$Assumptions$transit_mode == "AmTrak" &
                                                      rvs$Assumptions$unit == "avg_trip_miles"]


input_BTU_per_gallon_diesel <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                              Fuel_Factors_Baselines$units == "fuel_conversion_BTU"] #128500 
input_BTU_per_kWh <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "electricity" &
                                                    Fuel_Factors_Baselines$units == "fuel_conversion_BTU"] #3414 #From Fuel Factors/Base inputs
input_Diesel_CO2_kg_per_gallon <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                 Fuel_Factors_Baselines$units == "fuel_carbon_content"] #9.4 #From Fuel Factors
input_Locomotives_CH4_gCO2eq_per_gallon <- Fuel_Factors$CH4_g_per_gallon[Fuel_Factors$fuel_type=="Diesel" & Fuel_Factors$veh_type=="Locomotives"]*Warming_Potential$GWP[Warming_Potential$Gas == "CH4"]
input_Locomotives_N20_gCO2eq_per_gallon <- Fuel_Factors$N20_g_per_gallon[Fuel_Factors$fuel_type=="Diesel" & Fuel_Factors$veh_type=="Locomotives"]*Warming_Potential$GWP[Warming_Potential$Gas == "N20"] 


Passenger_Rail_State_Mileage <- Passenger_Rail_State_Mileage %>% 
  filter(state == state_ch)  %>%
  mutate(amtrak_miles = amtrak_riders*input_AmTrak_AvgTripLength)

Passenger_Rail <- Passenger_Rail_State_Mileage
for(yr in 2020:2050){
  Passenger_Rail_temp = Passenger_Rail_State_Mileage %>% mutate(year = yr)
  Passenger_Rail = rbind(Passenger_Rail, Passenger_Rail_temp)
}

Passenger_Rail_FuelFactors <- Passenger_Rail_FuelFactors %>%
  mutate(Diesel_Amtrak_CO2eq = amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
         Diesel_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
         Diesel_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon),
         Diesel_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_gallon_diesel)*(input_Diesel_CO2_kg_per_gallon*1000+input_Locomotives_CH4_gCO2eq_per_gallon+input_Locomotives_N20_gCO2eq_per_gallon)) %>%
  left_join(eemrate() %>% select(year, electricity_carbon_content) %>% filter(duplicated(.)))%>%
  mutate(Electric_Amtrak_CO2eq = amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
         Electric_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content)
})

# #Freight Rail ----------

#observeEvent(input$state_input,{ #not sure where we need this so I'm leaving it in this indeterminate form for now
  #req('')
  #state_ch <- rvs$Baseline$state
  #browser()
#Freight rail inputs
#input_FR_GrowthRate <- 0.006863941
#input_FR_BTU_per_tonmile <- 297.5798656
# 
# Freight_Rail_data <- read.csv("Data Extracts/Freight_Rail_Data.csv")
# ###End Freight rail inputs
# #need to add state filter to save memory
# Freight_Rail <- Freight_Rail_data
# for(yr in 2020:2050){
#   fr_temp <- Freight_Rail_data %>% mutate(year= yr) %>%
#     mutate(FR_million_tonmiles = FR_million_tonmiles*(1+input_FR_GrowthRate)^(yr-2019))
#   Freight_Rail <- rbind(Freight_Rail, fr_temp)
# }
# 
# Freight_Rail <- Freight_Rail %>% mutate(FR_Diesel_Em = input_FR_BTU_per_tonmile/input_BTU_per_gallon_diesel*input_Diesel_CO2_kg_per_gallon*1000)

})

# #Public Transit----
# Public_Transit <- Public_Transit_data %>% 
#   mutate(year = 2019) %>%
#   filter(State == input_state)
# 
# for(yr in 2020:2050){
#   Public_Transit_temp = Public_Transit_data %>% mutate(year = yr) 
#   Public_Transit = rbind(Public_Transit, Public_Transit_temp)
#   remove(Public_Transit_temp)
# }
# 
# Public_Transit <- Public_Transit %>%
#   left_join(eemrate %>% select(year, electricity_carbon_content) %>% filter(duplicated(.))) %>% #the filter duplciated is just removing different vehicle types with the same values
#   mutate(MB_diesel_emintensity = (1/MB_diesel_mpgge)*input_gal_diesel_per_gasoline_eq*input_Diesel_CO2_kg_per_gallon*1000 + input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile + input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile,
#          MB_cng_emintensity = (1/MB_cng_mpgge),
#          MB_electric_emintensity = (1/MB_electric_mpgge)*electricity_carbon_content, 
#          DR_gasoline_emintensity = (1/DR_gasoline_mpgge), 
#          DR_cng_emintensity = (1/DR_cng_mpgge),
#          DR_electric_emintensity = (1/DR_electric_mpgge)*electricity_carbon_content , 
#          CR_diesel_emintensity = (1/CR_diesel_mpgge),
#          CR_CNG_emintensity =  (1/CR_CNG_mpgge),
#          CR_electric_emintensity =  (1/CR_electric_mpgge)*electricity_carbon_content 
#          )
#   mutate(MB_Diesel_Emrate = MB_revenue_miles) %>%
#   left_join(eemrate %>% select(year, electricity_carbon_content) %>% filter(duplicated(.)))%>%
#   mutate(Electric_Amtrak_CO2eq = Amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
#          Electric_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
#          Electric_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
#          Electric_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content)
