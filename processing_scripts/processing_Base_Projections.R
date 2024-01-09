################################################################################
#Emmissions Base################################################################
################################################################################

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
eemrate <- reactive({ #this is electricity emission rate
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

EmRate_by_Tech <- reactive({ #this is emission rate for all vehicles types
  #browser()
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

Tech_Frac_Vision <- reactive({ #this is electiric vehicle projections
  
  col <- case_match(rvs$Baseline$veh_elec_baseline, !!!ev_forecast_mapping)
  Tech_Frac_Vision_temp <- TechFrac #dont think i need to reassign this to protect the original names
  
  names(Tech_Frac_Vision_temp)[names(Tech_Frac_Vision_temp) == col] <- "tech_frac_forecast"
  Tech_Frac_Vision_temp <- Tech_Frac_Vision_temp %>% select(veh_type, veh_subtype, year, stock_millions, tech_frac_forecast)
  
  return(Tech_Frac_Vision_temp)
})

#VMT_Type_Tech_Base <- reactive({ #Need to check
VMT_Type_Tech_Base <- reactive({ #this is VMT 

  state_ch <- rvs$Baseline$state
  nhs_ch <- rvs$Baseline$trans_system_scope
  #browser()
  
  nhs_vals <- filter(NHS_VMT, state == state_ch)
  tech_frac_temp <- Tech_Frac_Vision()
  
  if(nhs_ch == "Only NHS"){
    
    VMT_Type_Tech_Basetemp <- tech_frac_temp %>% 
      left_join(filter(VMT_VehType, state == state_ch), by = c('year','veh_type')) %>%
      mutate(veh_supertype = case_match(veh_type, !!!veh_types_mapping)) %>%
      mutate(mmt_by_type = ifelse(veh_supertype == "Light Duty Vehicles", 
                                  nhs_vals$LDV_pct_on_NHS[1]*state_vmt_vehtype * tech_frac_forecast,
                                  nhs_vals$TRK_pct_on_NHS[1]*state_vmt_vehtype * tech_frac_forecast)) 
  } else {
    
    VMT_Type_Tech_Basetemp <- tech_frac_temp %>% 
      left_join(filter(VMT_VehType, state == state_ch), by = c('year','veh_type')) %>%
      mutate(mmt_by_type = state_vmt_vehtype * tech_frac_forecast)
    
  }
  
  return(VMT_Type_Tech_Basetemp)
  
})

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
                                                        rvs$Assumptions$unit == "avg_trip_miles"] # maybe move this to advanced sorry
  
  
  input_BTU_per_gallon_diesel <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                Fuel_Factors_Baselines$units == "fuel_conversion_BTU"] # also get from advanced oops
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

#Freight Rail ----
observeEvent(input$state_input,{ #not sure where we need this so I'm leaving it in this indeterminate form for now
  req('')
  state_ch <- rvs$Baseline$state
  #browser()
  #Freight rail inputs
  input_FR_GrowthRate <- rvs$Advanced$value[rvs$Advanced$table_no_ui == 5 & rvs$Advanced$unit == "growth_rate"]
  input_FR_BTU_per_tonmile <- rvs$Advanced$value[rvs$Advanced$table_no_ui == 5 & rvs$Advanced$unit == "energy_intensity"] %>% as.numeric()
  input_BTU_per_gallon_diesel <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                   Fuel_Factors_Baselines$units == "fuel_conversion_BTU"]
  input_Diesel_CO2_kg_per_gallon <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                      Fuel_Factors_Baselines$units == "fuel_carbon_content"]

  #need to add state filter to save memory
  Freight_Rail <- Freight_Rail_Data
  for(yr in 2020:2050){
    fr_temp <- Freight_Rail_Data %>% mutate(year= yr) %>%
      mutate(FR_million_tonmiles = FR_million_tonmiles*(1+input_FR_GrowthRate)^(yr-2019))
    Freight_Rail <- rbind(Freight_Rail, fr_temp)
  }
  
  Freight_Rail <- Freight_Rail %>% mutate(FR_Diesel_Em = input_FR_BTU_per_tonmile/input_BTU_per_gallon_diesel*input_Diesel_CO2_kg_per_gallon*1000)
  
})

#Public Transit----
observeEvent(input$state_input,{ #not sure where we need this so I'm leaving it in this indeterminate form for now
  
  req('')
  input_MB_app_diesel<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Bus" & rvs$Advanced$fuel_type == "Diesel"] #these are apportionment for each public transit fueel type in baseline parameters
  input_MB_app_CNG<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Bus" & rvs$Advanced$fuel_type == "CNG"]
  input_MB_app_Electric<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Bus" & rvs$Advanced$fuel_type == "Electric"]
  
  input_DR_app_diesel<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Demand Response" & rvs$Advanced$fuel_type == "Diesel"] #these are apportionment for each public transit fueel type in baseline parameters
  input_DR_app_CNG<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Demand Response" & rvs$Advanced$fuel_type == "CNG"]
  input_DR_app_Electric<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Demand Response" & rvs$Advanced$fuel_type == "Electric"]
  
  
  input_CB_app_diesel <- rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Commuter Bus" & rvs$Advanced$fuel_type == "Diesel"] #these are apportionment for each public transit fueel type in baseline parameters
  input_CB_app_CNG<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Commuter Bus" & rvs$Advanced$fuel_type == "CNG"]
  input_CB_app_Electric<-rvs$Advanced$value[rvs$Advanced$table_no_ui == 3 & rvs$Advanced$transit_mode == "Commuter Bus" & rvs$Advanced$fuel_type == "Electric"]
  
  # #need to implement the custom or default code ig
  MB_diesel_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Bus" & rvs$Assumptions$fuel_type == "Diesel" & rvs$Assumptions$unit == "veh_fuel_economy"]
  MB_cng_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Bus" & rvs$Assumptions$fuel_type == "CNG" & rvs$Assumptions$unit == "veh_fuel_economy"]
  MB_electric_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Bus" & rvs$Assumptions$fuel_type == "Electric" & rvs$Assumptions$unit == "veh_fuel_economy"]
  DR_gasoline_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Demand Response" & rvs$Assumptions$fuel_type == "Gasoline" & rvs$Assumptions$unit == "veh_fuel_economy"]
  DR_cng_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Demand Response" & rvs$Assumptions$fuel_type == "CNG" & rvs$Assumptions$unit == "veh_fuel_economy"]
  DR_electric_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Demand Response" & rvs$Assumptions$fuel_type == "Electric" & rvs$Assumptions$unit == "veh_fuel_economy"]
  CR_diesel_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Commuter Bus" & rvs$Assumptions$fuel_type == "Diesel" & rvs$Assumptions$unit == "veh_fuel_economy"]
  CR_cng_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Commuter Bus" & rvs$Assumptions$fuel_type == "CNG" & rvs$Assumptions$unit == "veh_fuel_economy"]
  CR_electric_mpgge <- rvs$Assumptions$value[rvs$Assumptions$table_no_ui == 2 & rvs$Assumptions$transit_mode == "Commuter Bus" & rvs$Assumptions$fuel_type == "Electric" & rvs$Assumptions$unit == "veh_fuel_economy"]
  

  input_gal_diesel_per_gasoline_eq <- Fuel_Factors_Baselines$value[Fuel_Factors_Baselines$fuel_type == "Diesel" &
                                                                      Fuel_Factors_Baselines$units == "fuel_conversion_gasoline_equivalent"] #0.893 From baseline parameters
  input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile <- Fuel_Factors$GWP_CH4_g_per_mi[Fuel_Factors$fuel_type == "Diesel" &
                                                                         Fuel_Factors$veh_type == "Heavy Duty Trucks"]#0.238 From fuel Factors
  input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile <- Fuel_Factors$GWP_N20_g_per_mi[Fuel_Factors$fuel_type == "Diesel" &
                                                                                     Fuel_Factors$veh_type == "Heavy Duty Trucks"] #12.844

  state_ch <- rvs$Baseline$state
  
  Public_Transit <- Public_Transit_data %>%
    mutate(year = 2019) %>%
    filter(State == state_ch)
  
  for(yr in 2020:2050){
    Public_Transit_temp = Public_Transit_data %>% mutate(year = yr)
    Public_Transit = rbind(Public_Transit, Public_Transit_temp)
                           }
  
  Public_Transit <- Public_Transit %>%
    left_join(eemrate() %>% select(year, electricity_carbon_content) %>% filter(duplicated(.))) %>% #the filter duplciated is just removing different vehicle types with the same values
    mutate(MB_diesel_emintensity = (1/MB_diesel_mpgge)*input_gal_diesel_per_gasoline_eq*input_Diesel_CO2_kg_per_gallon*1000 + input_HeavyDutyTruck_Diesel_CH4_gCO2e_per_mile + input_HeavyDutryTruck_Diesel_NOX_gCO2e_per_mile,
           MB_cng_emintensity = (1/MB_cng_mpgge),
           MB_electric_emintensity = (1/MB_electric_mpgge)*electricity_carbon_content,
           DR_gasoline_emintensity = (1/DR_gasoline_mpgge),
           DR_cng_emintensity = (1/DR_cng_mpgge),
           DR_electric_emintensity = (1/DR_electric_mpgge)*electricity_carbon_content ,
           CR_diesel_emintensity = (1/CR_diesel_mpgge),
           CR_CNG_emintensity =  (1/CR_CNG_mpgge),
           CR_electric_emintensity =  (1/CR_electric_mpgge)*electricity_carbon_content
           ) %>%
    mutate(MB_Diesel_Emrate = MB_revenue_miles) %>%
    left_join(eemrate() %>% select(year, electricity_carbon_content) %>% filter(duplicated(.)))%>%
    mutate(Electric_Amtrak_CO2eq = Amtrak_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
           Electric_CR_CO2eq = CR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
           Electric_HR_CO2eq = HR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content,
           Electric_LR_CO2eq = LR_Energy_Intensity_BTUPerPaxMil*(1/input_BTU_per_kWh)*electricity_carbon_content)
  
})

### VMT_Forecast ----------------
### these tables don't have to be show to the user, but it is helpful to have them as reactive tables
#### SL - All the below are throwing erros for me I'm commenting out the VMT_Tyep_Tech_Base for now as it's missing 
#### some key info
VMT_Forecast <- reactive({
  AEO_VMT %>%
    left_join(y = State_Populations %>% filter(state == rv$Baseline$state) %>% select(year, state_pct_of_national), by = join_by(year)) %>%
    mutate(state_vmt = VMT_AEO * state_pct_of_national) # state VMT forecast
})

# VMT_Type_Tech_Base <- reactive({
#   Tech_Frac_Vision %>%
#     left_join(select(VMT_Forecast(), veh_type, year, state_vmt), by = join_by(veh_type, year)) %>%
#     mutate(state_vmt_by_subtype = state_vmt * aeo_tech_frac)
# })

### The below tables summarize the VMT_Type_Tech_Base
### It's in different tables because these categories are actually overlapping which is weird but maybe there's a reason
### You can check that it summarizes summarize(total = sum(state_pct_of_category), .by = c(year))

VMT_Type_Tech_LDV <- reactive({
  VMT_Type_Tech_Base() %>%
    filter(veh_type %in% c("Passenger Cars", "Light Duty Trucks")) %>% 
    summarize(veh_type, veh_subtype, state_vmt_by_subtype, state_pct_of_category = state_vmt_by_subtype / sum(state_vmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Light Duty Vehicles")
})

VMT_Type_Tech_MDHD <- reactive({
  VMT_Type_Tech_Base() %>%
    filter(veh_type %in% c("Medium Duty Trucks", "Heavy Duty Trucks")) %>% 
    summarize(veh_type, veh_subtype, state_vmt_by_subtype, state_pct_of_category = state_vmt_by_subtype / sum(state_vmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Medium/Heavy Duty Vehicles")
})

VMT_Type_Tech_Conventional_LDV <- reactive({
  VMT_Type_Tech_Base() %>%
    filter((veh_type == "Passenger Cars" & veh_subtype == "Gasoline ICE") |
             (veh_type == "Light Duty Trucks" & veh_subtype == "Gasoline ICE")) %>%
    summarize(veh_type, veh_subtype, state_vmt_by_subtype, state_pct_of_category = state_vmt_by_subtype / sum(state_vmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Conventional LDV")
})

VMT_Type_Tech_Conventional_MDHD <- reactive({
  VMT_Type_Tech_Base() %>%
    filter((veh_type == "Medium Duty Trucks" & veh_subtype == "Diesel") |
             (veh_type == "Medium Duty Trucks" & veh_subtype == "Gasoline") | 
             (veh_type == "Heavy Duty Trucks" & veh_subtype == "Diesel")) %>%
    summarize(veh_type, veh_subtype, state_vmt_by_subtype, state_pct_of_category = state_vmt_by_subtype / sum(state_vmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Conventional MDHD")
})

VMT_Type_Tech_Electric_LDV <- reactive({
  VMT_Type_Tech_Base() %>%
    filter(veh_subtype %in% c("EV100", "EV200", "EV300")) %>%
    summarize(veh_type, veh_subtype, state_vmt_by_subtype, state_pct_of_category = state_vmt_by_subtype / sum(state_vmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Electric LDV")
})

VMT_Type_Tech_Electric_MDHD <- reactive({
  VMT_Type_Tech_Base() %>%
    filter(veh_type %in% c("Medium Duty Trucks", "Heavy Duty Trucks") & veh_subtype == "Electric") %>% 
    summarize(veh_type, veh_subtype, state_vmt_by_subtype, state_pct_of_category = state_vmt_by_subtype / sum(state_vmt_by_subtype), 
              .by = year) %>%
    mutate(veh_category = "Electric MDHD")
})



