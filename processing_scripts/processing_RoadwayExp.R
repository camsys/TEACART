#load packages - delete in final deployment --------
library(dplyr)
library(tidyr)
library(stringr)

#set inputs - delete in final deployment -------

#these should be pulled from EmRate Tech
#light_duty_automobile_emrate_2025 = 311
#light_duty_automobile_emrate_2030 = 290
#light_duty_automobile_emrate_2050 = 257

light_duty_automobile_emrate = list(y2025 = 311,
                                    y2030 = 290,
                                    y2050 = 257)

#medium_heavy_duty_truck_emrate_2025 = 983
#medium_heavy_duty_truck_emrate_2035 = 886
#medium_heavy_duty_truck_emrate_2050 = 739

medium_heavy_duty_truck_emrate = list(y2025 = 983,
                                      y2030 = 886,
                                      y2050 = 739)


#project inputs - this should be deleted and pulled from UI -----
#freeway_urban_2025 = 20
#freeway_urban_2030 = 20
#freeway_urban_2050 = 20
freeway_urban = list(y2025=20,
                     y2035=20,
                     y2050=20)
# freeway_rural_2025 = 20
# freeway_rural_2030 = 20
# freeway_rural_2050 = 20
freeway_rural = list(y2025=20,
                     y2035=20,
                     y2050=20)
# arterial_urban_2025 = 20
# arterial_urban_2030 = 20 
# arterial_urban_2050 = 20
arterial_urban = list(y2025=20,
                      y2035=20,
                      y2050=20)
# arterial_rural_2025 = 20
# arterial_rural_2030 = 20
# arterial_rural_2050 = 20
arterial_rural = list(y2025=20,
                      y2035=20,
                      y2050=20)
#inputs ------

#delay emrate inputs
car_gallons_hour_delay = 0.4 # this is a hardcoded unmutable (hu) input
truck_gallons_hour_dealy = 1.7 # this is a hu input
gasoline_CO2_kg_per_gallon = 7.94 #this is a hu input from Fuel Factors tab
diesel_CO2_kg_per_gallon = 9.4 #this is a hu input from Fuel Factors tab

percent_truck_traffic = .18 #this is a hm input from Strategy Paramters 
arterial_percent_truck_traffic = .10 #this is a hm input from Strategy Paramters
freeway_percent_truck_traffic = .18 #this is a hm input from Strategy Paramters

#other inptus?
##AADT
arterial_urban_VMTperLaneMile = 20214
arterial_rural_VMTperLaneMile = 8618
freeway_urban_VMTperLaneMile = 5454
freeway_rural_VMTperLaneMile = 2265

##Travel Speed
arterial_urban_tspeed = 71
arterial_rural_tspeed = 71
freeway_urban_tspeed = 43
freeway_rural_tspeed = 60

##Elasticity Factors
freeway_VMT_elasticity  = 1
arterial_VMT_elasticity = 0.8
urban_traveltime_elasticity = -.30
rural_traveltime_elasticity = -.30
#functions ----
calculate_roadway_expansion_emmision_rates_per_hours_delays <- 
  function(
    car_emrate, #this is from EmRate by Tech
    truck_emrate, #this is from EmRate by Tech
    percent_truck_traffic = .18, #this changes whether or not it's freeway or arterial
    
    car_gallons_hour_delay = 0.4, # this is a hardcoded unmutable (hu) input
    truck_gallons_hour_dealy = 1.7, # this is a hu input
    gasoline_CO2_kg_per_gallon = 7.94, #this is a hu input from Fuel Factors tab
    diesel_CO2_kg_per_gallon = 9.4 #this is a hu input from Fuel Factors tab
  ){
    delay_emrate = car_emrate*car_gallons_hour_delay*7.94*1000*(1-percent_truck_traffic) + truck_emrate*truck_gallons_hour_delay*7.94*1000*(percent_truck_traffic)
    return(delay_emrate)
  }

calculate_MT_CO2e_change <- function(
    total_lane_miles = list(y2025=1,y2035=2,y2050=3),
    VMTperLaneMile, #roadway depedent
    VMT_elasticity, #roadway depedent - I HAVE BIG BEN Q! WHY DOES THIS OFTEN EVALUATE TO ZERO FOR VMT CHANGE
    base_speed, #roadway depedent
    CO2em_per_hour_delay, #roadway dependent use first fucntion
    tspeed, #roadway dependent
    VMT_elasticity, #roadway dependent
    existing_lanes = 6 #roadway dependent
){
  annual_VMTperLaneMile = VMTperLaneMile*300
  minutes_delay_saved_perVMT = 0.2*(1-VMT_elasticity)/(1-0.67)
  
  minutes_per_mile_base = 60/base_speed
  minutes_per_mile_new = minutes_delay_saved_perVMT - minutes_per_mile
  
  new_speed = 1/(minutes_per_mile_new*60) #in mph
  speed_change = new_speed - base_speed
  
  VMT_change = lapply(total_lane_miles,
                      function(x) VMT_elasticity*annual_VMTperLaneMile*x)
  VMT_increase = lapply(VMT_change,
                        function(x) tspeed*x) # could I combine these two lapply?
  delay_reduction = lapply(total_lane_miles, 
                      function(x) -((annual_VMTperLaneMile*existing_lanes*x/2)*(minutes_delay_saved_perVMT/60)*CO2em_per_hour_delay)/1000000)
  net_CO2_change = list(y2025 = VMT_increase$y2025 + delay_reduction$y2025,
                        y2030 = VMT_increase$y2030 + delay_reduction$y2030,
                        y2050 = VMT_increase$y2050 + delay_reduction$y2050)
  
}





