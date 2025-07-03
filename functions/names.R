# objects used to read in data --------------------------------------------

# this is also used to create tables - _tbl is appended
# also won't be necessary in the redone workflow

advanced_names <- c("ev_forecast_sheet",
                    "vmt_forecast_sheet",
                    "onroad_fuel_tech_frac_sheet",
                    "pass_rail_sheet",
                    "freight_rail_sheet",
                    "construction_sheet",
                    "fuel_apportionment_sheet")


assumptions_names <- c("bikeped_assmps",
                       "transit_assmps",
                       "tdm_assmps",
                       "micro_assmps",
                       "traffic_ops_assmps",
                       "mhdv_assmps",
                       "pnr_assmps",
                       "evsi_assmps")



projects_names <- c("bikeped_projs",
                    "transit_fixed_projs",
                    "transit_dr_projs",
                    "transit_el_projs",
                    "transit_bus_projs",
                    "public_rail_projs",
                    "tdm_projs",
                    "micro_projs",
                    "traffic_ops_projs",
                    "mhdev_projs",
                    "pnr_projs",
                    "evsi_projs",
                    "freight_projs",
                    "expansion_projs")


strategy_names <- c("Bicycle and Pedestrian",
                    "Transit Service Expansion",
                    "Micromobility",
                    "Travel Demand Management",
                    "Park-and-Ride",
                    "Transit Electrification",
                    "MD/HD Truck Replacement",
                    "Electric Vehicle Charging Infra.",
                    "Intermodal Freight Investment",
                    "Traffic Operations",
                    "Roadway Expansion",
                    "Custom Projects")

# these are costs inputs
costs_names <- c("bikeped_costs",
                 "transit_fixed_costs",
                 "transit_dr_costs",
                 "pub_trans_priority_costs",
                 "tdm_costs",
                 "pub_trans_rail_costs",
                 "micro_costs",
                 "traffic_ops_costs",
                 "mhdev_costs",
                 "pnr_costs",
                 "evsi_costs",
                 "roadway_expand_costs",
                 "fuel_costs",
                 "intermodal_costs")

costs_outputs_names <- c("bikeped_costs_outputs",
                         "transit_fixed_costs_outputs",
                         "transit_dr_costs_outputs",
                         "pub_trans_priority_costs_outputs",
                         "transit_zeb_costs_outputs",
                         "pub_trans_rail_costs_outputs",
                         "tdm_costs_outputs",
                         "micro_costs_outputs",
                         "traffic_ops_costs_outputs",
                         "mhdev_costs_outputs",
                         "pnr_costs_outputs",
                         "evsi_costs_outputs",
                         "roadway_expand_costs_outputs",
                         "intermodal_costs_outputs")