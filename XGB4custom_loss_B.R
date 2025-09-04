library(arrow)
library(data.table)

# Define file path
file_path <- "c:/C/OmiReport/2506/mtpl_monthly_big_datasets_2025_09_01/ominimo_policies_scored_with_correction_2025_09_01.parquet"
#dt <- as.data.table(read_parquet(file_path))
# library(openxlsx)
# write.xlsx(
#  dt[1],
#  file = "c:/C/OmiReport/2506/mtpl_monthly_big_datasets_2025_09_01/ominimo_policies_scored_with_correction_2025_09_01_header.xlsx",
#  rowNames = FALSE
# )
# write.xlsx(
#  dt[1:1000],
#  file = "c:/C/OmiReport/2506/mtpl_monthly_big_datasets_2025_09_01/ominimo_policies_scored_with_correction_2025_09_01_sample.xlsx",
#  rowNames = FALSE
# )
#dt[quote_type=='calculation' & substr(as.character(unique_id), 1, 3) == "811"]
# dt[is.na(unique_id)]
# dt[quote_type=='calculation' & substr(as.character(unique_id), 1, 3) == "811", .(
#  n_unique = uniqueN(unique_id),   # count distinct
#  min_id   = min(unique_id, na.rm = TRUE),
#  max_id   = max(unique_id, na.rm = TRUE)
# )]
# dt[unique_id==81100000706,.N]
# filtered <- dt[
#  quote_type == "calculation" & substr(as.character(unique_id), 1, 3) == "811"
# ]
# dupes <- filtered[duplicated(unique_id) | duplicated(unique_id, fromLast = TRUE)]


# Define columns to read
cols_to_read <- c(
 "unique_id",
 "postal_code",#sűrűséges
 "vehicle_power",
 "contractor_age",
 "bonus_malus_current",
 "policy_gwp",
 "genertel_predicted_price_sampling_config",
 "kh_predicted_price_sampling_config",
 "granit_predicted_price_sampling_config",
 "alfa_predicted_price_sampling_config",
 "groupama_predicted_price_sampling_config",
 "generali_predicted_price_sampling_config",
 "uniqa_predicted_price_sampling_config",
 "union_predicted_price_sampling_config",
 "allianz_predicted_price_sampling_config",
 "years_since_last_damage_caused",
 "number_of_damages_caused_in_last_5_years",
 "LicenceAge",
 "driver_experience",
 "vehicle_ownership_duration",
 "vehicle_maker",
 "vehicle_age",
 "vehicle_engine_size",
 "vehicle_weight_min",
 "vehicle_weight_max",
 "vehicle_number_of_seats",
 "steering_wheel_located_right",
 "OdometerValueKnown",
 "seasonal_tyres",
 "owner_driver_same",
 "Mileage",
 "mileage_domestic",
 "mileage_foreign",
 "youngest_driver_birth_year",
 "MinDriverAge"
)


cols_to_read <- c(
 "unique_id",
 "postal_code",
 "vehicle_power",
 "contractor_age",
 "bonus_malus_current",
 
 "policy_gwp",
 "quote_type",
 
 "genertel_predicted_price_sampling_config",
 "kh_predicted_price_sampling_config",
 "granit_predicted_price_sampling_config",
 "alfa_predicted_price_sampling_config",
 "groupama_predicted_price_sampling_config",
 "generali_predicted_price_sampling_config",
 "uniqa_predicted_price_sampling_config",
 "union_predicted_price_sampling_config",
 "allianz_predicted_price_sampling_config",
 
 "ominimo_territory_sampling_config",
 "ominimo_correction_table_key_sampling_config",
 "ominimo_base_price_criteria_sampling_config",
 "ominimo_5OrMoreCars_compare_value_orig_sampling_config",
 "ominimo_BoughtAbroad_compare_value_orig_sampling_config",
 "ominimo_CarAge_compare_value_orig_sampling_config",
 "ominimo_CarMake_compare_value_orig_sampling_config",
 "ominimo_CarModel_compare_value_orig_sampling_config",
 "ominimo_CarWeightMax_compare_value_orig_sampling_config",
 "ominimo_CarWeightMin_compare_value_orig_sampling_config",
 "ominimo_DriverAgeBonusMalus_compare_value_orig_sampling_config",
 "ominimo_DriverExperience_compare_value_orig_sampling_config",
 "ominimo_ExperiencedA0Driver_compare_value_orig_sampling_config",
 "ominimo_FuelType_compare_value_orig_sampling_config",
 "ominimo_InternationalDriving_compare_value_orig_sampling_config",
 "ominimo_IsSuperCar_compare_value_orig_sampling_config",
 "ominimo_MaxDriverMainAge_compare_value_orig_sampling_config",
 "ominimo_Mileage_compare_value_orig_sampling_config",
 "ominimo_MinDriverMainAge_compare_value_orig_sampling_config",
 "ominimo_MinimalPrice_compare_value_orig_sampling_config",
 "ominimo_MutualCancellation_compare_value_orig_sampling_config",
 "ominimo_NewCar_compare_value_orig_sampling_config",
 "ominimo_NumberOfDrivers_compare_value_orig_sampling_config",
 "ominimo_NumberOfSeats_compare_value_orig_sampling_config",
 "ominimo_OdometerValueKnown_compare_value_orig_sampling_config",
 "ominimo_OwnerDriverSame_compare_value_orig_sampling_config",
 "ominimo_OwnershipMainDuration_compare_value_orig_sampling_config",
 "ominimo_PaymentFrequency_compare_value_orig_sampling_config",
 "ominimo_PostalCode_compare_value_orig_sampling_config",
 "ominimo_PreviousCancellationInsurer_compare_value_orig_sampling_config",
 "ominimo_PreviousCancellationNoPayment_compare_value_orig_sampling_config",
 "ominimo_SeasonalTyres_compare_value_orig_sampling_config",
 "ominimo_SteeringWheelRight_compare_value_orig_sampling_config",
 "ominimo_YearsSinceLastDamage_compare_value_orig_sampling_config",
 "ominimo_YoungDriverDiscount_compare_value_orig_sampling_config",
 "ominimo_contractor_age_compare_value_orig_sampling_config",
 "ominimo_contractor_mtpl_years_with_current_insurer_compare_value_orig_sampling_config",
 "ominimo_number_of_damages_caused_in_last_3_years_compare_value_orig_sampling_config",
 "ominimo_technical_exam_in_last_180_days_compare_value_orig_sampling_config",
 "ominimo_weekly_commuting_km_compare_value_orig_sampling_config",
 "ominimo_youngest_child_birth_year_compare_value_orig_sampling_config"
)



# Read selected columns into a data.table
dt <- as.data.table(read_parquet(file_path, col_select = all_of(cols_to_read)))[quote_type=='calculation' & substr(as.character(unique_id), 1, 3) == "811"]

# Convert 'no'/'yes' to 0/1 for specified columns
#cols_to_binary <- c("steering_wheel_located_right", "OdometerValueKnown", "seasonal_tyres", "owner_driver_same")

#dt[, (cols_to_binary) := lapply(.SD, function(x) as.integer(tolower(x) == "yes")), .SDcols = cols_to_binary]#csak a langyi verzióhoz


# Create a named vector: names are bonus_malus_current, values are bmcorr
bmc_map <- setNames(c(-4:10,-4:9), c(paste0("M0", 4:1), "A00", paste0("B0", 1:9), "B10", paste0("M", 4:1), "A0", paste0("B", 1:9)))

# Direct vectorized lookup
dt[, bmcorr := bmc_map[bonus_malus_current]]
dt[, bonus_malus_current := NULL]
setnames(dt, c("vehicle_power", "contractor_age", "postal_code"), c("kw", "clientage", "zip"))



# Define breaks
kw_breaks <- c(0, 38, 51, 71, 101, Inf)
age_breaks <- c(-1, 0, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, Inf)  # add Inf to close last interval

# Apply banding
dt[, kw_band := cut(fifelse(is.na(kw),51,kw), breaks = kw_breaks, right = FALSE, include.lowest = TRUE)]
dt[, age_band := cut(fifelse(is.na(clientage),46,clientage), breaks = age_breaks, right = FALSE, include.lowest = TRUE)]

kw_lookup <- data.table(
 kw_band = c("[0,38)", "[38,51)", "[51,71)", "[71,101)", "[101,Inf]"),
 kw_freq = c(1, 1.38888012, 1.473136845, 1.516762341, 1.404669543),
 kw_sev  = c(1, 1.026556984, 1.142958421, 1.165422581, 1.328707779)
)

dt <- kw_lookup[dt, on = "kw_band"]

age_lookup <- data.table(
 age_band = c(
  "[-1,0)", "[0,26)", "[26,31)", "[31,36)", "[36,41)", "[41,46)",
  "[46,51)", "[51,56)", "[56,61)", "[61,66)", "[66,71)", "[71,Inf]"
 ),
 age_freq = c(NA, 1, 0.739909506, 0.67313488, 0.676178085, 0.68085622,
              0.675356259, 0.663488256, 0.677147855, 0.706400172,
              0.745618022, 0.932103154),
 age_sev = c(NA, 1, 0.918823181, 0.824049385, 0.836260769, 0.832945428,
             0.924648353, 0.808623699, 0.835246996, 0.806792662,
             0.845747658, 0.809447474)
)

dt <- age_lookup[dt, on = "age_band"]

bm_lookup <- data.table(
 bmcorr = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA),
 bm_freq = c(3.809263194, 2.36651353, 2.350580978, 1.661957612, 1,
             0.899796403, 0.787811329, 0.729202824, 0.694348435,
             0.662978251, 0.653133669, 0.635783734, 0.704524095,
             0.635397969, 0.476860979, 0.476860979),
 bm_sev  = c(0.946915244, 0.820782963, 1.039353992, 0.922213984, 1,
             0.944254997, 0.85438175, 0.951697059, 0.890818215,
             0.899447044, 0.923377596, 0.88650593, 0.811041787,
             0.876588315, 0.888564496, 0.888564496)
)

dt <- bm_lookup[dt, on = "bmcorr"]

kkta_zip_price_muls <- fread("c:/C/Territory/kkta_zip_price_muls.csv")
dt <- kkta_zip_price_muls[dt, on = "zip"]

dt[, pp :=0.067173373 * 1.022752496 * 702800 * poli * kw_freq * age_freq * bm_freq * kw_sev * age_sev * bm_sev]

dt[, nm := pmin(
 genertel_predicted_price_sampling_config,
 kh_predicted_price_sampling_config,
 granit_predicted_price_sampling_config,
 alfa_predicted_price_sampling_config,
 groupama_predicted_price_sampling_config,
 generali_predicted_price_sampling_config,
 uniqa_predicted_price_sampling_config,
 union_predicted_price_sampling_config,
 allianz_predicted_price_sampling_config,
 na.rm = TRUE
)]


dt<-dt[!is.na(pp)]

fwrite(dt, file = "c:/c/xgboost/xgb_input_B.csv",sep = ";",dec = ",")
#dupes<-dt[unique_id==81100000706]
