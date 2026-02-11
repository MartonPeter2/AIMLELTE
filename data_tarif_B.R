useELTE_CP=F
useML=F
jobname="B251231"#ha ez változik, újra kell futtatni a modellt
inputname="B251231"
RDSinputname="B251231"#dt:244904sor#ha ez változik, akkor újra kell futtatni a mocsár lassú sparse matrix generatort
modelname="B251231"#ha ez változik, újra kell futtatni a modellt
PWmonth=2512
claimmonth=20251231
#earnedcorr=1#10783742/9113926#(9772742-1032881)/9113926#0.9589568
#claimcorr=1#5766800/4554551 #4315798/4554551#0.9475792
#TPcorr=1#4315798/(1-1032881/9772742)/0.36/11981585#0.7158506#HAHÓ!!!! ITT (1-1032881/9772742) -TEL OSZTANI KÉNE, NEM SZOROZNI
#(
absolute=T
nrounds = 100
params = list(
 booster   = "gblinear",
 updater   = "shotgun",
 alpha     = 0.000,
 lambda    = 0.00001,
 eta       = 0.1)
alpha_u <- 0.00005
alpha_o <- 0.005
eps <- 1e-9
w_under_profit <- 10
w_over_profit  <- 10
w_mid_profit   <- 1
w_loss_high    <- 1
w_loss_low     <- 1
target_undercut_abs=100
target_undercut_rel=0.992
target_loss_ratio=0.45
prior_price=10.74#10.871#+14% ::10.74#46166
library(data.table)
library(readxl)
library(bit64)
c <- fread("c:/C/OmiDB/omiDB_clmref.csv")[ sysdate == claimmonth,.(claim = sum(clmmat+clmres+clmpay-regres-regpay)),by = .(polref = as.integer64(polnum))]
#print('Az 4 warning amit dobott E24298-ig, azokat most úgyse nézzük úh OK')
#modmul<-fread("c:/C/OmiReport/2506/mod_multipliers_ominimo_policies_scored_with_correction_2025_09_01.csv")

#library(Matrix)
library(mltools)
library(xgboost)
dt<-as.data.table(fread(file = paste0("c:/c/xgboost/xgb_input_",inputname,".csv"),sep = ";",dec = ","))
dt[policy_premium=="35953,00 FtT",policy_premium:="35953"]
dt[,policy_premium:=as.integer(policy_premium)]
# dt[, .(
#  n_unique = uniqueN(unique_id),   # count distinct
#  min_id   = min(unique_id, na.rm = TRUE),
#  max_id   = max(unique_id, na.rm = TRUE)
# )]
# library(bit64)  # only if unique_id is integer64
# # Aggregate by first 3 characters
# dt[, .(N = .N), by = .(prefix = substr(as.character(unique_id), 1, 3))][order(prefix)]

#dt[, policy_gwp := NULL]
setnames(dt, "ominimo_predicted_price_sampling_config", "a")
dt<- c[dt, on = .(polref = unique_id)]

pw <- as.data.table(fread("c:/C/EarnedPrem/earnedDB.csv"))[monthcode == PWmonth, .(earned = sum(earned)),by = .(polref)]
dt<- pw[dt, on = "polref"]

features <- c(
 # "unique_id",
 # "pp",
 # "nm",
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
 #"ominimo_MinimalPrice_compare_value_orig_sampling_config",
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
 #"ominimo_number_of_damages_caused_in_last_3_years_compare_value_orig_sampling_config",#B
 "ominimo_technical_exam_in_last_180_days_compare_value_orig_sampling_config",
 "ominimo_weekly_commuting_km_compare_value_orig_sampling_config",
 "ominimo_youngest_child_birth_year_compare_value_orig_sampling_config"
)

analytics <-c(
 'mileage_foreign',
 'vehicle_ownership_start_year',
 'vehicle_make_year',
 'phone_number',
 'vehicle_financed',
 'technical_exam_in_last_180_days',
 'vehicle_age',
 'payment_method',
 'youngest_child_birth_year',
 'Mileage',
 'YoungestChildAge',
 'years_since_last_damage_caused',
 'vehicle_maker',
 'vehicle_power',
 'mileage_domestic',
 'OdometerValueKnown',
 'ekom',
 'LicenceAge',
 'contract_start_year_start_with_current_insurer_mtpl',
 'year_of_last_damage_caused',
 'N_claims',
 'vehicle_ownership_duration',
 'bonus_malus_current',
 'vehicle_fuel_type',
 'contractor_age',
 'quote_type',
 'driver_experience',
 'odometer_value',
 'steering_wheel_located_right',
 '5OrMoreCars',
 'use_days_abroad',
 'driver_licence_year',
 'owner_driver_same',
 'postal_code_calculation',
 'vehicle_usage',
 'email',
 'vehicle_used_abroad',
 'payment_frequency',
 'contractor_gender',
 'youngest_driver_birth_year',
 'is_public_worker',
 'N_claims_adjusted',
 'number_of_damages_caused_in_last_5_years',
 'daily_commuting_km',
 'new_entrant_to_bonus_malus_system',
 'calculation_reason',
 'postal_code',
 'current_insurer_mtpl',
 'MinDriverAge',
 'vehicle_weight_max',
 'first_mtpl_insured_year',
 'bonus_malus_next',
 'vehicle_weight_min',
 'number_of_drivers',
 'last_damage_year',
 'vehicle_engine_size',
 'seasonal_tyres',
 'is_retired',
 'vehicle_imported_from_abroad',
 'vehicle_number_of_seats',
 'vehicle_type',
 'contractor_birth_year',
 'DriverAge',
 'contractor_mtpl_years_with_current_insurer')

dt[, (features) := lapply(.SD, function(x) ifelse(is.na(x), -999, x)), .SDcols = features]
dt<-dt[!is.na(pp)]
dt<-dt[!is.na(nm)]
dt<-dt[!is.na(earned)]#ilyen nincs amúgy, nem is lenne muszáj, lehetne 0


objective <- function(preds, dtrain) {
 
 conversion <- function(x) (1 + tanh(-8.78002 * x + 8.32548)) / 2
 
 sigma_under <- 1 / (1 + exp(-(tap - preds)/alpha_u))
 sigma_over  <- 1 / (1 + exp(-(preds -   u)/alpha_o))
 sigma_mid   <- 1 - sigma_under - sigma_over
 
 grad_vec <- numeric(length(preds))
 hess_vec <- numeric(length(preds))
 
 profit_mask <- (u > tap)
 
 # --- Profit-making policies ---
 if (any(profit_mask)) {
  idx <- which(profit_mask)
  p <- preds[idx]
  m_i <- u[idx]
  tap_i <- tap[idx]
  
  diff_tap_p <- tap_i - p
  diff_p_m   <- p - m_i
  
  grad_under <- w_under_profit * (-2 * diff_tap_p) + w_mid_profit * (2 * diff_p_m)
  grad_mid   <- w_mid_profit * (2 * diff_p_m)
  grad_over  <- w_over_profit * (2 * diff_p_m) * (m_i - tap_i)
  
  hess_under <- 2 * (w_under_profit + w_mid_profit)
  hess_mid   <- 2 * w_mid_profit
  hess_over  <- 2 * w_over_profit * (m_i - tap_i)
  
  grad_vec[idx] <- grad_under * sigma_under[idx] + grad_mid * sigma_mid[idx] + grad_over * sigma_over[idx]
  hess_vec[idx] <- hess_under * sigma_under[idx] + hess_mid * sigma_mid[idx] + hess_over * sigma_over[idx]
 }
 
 # --- Loss-making policies ---
 if (any(!profit_mask)) {
  idx <- which(!profit_mask)
  p <- preds[idx]
  tap_i <- tap[idx]
  m_i   <- m[idx]
  
  diff_m_p   <- m_i - p
  diff_tap_p <- tap_i - p
  
  grad_low  <- w_loss_low  * (-2 * diff_m_p)
  grad_high <- w_loss_high * (-2 * diff_tap_p)
  
  hess_low  <- 2 * w_loss_low
  hess_high <- 2 * w_loss_high
  
  grad_vec[idx] <- grad_low + grad_high
  hess_vec[idx] <- hess_low + hess_high
 }
 
 return(list(grad = grad_vec, hess = hess_vec))
}


dt[,nt:=pp/target_loss_ratio]
dt[,t:=if(absolute){log(nt)-prior_price}else{nt/a}]
dt[,m:=if(absolute){log(nm)-prior_price}else{nm/a}]
dt[,acc:=if(absolute){
    log(pmax(nm-target_undercut_abs,nm*target_undercut_rel,nt))-prior_price}
   else{pmax(nm-target_undercut_abs,nm*target_undercut_rel,nt)/a}]
dt[, nacc:=if(absolute){exp(acc+  prior_price)}else{acc*a}]
# 1️⃣ Filter to features with more than 1 unique value
features_multi <- features[sapply(dt[, ..features], uniqueN) > 1]

# 2️⃣ Convert once to factors
#dt_factor <- dt[, lapply(.SD, factor), .SDcols = features_multi]

# dt[, (features_multi) := lapply(.SD, function(x) {
#  x <- as.character(x)
#  x[is.na(x)] <- "NA_value"  # optional: explicit label for missing
#  
#  # find most frequent value
#  tab <- sort(table(x), decreasing = TRUE)
#  baseline <- names(tab)[1]
#  
#  # put the most frequent first, keep the rest in original order
#  factor(x, levels = c(baseline, setdiff(unique(x), baseline)))
# }), .SDcols = features_multi]


dt_factor <- dt[, lapply(.SD, function(x) {
 # Convert to factor with " " as the first level, then existing levels
 factor(x, levels = c(" ", sort(unique(x))))
}), .SDcols = features_multi]

# --- 80–20 TRAIN–TEST SPLIT ---
set.seed(1234)

# Create random index
train_idx <- sample(seq_len(nrow(dt_factor)), size = 0.8 * nrow(dt_factor))

# Split both dt and dt_factor
dt_train <- dt[train_idx]
dt_test  <- dt[-train_idx]
dt_factor_train <- dt_factor[train_idx]
dt_factor_test  <- dt_factor[-train_idx]

# Optional check
cat("Train:", nrow(dt_train), "rows; Test:", nrow(dt_test), "rows\n")

library(data.table)
library(Matrix)

fast_sparse_ohe_robust <- function(dt) {
 # Adat másolat készítése nélkül dolgozunk referenciával, de biztosítjuk a data.table formátumot
 dt <- as.data.table(dt)
 n_rows <- nrow(dt)
 
 # Listák az indexek tárolására
 j_indices <- list() # Oszlop indexek
 i_indices <- list() # Sor indexek
 
 current_col_offset <- 0
 
 cat("Feldolgozás oszloponként:\n")
 
 # Végigmegyünk az oszlopokon
 for (col_name in names(dt)) {
  # Kiolvassuk az oszlopot
  val <- dt[[col_name]]
  
  # Ha faktor vagy karakter, akkor kódoljuk
  if (is.factor(val) || is.character(val)) {
   # Faktorrá alakítás (NA-k kezelése: az NA itt elveszik, ami a sparse matrixban 0 lesz)
   val_fact <- as.factor(val)
   
   # Szintek száma
   n_levels <- length(levels(val_fact))
   
   # Integerré konvertálás
   val_int <- as.integer(val_fact)
   
   # Ahol NEM NA az érték, csak azokat tartjuk meg
   not_na_idx <- which(!is.na(val_int))
   
   # Hozzáadjuk a listához a megfelelő eltolással
   # Az 'i' a sor sorszáma, a 'j' az eltolt oszlop sorszáma
   i_indices[[col_name]] <- not_na_idx
   j_indices[[col_name]] <- val_int[not_na_idx] + current_col_offset
   
   # Növeljük az eltolást a következő változónak
   current_col_offset <- current_col_offset + n_levels
   
  } else {
   # Ha már eleve szám (numeric/integer), akkor dönteni kell.
   # Mivel 'dt_factor_train' a neve, feltételezem, mindent faktorként akarsz kezelni.
   # Ha vannak benne folytonos változók, azokat máshogy kéne, de most kódoljuk át ezt is:
   val_fact <- as.factor(val)
   n_levels <- length(levels(val_fact))
   val_int <- as.integer(val_fact)
   
   not_na_idx <- which(!is.na(val_int))
   
   i_indices[[col_name]] <- not_na_idx
   j_indices[[col_name]] <- val_int[not_na_idx] + current_col_offset
   
   current_col_offset <- current_col_offset + n_levels
  }
 }
 
 cat("Vektorok összefűzése...\n")
 # Összefűzzük a listákat egyetlen hosszú vektorrá
 final_i <- unlist(i_indices, use.names = FALSE)
 final_j <- unlist(j_indices, use.names = FALSE)
 
 cat("Sparse Matrix gyártása...\n")
 # Létrehozzuk a mátrixot
 # A dims szélessége a 'current_col_offset', ami az összes faktorszint összege
 X_sparse <- sparseMatrix(
  i = final_i,
  j = final_j,
  x = 1,
  dims = c(n_rows, current_col_offset)
 )
 
 return(X_sparse)
}

# --- FUTTATÁS ---
X_train <- fast_sparse_ohe_robust(dt_factor_train)
X_test <- fast_sparse_ohe_robust(dt_factor_test)
X_all <- fast_sparse_ohe_robust(dt_factor)

dtrain <- xgb.DMatrix(data = X_train)
dtest  <- xgb.DMatrix(data = X_test)
dall  <- xgb.DMatrix(data = X_all)
dt[, .N == uniqueN(polref)]

# tap <- dt_train[,t]
# m   <- dt_train[,m]
# u   <- dt_train[,acc]
#model <- xgb.train(data = dtrain,nrounds = nrounds,objective = objective,params = params)
#saveRDS(model, paste0(file = "c:/C/XGBoost/model_tr_",jobname,".rds"))
# 
# tap <- dt[,t]
# m   <- dt[,m]
# u   <- dt[,acc]
# model <- xgb.train(data = dall  ,nrounds = nrounds,objective = objective,params = params)
# saveRDS(model, paste0(file = "c:/C/XGBoost/model_all_",modelname,".rds"))

model<-readRDS(paste0(file = "c:/C/XGBoost/model_all_",modelname,".rds"))

if(!useELTE_CP){dt[, p := predict(model, dall)]}else{
load("w:/Aktuarius/ELTE/eredmeny_25_09/paraméter optimalizálás/data_tarif_B_tot.RData")

tmp <- data_tarif_B_tot[, .(
 polref = as.integer64(polref),
 p = premium
)]

setDT(tmp)
setDT(dt)

# legyenek kulcsaink
setkey(tmp, polref)
setkey(dt, polref)

# join: dt-hez add hozzá tmp$p-t
dt[tmp, p := if(absolute){log(i.p)-prior_price}else{i.p/a}]
}

dt[, np := if (absolute) exp(p + prior_price) else p * a]
dt[, omip:=ifelse(nm<=a,0,a-nt)]
dt[, modp:=ifelse(m<=p,0,np-nt)]
dt[, advantage:=modp-omip]
dt[, benefit:=modp-omip-mean(modp)+mean(omip)]

juncols <- c(
 "polref",
 "earned",
 "claim",
 "bonus_malus_current",
 "bm_freq",
 "bm_sev",
 "postal_code",
 "poli",
 "i.bm_freq",
 "i.bm_sev",
 "age_band",
 "age_freq",
 "age_sev",
 "kw_band",
 "kw_freq",
 "kw_sev",
 "policy_start_date",
 "vehicle_power",
 "contractor_age",
 "policy_premium",
 "a",
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
 "ominimo_technical_exam_in_last_180_days_compare_value_orig_sampling_config",
 "ominimo_weekly_commuting_km_compare_value_orig_sampling_config",
 "ominimo_youngest_child_birth_year_compare_value_orig_sampling_config",
 "mileage_foreign",
 "vehicle_ownership_start_year",
 "vehicle_make_year",
 "phone_number",
 "vehicle_financed",
 "technical_exam_in_last_180_days",
 "vehicle_age",
 "payment_method",
 "youngest_child_birth_year",
 "Mileage",
 "YoungestChildAge",
 "years_since_last_damage_caused",
 "vehicle_maker",
 "mileage_domestic",
 "OdometerValueKnown",
 "ekom",
 "LicenceAge",
 "contract_start_year_start_with_current_insurer_mtpl",
 "year_of_last_damage_caused",
 "N_claims",
 "vehicle_ownership_duration",
 "vehicle_fuel_type",
 "driver_experience",
 "odometer_value",
 "steering_wheel_located_right",
 "5OrMoreCars",
 "use_days_abroad",
 "driver_licence_year",
 "owner_driver_same",
 "postal_code_calculation",
 "vehicle_usage",
 "email",
 "vehicle_used_abroad",
 "payment_frequency",
 "contractor_gender",
 "youngest_driver_birth_year",
 "is_public_worker",
 "number_of_damages_caused_in_last_5_years",
 "daily_commuting_km",
 "new_entrant_to_bonus_malus_system",
 "calculation_reason",
 "current_insurer_mtpl",
 "MinDriverAge",
 "vehicle_weight_max",
 "first_mtpl_insured_year",
 "bonus_malus_next",
 "vehicle_weight_min",
 "number_of_drivers",
 "last_damage_year",
 "vehicle_engine_size",
 "seasonal_tyres",
 "is_retired",
 "vehicle_imported_from_abroad",
 "vehicle_number_of_seats",
 "pp",
 "nm",
 "nt",
 "t",
 "m",
 "acc",
 "nacc",
 "p",
 "np",
 "omip",
 "modp",
 "advantage",
 "benefit"
)
dt[,policy_premium:=as.integer(policy_premium)]
dt[, maxdij := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = patterns("predicted_price_sampling_config")]
dt[, atlag := rowMeans(.SD, na.rm = TRUE), .SDcols = patterns("predicted_price_sampling_config")]
dt[, mindij := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = patterns("predicted_price_sampling_config")]
dt[,emel5:=policy_premium*1.05]
dt[,emel20:=policy_premium*1.2]
dt[,kar0dij1:=mindij*1]
dt[,kar0dij2:=pmax(emel5,kar0dij1)]
dt[,kar0dij3:=pmax(nt,kar0dij2)]
dt[,kar0dij4:=pmin(emel20,kar0dij3)]
dt[,karosdij1:=policy_premium*2]
dt[,karosdij2:=pmax(atlag,karosdij1)]
dt[,karosdij3:=pmax(nt,karosdij2)]
dt[,karosdij4:=pmin(maxdij,karosdij3)]
dt[, dij := fifelse(claim <= 0|is.na(claim), kar0dij4,karosdij4)]
dt[,szorzo:=dij/a]

juncols <- c(
juncols, 
"maxdij", 
"atlag", 
"mindij",
"emel5", 
"emel20",
"kar0dij1", 
"kar0dij2", 
"kar0dij3", 
"kar0dij4",
"karosdij1", 
"karosdij2", 
"karosdij3", 
"karosdij4",
"dij",
"szorzo"
)

fwrite(dt[month(policy_start_date)==6,..juncols],  paste0(file = "c:/C/XGBoost/juniusi_fordulosok_",jobname,".csv"),  sep = ";", dec = ",")
fwrite(dt[,.(polref,price=round(np))],  paste0(file = "c:/C/XGBoost/polprice_",jobname,".csv"),  sep = ";", dec = ",")
fwrite(dt,  paste0(file = "c:/C/XGBoost/fitted_",jobname,".csv"),  sep = ";", dec = ",")

#fwrite(xgboost::xgb.importance(model = model), file = paste0(file = "c:/C/XGBoost/multipliers_",jobname,".csv"),sep = ";",dec = ",")
#xgb.dump(model)
# dt[, p := predict(model, dtrain)]
# dt[, np:=if(absolute){exp(p+  prior_price)}else{p*a}]
# fwrite(setorder(dt[, setdiff(names(dt), c("pp", "nm", "nt", "t", "m", "acc", "p","i.bm_freq","i.bm_sev")), with = FALSE],polref), file = paste0("C:/C/XGBoost/omi_portfolio_recalc_v2",jobname,".csv"),sep = ";", dec = ",")
# fwrite(setorder(dt[, c("polref","m", "nm", "nt", "t", "m", "acc", "nacc", "p","np")],polref), file = paste0("C:/C/XGBoost/omi_plotter_input",jobname,".csv"),sep = ";", dec = ",")

dt_test[, p := predict(model, dtest)]
dt_test[, np := if (absolute) exp(p + prior_price) else p * a]
fwrite(dt_test,  paste0(file = "c:/C/XGBoost/train_test_split_",jobname,".csv"),  sep = ";", dec = ",")

dt[, mean(a, na.rm = TRUE)]#46278.83  ELTE_LE2:46355.18
dt[, mean(np, na.rm = TRUE)]#46475.41 ELTE_LE2:48367.99
dt[, mean(pp, na.rm = TRUE)]#46278.83  ELTE_LE2:46355.18
dt[, mean(ppp, na.rm = TRUE)]#46475.41 ELTE_LE2:48367.99


if(useML){
load("W:\\Aktuarius\\ELTE\\ML\\UjData_class_id.RData")#UjData_class_id
UjData_class_id <- as.data.table(UjData_class_id)
dt <- UjData_class_id[dt, on = .(ujcid = polref)]
setnames(dt, old = "ujcid", new = "polref")
}


#Compute New counts and impact
write.table(rbindlist(list(
 dt[ujclass== 1   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='1.  nagy járművek, 7 ülés')],
 dt[ujclass== 2   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='2.  régóta meglévő jármű, jó bónusz')],
 dt[ujclass== 3   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='3.  fiatal szerződő és sofőr, rossz bónusz')],
 dt[ujclass== 4   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='4.  Nagyon nehéz jármű – lényegtelen, pici')],
 dt[ujclass== 5   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='5.  Sok hazai km, fiatal jármű')],
 dt[ujclass== 6   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='6.  Erős autók, súlyosak')],
 dt[ujclass== 7   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='7.  Fiatal vezető/szerződő, gyenge, kis autó')],
 dt[ujclass== 8   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='8.  Idős(ebb) vezető/szerződő, gyenge, kis autó')],
 dt[ujclass== 9   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='9.  Nagy teljesítmény, kis motor, nehéz, fiatal! autó = elektromos?')],
 dt[ujclass==10   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='10. Idős vezető/szerződő, öreg, gyenge, kis autó, régóta tulajdonban, nagyon jó bónusz')],
 dt[ujclass==11   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='11. Idős vezető/szerződő, fiatal, átlagos autó, viszonylag jó bónusz')],
 dt[ujclass==12   ,.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='12. nagyon régi jogsi, nemtúl jó bónusz')],
 dt[is.na(ujclass),.(Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), OmiFl=9, ModFl=9, OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='13. egyéb, nem besorolt')])
), "clipboard", sep = "\t", dec=",", row.names = FALSE, col.names = TRUE)
}


# Compute New counts and impact
write.table(rbindlist(list(
 dt[nm > nt & nm <= np & nm <= a,                   .(OmiFl=0, ModFl=0, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Mindkettő kiáraz jó szerződést')],
 dt[nm > nt & nm <= np & nm >  a & a <  nt,         .(OmiFl=1, ModFl=0, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Az új model kiárazott egy jó szerződést, aminek az Ominimo viszont annyira alávágott, hogy az már veszteséges lett')],
 dt[nm > nt & nm <= np & nm >  a & a >= nt,         .(OmiFl=1, ModFl=0, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Az új model kiárazott egy jó szerződést, amit az Ominimo legolcsóbbra tarifált, de azért még profitábilisra')],
 dt[nm > nt & nm >  np & nm >  a & a >= nt & np>=nt,.(OmiFl=1, ModFl=1, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Mindkettő legolcsóbbra és nyereségesre tarifált jó szerződést')],
 dt[nm > nt & nm >  np & nm >  a & a >= nt & np< nt,.(OmiFl=1, ModFl=1, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Mindkettő legolcsóbbra tarifált jó szerződést, de csak az Ominimo árával maradt nyereséges')],
 dt[nm > nt & nm >  np & nm >  a & a <  nt & np>=nt,.(OmiFl=1, ModFl=1, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Mindkettő legolcsóbbra tarifált jó szerződést, de csak az új model árával maradt nyereséges')],
 dt[nm > nt & nm >  np & nm >  a & a <  nt & np< nt,.(OmiFl=1, ModFl=1, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Mindkettő annyira alávágott egy jó szerződésnek, hogy veszteséges lett')],
 dt[nm > nt & nm >  np & nm <= a & np>=nt,          .(OmiFl=0, ModFl=1, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Az új model profitábilisan alávágott egy jó szerződének, amit az Ominimo kiárazott')],
 dt[nm > nt & nm >  np & nm <= a & np< nt,          .(OmiFl=0, ModFl=1, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Az új model veszteségesen alávágott egy amúgy jó szerződének, amit az Ominimo kiárazott')],
 dt[nm < nt & nm >  np & nm >  a,                   .(OmiFl=1, ModFl=1, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Mindkettő legolcsóbbra tarifált rossz szerződést')],
 dt[nm < nt & nm >  np & nm <= a,                   .(OmiFl=0, ModFl=1, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Az új model legolcsóbbra tarifált rossz szerződést, amit az Ominimo kiárazott')],
 dt[nm < nt & nm <= np & nm >  a,                   .(OmiFl=1, ModFl=0, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Az Ominimo legolcsóbbra tarifált rossz szerződést, amit az új model kiárazott')],
 dt[nm < nt & nm <= np & nm <= a,                   .(OmiFl=0, ModFl=0, Count = .N, Market=sum(nm), Technical = sum(nt), Value=sum(nm)-sum(nt), Omi=sum(a), Model=sum(np), OmiProfit=sum(ifelse(nm<=a,0,a-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=a,0,nm-a)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), Earned=sum(earned, na.rm=T), CurrentPrem=sum(policy_premium, na.rm=T), ActualClaim=sum(claim, na.rm = T), OmiCnt=sum(ifelse(nm<=a,0,1)), ModCnt=sum(ifelse(m<=p,0,1)), OmiPrem=sum(ifelse(nm<=a,0,a)), ModPrem=sum(ifelse(nm<=np,0,np)), OmiEarn=sum(ifelse(nm<=a,0,earned*a/policy_premium)), ModEarn=sum(ifelse(nm<=np,0,earned*np/policy_premium)), OmiCost=sum(ifelse(nm<=a,0,nt)), ModCost=sum(ifelse(m<=p,0,nt)), OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = T), ModClaim=sum(ifelse(m<=p,0,claim), na.rm = T), Label='Mindkettő kiárazott rossz szerződést')])
), "clipboard", sep = "\t", dec=",", row.names = FALSE, col.names = TRUE)


library(data.table)
library(ggplot2)
library(scales)

# Helper function to create the plot for a filtered data.table subset
plot_sorted_lines <- function(sub_dt, title) {
 tap_sorted <- sort(sub_dt$nt)
 m_sorted   <- sort(sub_dt$nm)
 p_sorted   <- sort(sub_dt$np)
 a_sorted   <- sort(sub_dt$a)
 
 max_len <- max(length(tap_sorted), length(m_sorted), length(p_sorted), length(a_sorted))
 
 plot_dt <- data.table(
  idx = 1:max_len,
  tap = rep(tap_sorted, length.out = max_len),
  m   = rep(m_sorted, length.out = max_len),
  p   = rep(p_sorted, length.out = max_len),
  a   = rep(a_sorted, length.out = max_len)
 )
 
 plot_long <- melt(
  plot_dt, id.vars = "idx", variable.name = "series", value.name = "value"
 )
 
 colors <- c(tap = "red", m = "green", p = "purple", a = "blue")
 p <- ggplot(plot_long, aes(x = idx, y = value, color = series)) +
  geom_line(size = 1) +
  scale_color_manual(
   values = colors,
   labels = c("Break-even", "Competition", "Model", "Ominimo"),  # legend labels
   name = "Premiums:"                                         # legend title
  ) +
  theme_minimal() +
  theme(axis.title.y = element_blank())+
  coord_cartesian(ylim = c(0, 1000000)) +
  theme(legend.position = "bottom") +
  labs(title=title, x = "Distribution/count")
 return(p)
}


plot_profit <- plot_sorted_lines(dt[m > t],paste0("Distributions for Profit-Making (CR<", percent(target_loss_ratio), ") Quotes"))
plot_non_profit <- plot_sorted_lines(dt[m <= t],paste0("Distributions for Loss-Making (CR>", percent(target_loss_ratio), ") Quotes"))
library(patchwork)
combined_plot <- plot_profit + plot_non_profit  # now this works
ggsave(
 filename = paste0("c:/c/xgboost/", jobname, "_distribution.png"),
 plot = combined_plot,
 width = 1820,
 height = 600,
 units = "px",
 dpi = 72
)

multipliers=xgb.importance(model = model)[,Multiplier:=exp(Weight)][,Weight:=NULL]
#multipliers=xgb.importance(model = model)[,Multiplier:=Weight]
fwrite(multipliers, paste0("c:/c/xgboost/multipliers_",jobname,".csv"), sep = ";", dec = ",")
#fwrite(xgb.importance(model = model), paste0("c:/c/xgboost/multipliers_native_weights.csv"), sep = ";", dec = ",")
#fwrite(data.table(line = gsub("\\.", ",",xgb.dump(model))), paste0("c:/c/xgboost/multipliers_native_dump.csv"), sep = ";", dec = ",")


featureoneway <- rbindlist(lapply(features, function(f) {
 dt[ , .(feature = f,
    value = as.character(get(f)),
    #benefit = mean(benefit, na.rm = TRUE),
    #advantage = sum(advantage, na.rm = TRUE),
    Count = .N,
    Market=sum(nm),
    Technical = sum(nt),
    possprofit=sum(nm)-sum(nt),
    Omi=sum(a),
    Model=sum(np),
    ModMul=sum(np)/sum(a),
    OmiProfit=sum(ifelse(nm<=a,0,a-nt)),
    ModelProfit=sum(ifelse(m<=p,0,np-nt)),
    OmiUndercut=sum(ifelse(nm<=a,0,nm-a)),
    ModelUndercut=sum(ifelse(m<=p,0,nm-np)),
    ActualClaim=sum(claim, na.rm = T),
    ActualEarned=sum(earned, na.rm = T)
  ),
  by = f][, (f) := NULL]
}), use.names = TRUE, fill = TRUE)

featureoneway[, Feature := paste0(feature, value)]

merged <- merge(
 featureoneway,
 xgboost::xgb.importance(model = model),
 by = "Feature",
 all.x = TRUE
)

merged[, Feature := NULL]
merged[, multiplier := exp(Weight)]
merged[, Weight := NULL]

# write.table(merged[feature %in% c('ominimo_contractor_age_compare_value_orig_sampling_config'
#                                  ,'ominimo_DriverAgeBonusMalus_compare_value_orig_sampling_config'
#                                  ,'ominimo_territory_sampling_config')], "clipboard", sep = "\t", dec=",", row.names = FALSE, col.names = TRUE)
# 
# fwrite(merged[feature %in% c('ominimo_contractor_age_compare_value_orig_sampling_config'
#                              ,'ominimo_DriverAgeBonusMalus_compare_value_orig_sampling_config'
#                              ,'ominimo_territory_sampling_config')], "c:/c/xgboost/top_change_whole_feature.csv", sep = ";", dec = ",")
# 
# 
# fwrite(merged[feature %in% c('ominimo_CarAge_compare_value_orig_sampling_config'
#                              ,'ominimo_CarMake_compare_value_orig_sampling_config'
#                              ,'ominimo_FuelType_compare_value_orig_sampling_config')], "c:/c/xgboost/top_change_whole_feature2.csv", sep = ";", dec = ",")

fwrite(merged, paste0("c:/c/xgboost/top_change_whole_feature_",jobname,".csv"), sep = ";", dec = ",")
#dt[,.N,.(ominimo_contractor_age_compare_value_orig_sampling_config,clientage)]<---------HIBA!!!


analyticsoneway_test <- rbindlist(lapply(setdiff(analytics, c("postal_code","postal_code_calculation")), function(f) {
 dt_test[ , .(feature = f,
         value = as.character(get(f)),
         #benefit = mean(benefit, na.rm = TRUE),
         #advantage = sum(advantage, na.rm = TRUE),
         Count = .N,
         Market=sum(nm),
         Technical = sum(nt),
         possprofit=sum(nm)-sum(nt),
         Omi=sum(a),
         Model=sum(np),
         ModMul=sum(np)/sum(a),
         OmiProfit=sum(ifelse(nm<=a,0,a-nt)),
         ModelProfit=sum(ifelse(m<=p,0,np-nt)),
         OmiUndercut=sum(ifelse(nm<=a,0,nm-a)),
         ModelUndercut=sum(ifelse(m<=p,0,nm-np)),
         ActualClaim=sum(claim, na.rm = T),
         OmiClaim=sum(ifelse(nm<=a,0,claim), na.rm = TRUE),
         ModelClaim=sum(ifelse(m<=p,0,claim), na.rm = TRUE)
 ),
 by = f][, (f) := NULL]
}), use.names = TRUE, fill = TRUE)
analyticsoneway_test[,improve:=pmin(2,pmax(0,ModelClaim/OmiClaim*Omi/Model))]
analyticsoneway_test[is.na(improve), improve := 1]
fwrite(analyticsoneway_test, paste0("c:/c/xgboost/change_analytics_",jobname,".csv"), sep = ";", dec = ",")

library(data.table)
library(ggplot2)
library(data.table)
library(ggplot2)

# Suppose analyticsoneway is your data.table
# analyticsoneway <- data.table(feature=..., value=..., Count=..., ModMul=...)

# File path for PDF
pdf(paste0("c:/c/xgboost/modmul_",jobname,".pdf"), width = 10, height = 6)

features <- unique(analyticsoneway_test$feature)


for (f in features) {
 
 dtf <- analyticsoneway_test[feature == f]
 
 # Try to convert to numeric
 dtf[, value_num := suppressWarnings(as.numeric(value))]
 
 # Split numeric vs non-numeric
 numeric_part     <- dtf[!is.na(value_num)][order(value_num)]
 non_numeric_part <- dtf[is.na(value_num)][order(value)]
 
 # Combine: numeric first, then non-numeric
 dtf_sorted <- rbindlist(list(numeric_part, non_numeric_part), use.names = TRUE)
 
 # Create ordered factor for plotting
 dtf_sorted[, value_plot := factor(value, levels = value)]
 
 # Scale Count for dual-axis plotting
 scaleFactor <- max(dtf_sorted$ModMul, na.rm = TRUE) / max(dtf_sorted$Count, na.rm = TRUE)
 dtf_sorted[, Count_scaled := Count * scaleFactor]
 
 # Create plot
 p <- ggplot(dtf_sorted, aes(x = value_plot)) +
  geom_bar(aes(y = Count_scaled), stat = "identity",
           fill = "yellow", color = "blue") +
  geom_hline(yintercept = 1, color = "cyan", linetype = "solid") +
  geom_line(aes(y = ModMul, group = 1), color = "red", size = 1) +
  scale_y_continuous(
   name = "Multiplier modification recommendation (ceteris paribus)",
   sec.axis = sec_axis(~ . / scaleFactor, name = "Count")
  ) +
  ggtitle(paste("Where Red is above Cyan, Price Increase Recommended             ",f)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 print(p)
}

dev.off()
 cat("All plots saved to:...\n")
################################################################################
pdf(paste0("c:/c/xgboost/CR_change_",jobname,".pdf"), width = 10, height = 6)

features <- unique(analyticsoneway_test$feature)


for (f in features) {
 
 dtf <- analyticsoneway_test[feature == f]
 
 # Try to convert to numeric
 dtf[, value_num := suppressWarnings(as.numeric(value))]
 
 # Split numeric vs non-numeric
 numeric_part     <- dtf[!is.na(value_num)][order(value_num)]
 non_numeric_part <- dtf[is.na(value_num)][order(value)]
 
 # Combine: numeric first, then non-numeric
 dtf_sorted <- rbindlist(list(numeric_part, non_numeric_part), use.names = TRUE)
 
 # Create ordered factor for plotting
 dtf_sorted[, value_plot := factor(value, levels = value)]
 
 # Scale Count for dual-axis plotting
 scaleFactor <- max(dtf_sorted$improve, na.rm = TRUE) / max(dtf_sorted$Count, na.rm = TRUE)
 dtf_sorted[, Count_scaled := Count * scaleFactor]
 
 # Create plot
 p <- ggplot(dtf_sorted, aes(x = value_plot)) +
  geom_bar(aes(y = Count_scaled), stat = "identity",
           fill = "yellow", color = "blue", size = 0.3) +
  geom_hline(yintercept = 1, color = "cyan", linetype = "solid") +
  geom_line(aes(y = improve, group = 1), color = "red", size = 1) +
  scale_y_continuous(
   name = "Ratio of Claim Ratios (capped by 2)",
   sec.axis = sec_axis(~ . / scaleFactor, name = "Count")
  ) +
  geom_point(                # dot markers on the red line
   aes(y = improve),
   color = "red",
   size = 2
  ) +
  ggtitle(paste("Where Red is below Cyan, Claim Ratio improved             ",f)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 print(p)
}

dev.off()
cat("All plots saved to:...\n")
################################################################################
pdf_file <- "C:/C/xgboost/analyticsoneway_TP_change2.pdf"
pdf(pdf_file, width = 10, height = 6)

factoneway <- rbindlist(lapply(setdiff(analytics, c("postal_code","postal_code_calculation")), function(f) {
 dt[ , .(feature = f,
         value = as.character(get(f)),
         Count = .N,
         Market=sum(nm),
         Technical = sum(nt),
         possprofit=sum(nm)-sum(nt),
         Omi=sum(a),
         ActualClaim=sum(claim, na.rm = T)
 ),
 by = f][, (f) := NULL]
}), use.names = TRUE, fill = TRUE)

for (f in unique(factoneway$feature)) {
 # Subset feature
 dtf <- factoneway[feature == f]
 
 # Try to convert to numeric
 dtf[, value_num := suppressWarnings(as.numeric(value))]
 
 # Split numeric vs non-numeric
 numeric_part     <- dtf[!is.na(value_num)][order(value_num)]
 non_numeric_part <- dtf[is.na(value_num)][order(value)]
 
 # Combine: numeric first, then non-numeric
 dtf_sorted <- rbindlist(list(numeric_part, non_numeric_part), use.names = TRUE)
 
 # Ordered factor for x-axis
 dtf_sorted[, value_plot := factor(value, levels = unique(value))]
 
 # Create plot with two lines
 p <- ggplot(dtf_sorted, aes(x = value_plot)) +
  geom_line(aes(y = ActualClaim, group = 1), color = "red", size = 1) +
  geom_point(aes(y = ActualClaim), color = "red", size = 2) +
  geom_line(aes(y = Market, group = 1), color = "blue", size = 1) +
  geom_point(aes(y = Market), color = "blue", size = 2) +
  ylab("Value") +
  ggtitle(paste("Feature:", f)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 print(p)
}

dev.off()
cat("All plots saved to:", pdf_file, "\n")
################################################################################
pdf_file <- "C:/C/xgboost/Omi_Market_vs_normActualClaim.pdf"
pdf(pdf_file, width = 10, height = 6)

factoneway <- rbindlist(lapply(setdiff(analytics, c("postal_code","postal_code_calculation")), function(f) {
 dt[ , .(feature = f,
         value = as.character(get(f)),
         Count = .N,
         Market = sum(nm),
         Technical = sum(nt),
         possprofit = sum(nm)-sum(nt),
         Omi = sum(a),
         ActualClaim = sum(claim, na.rm = TRUE)
 ),
 by = f][, (f) := NULL]
}), use.names = TRUE, fill = TRUE)

for (f in unique(factoneway$feature)) {
 
 # Subset feature
 dtf <- factoneway[feature == f]
 
 # Try to convert to numeric
 dtf[, value_num := suppressWarnings(as.numeric(value))]
 
 # Split numeric vs non-numeric
 numeric_part     <- dtf[!is.na(value_num)][order(value_num)]
 non_numeric_part <- dtf[is.na(value_num)][order(value)]
 
 # Combine: numeric first, then non-numeric
 dtf_sorted <- rbindlist(list(numeric_part, non_numeric_part), use.names = TRUE)
 
 # Ordered factor for x-axis
 dtf_sorted[, value_plot := factor(value, levels = unique(value))]
 
 # Scale ActualClaim to sum(ActualClaim) = sum(Market)
 scaleFactor <- sum(dtf_sorted$Market, na.rm = TRUE) / sum(dtf_sorted$ActualClaim, na.rm = TRUE)
 dtf_sorted[, ActualClaim_scaled := ActualClaim * scaleFactor]
 
 # Create plot with two lines
 p <- ggplot(dtf_sorted, aes(x = value_plot)) +
  geom_line(aes(y = ActualClaim_scaled, group = 1), color = "red", size = 1) +
  geom_point(aes(y = ActualClaim_scaled), color = "red", size = 2) +
  geom_line(aes(y = Market, group = 1), color = "blue", size = 1) +
  geom_point(aes(y = Market), color = "blue", size = 2) +
  ylab("Value") +
  ggtitle(paste("Feature:", f)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 print(p)
}

dev.off()
cat("All plots saved to:", pdf_file, "\n")

library(ggplot2)
library(data.table)

ggplot(dt_test) +
 geom_histogram(aes(x = log(a), y = ..density..), fill = "blue", alpha = 0.5, bins = 30) +
 geom_histogram(aes(x = log(np), y = ..density..), fill = "red", alpha = 0.5, bins = 30) +
 labs(x = "Log(price)", y = "Density", title = "Omi (blue) and MP (red) price") +
 theme_minimal()

library(ggplot2)
library(data.table)

# Log-transform densities but show original prices on X-axis
ggplot(dt_test) +
 geom_density(aes(x = log(a), color = "Omi"), size = 1) +
 geom_density(aes(x = log(np), color = "Mod"), size = 1) +
 scale_x_continuous(
  name = "Offered Price Distribution",
  breaks = log(c(exp(9), exp(10), exp(11), exp(12), exp(13), exp(14))),  # adjust as needed
  labels = function(x) round(exp(x))#labels = c(exp(9), 5000, 10000, 50000, 100000, 500000)
 ) +
 scale_color_manual(name = "Variable", values = c("Omi" = "blue", "Mod" = "red")) +
 labs(y = "Density", title = paste0("Omi (blue) and ",inputname," (red) price")) +
 theme_minimal()

################################################################################