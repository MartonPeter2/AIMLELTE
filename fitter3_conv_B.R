library(data.table)
library(readxl)
library(bit64)
c <- as.data.table(read_excel("c:/C/OmiDB/omiDB2R.xlsx"))[as.Date(HONAP) == as.Date("2025-07-31"), 
                                                          .(claim = sum(Mattart + Függőkár + Költségtartalék + 
                                                                         Kárkifizetés + `Kárrendezési költség` - 
                                                                         `Kármegtérülés` - `Regressz tartalék`)),
                                                          #by = .(polref = as.integer64(Kötvényszám))]
                                                          by = .(polref = as.integer64(Kötvényszám))]
#dob 4 warningot E24298-ig, azokat most úgyse nézzük

library(Matrix)
library(xgboost)
library(data.table)
dt<-as.data.table(fread(file = "c:/c/xgboost/xgb_input_B.csv",sep = ";",dec = ","))
# dt[, .(
#  n_unique = uniqueN(unique_id),   # count distinct
#  min_id   = min(unique_id, na.rm = TRUE),
#  max_id   = max(unique_id, na.rm = TRUE)
# )]
# library(bit64)  # only if unique_id is integer64
# # Aggregate by first 3 characters
# dt[, .(N = .N), by = .(prefix = substr(as.character(unique_id), 1, 3))][order(prefix)]

dt<- c[dt, on = .(polref = unique_id)]


# Step 1: Feature list
features <- c(
 "LicenceAge", "driver_experience", "vehicle_ownership_duration",
 #"vehicle_maker", 
 "vehicle_age", "vehicle_engine_size", "vehicle_weight_min",
 "vehicle_weight_max", "vehicle_number_of_seats", "steering_wheel_located_right",
 "OdometerValueKnown", "seasonal_tyres", "owner_driver_same", "Mileage",
 "mileage_domestic", "mileage_foreign", "youngest_driver_birth_year",
 "MinDriverAge", "number_of_damages_caused_in_last_5_years", "zip", "kw",
 "clientage", "bmcorr"
)


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

dt[, (features) := lapply(.SD, function(x) ifelse(is.na(x), -999, x)), .SDcols = features]
dt<-dt[!is.na(pp)]
dt<-dt[!is.na(nm)]


objective <- function(preds, dtrain) {
 # preds: raw predictions (numeric vector)
 # dtrain: xgb.DMatrix, labels and base_margin stored here
 
 tap <- dt[,t]        # tap (target anchor point)
 m   <- dt[,m]  # m (market price)
 
 alpha <- 0.005  # smoothing parameter for sigmoid
 
 sigmoid <- function(x) 1 / (1 + exp(-x / alpha))
 conversion <- function(x) (1 + tanh(-8.78002 * x + 8.32548)) / 2
 
 eps <- 1e-9
 
 sigma_under <- sigmoid(tap - preds)
 sigma_over  <- conversion(preds - m)
 sigma_mid   <- 1 - sigma_under - sigma_over
 
 #sigma_total <- sigma_under + sigma_mid + sigma_over
 sigma_under <- sigma_under# / sigma_total
 sigma_mid   <- sigma_mid #/ sigma_total
 sigma_over  <- sigma_over #/ sigma_total
 
 w_under_profit <- 100
 w_over_profit  <- 5
 w_mid_profit   <- 1
 w_loss_high    <- 1
 w_loss_low     <- 10
 
 grad_vec <- numeric(length(preds))
 hess_vec <- numeric(length(preds))
 
 profit_mask <- (m > tap)
 
 # --- Profit-making policies ---
 if (any(profit_mask)) {
  idx <- which(profit_mask)
  p <- preds[idx]
  m_i <- m[idx]
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


alpha = 0.05
w_under_profit = 100
w_mid_profit   = 1
w_over_profit  = 50
w_loss_high    = 3
w_loss_low     = 1
target_undercut_abs=100
target_loss_ratio=0.45
dt[,nt:=pp/target_loss_ratio]
dt[,t:=log(nt)]
dt[,m:=log(nm)]
dt[,acc:=log(pmax(nm-target_undercut_abs,nt))]

# 1️⃣ Filter to features with more than 1 unique value
#features_multi <- features[sapply(dt[, ..features], uniqueN) > 1]

# 2️⃣ Convert once to factors
#dt_factor <- dt[, lapply(.SD, factor), .SDcols = features_multi]

# 3️⃣ One-hot encode (sparse)
# X_sparse <- sparse.model.matrix(~ . - 1, data = dt_factor)
# saveRDS(X_sparse, file = "c:/C/XGBoost/X_sparse.rds")#ez a rész lassú
X_sparse <- readRDS("c:/C/XGBoost/X_sparse.rds")


# 4️⃣ Create DMatrix
dtrain <- xgb.DMatrix(data = X_sparse)#, label = dt$target_column)


#dtrain <- xgb.DMatrix(data = as.matrix(dt[, ..features]))
#setinfo(dtrain, "base_margin", dt[,acc])
set.seed(1234)  # fix R's random seed
model<-xgb.train(
 data = dtrain,
 objective = objective,
 params = list(booster = "gblinear"),
 updater   = "shotgun",
 nrounds = 100,
 alpha     = 0.000,#hold'em all   # tiny L1 → almost no pruning
 lambda    = 0.001,               # tiny L2 → just stabilizes updates
 eta       = 0.1                   # learning rate 
 #,nthread = 1
)
w <- xgboost::xgb.dump(model, with_stats = FALSE)#w5<-w
importance <- xgboost::xgb.importance(model = model)#importance5<-importance

write.table(importance, "clipboard", sep = "\t", dec=",", row.names = FALSE, col.names = TRUE)
fwrite(importance, file = "c:/c/xgboost/B_importance.csv",sep = ";",dec = ",")
fwrite(data.table(tree_dump = xgb.dump(model, with_stats = FALSE)), file = "c:/c/xgboost/B_dump.csv",sep = ";",dec = ",")
fwrite(data.table(feature = model$feature_names), file = "c:/c/xgboost/B_dump_fea.csv",sep = ";",dec = ",")

# Get feature names
feat_names <- model$feature_names

# Extract beta coefficients
# xgb.dump() returns text; coefficients are on a single line
dumped <- xgb.dump(model, with_stats = TRUE)
beta_line <- grep("^bias:", dumped, invert = TRUE, value = TRUE)
beta_vals <- as.numeric(gsub(".*:|\\[|\\]", "", beta_line))

# Make a data.table with feature names and betas
beta_dt <- data.table(Feature = feat_names, Beta = beta_vals)

# Get importance (in gblinear, it's often just Weight counts)
imp_dt <- as.data.table(xgb.importance(model = model))

# Merge them
merged <- merge(imp_dt, beta_dt, by = "Feature", all = TRUE)

# View or export
print(merged)
fwrite(merged, "c:/c/xgboost/gblinear_feature_importance.csv", sep = ";", dec = ",")



# Suppose you trained your model with dt as data.table
feature_names <- colnames(dtrain) |>
 (\(x) gsub(" ", "_", x, fixed = TRUE))() |>
 (\(x) gsub("-", "_", x, fixed = TRUE))() |>
 (\(x) gsub("ó", "o", x, fixed = TRUE))() |>
 (\(x) gsub("ö", "o", x, fixed = TRUE))() |>
 (\(x) gsub("ő", "o", x, fixed = TRUE))() |>
 (\(x) gsub("ú", "u", x, fixed = TRUE))() |>
 (\(x) gsub("ü", "u", x, fixed = TRUE))() |>
 (\(x) gsub("ű", "u", x, fixed = TRUE))() |>
 (\(x) gsub("á", "a", x, fixed = TRUE))() |>
 (\(x) gsub("é", "e", x, fixed = TRUE))() |>
 (\(x) gsub("í", "i", x, fixed = TRUE))()

# Create a feature map file
fmap <- "xgb.fmap"
write.table(
 data.frame(
  V1 = seq_along(feature_names) - 1,
  V2 = feature_names,
  V3 = "i"
 ),
 file = fmap,
 quote = FALSE,
 sep = "\t",
 row.names = FALSE,
 col.names = FALSE
)
fmap_dt <- fread("xgb.fmap", header = FALSE, sep = "\t")

# dump the xgboost model
#dump_text <- xgb.dump(model, with_stats = TRUE, fmap = "xgb.fmap")

# remove unwanted lines
# dump_text_clean <- dump_text[!grepl("^booster\\[0\\]|^bias:|^weight:", dump_text)]
# dump_text_clean <- as.numeric(dump_text_clean)#hogy összeadhatók legyenek exp(as.numeric(dump_text_clean))
# 
# # combine with fmap_dt
# # assuming fmap_dt is a data.table with one column
# result_dt <- data.table(fmap_dt_column = c("bias", fmap_dt[[2]])
#                         , dump_text_column = dump_text_clean)
# 
# write.table(result_dt, "clipboard", sep = "\t", dec=",", row.names = FALSE, col.names = TRUE)
# 
# print(result_dt)



#csomóü irányítószámo vacak: ominimo_PostalCode_compare_value_orig_sampling_config{'value': 9985} 0.00000000000000
#egyedi cuccok jjöhetnek: ominimo_correction_table_key_sampling_config1983-12_3041_0085 0.00000003967849
#gyorsan nő, pl ominimo_correction_table_key_sampling_config1969-12_2476_0065 0.00000152138659

# xgb.dump(model, with_stats = TRUE)
dt[, p := predict(model, dtrain)]
dt[, np:=exp(p)]
dt[, omip:=ifelse(nm<=policy_gwp,0,policy_gwp-nt)]
dt[, modp:=ifelse(m<=p,0,np-nt)]
dt[, advantage:=modp-omip]
dt[, benefit:=modp-omip-mean(modp)+mean(omip)]

# dt[m>t & m<=p,.N]#vesztett:230066 #dtrain: 23183
# dt[m>t & m>p,.N]#nyert:56187         263070
# dt[m<t & m<=p,.N]#hárított:0         373891
# dt[m<t & m>p,.N]#benyelt:134942

# Compute New counts and impact
write.table(rbindlist(list(
 dt[m > t & m <= p & nm <= policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő kiáraz jó szerződést')],
 dt[m > t & m <= p & nm >  policy_gwp & policy_gwp <  nt,          .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model kiárazott egy jó szerződést, aminek az Ominimo viszont annyira alávágott, hogy az már veszteséges lett')],
 dt[m > t & m <= p & nm >  policy_gwp & policy_gwp >= nt,          .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model kiárazott egy jó szerződést, amit az Ominimo legolcsóbbra tarifált, de azért még profitábilisra')],
 dt[m > t & m >  p & nm >  policy_gwp & policy_gwp >= nt & np>=nt, .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő legolcsóbbra és nyereségesre tarifált jó szerződést')],
 dt[m > t & m >  p & nm >  policy_gwp & policy_gwp >= nt & np< nt, .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő legolcsóbbra tarifált jó szerződést, de csak az Ominimo árával maradt nyereséges')],
 dt[m > t & m >  p & nm >  policy_gwp & policy_gwp <  nt & np>=nt, .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő legolcsóbbra tarifált jó szerződést, de csak az új model árával maradt nyereséges')],
 dt[m > t & m >  p & nm >  policy_gwp & policy_gwp <  nt & np< nt, .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő annyira alávágott egy jó szerződésnek, hogy veszteséges lett')],
 dt[m > t & m >  p & nm <= policy_gwp & np>=nt,                    .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model profitábilisan alávágott egy jó szerződének, amit az Ominimo kiárazott')],
 dt[m > t & m >  p & nm <= policy_gwp & np< nt,                    .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model veszteségesen alávágott egy amúgy jó szerződének, amit az Ominimo kiárazott')],
 dt[m < t & m >  p & nm >  policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő legolcsóbbra tarifált rossz szerződést')],
 dt[m < t & m >  p & nm <= policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model legolcsóbbra tarifált rossz szerződést, amit az Ominimo kiárazott')],
 dt[m < t & m <= p & nm >  policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az Ominimo legolcsóbbra tarifált rossz szerződést, amit az új model kiárazott')],
 dt[m < t & m <= p & nm <= policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő kiárazott rossz szerződést')])
), "clipboard", sep = "\t", dec=",", row.names = FALSE, col.names = TRUE)

write.table(rbindlist(list(
 dt[clientage==19 & m > t & m <= p & nm <= policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő kiáraz jó szerződést')],
 dt[clientage==19 & m > t & m <= p & nm >  policy_gwp & policy_gwp <  nt,          .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model kiárazott egy jó szerződést, aminek az Ominimo viszont annyira alávágott, hogy az már veszteséges lett')],
 dt[clientage==19 & m > t & m <= p & nm >  policy_gwp & policy_gwp >= nt,          .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model kiárazott egy jó szerződést, amit az Ominimo legolcsóbbra tarifált, de azért még profitábilisra')],
 dt[clientage==19 & m > t & m >  p & nm >  policy_gwp & policy_gwp >= nt & np>=nt, .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő legolcsóbbra és nyereségesre tarifált jó szerződést')],
 dt[clientage==19 & m > t & m >  p & nm >  policy_gwp & policy_gwp >= nt & np< nt, .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő legolcsóbbra tarifált jó szerződést, de csak az Ominimo árával maradt nyereséges')],
 dt[clientage==19 & m > t & m >  p & nm >  policy_gwp & policy_gwp <  nt & np>=nt, .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő legolcsóbbra tarifált jó szerződést, de csak az új model árával maradt nyereséges')],
 dt[clientage==19 & m > t & m >  p & nm >  policy_gwp & policy_gwp <  nt & np< nt, .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő annyira alávágott egy jó szerződésnek, hogy veszteséges lett')],
 dt[clientage==19 & m > t & m >  p & nm <= policy_gwp & np>=nt,                    .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model profitábilisan alávágott egy jó szerződének, amit az Ominimo kiárazott')],
 dt[clientage==19 & m > t & m >  p & nm <= policy_gwp & np< nt,                    .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model veszteségesen alávágott egy amúgy jó szerződének, amit az Ominimo kiárazott')],
 dt[clientage==19 & m < t & m >  p & nm >  policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő legolcsóbbra tarifált rossz szerződést')],
 dt[clientage==19 & m < t & m >  p & nm <= policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az új model legolcsóbbra tarifált rossz szerződést, amit az Ominimo kiárazott')],
 dt[clientage==19 & m < t & m <= p & nm >  policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Az Ominimo legolcsóbbra tarifált rossz szerződést, amit az új model kiárazott')],
 dt[clientage==19 & m < t & m <= p & nm <= policy_gwp,                             .(Count = .N, Market=sum(nm), Technical = sum(nt), OmiTeny=sum(policy_gwp), Model=sum(np), OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)), ModelProfit=sum(ifelse(m<=p,0,np-nt)), OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)), ModelUndercut=sum(ifelse(m<=p,0,nm-np)), ActualClaim=sum(claim, na.rm = T), Label='Mindkettő kiárazott rossz szerződést')])
), "clipboard", sep = "\t", dec=",", row.names = FALSE, col.names = TRUE)

featureoneway <- rbindlist(lapply(features, function(f) {
 dt[ , .(feature = f,
    value = as.character(get(f)),
    benefit = mean(benefit, na.rm = TRUE),
    advantage = sum(advantage, na.rm = TRUE),
    Count = .N,
    Market=sum(nm),
    Technical = sum(nt),
    OmiTeny=sum(policy_gwp),
    Model=sum(np),
    OmiProfit=sum(ifelse(nm<=policy_gwp,0,policy_gwp-nt)),
    ModelProfit=sum(ifelse(m<=p,0,np-nt)),
    OmiUndercut=sum(ifelse(nm<=policy_gwp,0,nm-policy_gwp)),
    ModelUndercut=sum(ifelse(m<=p,0,nm-np)),
    ActualClaim=sum(claim, na.rm = T)
  ),
  by = f][, (f) := NULL]
}), use.names = TRUE, fill = TRUE)

featureoneway[, Feature := paste0(feature, value)]

merged <- merge(
 featureoneway,
 importance,
 by = "Feature",
 all.x = TRUE
)

merged[, Feature := NULL]
merged[, multiplier := exp(Weight)]
merged[, Weight := NULL]

write.table(merged[feature %in% c('ominimo_contractor_age_compare_value_orig_sampling_config'
                                 ,'ominimo_DriverAgeBonusMalus_compare_value_orig_sampling_config'
                                 ,'ominimo_territory_sampling_config')], "clipboard", sep = "\t", dec=",", row.names = FALSE, col.names = TRUE)

fwrite(merged[feature %in% c('ominimo_contractor_age_compare_value_orig_sampling_config'
                             ,'ominimo_DriverAgeBonusMalus_compare_value_orig_sampling_config'
                             ,'ominimo_territory_sampling_config')], "c:/c/xgboost/top_change_whole_feature.csv", sep = ";", dec = ",")


fwrite(merged[feature %in% c('ominimo_CarAge_compare_value_orig_sampling_config'
                             ,'ominimo_CarMake_compare_value_orig_sampling_config'
                             ,'ominimo_FuelType_compare_value_orig_sampling_config')], "c:/c/xgboost/top_change_whole_feature2.csv", sep = ";", dec = ",")

#dt[,.N,.(ominimo_contractor_age_compare_value_orig_sampling_config,clientage)]<---------HIBA!!!

library(data.table)
library(ggplot2)



# Helper function to create the plot for a filtered data.table subset
plot_sorted_lines <- function(sub_dt, title) {
 tap_sorted <- sort(sub_dt$nt)
 m_sorted   <- sort(sub_dt$nm)
 p_sorted   <- sort(sub_dt$np)
 
 max_len <- max(length(tap_sorted), length(m_sorted), length(p_sorted))
 print(max_len)
 
 plot_dt <- data.table(
  idx = 1:max_len,
  tap = rep(tap_sorted, length.out = max_len),
  m   = rep(m_sorted, length.out = max_len),
  p   = rep(p_sorted, length.out = max_len)
 )
 
 plot_long <- data.table::melt(
  plot_dt, id.vars = "idx", variable.name = "series", value.name = "value"
 )
 
 colors <- c(tap = "red", m = "green", p = "purple")
 
 p <- ggplot(plot_long, aes(x = idx, y = value, color = series)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  labs(title = title,
       x = "Index (sorted order)",
       y = "Value",
       color = "Series") +
  theme_minimal()
 
 return(p)
}


# Make sure dt has columns tap, m, p
# Filter profit-making: m > tap
profit_dt <- dt[m > t]
profit_dt[is.na(nt)|is.na(t)|(is.na(np))|is.na(p)|is.na(nm)|is.na(m)]

# Filter non-profit-making: m <= tap
non_profit_dt <- dt[m <= t]
non_profit_dt[is.na(nt)|is.na(t)|(is.na(np))|is.na(p)|is.na(nm)|is.na(m)]

# Plot 1: Profit-making only
plot_profit <- plot_sorted_lines(profit_dt, "Monotone Increasing Sorted Lines (Profit-Making Rows)")

# Plot 2: Non-profit-making only
plot_non_profit <- plot_sorted_lines(non_profit_dt, "Monotone Increasing Sorted Lines (Loss-Making Rows)")

# Show plots
#print(plot_profit)
#print(plot_non_profit)
library(patchwork)
plot_profit + plot_non_profit

dt[, mean(policy_gwp, na.rm = TRUE)]#51949.69
dt[, mean(np, na.rm = TRUE)]#45367.4
