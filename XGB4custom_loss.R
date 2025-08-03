library(arrow)
library(data.table)

# Define file path
file_path <- "c:/C/OmiReport/2506/mtpl_monthly_big_datasets_2025_09_01/ominimo_policies_scored_with_correction_2025_09_01.parquet"

# Define columns to read
cols_to_read <- c(
 "unique_id",
 "postal_code",
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

# Read selected columns into a data.table
dt <- as.data.table(read_parquet(file_path, col_select = all_of(cols_to_read)))

# Convert 'no'/'yes' to 0/1 for specified columns
cols_to_binary <- c("steering_wheel_located_right", "OdometerValueKnown", "seasonal_tyres", "owner_driver_same")

dt[, (cols_to_binary) := lapply(.SD, function(x) as.integer(tolower(x) == "yes")), .SDcols = cols_to_binary]


# Create a named vector: names are bonus_malus_current, values are bmcorr
bmc_map <- setNames(-4:10, c(paste0("M0", 4:1), "A00", paste0("B0", 1:9), "B10"))

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

dt[, tp :=0.067173373 * 1.022752496 * 702800 * poli * kw_freq * age_freq * bm_freq * kw_sev * age_sev * bm_sev]
dt[, tap :=log(2*tp)]

dt[, m := log(pmin(
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
))]

fwrite(
 dt[, .(m=exp(m), tap=exp(tap), logm=m, logtap=tap)],
 file = "c:/C/XGBoost/m_tp.csv",
 sep = ";",
 dec = ","
)

dt<-dt[!is.na(tp)]

fwrite(dt, file = "c:/c/xgboost/xgb_input.csv",sep = ";",dec = ",")


library(Matrix)
library(xgboost)

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

dt[, (features) := lapply(.SD, function(x) ifelse(is.na(x), -999, x)), .SDcols = features]
dtrain <- xgb.DMatrix(data = as.matrix(dt[, ..features]), label = dt[,tap])
setinfo(dtrain, "base_margin", dt[,m])
# 
# custom_loss_smooth <- function(preds, dtrain) {
#  tap <- getinfo(dtrain, "label")
#  m   <- getinfo(dtrain, "base_margin")
#  p   <- preds
#  
#  alpha <- 0.1
#  s  <- (p - m) / alpha
#  sigma  <- 1 / (1 + exp(-s))            # σ
#  dsig   <- sigma * (1 - sigma) / alpha  # σ'
#  d2sig  <- dsig * (1 - 2 * sigma) / alpha  # σ''
#  
#  # gradient
#  grad <- ifelse(tap >= m,
#                 -(1 - sigma) - (tap - p) * dsig,
#                 dsig * (m - tap) - (1 - sigma))
#  
#  # hessian
#  hess <- ifelse(tap >= m,
#                 -dsig + (tap - p) * d2sig,
#                 d2sig * (m - tap) + dsig)
#  
#  hess <- pmax(hess, 1e-6)   # keep it positive
#  list(grad = grad, hess = hess)
# }
# 
# params <- list(
#  objective = custom_loss_smooth,
#  eta = 0.1,
#  max_depth = 6
# )
# 
# model<-xgb.train(params = params, data = dtrain, nrounds = 10)
# dt[, p := predict(model, dtrain)]
# 
# tmp <- custom_loss_smooth(predict(model, dtrain), dtrain)
# summary(tmp$grad)
# summary(tmp$hess)
# 
# ##############
# library(ggplot2)
# 
# length_out <- 500
# id <- 1
# 
# # Extract tap and m from the first row of dtrain
# tap_val <- getinfo(dtrain, "label")[id]
# m_val <- getinfo(dtrain, "base_margin")[id]
# 
# # Use the same smoothing alpha as in your loss function
# alpha <- 0.1
# 
# # Create a sequence of p values around tap_val and m_val
# p_vals <- seq(0.9 * pmin(tap_val, m_val,dt[id, p]), 1.1 * pmax(tap_val, m_val,dt[id, p]), length.out = length_out)
# 
# # Compute sigma for each p
# s <- (p_vals - m_val) / alpha
# sigma <- 1 / (1 + exp(-s))
# 
# # Compute loss vectorized using updated formula:
# loss_vals <- if (tap_val >= m_val) {
#  (1 - sigma) * (p_vals - tap_val)
# } else {
#  sigma * (m_val - tap_val) + (1 - sigma) * (m_val - p_vals)
# }
# 
# # Prepare data frame for plotting
# df <- data.frame(p = p_vals, loss = loss_vals)
# 
# # Compose title string
# title_str <- if (tap_val < m_val) {
#  paste0("Loss vs Price for Profit Making\n(tap = ", round(tap_val, 3), ", m = ", round(m_val, 3), ")")
# } else {
#  paste0("Loss vs Price for Loss Making\n(m = ", round(m_val, 3), ", tap = ", round(tap_val, 3), ")")
# }
# 
# # Plot
# ggplot(df, aes(x = p, y = loss)) +
#  geom_line(color = "blue", size = 1) +
#  geom_vline(xintercept = m_val, linetype = "dashed", color = "green", size = 1) +
#  geom_vline(xintercept = tap_val, linetype = "dashed", color = "red", size = 1) +
#  geom_vline(xintercept = dt[id, p], linetype = "dashed", color = "purple", size = 1) +
#  labs(title = title_str,
#       x = "Prediction p",
#       y = "Loss") +
#  theme_minimal()