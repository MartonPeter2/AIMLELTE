import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import log_loss, roc_auc_score



def sigmoid(x):
    # Prevent overflow by clipping input
    z = np.clip(x, -35, 35)  # clipping range can be tuned
    return 1 / (1 + np.exp(-z))

# ---- Step 1: Compute typicality weights ----
def compute_typicality(X, alpha=0.5, scaler=None):
    if scaler is None:
        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(X)
    else:
        X_scaled = scaler.transform(X)
    dist = np.sqrt(np.sum(X_scaled**2, axis=1))
    weights = np.exp(-alpha * dist)
    return weights, scaler

# ---- Step 2: Custom profit-based loss with weights ----
def make_profit_loss_with_typicality(market_price, weights):
    def profit_loss_with_typicality(preds, dtrain):
        labels = dtrain.get_label()

        p = sigmoid(preds)
        A = labels

        # Make sure market_price and weights are numpy arrays (if they aren't already)
        M = np.array(market_price)
        w = np.array(weights)

        grad = w * (M * p * (1 - p) - A * (1 - p))
        hess = w * (M * (1 - 2*p) * p * (1 - p) + A * p * (1 - p))

        return grad, hess
    return profit_loss_with_typicality

# def make_profit_loss_with_typicality(market_price, weights):
#  def dummy_profit_loss(preds, dtrain):
#      labels = dtrain.get_label()
#      p = sigmoid(preds)
#      grad = p - labels
#      hess = p * (1 - p)
#      return grad, hess
#  return dummy_profit_loss

# ---- Step 3: Prepare data ----
df = pd.read_csv(r"c:\C\OmiClaimJoiningAnalyseses\B_tarifa_1_eves_adatok_v3.csv",decimal=',',sep=';')

# Create y: 1 if df['clmdate'] is not null, else 0
y = df['clmdate'].notnull().astype(int)

# Columns to select
cols = [
    "contractor_age",
    "bonus_malus_current",
    "years_since_last_damage_caused",
    "number_of_damages_caused_in_last_5_years",
    "LicenceAge",
    "driver_experience",
    "vehicle_ownership_duration",
    "vehicle_power",
    "vehicle_engine_size",
    "vehicle_age",
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
]

X = df[cols].copy()

# Map bonus_malus_current codes to numeric
bonus_map = {
    'M04': -4,
    'M03': -3,
    'M02': -2,
    'M01': -1,
    'A00': 0,
    'B01': 1,
    'B02': 2,
    'B03': 3,
    'B04': 4,
    'B05': 5,
    'B06': 6,
    'B07': 7,
    'B08': 8,
    'B09': 9,
    'B10': 10
}
X['bonus_malus_current'] = X['bonus_malus_current'].map(bonus_map)

# Convert yes/no to 1/0
yes_no_cols = [
    "steering_wheel_located_right",
    "OdometerValueKnown",
    "seasonal_tyres",
    "owner_driver_same"
]

for col in yes_no_cols:
    X[col] = X[col].map({'yes': 1, 'no': 0, 'Yes': 1, 'No': 0}).astype(int)


# 1) For years_since_last_damage_caused: keep NA as is (or fill with -1 as a signal)
X['years_since_last_damage_caused'] = X['years_since_last_damage_caused'].fillna(-1)

# 2) For LicenceAge and driver_experience:
#    NA means "no licence" → encode as a special number, e.g. -1
X['LicenceAge'] = X['LicenceAge'].fillna(-1)
X['driver_experience'] = X['driver_experience'].fillna(-1)

# 3) For other columns, drop rows with any NA
cols_to_check = X.columns.difference(['years_since_last_damage_caused', 'LicenceAge', 'driver_experience'])
X = X.dropna(subset=cols_to_check)

# Optionally, after filtering X, also filter your y and other aligned data
aligned_index = X.index
y = y.loc[aligned_index]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)

cols = [
    "ominimo_predicted_price_sampling_config",
    "genertel_predicted_price_sampling_config",
    "kh_predicted_price_sampling_config",
    "granit_predicted_price_sampling_config",
    "alfa_predicted_price_sampling_config",
    "groupama_predicted_price_sampling_config",
    "generali_predicted_price_sampling_config",
    "uniqa_predicted_price_sampling_config",
    "union_predicted_price_sampling_config",
    "allianz_predicted_price_sampling_config"
]

market_price = df[cols].min(axis=1)
market_price = market_price.loc[aligned_index]

market_price_train_raw = market_price.loc[X_train.index]
market_price_test_raw = market_price.loc[X_test.index]

market_price_train = market_price_train_raw / np.median(market_price_train_raw)
market_price_test = market_price_test_raw / np.median(market_price_train_raw)  # use train median to avoid leakage

weights_train, scaler = compute_typicality(X_train)
weights_test, _ = compute_typicality(X_test, scaler=scaler)

dtrain = xgb.DMatrix(X_train, label=y_train, weight=weights_train)
dtest = xgb.DMatrix(X_test, label=y_test, weight=weights_test)


# ---- Step 4: Train model ----
params = {
    "objective": "binary:logistic",
    "eta": 0.05,
    "max_depth": 6,
    "eval_metric": "logloss"
}
obj = make_profit_loss_with_typicality(market_price_train, weights_train)
bst = xgb.train(params, dtrain, num_boost_round=100, obj=obj)

params_reg=params.copy()
reg = xgb.train(params_reg, dtrain, num_boost_round=100)

# Regular model predictions
y_pred_reg = reg.predict(dtest)

# Custom loss model predictions
y_pred_bst = bst.predict(dtest)

#Step 5: Evaluate traditional metrics
print("Baseline Model")
print("Logloss:", log_loss(y_test, y_pred_reg))
print("AUC:", roc_auc_score(y_test, y_pred_reg))

print("\nCustom Loss Model")
print("Logloss:", log_loss(y_test, y_pred_bst))
print("AUC:", roc_auc_score(y_test, y_pred_bst))
