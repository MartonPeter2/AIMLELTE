 objective <- function(preds, dtrain) {
  # preds: raw predictions (numeric vector)
  # dtrain: xgb.DMatrix, labels and base_margin stored here
  
  tap <- getinfo(dtrain, "label")        # tap (target anchor point)
  m   <- getinfo(dtrain, "base_margin") # m (market price)
  
  alpha <- 0.005
  
  # sigmoid helper (smooth indicator)
  sigmoid <- function(x) 1 / (1 + exp(-x / alpha))
  
  # Compute smoothing weights
  sigma_under <- sigmoid(tap - preds)
  sigma_over  <- sigmoid(preds - m)
  sigma_mid   <- 1 - sigma_under - sigma_over
  
  sigma_total <- sigma_under + sigma_mid + sigma_over
  sigma_under <- sigma_under / sigma_total
  sigma_mid   <- sigma_mid   / sigma_total
  sigma_over  <- sigma_over  / sigma_total
  
  # Weights
  w_under_profit <- 10
  w_over_profit  <- 5
  w_mid_profit   <- 1
  w_loss_high    <- 1
  w_loss_low     <- 3
  
  grad <- numeric(length(preds))
  hess <- numeric(length(preds))
  
  profit_mask <- (m > tap)
  
  # --- Profit-making policies ---
  if (any(profit_mask)) {
   idx <- which(profit_mask)
   p <- preds[idx]
   tap_i <- tap[idx]
   m_i <- m[idx]
   
   # loss components
   diff_tap_p <- tap_i - p
   diff_p_m   <- p - m_i
   
   # Loss terms
   loss_under <- w_under_profit * diff_tap_p^2 + w_mid_profit * diff_p_m^2
   loss_mid   <- w_mid_profit   * diff_p_m^2
   loss_over  <- w_over_profit  * diff_p_m^2 * (m_i - tap_i)
   
   # Gradients of each loss term w.r.t p:
   # d/dp of (tap - p)^2 = -2 * (tap - p)
   # d/dp of (p - m)^2   = 2 * (p - m)
   grad_under <- w_under_profit * (-2 * diff_tap_p) + w_mid_profit * (2 * diff_p_m)
   grad_mid   <- w_mid_profit   * (2 * diff_p_m)
   grad_over  <- w_over_profit  * (2 * diff_p_m) * (m_i - tap_i)
   
   # Hessians:
   # d²/dp² of (tap - p)^2 = 2
   # d²/dp² of (p - m)^2 = 2
   hess_under <- 2 * (w_under_profit + w_mid_profit)
   hess_mid   <- 2 * w_mid_profit
   hess_over  <- 2 * w_over_profit * (m_i - tap_i)
   
   # Compute gradient and hessian weighted by smooth masks
   grad[idx] <- grad_under * sigma_under[idx] + grad_mid * sigma_mid[idx] + grad_over * sigma_over[idx]
   hess[idx] <- hess_under * sigma_under[idx] + hess_mid * sigma_mid[idx] + hess_over * sigma_over[idx]
  }
  
  # --- Loss-making policies ---
  if (any(!profit_mask)) {
   idx <- which(!profit_mask)
   p <- preds[idx]
   tap_i <- tap[idx]
   m_i <- m[idx]
   
   diff_m_p <- m_i - p
   diff_tap_p <- tap_i - p
   
   loss_low  <- w_loss_low  * diff_m_p^2
   loss_high <- w_loss_high * diff_tap_p^2
   
   # Gradient and Hessian
   grad_low  <- w_loss_low  * (-2 * diff_m_p)
   grad_high <- w_loss_high * (-2 * diff_tap_p)
   
   hess_low  <- 2 * w_loss_low
   hess_high <- 2 * w_loss_high
   
   # Since loss = loss_low + loss_high
   grad[idx] <- grad_low + grad_high
   hess[idx] <- hess_low + hess_high
  }
  
  return(list(grad = grad, hess = hess))
 }


alpha = 0.05
w_under_profit = 10
w_mid_profit   = 1
w_over_profit  = 5
w_loss_high    = 3
w_loss_low     = 1
set.seed(1234)  # fix R's random seed
model<-xgb.train(
 data = dtrain,
 objective = objective,
 params = list(booster = "gblinear"),
 nrounds = 100
 #,nthread = 1
)
xgb.dump(model, with_stats = TRUE)
dt[, p := predict(model, dtrain)]