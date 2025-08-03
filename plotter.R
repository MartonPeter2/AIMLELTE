library(ggplot2)
library(data.table)

convex_smooth_loss <- function(p, tap, m,
                               alpha = 0.005,
                               w_under_profit = 10,
                               w_over_profit  = 5,
                               w_mid_profit   = 1,
                               w_loss_high    = 1,
                               w_loss_low     = 3) {
 
 # Smoothing masks
 sigmoid <- function(x) 1 / (1 + exp(-x / alpha))
 sigma_under <- sigmoid(tap - p)
 sigma_over  <- sigmoid(p - m)
 sigma_mid   <- 1 - sigma_under - sigma_over
 
 # Ensure sum to 1 numerically
 sigma_total <- sigma_under + sigma_mid + sigma_over
 sigma_under <- sigma_under / sigma_total
 sigma_mid   <- sigma_mid   / sigma_total
 sigma_over  <- sigma_over  / sigma_total
 
 if (m > tap) {
  # Profit-making policy
  # Add mid penalty to under region for continuity
  loss_under <- w_under_profit * (tap - p)^2 + w_mid_profit * (p - m)^2
  loss_mid   <- w_mid_profit   * (p - m)^2
  loss_over  <- w_over_profit  * (p - m)^2 * (m - tap)
  loss <- loss_under * sigma_under + loss_mid * sigma_mid + loss_over * sigma_over
 } else {
  # Loss-making policy
  loss_low  <- w_loss_low  * (m - p)^2
  loss_high <- w_loss_high * (tap - p)^2
  loss <- loss_low + loss_high
 }
 
 return(loss)
}



# Parameters
id <- 1
length_out <- 500
alpha <- 0.00001  # smoothing sharpness

# Penalty weights
w_under_profit <- 10
w_over_profit  <- 5
w_mid_profit   <- 1
w_loss_high    <- 1
w_loss_low     <- 3

# Extract tap, m, and optionally p
tap_val <- getinfo(dtrain, "label")[id]
m_val   <- getinfo(dtrain, "base_margin")[id]
p_val   <- if (exists("dt")) dt[id, p] else NA  # fallback
is_profit <- m_val > tap_val

# Generate p values
p_min <- min(tap_val, m_val, if (!is.na(p_val)) p_val else Inf)
p_max <- max(tap_val, m_val, if (!is.na(p_val)) p_val else -Inf)
padding <- (p_max - p_min) / 10
p_vals <- seq(p_min - padding, p_max + padding, length.out = length_out)

# Compute loss
loss_vals <- loss_vals <- convex_smooth_loss(
 p = p_vals,
 tap = tap_val,
 m = m_val,
 alpha = alpha,
 w_under_profit = w_under_profit,
 w_over_profit  = w_over_profit,
 w_mid_profit   = w_mid_profit,
 w_loss_high    = w_loss_high,
 w_loss_low     = w_loss_low
)


# Data frame for plotting
df <- data.table(p = p_vals, loss = loss_vals, ep = exp(p_vals))

# Title
title_str <- if (is_profit) {
 paste0("Loss for Profit-Making Policy (tap = ", round(exp(tap_val)),
        ", m = ", round(exp(m_val)), ")")
} else {
 paste0("Loss for Loss-Making Policy (tap = ", round(exp(tap_val)),
        ", m = ", round(exp(m_val)), ")")
}

# Plot
ggplot(df, aes(x = ep, y = loss)) +
 geom_line(color = "blue", size = 1) +
 geom_vline(xintercept = exp(m_val), linetype = "dashed", color = "green", size = 1) +
 geom_vline(xintercept = exp(tap_val), linetype = "dashed", color = "red", size = 1) +
 { if (!is.na(p_val)) geom_vline(xintercept = exp(p_val), linetype = "dashed", color = "purple", size = 1) else NULL } +
 labs(title = title_str,
      x = "Predicted price p",
      y = "Loss") +
 theme_minimal()
