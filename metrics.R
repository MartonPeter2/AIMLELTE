metric_avg_undercut <- function(preds, dtrain) {
 tap <- getinfo(dtrain, "label")
 m   <- getinfo(dtrain, "base_margin")
 
 # Profit-making policies where preds < m (actual undercuts)
 profit_mask <- (m > tap & preds < m)
 if (sum(profit_mask) == 0) return(list(metric = "avg_undercut", value = 0))
 
 p <- preds[profit_mask]
 tap_pm <- tap[profit_mask]
 m_pm <- m[profit_mask]
 
 # Calculate undercut on actual price scale: exp(tap) - exp(p)
 # But since you only care about undercuts (p < m), let's show that difference
 undercut <- exp(m_pm) - exp(p)
 
 avg_undercut <- mean(undercut)
 
 return(list(metric = "avg_undercut", value = avg_undercut))
}


metric_profit_missed_count <- function(preds, dtrain) {
 tap <- getinfo(dtrain, "label")
 m   <- getinfo(dtrain, "base_margin")
 
 profit_mask <- (m > tap)
 if (sum(profit_mask) == 0) return(list(metric = "profit_missed_count", value = 0))
 
 p <- preds[profit_mask]
 tap_pm <- tap[profit_mask]
 
 missed <- sum(p < tap_pm)
 
 return(list(metric = "profit_missed_count", value = missed))
}

metric_profit_impact <- function(preds, dtrain) {
 tap <- getinfo(dtrain, "label")
 m   <- getinfo(dtrain, "base_margin")
 
 profit_mask <- (m > tap)
 if (sum(profit_mask) == 0) return(list(metric = "profit_impact", value = 0))
 
 p <- preds[profit_mask]
 tap_pm <- tap[profit_mask]
 
 undercut_log <- pmax(0, tap_pm - p)
 
 # Compute lost profit on actual price scale
 profit_loss <- sum((exp(tap_pm) - exp(p)) * (undercut_log > 0))
 
 return(list(metric = "profit_impact", value = profit_loss))
}

metric_loss_got_count <- function(preds, dtrain) {
 tap <- getinfo(dtrain, "label")
 m   <- getinfo(dtrain, "base_margin")
 
 loss_mask <- (m <= tap)
 if (sum(loss_mask) == 0) return(list(metric = "loss_got_count", value = 0))
 
 p <- preds[loss_mask]
 tap_lm <- tap[loss_mask]
 
 got <- sum(p >= tap_lm)
 
 return(list(metric = "loss_got_count", value = got))
}


metric_avg_undercut(dt[,p], dtrain)
metric_profit_missed_count(dt[,p], dtrain)
metric_profit_impact(dt[,p], dtrain)
metric_loss_got_count(dt[,p], dtrain)
