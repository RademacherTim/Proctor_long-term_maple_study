#===============================================================================
# Script to plot initial data for exploration
#-------------------------------------------------------------------------------

# Source data ----
source("0_read_data.R")

# Create treatment means and standard deviations ----
means <- data %>% group_by (tn, date) %>% 
  summarise(m_ssc = mean (ssc, na.rm = TRUE),
            sd_ssc = sd (ssc, na.rm = TRUE), .groups = "keep")

# Add a little bit of jitter to the date ----
data$date <- data$date + runif(n = nrow(data), -0.5, 0.5)

# Change plot margins ----
par(mar = c(5, 5, 1, 1))

# plot with pane for 2024 data ----
plot (x = data$date[year(data$date) == 2024 & data$tn == 2],
      y = data$ssc[year(data$date) == 2024 & data$tn == 2],
      pch = 19, lty = 1, lwd = 0.5, col = "white",
      axes = FALSE, ylim = c(0, 7),
      xlab = "", ylab = "Soluble sugar concentration (°Brix)") 
axis (side = 1, 
      at = as_date(c("2024-02-26", "2024-03-04", "2024-03-11", "2024-03-18", "2024-03-25", "2024-04-01")),
      labels = c("26 Feb", "4 Mar", "11 Mar", "18 Mar", "25 Mar", "1 Apr"))
axis (side = 2, las = 1)
for (t in unique(data$tree)){
  con <- year(data$date) == 2024 & data$tree == t 
  tr <- unique(data$tn [data$tree == t])
  lines (x = data$date [con],
         y = data$ssc [con], 
         col = ifelse(tr == 2, "#1f77b4aa", ifelse (tr == 3, "#ff7f0eaa", "#7f7f7f")), 
         lwd = 0.3)
}
points (x = data$date[year(data$date) == 2024 & data$tn == 2],
        y = data$ssc[year(data$date) == 2024 & data$tn == 2],
        pch = 19, lty = 1, lwd = 0.5, col = "#1f77b4aa")# Gravity-tapped trees
points (x = data$date[year(data$date) == 2024 & data$tn == 3],
        y = data$ssc[year(data$date) == 2024 & data$tn == 3],
        pch = 19, lty = 1, lwd = 0.5, col = "#ff7f0eaa")# Vacuum-tapped trees
points (x = data$date[year(data$date) == 2024 & data$tn == 4],
        y = data$ssc[year(data$date) == 2024 & data$tn == 4],
        pch = 19, lty = 1, lwd = 0.5, col = "#7f7f7faa")# Control trees

# Overlay the mean on the data ----
points(x = means$date[year(means$date) == 2024 & means$tn == 2],
       y = means$m_ssc[year(means$date) == 2024 & means$tn == 2], 
       pch = 19,cex = 4, col = "#1f77b4cc")
points(x = means$date[year(means$date) == 2024 & means$tn == 3],
       y = means$m_ssc[year(means$date) == 2024 & means$tn == 3], 
       pch = 19,cex = 4, col = "#ff7f04cc")
points(x = means$date[year(means$date) == 2024 & means$tn == 4],
       y = means$m_ssc[year(means$date) == 2024 & means$tn == 4], 
       pch = 19,cex = 4, col = "#7f7f7fcc")


# plot with pane for 2025 data ----
plot (x = data$date[year(data$date) == 2025 & data$tn == 2],
      y = data$ssc[year(data$date) == 2025 & data$tn == 2],
      pch = 19, lty = 1, lwd = 0.5, col = "white",
      axes = FALSE, ylim = c(0, 7),
      xlab = "", ylab = "Soluble sugar concentration (°Brix)") 
axis (side = 1, 
      at = as_date(c("2025-03-04", "2025-03-11", "2025-03-18", "2025-03-25", "2025-04-01", "2025-04-08", "2025-04-15", "2025-04-22")),
      labels = c("4 Mar", "11 Mar", "18 Mar", "25 Mar", "1 Apr", "8 Apr", "15 Apr", "23 Apr"))
axis (side = 2, las = 1)
for (t in unique(data$tree)){
  con <- year(data$date) == 2025 & data$tree == t 
  tr <- unique(data$tn [data$tree == t])
  lines (x = data$date [con],
         y = data$ssc [con], 
         col = ifelse(tr == 2, "#1f77b4aa", ifelse (tr == 3, "#ff7f0eaa", "#7f7f7f")), 
         lwd = 0.3)
}
points (x = data$date[year(data$date) == 2025 & data$tn == 2],
        y = data$ssc[year(data$date) == 2025 & data$tn == 2],
        pch = 19, lty = 1, lwd = 0.5, col = "#1f77b4aa")# Gravity-tapped trees
points (x = data$date[year(data$date) == 2025 & data$tn == 3],
        y = data$ssc[year(data$date) == 2025 & data$tn == 3],
        pch = 19, lty = 1, lwd = 0.5, col = "#ff7f0eaa")# Vacuum-tapped trees
points (x = data$date[year(data$date) == 2025 & data$tn == 4],
        y = data$ssc[year(data$date) == 2025 & data$tn == 4],
        pch = 19, lty = 1, lwd = 0.5, col = "#7f7f7faa")# Control trees

# Overlay the mean on the data ----
points(x = means$date[year(means$date) == 2025 & means$tn == 2],
       y = means$m_ssc[year(means$date) == 2025 & means$tn == 2], 
       pch = 19,cex = 4, col = "#1f77b4cc")
points(x = means$date[year(means$date) == 2025 & means$tn == 3],
       y = means$m_ssc[year(means$date) == 2025 & means$tn == 3], 
       pch = 19,cex = 4, col = "#ff7f04cc")
points(x = means$date[year(means$date) == 2025 & means$tn == 4],
       y = means$m_ssc[year(means$date) == 2025 & means$tn == 4], 
       pch = 19,cex = 4, col = "#7f7f7fcc")

#===============================================================================