#===============================================================================
# Script to calculate basic sap sugar content stats
#-------------------------------------------------------------------------------

# Source weekly and annual sap sugar content (SS and mSS, respectively) ----
source("0_read_data.R")

# Basic stats ----
mean(mSS$ssc, na.rm = T)
sd(mSS$ssc, na.rm = T)
range(mSS$ssc, na.rm = T)

# Calculate ranking of sweetness ----
tmp <- data %>% 
  #filter(!is.na(ssc)) %>% 
  #filter(t != 4) %>% 
  group_by(year) %>%
  mutate(ssc_rank = rank(ssc, na.last = TRUE, ties.method = "average")) %>% 
  ungroup()

# Spearman rank correlation ----
cor(tmp %>% select(ID, ssc_rank), method = "spearman")

# Ranking over time ----
plot(x = tmp$year[tmp$ID == 833],
     y = tmp$rank[tmp$ID == 833],
     xlab = "year", ylab = "Sweetness Rank", 
     xlim = c (2013, 2025), ylim = c (0, 90),
     typ = "l", lty = 1, lwd = 0.5, col = "white", axes = FALSE)
axis(side = 1)
axis(side = 2, las = 1)
for(tree in unique(tmp$ID)){
  lines(x = tmp$year[tmp$ID == tree],
        y = tmp$rank[tmp$ID == tree],
        typ = "l", lty = 1, lwd = 0.5)
}
# Trees with top sugar content ----
tmp$ID[which(tmp$rank == 1)]
# Highlight higher sugar content trees ----
# 817 is the tree with the highest average sugar content
lines(x = tmp$year[tmp$ID == 817],
      y = tmp$rank[tmp$ID == 817],
      typ = "l", lty = 1, lwd = 3, col = "darkred")

