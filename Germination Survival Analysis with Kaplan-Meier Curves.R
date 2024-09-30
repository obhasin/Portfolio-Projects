# Load required libraries
install.packages("survival")
install.packages("survminer")
library(survival)
library(survminer)

# Read the data file (modify the path as per your file location)
dat.tab <- read.csv2("/Users/Oria/Documents/ULB/Article_Seed_Dispersal/Germ_F_UF_RF_E.csv", header = TRUE, dec = ",", sep = ";")

# Convert date columns to Date format
dat.tab$start.date <- as.Date(dat.tab$Date, format = "%d/%m/%Y")
dat.tab$end.date <- as.Date(dat.tab$Germination_Date, format = "%d/%m/%Y")

# Calculate number of days from start to germination
dat.tab$days <- as.numeric(dat.tab$end.date - dat.tab$start.date)

# Create survival object
surv.object <- survfit(Surv(dat.tab$days, dat.tab$Germination) ~ dat.tab$Treatment, data = dat.tab, type = "kaplan-meier")

# Print summary of survival object
print(summary(surv.object))

# Plot Kaplan-Meier curves
ggsurvplot(
  surv.object, 
  data = dat.tab,
  risk.table = TRUE,            # Add risk table below the plot
  pval = TRUE,                  # Add p-value for log-rank test
  xlim = c(0, 80),              # Limit the x-axis range
  break.time.by = 20,           # Breaks on the x-axis
  risk.table.y.text.col = TRUE, # Color risk table text by strata
  risk.table.y.text = FALSE,    # Show text in risk table on same line
  legend.title = "Treatments",  # Title for legend
  legend.labs = c("Faeces", "Ripe Fruits", "Unripe Fruits"), # Labels for strata
  xlab = "Time [days]", 
  ylab = "Probability of Not Germinating",
  title = "Germination Curves of Seeds Under Different Treatments",
  subtitle = "Kaplan-Meier Estimates",
  palette = "grey",             # Color palette for curves
  linetype = c("solid", "dashed", "dotted") # Different line types for curves
)

# Perform Log-Rank Tests to Compare Survival Curves
# Overall comparison between all treatments
test.all <- survdiff(Surv(dat.tab$days, dat.tab$Germination) ~ dat.tab$Treatment, data = dat.tab)
print(test.all)

# Pairwise comparisons
# Faeces vs. Ripe Fruits
test.f_rf <- survdiff(Surv(dat.tab$days, dat.tab$Germination) ~ dat.tab$Treatment, data = dat.tab, subset = (dat.tab$Treatment != "UF"))
print(test.f_rf)

# Faeces vs. Unripe Fruits
test.f_uf <- survdiff(Surv(dat.tab$days, dat.tab$Germination) ~ dat.tab$Treatment, data = dat.tab, subset = (dat.tab$Treatment != "RF"))
print(test.f_uf)

# Ripe Fruits vs. Unripe Fruits
test.rf_uf <- survdiff(Surv(dat.tab$days, dat.tab$Germination) ~ dat.tab$Treatment, data = dat.tab, subset = (dat.tab$Treatment != "F"))
print(test.rf_uf)
