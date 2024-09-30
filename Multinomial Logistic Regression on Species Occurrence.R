# Load required libraries
library(nnet)
library(reshape2)
library(ggplot2)
library(foreign)

# Read and prepare data
DO <- read.csv2("/Users/Oria/Documents/ULB/Article_Interactions/DO_MLR.csv", header = TRUE, dec = ",", sep = ";")

# Ensure that DO$Species is a factor
DO$Species <- as.factor(DO$Species)

# Relevel the Species factor to set "Long-tailed macaque" as the reference level
DO$Species2 <- relevel(DO$Species, ref = "Long-tailed macaque")

# Fit multinomial logistic regression model
test <- multinom(Species2 ~ Time_Class + Position, data = DO)
summary(test)


# Calculate z-values and p-values
z <- summary(test)$coefficients / summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Calculate exponentiated coefficients and predicted probabilities
exp_coef <- exp(coef(test))
head(pp <- fitted(test))

# Prepare data for prediction
dPosition <- data.frame(Position = c("Low", "Low-Middle", "Middle", "Middle-Top", "Top"))
dTime_Class <- data.frame(Time_Class = c("Early morning", "Morning", "Afternoon", "Late afternoon", "Night"))
m <- merge(dPosition, dTime_Class)

# Generate predicted probabilities
pred <- predict(test, m, "probs")
pred <- cbind(m, pred)
lpp <- melt(pred, id.vars = c("Position", "Time_Class"), value.name = "probability")
colnames(lpp) <- c("Position", "Time_Class", "Species", "probability")

# Factor levels for consistent ordering in the plot
lpp$Position2 <- factor(lpp$Position, levels = c("Top", "Middle-Top", "Middle", "Low-Middle", "Low"))

# Define x and y axis limits for plot
x_limits <- c("Early morning", "Morning", "Afternoon", "Late afternoon", "Night")
y_limits <- c("Buffy fish owl",
              "Great egret",
              "Large flying fox",
              "Long-tailed macaque",
              "Oriental pied hornbill",
              "Proboscis monkey",
              "Silvered leaf monkey")

# Align the factor levels in lpp with y_limits
lpp$Species <- factor(lpp$Species, levels = y_limits)

# Check for any levels in lpp$Species that are not in y_limits
missing_levels <- setdiff(levels(lpp$Species), y_limits)
if(length(missing_levels) > 0) {
  warning("Some levels in Species are not in y_limits: ", paste(missing_levels, collapse = ", "))
}

# Define a grey color palette manually, ensuring the number of colors matches y_limits
grey_palette <- gray.colors(length(y_limits), start = 0.1, end = 0.9)

# Plotting with ggplot2
fig2 <- ggplot(data = lpp, aes(x = Time_Class, y = probability, fill = Species)) +
  geom_bar(position = "dodge", stat = "identity") +  # Use geom_bar for categorical data
  facet_grid(Position2 ~ .) +
  ylab("Probability") +
  scale_x_discrete(name = "Time slots", limits = x_limits) +
  scale_fill_manual(name = "Species", values = grey_palette, limits = y_limits) +  # Use manually defined grey colors
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve readability of x-axis labels

# Display the plot
print(fig2)
