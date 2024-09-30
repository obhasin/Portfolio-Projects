# Import necessary Library
library(VennDiagram)

# Create a Venn diagram with four sets using VennDiagram
venn.plot <- draw.quad.venn(
  area.vector = c(12, 3, 12, 3, 2, 14, 3, 0, 14, 7, 0, 1, 3, 5, 3), # Sizes and overlaps of each set
  category = c('Interception', 'Blue', 'White', 'Yellow'),          # Names of each set
  filename = NULL,                                                  # No output file, plot directly
  col = "black",                                                    # Border color for each set
  lty = rep("solid", 4),                                            # Line type for borders
  lwd = rep(2, 4),                                                  # Line width for borders
  fill = c("grey", "cornflowerblue", "white", "yellow"),            # Fill colors for each set
  fontface = rep("plain", 15),                                      # Font face for set labels
  fontfamily = rep("serif", 15),                                    # Font family for set labels
  alpha = 0.50,                                                     # Transparency level for fills
  direct.area = TRUE                                                # Use exact area sizes
)

# Draw the Venn diagram on a grid
grid.draw(venn.plot)


# Import necessary Library
library(nVennR)

# Create a Venn diagram object with nVennR
myV <- createVennObj(
  nSets = 4,                           # Number of sets
  sNames = c('Interception', 'Blue', 'White', 'Yellow'), # Names of the sets
  sSizes = c(0, 12, 12, 3, 5, 0, 3, 3, 14, 7, 3, 2, 3, 0, 1, 14) # Sizes of intersections
)

# Plot the Venn diagram
myV <- plotVenn(
  nVennObj = myV, 
  setColors = c("darkorchid1", "cornflowerblue", "white", "yellow"), # Colors for each set
  borderWidth = 3,              # Width of the borders
  outFile = "",                 # No output file, plot directly
  systemShow = TRUE,            # Show plot in system viewer
  opacity = 0.4,                # Transparency level
  showLegend = TRUE,            # Show legend
  labelRegions = FALSE,         # Do not label individual regions
  showNumbers = TRUE,           # Show numbers in the Venn diagram
  fontScale = 1                 # Scale of the font for numbers
)
