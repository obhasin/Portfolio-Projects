# Install and import necessary Libraries
library(spatstat)

# Set working directory
setwd("/Users/Oria/Documents/ULB/PhD/Microsatellites_OKA/Article_GeneFlow&SGS/Aggreg_OKA")

# Allows to read tabular data from a delimited text file and load it into R for further analysis.
points=read.delim("pointsCEB.txt")
polyg1=read.delim("polyg1CEB.txt")
polyg2=read.delim("polyg2CEB.txt")

# Combine multiple data frames or matrices by row-binding them together.
polygCEB=rbind(polyg1, polyg2)

# The plot() function is used to draw points (markers) in a diagram. 
# The function takes parameters for specifying points in the diagram. 
# Parameter 1 specifies points on the x-axis. Parameter 2 specifies points on the y-axis.
# asp is a parameter of the plot() function in R Language is used to set aspect ratio of plots (Scatterplot and Barplot). 
plot(points[,1:2], asp=1)

# Taking coordinates given in various ways and joining the corresponding points with line segments.
lines(polygCEB[,1:2])

# Create a point pattern from (x, y) for polygonal window.
dat=ppp(x=points[,1],y=points[,2],poly=polygCEB[,1:2])

# Compute simulation envelopes for a fitted model. Pair Correlation Function.
# Pointwise critical envelopes for g(r) and observed value for ‘dat’
# Obtained from 399 simulations of CSR
# Alternative: two.sided
# Significance level of pointwise Monte Carlo test: 20/400 = 0.05
e <- envelope(dat, fun=pcf, r=0:1000, nsim = 399, nrank=10, correction="Ripley")
e

# The pair correlation function, denoted g(r), is a function of an inter-point distance (r). 
# The black solid line represents the pair correlation observed for the data, 
# the red dotted line represents point pattern generated from a homogenous Poisson process 
# (i.e., spatially random distribution of trees) and the grey area indicate the 95% critical envelope under this process.
plot(e, ylim=c(0,5), xlim=c(0,1000))

# The write.table function is used to export a dataframe or matrix to a file. The cbind combines vectors as columns.
e$r
e$obs
write.table(cbind(e$r,e$obs,e$lo,e$hi),"pcfCEB399.txt")

# Conditional Monte Carlo test of CSR using quadrat counts. Test statistic: Pearson X2 statistic.
qTest <- quadrat.test(dat, nx = 2.89, ny = 2.89, method="MonteCarlo", nsim=399)
qTest
plot(qTest)
