# Read data file with dbh and tree dominance status to use Kendall’s rank correlation coefficient (τb) to examine the relationship between them.
my_data <- read.csv("/Users/Oria/Documents/ULB/PhD/Microsatellites_OKA/Article_GeneFlow&SGS/Correlation_DBHandDOM.csv", sep=";")
head(my_data, 6)
#The Kendall rank correlation coefficient or Kendall’s tau statistic is used to estimate a rank-based measure of association. 
#This test may be used if the data do not necessarily come from a bivariate normal distribution.
res <- cor.test(my_data$DBH, my_data$DOM,  method="kendall")
res
