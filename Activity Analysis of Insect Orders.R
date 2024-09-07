# Install and import necessary Libraries
install.packages("devtools") # if not already installed
devtools::install_github("animalnexus/feedr")
install.packages(activity)
library(feedr)
library(activity)
# Read data file with each individuals (insect orders) and the corresponding time of observation.
Rsp <-read.csv2("/Users/Oria/Documents/ULB/PhD/Raspicam/Activity_MOV_Nflvisit.csv", header=T, dec=",",sep=";")
# Create a data frame which is a table or a two-dimensional array-like structure in which each column contains values of one variable and each row contains one set of values from each column.
df <- data.frame(Rsp)
# Used to return TRUE if the specified data type is a data frame else return FALSE.
is.data.frame(df) 

# Among the subset of individuals (insect orders) and the corresponding time of observation, list of the corresponding time of observation of the different insect orders.
tCol <- Rsp$Time[Rsp$Order=="Coleoptera"]
tLep <- Rsp$Time[Rsp$Order=="Lepidoptera"]
tHym <- Rsp$Time[Rsp$Order=="Hymenoptera"]
tDip <- Rsp$Time[Rsp$Order=="Diptera"]
tBla <- Rsp$Time[Rsp$Order=="Blattodea"]

# Fits kernel density to radian time-of-day data and estimates activity level from this distribution. 
## Reps=Number of bootstrap iterations to perform.
fHym <- fitact(tHym, sample="data", reps=10)
fLep <- fitact(tLep, sample="data", reps=10)
fCol <- fitact(tCol, sample="data", reps=10)
fDip <- fitact(tDip, sample="data", reps=10)
fBla <- fitact(tBla, sample="data", reps=10)

# Randomisation test for the probability that two sets of circular observations come from the same distribution.
compareCkern(fCol,fLep,reps=10)

# Activity budgets for each Order. On the x-axis, the number of individuals observed and on the y-axis, the time of observation. 
## Histogram bars represent observed activity frequencies as a function of time of day. The solid line is the frequency distribution fitted to the circular kernel, and the dashed lines are the confidence intervals of this distribution.
plot (fHym)
plot (fLep)
plot (fCol)
plot (fDip)
plot (fBla)
