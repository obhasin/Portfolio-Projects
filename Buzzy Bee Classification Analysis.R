#Analysis of Buzzy Bee classification 

setwd("/Users/Oria/Documents/ULB/PhD/Zooniverse/Script_Subjects")

# File names (to adjust if necessary)
question_file <- "question_extractor_MOV.csv"
survey_file <- "survey_extractor_MOV.csv"

# Read data file with #yes and #no per video and observer
#OMV--> sep=","
Qvo <- read.csv(question_file, sep=",")
Qvo = Qvo[,c(1,2,7,9,11)]

# Read data file with observations per video and observer
#AYO--> sep=";"
Svo <- read.csv(survey_file, sep=",")
Svo = Svo[,c(1,2,7,9)]

# Read data file with time and tree corresponding to each video
videoT <- read.table("subject_ID_MOV.txt", header=T, sep='\t')
str(videoT)
#convert Dates, Times and factor formats
videoT$Tree.sp = factor(videoT$Tree.sp)
videoT$Tree.ID = factor(videoT$Tree.ID)
videoT$Cam.ID = factor(videoT$Cam.ID)
library(chron)
videoT$Date = as.Date(videoT$Date, "%d/%m/%y")
videoT$Start.time=times(videoT$Start.time)
videoT$End.time=times(videoT$End.time)
str(videoT)

# Create table with answers per video and observer with animals
library(reshape2)
Svo2 <- dcast(Svo, classification_id + subject_id + user_name ~ data.choice, value.var='data.choice', fun.aggregate = length)
Svo2$Totals = rowSums(Svo2[,-c(1:3)])

# Create table with answers per video and observer 
Tvo = cbind(Qvo, matrix(nrow=nrow(Qvo), ncol=19))
colnames(Tvo)[6:24] = c("Total",colnames(Svo2)[c(4:21)])
Tvo[,6:24] = 0
Tvo[match(Svo2$classification_id, Tvo$classification_id), 6:24] = Svo2[,c(22,4:21)]
Tvo$user_name = factor(Tvo$user_name)
Tvo[,4:5][is.na(Tvo[,4:5])] = 0     #replace na by 0

# Show cases where there are probably too many animals to be true and check some case
Tvo[Tvo$Total > 6,]
Tvo[Tvo$user_name == "Alonaroxas",] 
Tvo[Tvo$user_name == "not-logged-in-358b4e4ebfc875f5e313",]
#filter out these cases  
Tvo = Tvo[Tvo$Total < 10,]

# Check coherence of each observation with respect to the other ones for the same video
Tvo$coher.id = Tvo$coher.det = NA
for(i in 1:nrow(Tvo)){ #scan each observation to count differences with the other observations made on the same video
  m <- as.matrix(Tvo[Tvo$subject_id==Tvo$subject_id[i] & (1:nrow(Tvo)!=i), c(5,7:24)])
  if(nrow(m)==0) next
  f <- as.matrix(Tvo[i,c(5,7:24)])
  Tvo$coher.det[i] = sum(m[,1] == f[,1]) / nrow(m) #proportion of other observations on same video that agree in (non-)detecting a visitor
  if(nrow(m)>1){
    mf <- matrix(rep(f, times=nrow(m)), nrow=nrow(m), byrow=T)
    Tvo$coher.id[i] = sum( rowSums(m[,-1] - mf[,-1])==0 ) / nrow(m)  #proportion of other observations on same video that fully agree in identifying visitor(s)
  }
  if(nrow(m)==1) Tvo$coher.id[i] = sum( sum(m[,-1] - mf[,-1])==0 )
}
#reorder columns to ease reading
#Tvo = Tvo[,c(1:6,25:26,7:24)]

# Means per observer
To <- aggregate(. ~ user_name, Tvo[,-c(1,3)], mean)
To$Nvideo <- aggregate(. ~ user_name, Tvo[,-c(1,3)], length)[,2]
To = To[order(-To$Nvideo),c(1,25,2:3,23:24,4:22)]

# Means per video
Tv <- aggregate(. ~ subject_id, Tvo[,-c(1,2)], mean)
Tv$Nobs <- aggregate(. ~ subject_id, Tvo[,-c(1,2)], length)[,2]
Tv = Tv[,c(1,25,2:3,23:24,4:22)]
#show histogram of number of obs per video
hist(Tv$Nobs, breaks= (0:max(Tv$Nobs))+0.5)

# Add a column for "interesting" animals
interesting.animals = c("butterfly", "fly", "honeybee", "mosquito", "moth", "stinglessbee", "toosmall", "unidentified", "wasp")
Tv$interesting.animals = rowSums(Tv[,interesting.animals])

# Add date, time and tree per video
Tv = cbind(Tv, videoT[match(Tv$subject_id, videoT$Subject.ID), c("Tree.ID","Date","Start.time")] )

# Check observers
To[1:20,]
Tvo[Tvo$user_name=="24mtingler",] #see nothing
Tvo[Tvo$user_name=="Brietterick",] #see mostly ants
#check if coherence in detection or identification varies according to the number of videos assessed per observer
plot(To$coher.det ~ To$Nvideo, log="x")
plot(To$coher.id ~ To$Nvideo, log="x")

# Ordination of videos to show species scores
library(vegan)
cca <- cca(Tv[Tv$Total>0,8:25])
plot(cca, display = c("sp"))
# possible interpretation (mais je ne suis pas s?r!): honeybee, moth and butterfly (+wasp?) tend to be identified coherently while other groups tend to be mixed
#same ordination without honeybee, moth and butterfly 
#cca2 <- cca(Tv[rowSums(Tv[,animals[-c(3,9,11)]])>0, animals[-c(3,9,11)] ])
cca2 <- cca(Tv[rowSums(Tv[,[-c(3,9,11)]])>0, [-c(3,9,11)] ])
plot(cca2, display = c("sp"))
#same ordination without cicada, cricket and truebug
cca3 <- cca(Tv[rowSums(Tv[,animals[-c(3,5,7,9,11,16)]])>0, animals[-c(3,5,7,9,11,16)] ])
plot(cca3, display = c("sp"))

# Ordination limited to "interesting.animals"
ccaIA <- cca(Tv[Tv$interesting.animals>0, interesting.animals])
plot(ccaIA, display = c("sp"))

interesting.animals2 = c("fly", "honeybee", "mosquito", "stinglessbee", "toosmall", "unidentified", "wasp")
ccaIA2 <- cca( Tv[rowSums(Tv[,interesting.animals2])>0, interesting.animals2])
plot(ccaIA2, display = c("sp"))
# fly, mosquito and stinglessbee might be confounded by different observers

# Show mean values across videos to represent the relative abundance of each animal
colmeans <- colMeans(Tv[,1:26])
colmeans[2:7]
colmeans[8:26]
barplot(colmeans[8:26], las=2, ylab="proportions across all videos")
barplot(colmeans[9:25], las=2, ylab="proportions across all videos") #without ants

# Make a table considering only observations agreed by a majority of observers
Tv.maj <- Tv
Tv.maj[8:26][Tv.maj[8:26] < 0.5] = 0
colmeans.maj <- colMeans(Tv.maj[,1:26])
colmeans.maj[8:26]
barplot(colmeans.maj[8:26], las=2, ylab="proportions of majority classifications across all videos")


# Visits according to time
#describe available observations through time per tree
plot(x=Tv$Start.time, y=factor(Tv$Tree.ID), col= factor(Tv$Tree.ID))
plot(x=Tv$Start.time, y=factor(Tv$Tree.ID), col= factor(Tv$Tree.ID), xlim=times(c("0:00:00","23:00:00")), xaxt="n", yaxt="n", xlab=NA, ylab=NA)
axis(1,at=times((0:23)/24), labels=paste0(c(0:23),"h"))
axis(2,at=1:3, labels=levels(Tv$Tree.ID))

# Freq of observations per tree
tapply(Tv$data.yes, Tv$Tree.ID, FUN="mean")
tapply(Tv$interesting.animals, Tv$Tree.ID, FUN=mean)
tapply(Tv$honeybee, Tv$Tree.ID, FUN="mean")

# Using all observations
plot(Tv$ant ~ Tv$Start.time)
plot(Tv$butterfly ~ Tv$Start.time)
plot(Tv$fly ~ Tv$Start.time)
plot(Tv$honeybee ~ Tv$Start.time)
plot(Tv$stinglessbee ~ Tv$Start.time)
plot(Tv$wasp ~ Tv$Start.time)
#using classifications by majority of observers
plot(Tv.maj$ant ~ Tv$Start.time)
plot(Tv.maj$butterfly ~ Tv$Start.time)
plot(Tv.maj$fly ~ Tv$Start.time)
plot(Tv.maj$honeybee ~ Tv$Start.time)
plot(Tv.maj$stinglessbee ~ Tv$Start.time)
plot(Tv.maj$wasp ~ Tv$Start.time)

# Select videos with particular groups (seen by a proportion of observers above some threshold)
threshold.observers = 0.25
video.butterfly <- Tv$subject_id[Tv$butterfly >= threshold.observers]
video.fly <- Tv$subject_id[Tv$fly >= threshold.observers]
video.honeybee <- Tv$subject_id[Tv$honeybee >= threshold.observers]
video.stinglessbee <- Tv$subject_id[Tv$stinglessbee >= threshold.observers]
video.wasp <- Tv$subject_id[Tv$wasp >= threshold.observers]
video.spider <- Tv$subject_id[Tv$spider >= threshold.observers]
video.IA <- Tv$subject_id[Tv$interesting.animals >= threshold.observers]

# Check if results would be biased using only 3 observations per video
Tv3o <- Tvo[sample(1:nrow(Tvo)),]
Tv3o <- Tv3o[order(Tv3o$subject_id),]
Tv3o$num <- 1
for(i in 2:nrow(Tv3o)){ 
  if(Tv3o$subject_id[i] == Tv3o$subject_id[i-1]) Tv3o$num[i] = Tv3o$num[i-1]+1
}
Tv3o = Tv3o[Tv3o$num<=3, 1:(ncol(Tv3o)-1)]
#means per video
Tv3 <- aggregate(. ~ subject_id, Tv3o[,-c(1,2)], mean)
Tv3$Nobs <- aggregate(. ~ subject_id, Tv3o[,-c(1,2)], length)[,2]
Tv3 = Tv3[,c(1,25,2:3,23:24,4:22)]
#show histogram of number of obs per video
hist(Tv3$Nobs, breaks= (0:max(Tv3$Nobs))+0.5)
#add a column for "interesting" animals
Tv3$interesting.animals = rowSums(Tv3[,interesting.animals])

# Show mean values across videos to represent the relative abundance of each animal
colmeans3 <- colMeans(Tv3[,1:26])
colmeans3[2:7]
colmeans3[8:26]
barplot(colmeans3[8:26], las=2, ylab="proportions across all videos")
barplot(colmeans3[9:25], las=2, ylab="proportions across all videos") #without ants

# Select videos with particular groups (seen by a proportion of observers above some threshold)
threshold.observers = 0.5
video3.butterfly <- Tv3$subject_id[Tv3$butterfly >= threshold.observers]
video3.fly <- Tv3$subject_id[Tv3$fly >= threshold.observers]
video3.honeybee <- Tv3$subject_id[Tv3$honeybee >= threshold.observers]
video3.stinglessb3ee <- Tv3$subject_id[Tv3$stinglessbee >= threshold.observers]
video3.wasp <- Tv3$subject_id[Tv3$wasp >= threshold.observers]
video3.spider <- Tv3$subject_id[Tv3$spider >= threshold.observers]
video3.IA <- Tv3$subject_id[Tv3$interesting.animals >= threshold.observers]

# Overlap between selected videos using whole data vs 3 observers per video
animals=colnames(Tv[,8:26])

Nvideo <- matrix(ncol=3,nrow=length(animals))
rownames(Nvideo)=animals
colnames(Nvideo)=c("Nvideo.all","Nvideo3","Nvideo.shared")
Nvideo <- as.data.frame(Nvideo)

for(a in animals){
  video <- Tv$subject_id[Tv[,a] >= threshold.observers]
  video3 <- Tv3$subject_id[Tv3[,a] >= threshold.observers]
  Nvideo[a,] = c(length(video),length(video3),sum(video %in% video3))
}
threshold.observers
Nvideo # show number of videos with prop of observers identifying each an animal above the threshold 
       # for whole dataset, dataset keeping just 3 observers per video, and shared 

# View videos on Playlists
## Videos of "interesting animals" seen by a min proportion of observers
threshold.observers = 0.25
video3.IA <- Tv3$subject_id[Tv3$interesting.animals >= threshold.observers]
video.IA <- Tv$subject_id[Tv$interesting.animals >= threshold.observers]

# Among the subset of videos seen by just 3 observers, 
## List of videos of "interesting animals" seen by just 1, 2 or 3 observers
video3.IA_1o <- Tv3$subject_id[Tv3$interesting.animals > 0 & Tv3$interesting.animals < 0.5]
video3.IA_2o <- Tv3$subject_id[Tv3$interesting.animals >= 0.5 & Tv3$interesting.animals < 0.7]
video3.IA_3o <- Tv3$subject_id[Tv3$interesting.animals >= 0.7]

video.IA_not_in_video3.IA = video.IA[!(video.IA %in% video3.IA_1o | video.IA %in% video3.IA_2o  | video.IA %in% video3.IA_3o)]

# Create playlist
#subject_id = video.IA_not_in_video3.IA
subject_id = video3.IA_3o

#videoT <- read.table("subject_ID_MOV.txt", header=T, sep='\t')
#treeT <- read.table("treeID.txt", header=T, sep='\t') 
#rownames(treeT) = treeT$tree

playlist <- videoT[videoT$Subject.ID %in% subject_id,]$Subject.Data
tree_id <- videoT[videoT$Subject.ID %in% subject_id,]$Tree.ID

# Add directory (format: "/Volumes/LYDIA/Oriana/Videos/Cameroun/Movingui_NK384/pi9/pi9_C759E7_018_1_split/")
dir0 <- "/Volumes/LYDIA/Oriana/Videos/"
dir1 <- paste0(treeT[tree_id,]$country, "/", treeT[tree_id,]$code)
dir2 <- substr(playlist,1, sapply(gregexpr('_', playlist), "[[", 1)-1)
dir3 <- substr(playlist,1, sapply(gregexpr('-', playlist), "[[", 1)-1)
playlist1 = paste0(dir0, dir1,"/",dir2,"/",dir3,"_split","/",playlist)

# Write playlist
n=length(playlist1)
playlistF = paste0("File", 1:n, "=", playlist1)
playlistF = c("[playlist]", paste0("NumberOfEntries=",n), playlistF)
filename = paste0("playlist_video3.IA(thrd=",threshold.observers,").pls")
#filename = paste0("playlist_video3.IA(3obs).pls")
#filename = paste0("playlist_video3.IA(thrd=0.25)_no_in_video3.IA.pls")
write(playlistF, file = filename)

