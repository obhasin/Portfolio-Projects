#Analysis of Buzzy Bee classification 

# Set working directory
setwd("/Users/Oria/Documents/ULB/PhD/Zooniverse/Zoo_Data_Analysis/Class_expert")

# Define the taxon and adjust file paths as necessary
taxon <- "All"  # Options: "MOV", "AYO", "OKA", "OMV", "All"
question_file <- paste0("question_extractor_", taxon, "_juin2021.csv")
survey_file <- paste0("survey_extractor_", taxon, "_juin2021.csv")

# Load required libraries
library(reshape2)
library(vegan)

# Read data files with the appropriate separators
Qvo <- read.csv(question_file, sep=";")
Qvo <- Qvo[, c(1, 2, 7, 9, 11)]

Svo <- read.csv(survey_file, sep=";")
Svo <- Svo[, c(1, 2, 7, 9)]

# Create a table with answers per video and observer for animal identification
Svo2 <- dcast(Svo, classification_id + subject_id + user_name ~ data.choice, value.var = 'data.choice', fun.aggregate = length)
Svo2$Totals <- rowSums(Svo2[,-c(1:3)])

# Combine Qvo and Svo2 to build the table with observation data
Tvo <- cbind(Qvo, matrix(nrow = nrow(Qvo), ncol = 19))
colnames(Tvo)[6:24] <- c("Total", colnames(Svo2)[c(4:21)])
Tvo[, 6:24] <- 0
Tvo[match(Svo2$classification_id, Tvo$classification_id), 6:24] <- Svo2[, c(22, 4:21)]
Tvo$user_name <- factor(Tvo$user_name)
Tvo[, 4:5][is.na(Tvo[, 4:5])] <- 0  # Replace NA with 0

# Handle ant observations by resetting values and adjusting totals
Tvo$Total <- Tvo$Total - Tvo$ant
Tvo$ant <- 0
Tvo$data.no[Tvo$Total == 0] <- 1
Tvo$data.yes[Tvo$Total == 0] <- 0

# Check coherence of each observation with respect to other observations on the same video
Tvo$coher.id <- Tvo$coher.det <- NA
for (i in 1:nrow(Tvo)) {
  m <- as.matrix(Tvo[Tvo$subject_id == Tvo$subject_id[i] & (1:nrow(Tvo) != i), c(5, 7:24)])
  if (nrow(m) == 0) next
  f <- as.matrix(Tvo[i, c(5, 7:24)])
  Tvo$coher.det[i] <- sum(m[,1] == f[,1]) / nrow(m)  # Proportion agreement on detection
  if (nrow(m) > 1) {
    mf <- matrix(rep(f, times = nrow(m)), nrow = nrow(m), byrow = TRUE)
    Tvo$coher.id[i] <- sum(rowSums(m[, -1] - mf[, -1]) == 0) / nrow(m)  # Full agreement on identification
  }
  if (nrow(m) == 1) Tvo$coher.id[i] <- sum(sum(m[, -1] - mf[, -1]) == 0)
}

# Aggregate data by observer and video for analysis
To <- aggregate(. ~ user_name, Tvo[,-c(1, 3)], mean)
To$Nvideo <- aggregate(. ~ user_name, Tvo[,-c(1, 3)], length)[,2]
To <- To[order(-To$Nvideo), c(1, 25, 2:3, 23:24, 4:22)]

Tv <- aggregate(. ~ subject_id, Tvo[,-c(1, 2)], mean)
Tv$Nobs <- aggregate(. ~ subject_id, Tvo[,-c(1, 2)], length)[,2]
Tv <- Tv[, c(1, 25, 2:3, 23:24, 4:22)]

# Perform correspondence analysis to show species scores
cca <- cca(Tv[Tv$Total > 0, 8:25])
plot(cca, display = c("sp"))

# Calculate and plot mean proportions of each species across all videos
colmeans <- colMeans(Tv)
barplot(colmeans[8:25], las = 2, ylab = "Proportions across all videos")

# Consider only majority classifications and visualize results
Tv.maj <- Tv
Tv.maj[8:25][Tv.maj[8:25] < 0.5] <- 0
colmeans.maj <- colMeans(Tv.maj)
barplot(colmeans.maj[8:25], las = 2, ylab = "Proportions of majority classifications")

# Load additional formatted data if required
Tv_file <- paste0("Tvo_", taxon, "_Exp.csv")
Tvexp <- read.csv(Tv_file, sep = ";")

# Aggregate results and calculate coherence between expert and majority classifications
Rexp <- Tvexp[, c(1, 2, 3, 6:22)]
Rbb <- Tv.maj[, c("subject_id", "data.no", "data.yes", colnames(Tv.maj)[9:25])]
Rbb[, 4:20][Rbb[, 4:20] >= 0.5] <- 1
Rbb$data.no[Rbb$data.no > 0.5] <- 1
Rbb$data.no[Rbb$data.no <= 0.5] <- 0

# Confusion matrix to compare classifications
Rbb_exp <- data.frame(id = Rbb$subject_id, bb = Rbb$time, exp = Rexp$time)
ConM <- table(Rbb_exp$bb, Rbb_exp$exp)

# Visualize confusion matrix
print(ConM, digits = 2)
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

