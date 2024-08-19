# Import necessary Libraries
library(reshape2)
library(vegan)
library(BiodiversityR)
library(ggplot2)

# Preparation dataframe all
All_datas <- read.delim("/Users/Oria/Documents/ULB/PhD/Marius/AFC/Rtab_alldatas.txt", header=TRUE)

# Add country + distinguish morphosp from each country (assumes no shared species)
All_datas$Country <- NA
All_datas$Country[All_datas$Tree.ID=="NK397" | All_datas$Tree.ID=="NK458"] = "C"
All_datas$Country[is.na(All_datas$Country)] = "G"
All_datas$Insect.ID2 <- interaction(All_datas$Country,All_datas$Insect.ID)

# Add trap type
All_datas$TrapType = "Pas"
All_datas$TrapType[All_datas$Trap=="Butterfly net" | All_datas$Trap=="Aspiration"] = "Act"
All_datas$Tree.abr = factor(All_datas$Tree.abr)
All_datas$Trap = factor(All_datas$Trap)
All_datas$TrapType = factor(All_datas$TrapType)
All_datas$Insect.ID2 = factor(All_datas$Insect.ID2)
All_datas$Order = factor(All_datas$Order)
All_datas$Family = factor(All_datas$Family)

# Reorder traps
trap=levels(All_datas$Trap)
All_datas$Trap = factor(All_datas$Trap, levels=trap[c(1,3,4,2,5,6)])

# Create matrix morpho-sp per tree
msp_treeAll = dcast(All_datas,  Insect.ID2 ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(msp_treeAll)=msp_treeAll$Insect.ID2
msp_treeAll = msp_treeAll[,-1]

# Create matrix family per tree
fam_treeAll = dcast(All_datas,  Family ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(fam_treeAll)=fam_treeAll$Family
fam_treeAll = fam_treeAll[,-1]

# Create matrix morpho_sp per tree (passive traps)
msp_treePas = dcast(All_datas[All_datas$TrapType=='Pas',],  Insect.ID2 ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(msp_treePas)=msp_treePas$Insect.ID2
msp_treePas = msp_treePas[,-1]

# Create matrix family per tree (passive traps)
fam_treePas = dcast(All_datas[All_datas$TrapType=='Pas',],  Family ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(fam_treePas)=fam_treePas$Family
fam_treePas = fam_treePas[,-1]

# Create matrix morpho_esp per tree (active traps)
msp_treeAct = dcast(All_datas[All_datas$TrapType=='Act',],  Insect.ID2 ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(msp_treeAct)=msp_treeAct$Insect.ID2
msp_treeAct = msp_treeAct[,-1]

# Create matrix family per tree (active traps)
fam_treeAct = dcast(All_datas[All_datas$TrapType=='Act',],  Family ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(fam_treeAct)=fam_treeAct$Family
fam_treeAct = fam_treeAct[,-1]

# Create matrix morpho_esp per trap
msp_TrapAll = dcast(All_datas,  Insect.ID2 ~ Trap, value.var='Quantity', fun.aggregate = sum)
row.names(msp_TrapAll)=msp_TrapAll$Insect.ID2
msp_TrapAll = msp_TrapAll[,-1]

# Create matrix family per trap
fam_TrapAll = dcast(All_datas,  Family ~ Trap, value.var='Quantity', fun.aggregate = sum)
row.names(fam_TrapAll)=fam_TrapAll$Family
fam_TrapAll = fam_TrapAll[,-1]

# Create matrix morpho_esp per Passive trap
msp_TrapPas = dcast(All_datas[All_datas$TrapType=='Pas',],  Insect.ID2 ~ Trap, value.var='Quantity', fun.aggregate = sum)
row.names(msp_TrapPas)=msp_TrapPas$Insect.ID2
msp_TrapPas = msp_TrapPas[,-1]

# Create matrix family per Passive trap
fam_TrapPas = dcast(All_datas[All_datas$TrapType=='Pas',],  Family ~ Trap, value.var='Quantity', fun.aggregate = sum)
row.names(fam_TrapPas)=fam_TrapPas$Family
fam_TrapPas = fam_TrapPas[,-1]

# Create matrix morpho_esp per Active trap
msp_TrapAct = dcast(All_datas[All_datas$TrapType=='Act',],  Insect.ID2 ~ Trap, value.var='Quantity', fun.aggregate = sum)
row.names(msp_TrapAct)=msp_TrapAct$Insect.ID2
msp_TrapAct = msp_TrapAct[,-1]

# Create matrix family per Active trap
fam_TrapAct = dcast(All_datas[All_datas$TrapType=='Act',],  Family ~ Trap, value.var='Quantity', fun.aggregate = sum)
row.names(fam_TrapAct)=fam_TrapAct$Family
fam_TrapAct = fam_TrapAct[,-1]

# Create matrix order per tree (ALL)
Ord_treeAll = dcast(All_datas,  Order ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(Ord_treeAll)=Ord_treeAll$Order
Ord_treeAll = Ord_treeAll[,-1]

## Diversity values## Abundance/richness/Shannon/Simpson (1-D)/Rarefy richness (sample=minimum observed)

# Tree fam
Res.fam_treeAll = diversityresult(t(fam_treeAll), index="abundance", method="each site", digits=5)
Res.fam_treeAll = cbind(Res.fam_treeAll,diversityresult(t(fam_treeAll), index="richness", method="each site", digits=4))
Res.fam_treeAll = cbind(Res.fam_treeAll,diversityresult(t(fam_treeAll), index="Shannon", method="each site", digits=3))
Res.fam_treeAll = cbind(Res.fam_treeAll,diversityresult(t(fam_treeAll), index="Simpson", method="each site", digits=2))
Res.fam_treeAll$Rarefy72=rarefy(t(fam_treeAll),sample=72)

Res.fam_treePas = diversityresult(t(fam_treePas), index="abundance", method="each site", digits=5)
Res.fam_treePas = cbind(Res.fam_treePas,diversityresult(t(fam_treePas), index="richness", method="each site", digits=4))
Res.fam_treePas = cbind(Res.fam_treePas,diversityresult(t(fam_treePas), index="Shannon", method="each site", digits=3))
Res.fam_treePas = cbind(Res.fam_treePas,diversityresult(t(fam_treePas), index="Simpson", method="each site", digits=2))
Res.fam_treePas$Rarefy38=rarefy(t(fam_treePas),sample=38)

Res.fam_treeAct = diversityresult(t(fam_treeAct), index="abundance", method="each site", digits=5)
Res.fam_treeAct = cbind(Res.fam_treeAct,diversityresult(t(fam_treeAct), index="richness", method="each site", digits=4))
Res.fam_treeAct = cbind(Res.fam_treeAct,diversityresult(t(fam_treeAct), index="Shannon", method="each site", digits=3))
Res.fam_treeAct = cbind(Res.fam_treeAct,diversityresult(t(fam_treeAct), index="Simpson", method="each site", digits=2))
Res.fam_treeAct$Rarefy11=rarefy(t(fam_treeAct),sample=11)

# Tree msp
Res.msp_treeAll = diversityresult(t(msp_treeAll), index="abundance", method="each site", digits=5)
Res.msp_treeAll = cbind(Res.msp_treeAll,diversityresult(t(msp_treeAll), index="richness", method="each site", digits=4))
Res.msp_treeAll = cbind(Res.msp_treeAll,diversityresult(t(msp_treeAll), index="Shannon", method="each site", digits=3))
Res.msp_treeAll = cbind(Res.msp_treeAll,diversityresult(t(msp_treeAll), index="Simpson", method="each site", digits=2))
Res.msp_treeAll$Rarefy72=rarefy(t(msp_treeAll),sample=72)

Res.msp_treePas = diversityresult(t(msp_treePas), index="abundance", method="each site", digits=5)
Res.msp_treePas = cbind(Res.msp_treePas,diversityresult(t(msp_treePas), index="richness", method="each site", digits=4))
Res.msp_treePas = cbind(Res.msp_treePas,diversityresult(t(msp_treePas), index="Shannon", method="each site", digits=3))
Res.msp_treePas = cbind(Res.msp_treePas,diversityresult(t(msp_treePas), index="Simpson", method="each site", digits=2))
Res.msp_treePas$Rarefy38=rarefy(t(msp_treePas),sample=38)

Res.msp_treeAct = diversityresult(t(msp_treeAct), index="abundance", method="each site", digits=5)
Res.msp_treeAct = cbind(Res.msp_treeAct,diversityresult(t(msp_treeAct), index="richness", method="each site", digits=4))
Res.msp_treeAct = cbind(Res.msp_treeAct,diversityresult(t(msp_treeAct), index="Shannon", method="each site", digits=3))
Res.msp_treeAct = cbind(Res.msp_treeAct,diversityresult(t(msp_treeAct), index="Simpson", method="each site", digits=2))
Res.msp_treeAct$Rarefy11=rarefy(t(msp_treeAct),sample=11)

# Trap fam
Res.fam_TrapAll = diversityresult(t(fam_TrapAll), index="abundance", method="each site", digits=5)
Res.fam_TrapAll = cbind(Res.fam_TrapAll,diversityresult(t(fam_TrapAll), index="richness", method="each site", digits=4))
Res.fam_TrapAll = cbind(Res.fam_TrapAll,diversityresult(t(fam_TrapAll), index="Shannon", method="each site", digits=3))
Res.fam_TrapAll = cbind(Res.fam_TrapAll,diversityresult(t(fam_TrapAll), index="Simpson", method="each site", digits=2))
Res.fam_TrapAll$Rarefy45=rarefy(t(fam_TrapAll),sample=45)

Res.fam_TrapPas = diversityresult(t(fam_TrapPas), index="abundance", method="each site", digits=5)
Res.fam_TrapPas= cbind(Res.fam_TrapPas,diversityresult(t(fam_TrapPas), index="richness", method="each site", digits=4))
Res.fam_TrapPas = cbind(Res.fam_TrapPas,diversityresult(t(fam_TrapPas), index="Shannon", method="each site", digits=3))
Res.fam_TrapPas = cbind(Res.fam_TrapPas,diversityresult(t(fam_TrapPas), index="Simpson", method="each site", digits=2))
Res.fam_TrapPas$Rarefy108=rarefy(t(fam_TrapPas),sample=108)

Res.fam_TrapAct = diversityresult(t(fam_TrapAct), index="abundance", method="each site", digits=5)
Res.fam_TrapAct= cbind(Res.fam_TrapAct,diversityresult(t(fam_TrapAct), index="richness", method="each site", digits=4))
Res.fam_TrapAct = cbind(Res.fam_TrapAct,diversityresult(t(fam_TrapAct), index="Shannon", method="each site", digits=3))
Res.fam_TrapAct = cbind(Res.fam_TrapAct,diversityresult(t(fam_TrapAct), index="Simpson", method="each site", digits=2))
Res.fam_TrapAct$Rarefy45=rarefy(t(fam_TrapAct),sample=45)

# Trap msp
Res.msp_TrapAll = diversityresult(t(msp_TrapAll), index="abundance", method="each site", digits=5)
Res.msp_TrapAll = cbind(Res.msp_TrapAll,diversityresult(t(msp_TrapAll), index="richness", method="each site", digits=4))
Res.msp_TrapAll = cbind(Res.msp_TrapAll,diversityresult(t(msp_TrapAll), index="Shannon", method="each site", digits=3))
Res.msp_TrapAll = cbind(Res.msp_TrapAll,diversityresult(t(msp_TrapAll), index="Simpson", method="each site", digits=2))
Res.msp_TrapAll$Rarefy45=rarefy(t(msp_TrapAll),sample=45)

Res.msp_TrapPas = diversityresult(t(msp_TrapPas), index="abundance", method="each site", digits=5)
Res.msp_TrapPas= cbind(Res.msp_TrapPas,diversityresult(t(msp_TrapPas), index="richness", method="each site", digits=4))
Res.msp_TrapPas = cbind(Res.msp_TrapPas,diversityresult(t(msp_TrapPas), index="Shannon", method="each site", digits=3))
Res.msp_TrapPas = cbind(Res.msp_TrapPas,diversityresult(t(msp_TrapPas), index="Simpson", method="each site", digits=2))
Res.msp_TrapPas$Rarefy108=rarefy(t(msp_TrapPas),sample=108)

Res.msp_TrapAct = diversityresult(t(msp_TrapAct), index="abundance", method="each site", digits=5)
Res.msp_TrapAct= cbind(Res.msp_TrapAct,diversityresult(t(msp_TrapAct), index="richness", method="each site", digits=4))
Res.msp_TrapAct = cbind(Res.msp_TrapAct,diversityresult(t(msp_TrapAct), index="Shannon", method="each site", digits=3))
Res.msp_TrapAct = cbind(Res.msp_TrapAct,diversityresult(t(msp_TrapAct), index="Simpson", method="each site", digits=2))
Res.msp_TrapAct$Rarefy45=rarefy(t(msp_TrapAct),sample=45)

# Tree Order
Res.Ord_treeAll = diversityresult(t(Ord_treeAll), index="abundance", method="each site", digits=5)
Res.Ord_treeAll= cbind(Res.Ord_treeAll,diversityresult(t(Ord_treeAll), index="richness", method="each site", digits=4))
Res.Ord_treeAll = cbind(Res.Ord_treeAll,diversityresult(t(Ord_treeAll), index="Shannon", method="each site", digits=3))
Res.Ord_treeAll = cbind(Res.Ord_treeAll,diversityresult(t(Ord_treeAll), index="Simpson", method="each site", digits=2))
Res.Ord_treeAll$Rarefy45=rarefy(t(Ord_treeAll),sample=45)

## Export table

# Tree
write.table(Res.fam_treeAll,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.fam_treeAll.txt",sep="\t")
write.table(Res.fam_treePas,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.fam_treePas.txt",sep="\t")
write.table(Res.fam_treeAct,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.fam_treeAct.txt",sep="\t")
write.table(Res.msp_treeAll,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.msp_treeAll.txt",sep="\t")
write.table(Res.msp_treePas,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.msp_treePas.txt",sep="\t")
write.table(Res.msp_treeAct,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.msp_treeAct.txt",sep="\t")

# Trap
write.table(Res.fam_TrapAll,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.fam_TrapAll.txt",sep="\t")
write.table(Res.fam_TrapPas,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.fam_TrapPas.txt",sep="\t")
write.table(Res.fam_TrapAct,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.fam_TrapAct.txt",sep="\t")
write.table(Res.msp_TrapAll,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.msp_TrapAll.txt",sep="\t")
write.table(Res.msp_TrapPas,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.msp_TrapPas.txt",sep="\t")
write.table(Res.msp_TrapAct,"/Users/Oria/Documents/ULB/PhD/Marius/AFC/Res.msp_TrapAct.txt",sep="\t")

## AFC script
# Biplot Tree stems (family)
AFC_tree<- dcast(All_datas,  Family ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(AFC_tree)=AFC_tree$Family
AFC_tree =AFC_tree[,-1]
t.AFC_tree<- t(AFC_tree)
tree.ca<- cca(t.AFC_tree)
screeplot(tree.ca, bstick = TRUE)
plot(tree.ca, scaling=2, main="Tree stems (Family)-biplot scaling 2")
text(tree.ca, dis="species", scaling="species", col="red", cex=.5, pos=4)
points(tree.ca, pch=16, scaling="species", col="black")
text(tree.ca, scaling="species", col="blue", cex=.6)
text(tree.ca, scaling="species", col="black", cex=.6)

# Biplot Tree stems (family) - PASSIVE TRAPS ONLY
AFC_tree<- dcast(All_datas[All_datas$TrapType=='Pas',],  Family ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(AFC_tree)=AFC_tree$Family
AFC_tree =AFC_tree[,-1]
t.AFC_tree<- t(AFC_tree)
tree.ca<- cca(t.AFC_tree)
screeplot(tree.ca, bstick = TRUE)
plot(tree.ca, scaling=2, main="Tree stems (Family)/ biplot scaling 2")
text(tree.ca, dis="species", scaling="species", col="red", cex=.5, pos=4)
points(tree.ca, pch=16, scaling="species", col="black")
text(tree.ca, scaling="species", col="blue", cex=.6)
text(tree.ca, scaling="species", col="black", cex=.6)

# Biplot Tree stems (order) - PASSIVE TRAPS ONLY
AFC_treeOrd<- dcast(All_datas[All_datas$TrapType=='Pas',],  Order ~ Tree.abr, value.var='Quantity', fun.aggregate = sum)
row.names(AFC_treeOrd)=AFC_treeOrd$Order
AFC_treeOrd =AFC_treeOrd[,-1]
t.AFC_treeOrd<- t(AFC_treeOrd)
treeOrd.ca<- cca(t.AFC_treeOrd)
screeplot(treeOrd.ca, bstick = TRUE)
plot(treeOrd.ca, scaling=2, main="Tree stems - Passive traps (Order) / biplot scaling 2")
points(treeOrd.ca, pch=20, scaling="species", col="black")

# Biplot Traps (family)
AFC_trap<- dcast(All_datas,  Family ~ Trap, value.var='Quantity', fun.aggregate = sum)
row.names(AFC_trap)=AFC_trap$Family
AFC_trap =AFC_trap[,-1]
t.AFC_trap<- t(AFC_trap)
trap.ca<- cca(t.AFC_trap)
screeplot(trap.ca, bstick = TRUE)
plot(trap.ca, scaling=2, main="Traps-biplot scaling 2")
text(trap.ca, dis="species", scaling="species", col="red", cex=.5, pos=4)
points(trap.ca, pch=16, scaling="species", col="black")
text(trap.ca, scaling="species", col="blue", cex=.6)
text(trap.ca, scaling="species", col="black", cex=.6)

# Biplot Passive Traps (family)
AFC_trapPas<- dcast(All_datas[All_datas$TrapType=='Pas',],  Family ~ Trap, value.var='Quantity', fun.aggregate = sum)
row.names(AFC_trapPas)=AFC_trapPas$Family
AFC_trapPas =AFC_trapPas[,-1]
t.AFC_trapPas<- t(AFC_trapPas)
trapPas.ca<- cca(t.AFC_trapPas)
screeplot(trapPas.ca, bstick = TRUE)
plot(trapPas.ca, scaling=2, main="Passive traps-biplot scaling 2")
text(trapPas.ca, dis="species", scaling="species", col="red", cex=.5, pos=4)
points(trapPas.ca, pch=16, scaling="species", col="black")
text(trapPas.ca, scaling="species", col="blue", cex=.6)
text(trapPas.ca, scaling="species", col="black", cex=.6)
