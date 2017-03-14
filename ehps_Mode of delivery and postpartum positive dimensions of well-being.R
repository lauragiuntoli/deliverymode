# ehps (2017): Mode of delivery and postpartum positive dimensions of well-being

dd <- read.csv2("ehps_Mode of delivery.csv")
dim(dd)
# from the whole sample (n=208) I select only the partecipants
# in the period from 1 to 3 months post partum
monthsPP <- dd[,3]
str(monthsPP)
data <- subset(dd, monthsPP < 4)
dim(data)

# Total scores
swls <- rowSums(data[,7:11])
fs <- rowSums(data[,12:19])
spane <- data[,20:31]
Sp <- rowSums(spane[,c(1,3,5,7,10,12)])
Sn <- rowSums(spane[,c(2,4,6,8,9,11)])
bdi <- rowSums(data[,32:52])
epds <- rowSums(data[,63:72])

# Correlation matrix
round(cor(cbind(swls,fs,Sp,Sn,bdi,epds)),2)

# Mode of delivery
group <- data[,5]
table(group)

# Boxplots
boxplot(swls ~ group, main = "SWLS")
boxplot(fs ~ group, main = "FS")
boxplot(Sp ~ group, main = "SPANE-P")
boxplot(Sn ~ group, main = "SPANE-N")
boxplot(bdi ~ group, main = "BDI-II")
boxplot(epds ~ group, main = "EPDS")

# Setting contrast
# vs(spontaneous vaginal delivery) = reference group
contrasts(group) <- contr.treatment(5, base=5)

model1 <- lm(fs ~ group) 
summary(model1)

model2 <- lm(swls ~ group)
summary(model2)

model3 <- lm(Sp ~ group) 
summary(model3)

model4 <- lm(Sn ~ group) 
summary(model4)

model5 <- lm(bdi ~ group) 
summary(model5)

model6 <- lm(epds ~ group) 
summary(model6)

##################################################
# Overall comparisons between positive and 
# negative dimensions of well-being

WB <- scale(swls) + scale(fs) + scale(Sp)
D <- scale(bdi)  + scale(epds)

boxplot(WB ~ group, main = "well-being")
boxplot(D ~ group, main = "depression")

model7 <- lm(WB ~ group) 
summary(model7)

model8 <- lm(IB ~ group) 
summary(model8)