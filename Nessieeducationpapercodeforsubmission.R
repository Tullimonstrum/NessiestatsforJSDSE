####################################################### Loch Ness Monster Report Analysis

###### undertaken in R 4.1.2


### Open relevant R library 
library(car)

###### Load in data
setwd("")    ####set working directory to preferred location e.g. setwd("C:/Users/cgp2/Documents/Nessie")  

Nessie <- read.table("Nessiereducedforeducationpaper.csv", header = T, sep = ",") # n=1433



#################################################### Basic descriptive statistics

###### Figure 1 Plot
par (mfrow=c(3,1))
Month <- tapply(Nessie$EncounterMonthNumeric, Nessie$Event.ID, mean, na.rm = TRUE)
Month <- Month[is.na(Month) == FALSE]
length(Month)
Month2 <- hist(as.numeric(Month), main = "", breaks = seq(0, 12), xlab = "Month")
par(mfrow = c(3, 1))
barplot(height = Month2$counts, width = 1, space = NULL, xlab = "Month", names.arg = seq(1, 12), ylab = "Frequency", col = "white", main = "a)")

###### Time of Day

TimeSI <- tapply(Nessie$TimeSI, Nessie$Event.ID, mean, na.rm = T)
TimeSI <- TimeSI[is.na(TimeSI) == FALSE]
hist(TimeSI, breaks = seq(0, 24), xlab = "Time of Day", main = "b)", col = "white")
length(TimeSI)

###### BSS

BSSTranslated <- tapply(Nessie$BSSTranslated, Nessie$Event.ID, mean, na.rm = T)
BSSTranslated <- BSSTranslated[is.na(BSSTranslated) == FALSE]
temp <- hist(BSSTranslated, breaks = seq(0, 10), plot = FALSE)
barplot(height = temp$counts, width = 1, space = NULL, xlab = "Interpreted Beaufort Wind Force Scale", names.arg = seq(0, 9), ylab = "Frequency", col = "white", main = "c)")
length (BSSTranslated)
##### Summary of duration
summary (Nessie$Duration)

############################################### Analysis
par (mfrow=c(1,1))

####Need to rearrange data
####Creates an array
Nessie$Location <- ifelse(is.na(Nessie$Location) == TRUE, "NK", Nessie$Location)
Nessie$Location <- ifelse(Nessie$Location == "Submarine", "Other", Nessie$Location)
Nessie$Location <- ifelse(Nessie$Location == "Aerial", "Other", Nessie$Location)
Nessie$Location <- ifelse(Nessie$Location == "Webcam", "Other", Nessie$Location)
summary(as.factor(Nessie$Location))
table1 <- table(Nessie$Location, Nessie$Handedness, Nessie$Anonymous.at.point.of.interview)
table1
### then collapse table
final <- rep(NA, 16)
final[1:4] <- table1[, 1, 1]
final[5:8] <- table1[, 2, 1]
final[9:12] <- table1[, 1, 2]
final[13:16] <- table1[, 2, 2]

####Create vectors for loglinear models
Location <- rep(c("Boat", "NK","Other","Shore") , 4)
Handedness <- rep(rep(c("1st", "2nd"), each = 4), 2)
Anonymous <- rep(c("No", "Yes"), each = 8)
df1 <- data.frame(final, Location, Handedness, Anonymous)


####Loglinear model for interest (not in paper)
modelFreq <- glm(final ~ Location * Handedness * Anonymous - Location:Handedness:Anonymous, data = df1, family =poisson)
Anova(modelFreq)
modelFreq <- glm(final ~ Location * Handedness * Anonymous - Location:Handedness:Anonymous - Handedness:Anonymous, data = df1, family = poisson)
Anova(modelFreq)



###### Chi-squared tests of association

table1 <- table(Nessie$Location, Nessie$Handedness)
table1
test1 <- chisq.test(table1)
test1$observed
test1$expected
((test1$observed-test1$expected)^2)/test1$expected         ###chi-squared components
test1

table2<- table(Nessie$Location, Nessie$Anonymous.at.point.of.interview)
table2
test2<- chisq.test(table2)
test2$observed
test2$expected
((test2$observed-test2$expected)^2)/test2$expected         ###chi-squared components
test2

table3<- table(Nessie$Anonymous.at.point.of.interview, Nessie$Handedness)
test3<- chisq.test(table3)
test3$observed
test3$expected
test3




####### Analysis of distances   - GLMs
par (mfrow=c(2,2))    ##Four graphs per image
Nessie2 <- Nessie
Nessie2$AllDistNADpriority <- ifelse (Nessie2$AllDistNADpriority==0,0.01, Nessie2$AllDistNADpriority)  ###makes one 0 datum not zero. 
modelNAD <- glm(AllDistNADpriority ~ Report.Year.for.analysis + Handedness + Location + Anonymous.at.point.of.interview, data = Nessie2, family = Gamma)
plot (modelNAD)    ##Diagnostics not really great so we made need to bootstrap


# Remove Anonymous
modelNAD <- glm(AllDistNADpriority ~ Report.Year.for.analysis + Handedness + Location, data = Nessie2, family = Gamma)
Anova(modelNAD)
plot (modelNAD) ##diagnostics still not good

#Remove location
modelNAD <- glm(AllDistNADpriority ~ Report.Year.for.analysis + Handedness, data = Nessie2, family = Gamma)
Anova(modelNAD)
plot (modelNAD)   ##diagnostics still not ideal. 

#Remove report year
modelNAD <- glm(AllDistNADpriority ~ Handedness, data = Nessie2, family = Gamma)
Anova(modelNAD)
plot (modelNAD)

#Diagnostics not great so use Kruskal-Wallace test 
modelNAD <- kruskal.test (AllDistNADpriority ~ Handedness, data = Nessie)
modelNAD

##Figure 2
boxplot (AllDistNADpriority ~ Handedness, data=Nessie, ylab= "Reported Nearest Approach Distance (m)", cex=0)
set.seed (101)
points (jitter(ifelse (Nessie$Handedness=="1st",1,2), amount=0.05), Nessie$AllDistNADpriority, pch=20, cex=0.8)

tapply (Nessie$AllDistNADpriority, Nessie$Handedness,summary)
tapply (Nessie$AllDistNADpriority, Nessie$Handedness,length)


###### Analsis of estimated seen length

modelLength <- glm(SeenLengthSI ~ Report.Year.for.analysis + Handedness + Location + Anonymous.at.point.of.interview, data = Nessie,)
Anova(modelLength)
plot (modelLength)

modelLength <- glm(SeenLengthSI ~ Report.Year.for.analysis + Location + Anonymous.at.point.of.interview, data = Nessie, family = Gamma)
Anova(modelLength)
modelLength <- glm(SeenLengthSI ~ Report.Year.for.analysis + Anonymous.at.point.of.interview, data = Nessie, family = Gamma)
Anova(modelLength)
plot (modelLength)

modelLength <- glm(SeenLengthSI ~ Report.Year.for.analysis, data = Nessie, family=Gamma)
Anova(modelLength)
par (mfrow =c(2,2))
plot (modelLength)

## Figure 3 as a bootstrap as used in current draft
plot(Nessie$Report.Year.for.analysis, Nessie$SeenLengthSI, xlim = c(1931, 2020), xlab = "Report Year", ylab = "Estimated seen length (m)", pch = 20)

Report.Year.for.analysis <- seq (1933, 2016)
df1=data.frame (Report.Year.for.analysis) 
matrixLength <- matrix (NA, length (Report.Year.for.analysis),10000)


set.seed (102)
for (i in 1:10000){
    btNessie <- Nessie[sample (1:dim (Nessie)[1], size =dim (Nessie)[1], replace=T),]
    btmodelLength <- glm(SeenLengthSI ~ Report.Year.for.analysis, data = btNessie,  family = Gamma)
    matrixLength[,i ]=predict (btmodelLength, df1, type="response")
}

lowerCI <- apply (matrixLength, 1, quantile, 0.005)
upperCI <- apply (matrixLength, 1, quantile, 0.995)

lines(Report.Year.for.analysis, predict (modelLength, df1, type="response"), col = 1)
lines(Report.Year.for.analysis, lowerCI, lty = 2, lwd = 2, col = 1)
lines(Report.Year.for.analysis, upperCI, lty = 2, lwd = 2, col = 1)


###### Analysis of duration

modelDuration <- glm(Duration ~ Report.Year.for.analysis + Handedness + Location + Anonymous.at.point.of.interview, data = Nessie, family = Gamma)
Anova (modelDuration)

modelDuration <- glm(Duration ~ Report.Year.for.analysis + Handedness + Location, data = Nessie, family = Gamma)
Anova (modelDuration)

modelDuration <- glm(Duration ~ Report.Year.for.analysis + Handedness, data = Nessie, family = Gamma)
Anova (modelDuration)

modelDuration <- glm(Duration ~ Report.Year.for.analysis, data = Nessie, family = Gamma)
Anova (modelDuration)
plot (modelDuration)


## Figure 4
plot(Nessie$Report.Year.for.analysis, Nessie$Duration, pch = 20, xlab = "Report Year", ylab = "Duration (mins)", xlim = c(1930, 2020))

Report.Year.for.analysis <- seq (1933, 2016)
df1=data.frame (Report.Year.for.analysis) 
matrixLength <- matrix (NA, length (Report.Year.for.analysis),10000)


set.seed (102)
for (i in 1:10000){
    btNessie <- Nessie[sample (1:dim (Nessie)[1], size =dim (Nessie)[1], replace=T),]
    btmodelDuration <- glm(Duration ~ Report.Year.for.analysis, data = btNessie, family = Gamma)
    matrixLength[,i ]=predict (btmodelDuration, df1, type="response")
}

lowerCI <- apply (matrixLength, 1, quantile, 0.005)
upperCI <- apply (matrixLength, 1, quantile, 0.995)

lines(Report.Year.for.analysis, predict (modelDuration, df1, type="response"), col = 1)
lines(Report.Year.for.analysis, lowerCI, lty = 2, lwd = 2, col = 1)
lines(Report.Year.for.analysis, upperCI, lty = 2, lwd = 2, col = 1)



