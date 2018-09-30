mydata <- read_csv("~/Desktop/Projects/Abalone_1/mydata.csv")
str(mydata)
#Convert to factor
mydata$CLASS<-as.factor(mydata$CLASS)
mydata$SEX<-as.factor(mydata$SEX)
str(mydata)
#Add in new varaibles
mydata$VOLUME<-mydata$LENGTH*mydata$DIAM*mydata$HEIGHT
mydata$RATIO<-mydata$SHUCK/mydata$VOLUME
str(mydata)
#1a
summary(mydata)
#1b
mytable<-table(mydata$SEX, mydata$CLASS)
addmargins(mytable)
barplot(mytable, main = "Comparison of Sex and Class of Abalones", ylab = "Frequency", ylim = c(0,170), xlab = "Class", beside = TRUE, col = c("mistyrose", "cornsilk", "lightblue"))
legend("topright", inset = .02, title = "Sex of Abalones", c("Female", "Infant", "Male"), fill=c("mistyrose", "cornsilk", "lightblue"), cex=0.8 )
#1c
set.seed(123)
work<-mydata[sample(nrow(mydata), 200), ]
plot(work[,2:6])
#2a
plot(mydata$VOLUME,mydata$WHOLE,xlab="Volume", ylab="Whole", main = "Volume vs Whole", col="red" )
#2b
plot( mydata$WHOLE,mydata$SHUCK, xlab="Shuck", ylab="Whole", main = "Shuck vs Whole", col="blue")
abline(a=0, b=(max(mydata$SHUCK/mydata$WHOLE)), col= "red") 

#3a
par(mfrow=c(3,3))
#Create subsets by gender
femaleratio<-subset(mydata$RATIO, mydata$SEX=="F")
infantratio<-subset(mydata$RATIO, mydata$SEX=="I")
maleratio<-subset(mydata$RATIO, mydata$SEX=="M")
#Create histograms
hist(femaleratio, main = "Ratio-Female", ylab = "Frequency", xlab = "Ratio", col = "mistyrose" )
hist(infantratio, main = "Ratio-Infant", ylab = "Frequency", xlab = "Ratio", col = "yellow" )
hist(maleratio, main = "Ratio-Male", ylab = "Frequency", xlab = "Ratio", col = "lightblue" )
#Create boxplots
boxplot(femaleratio, range = 1.5, main = "Ratio-Female", col = "mistyrose")
boxplot(infantratio, range = 1.5, main = "Ratio-Infant", col = "yellow")
boxplot(maleratio, range = 1.5, main = "Ratio-Male", col = "lightblue")
#Create QQ Plots
qqnorm(femaleratio, main = "Q-Q Plot for Female ratio", ylab = "Sample Quantiles for Female ratio", col="mistyrose")
qqline(femaleratio, col="green")
qqnorm(infantratio, main = "Q-Q Plot for Infant ratio", ylab = "Sample Quantiles for Infant ratio", col="yellow")
qqline(infantratio, col="green")
qqnorm(maleratio, main = "Q-Q Plot for Male ratio", ylab = "Sample Quantiles for Male ratio", col="lightblue")
qqline(maleratio, col="green")
par(mfrow=c(1,1))

#3b
boxplot.stats(femaleratio, coef = 1.5)
boxplot.stats(femaleratio, coef = 3.0)
boxplot.stats(infantratio, coef = 1.5)
boxplot.stats(infantratio, coef = 3.0)
boxplot.stats(maleratio, coef = 1.5)
boxplot.stats(maleratio, coef = 3.0)
#Need to identify which Abalones are the ouliers. 
which(femaleratio>=.212)
which(femaleratio<=0.0674)
which(infantratio>=.2218)
which(maleratio>=0.2286)
#Matrix of all values
female<-subset(mydata, mydata$SEX=="F")
infant<-subset(mydata, mydata$SEX=="I")
male<-subset(mydata, mydata$SEX=="M")
female[c(21,50,91,92,129,257),]
infant[c(3,37,42,58,67,89,105,200),]
male[c(91,99,148,155,197),]
summary(infant)

#4a
install.packages(c("ggplot2", "gridExtra", "moments"))
library(ggplot2)
library(gridExtra)
#2 side by side boxplots of each class for whole and volume
par(mfrow=c(2,2))
boxplot(mydata$VOLUME~mydata$CLASS, data = mydata, xlab="Class", ylab="Volume")
boxplot(mydata$WHOLE~mydata$CLASS, data = mydata, xlab="Class", ylab="Whole")
#scatterplot of volume vs rings and whole vs rings 
plot(mydata$VOLUME, mydata$RINGS, col="red", xlab = "Volume", ylab = "Rings")
plot(mydata$WHOLE, mydata$RINGS, col="blue", xlab = "Whole", ylab = "Rings")
par(mfrow=c(1,1))

#5a
agg.vol<-aggregate(VOLUME~SEX+CLASS, data=mydata, mean)
agg.shuck<-aggregate(SHUCK~SEX+CLASS, data=mydata, mean)
agg.ratio<-aggregate(RATIO~SEX+CLASS, data=mydata, mean)
#Create matrix of mean values
matrix(agg.vol[,3], nrow=3, ncol=5, byrow=FALSE, dimnames = list(c("Female", "Infant","Male"), c("A1","A2","A3","A4", "A5")))
matrix(agg.shuck[,3], nrow=3, ncol=5, byrow=FALSE, dimnames = list(c("Female", "Infant","Male"), c("A1","A2","A3","A4", "A5")))
matrix(agg.ratio[,3], nrow=3, ncol=5, byrow=FALSE, dimnames = list(c("Female", "Infant","Male"), c("A1","A2","A3","A4", "A5")))

#5b
ggplot(data=agg.vol, aes(x=CLASS, y=VOLUME, group=SEX, color=SEX))+geom_line()+geom_point(size=4)+ggtitle("Plot of Mean Volume versus Class for three sexes")
ggplot(data=agg.shuck, aes(x=CLASS, y=SHUCK, group=SEX, color=SEX))+geom_line()+geom_point(size=4)+ggtitle("Plot of Mean Shuck versus Class for three sexes")
ggplot(data=agg.ratio, aes(x=CLASS, y=RATIO, group=SEX, color=SEX))+geom_line()+geom_point(size=4)+ggtitle("Plot of Mean Ratio versus Class for three sexes")
