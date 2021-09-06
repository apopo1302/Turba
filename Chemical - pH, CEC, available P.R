#Install package
install.packages("ggplot2")
install.packages("agricolae")

#library package
library(ggplot2)
library(agricolae)

# Set your working directory (this is where your data is stored on your computer)
# Click 'Session' and 'Set Working Directory' and 'Choose Directory'
d1<-read.csv("Chemical - pH, CEC, available P.csv",header=T)
names(d1)

#______________________pH______________________
MWD <- ggplot(data=d1, aes(x=Size,y=MWD))+geom_boxplot()+xlab("Treatments")+ylab("Aggregate stability (MWD) (mm)")+facet_grid(. ~ Ã¯..Type)
MWD
# ANOVA to analyse MWD
MWD <-aov(MWD ~ Size, subset(d1, Ã¯..Type=="Turba"))
TukeyHSD(MWD)
model<-aov(MWD ~ Size + Plant, subset(d1, Ã¯..Type=="Turba", Water=="Dry"))
summary(model)
model <- aov(pH ~ Treatment, d1)
summary(model)
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)

#______________________EC______________________
model <- aov(EC ~ Treatment, subset(d1, ï..Type =="Sand"))
model <- aov(EC ~ ï..Type, d1)
summary(model)
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)

#______________________Ca______________________
model <- aov(Ca ~ Treatment, subset(d1, ï..Type =="Soil"))
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)

#______________________K______________________
model <- aov(K ~ Treatment, subset(d1, ï..Type =="Sand"))
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)

#______________________Mg______________________
model <- aov(Mg ~ Treatment, subset(d1, ï..Type =="Turba"))
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)

#______________________Na______________________
model <- aov(Na ~ Treatment, subset(d1, ï..Type =="Sand"))
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)

#______________________ECEC______________________
model <- aov(ECEC ~ Treatment, subset(d1, ï..Type =="Soil"))
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)


#______________________ESP______________________
model <- aov(ESP ~ Treatment, subset(d1, ï..Type =="Sand"))
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)

#______________________Available.P______________________
model <- aov(Available.P ~ Treatment, subset(d1, ï..Type =="Turba"))
HSD.test(model,"Treatment", group=T,console=T)
TukeyHSD(model)

