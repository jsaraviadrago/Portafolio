# Script for the SEM paper

library(xlsx)
jh <- read.xlsx("/Users/jcsaravia/Documents/Documentos/QASS Leuven/Structural equation modelling/Assignment/data_base_SEM.xlsx", 1)
head(jh)
jhlimpio <- na.omit(jh)
head(jhlimpio)

# Make some labels specially in the grouping variables

jhlimpio$gender <- factor(jhlimpio$Sex) # First change the variable into a factor
summary(jhlimpio$gender) # Check it has change
library(plyr) # Run plyr library to recode variables

jhlimpio$gender <- revalue(jhlimpio$gender, c("1"="Female", "2"="Male")) # Recoded variables
summary(jhlimpio$gender) # Check the new variables
# Female = 410
# Male 226
# NA 45
# "0" = 74 (have to check what that is)

# Checking age groups

jhlimpio$Agen <- as.numeric(levels(jhlimpio$Age))[jhlimpio$Age] # Change from factor to numeric
summary(jhlimpio$Agen) # Summarize age to get descriptives
library(psych)
describe(jhlimpio$Agen) # Here I get all the descriptive statistics. 
class(jhlimpio$training)
jhlimpio$training1 <- factor(jhlimpio$training)
# recoding variable
jhlimpio$training1 <- revalue(jhlimpio$training1, c("1"="primary education", "2"="Lower secondary technical or vocational education", "3"="Lower secondary general education", "4"="Upper secondary technical or vocational education", "5"="Upper secondary general education", "6"="Higher education outside university / professional degree", "7"="University / academic bachelor or master"))
summary(jhlimpio$training1)

# Descriptive statistics means and standard deviations
# Note reliability was calculated in spss

# Creating this
jhlimpio$rolamb <- (jh$rrola1 + jh$rrola2 + jh$rrola3)/3
jhlimpio$emodemand <- (jh$emo1 + jh$emo2 + jh$emo3)/3
head(jhlimpio)

mean(jhlimpio$Engagement) 
sd(jhlimpio$Engagement)
mean(jhlimpio$Exhaustion) 
sd(jhlimpio$Exhaustion)
mean(jhlimpio$emodemand)
sd(jhlimpio$emodemand)
mean(jhlimpio$rolamb)
sd(jhlimpio$rolamb)
mean(jhlimpio$Mindfulness)
sd(jhlimpio$Mindfulness)

# Descriptive statistics (multiple regression)
install.packages("QuantPsyc") 
library(QuantPsyc) # Standardized betas
# Step 1 Checking the relationship between the variables X and Y no mediator
hindengs <- lm(Engagement ~emodemand + rolamb, data=jhlimpio)
summary(hindengs)
lm.beta(hindengs)
hindexhs <- lm(Exhaustion ~ emodemand + rolamb, data=jhlimpio)
summary(hindexhs)
lm.beta(hindexhs)
challengs <- lm(Engagement ~ workload + cognload, data=jhlimpio)
summary(challengs)
lm.beta(challengs)
challexhs <- lm(Exhaustion ~ workload + cognload, data=jhlimpio)
summary(challexhs)
lm.beta(challexhs)

# Step 2 checking relationship between predictors and mediator
mindhind <- lm(Mindfulness ~ emodemand + rolamb, data=jhlimpio)
summary(mindhind)
lm.beta(mindhind)
mindchall <- lm(Mindfulness ~ workload + cognload, data=jhlimpio)
summary(mindchall)
lm.beta(mindchall)

# Step 3 Checking full model with all and mediators
hindeng <- lm(Engagement ~ Mindfulness + emodemand + rolamb, data=jhlimpio)
summary(hindeng)
lm.beta(hindeng)
hindexh <- lm(Exhaustion ~ Mindfulness + emodemand + rolamb, data=jhlimpio)
summary(hindexh)
lm.beta(hindexh)
challeng <- lm(Engagement ~ Mindfulness + workload + cognload, data=jhlimpio)
summary(challeng)
lm.beta(challeng)
challexh <- lm(Exhaustion ~ Mindfulness + workload + cognload, data=jhlimpio)
summary(challexh)
lm.beta(challexh)

# Step 4: Check for partial or full mediation
#Full mediation if the effect of X on Y controlling for M (path c') should be zero
# Partial mediation if the first three steps are met but the Step 4 is not, then partial mediation is indicated.

# Creating a cfa to check if a latent variable for job hindrance can be created

hind <- c("whi1","whi2","whi3","whi4","rrola1","rrola2","rrola3","emo1","emo2","emo3") # creating the object to create the new data
hinddata <- jh[hind] # creating a new data set from the big one
head(hinddata) # checking data
Chinddata <- NA
chinddata <- na.omit(hinddata) # base de datos sin missings
cov(chinddata) # crear la matriz de covarianza

library(lavaan)
modelhind <- 'f =~  whi1 + whi2 + whi3 + whi4 + rrola1 +rrola2 + rrola3 + emo1 + emo2 + emo3' #creating the model to fit it
mhind1 <- cfa(modelhind, chinddata)  # fit the model
summary(mhind1,fit.measures=T,standardized=T)

library(semPlot) 
semPaths(mhind1,"model","stand",style="LISREL",rotation=1,
                          edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model


# Using a cfa to check if a variable well being that combines engagement and exhaustion can be created

# engagement
wbdata <- c("eng1","eng2","eng3","eng4","eng5","eng6","eng7","eng8","eng9","uit1","uit2","uit3","uit4", "uit5")
wbdata1 <- jh[wbdata] # creating a new data set from the big one
wbdatal <- na.omit(wbdata1) # base de datos sin missings
cov(wbdatal)


modelwb <- 'f =~  eng1 + eng2 + eng3 + eng4 + eng5 + eng6 + eng7 + eng8 + eng9 + uit1 + uit2 + uit3 + uit4 + uit5' #creating the modle to fit it
mwb <- cfa(modelwb, wbdatal)  # fit the model
summary(mwb,fit.measures=T,standardized=T)

semPaths(mwb,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

# Better to use engagement and exhaustion seperatly 

mindfuldata <- c("mind1","mind2","mind3","mind4","mind5","mind6","mind7","mind8","mind9","mind10","mind11","mind12","mind13","mind14","mind15")
mfdata1 <- jh[mindfuldata] # creating a new data set from the big one
mfdatal <- na.omit(mfdata1) # base de datos sin missings

mindful <- 'f =~ mind1 + mind2 + mind3 + mind4 + mind5 + mind6 + mind7 + mind8 + mind9 + mind10 + mind11 + mind12 + mind13 + mind14 + mind15'
mindful2 <- cfa(mindful, mfdatal)  # fit the model
summary(mindful2,fit.measures=T,standardized=T)

semPaths(mindful2,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mindful2,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

mindful2 <- 'f =~ mind2 + mind3 + mind4 + mind5 + mind7 + mind8 + mind9 + mind10 + mind11 + mind12 + mind13 + mind14 + mind15'
mindful3 <- cfa(mindful2, mfdatal)  # fit the model
summary(mindful3,fit.measures=T,standardized=T)

semPaths(mindful3,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mindful3 <- 'f =~ mind2 + mind3 + mind4 + mind5 + mind7 + mind8 + mind9 + mind10 + mind11 + mind12 + mind14 + mind15'
mindful4 <- cfa(mindful3, mfdatal)  # fit the model
summary(mindful4,fit.measures=T,standardized=T)

semPaths(mindful4,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mindful4,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

mindful4 <- 'f =~ mind2 + mind3 + mind4 + mind5 + mind7 + mind8 + mind9 + mind10 + mind11 + mind12 + mind14 + mind15
mind4 ~~  mind5'
mindful5 <- cfa(mindful4, mfdatal)  # fit the model
summary(mindful5,fit.measures=T,standardized=T)

semPaths(mindful5,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mindful5,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

mindful5 <- 'f =~ mind2 + mind3 + mind4 + mind5 + mind7 + mind8 + mind9 + mind10 + mind11 + mind12 + mind14 + mind15
mind4 ~~  mind5
mind2 ~~  mind3'
mindful6 <- cfa(mindful5, mfdatal)  # fit the model
summary(mindful6,fit.measures=T,standardized=T)

semPaths(mindful6,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

mi<-inspect(mindful6,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

mindful6 <- 'f =~ mind2 + mind3 + mind4 + mind5 + mind7 + mind8 + mind9 + mind10 + mind11 + mind12 + mind14 + mind15
mind4 ~~  mind5
mind2 ~~  mind3
mind8 ~~ mind12'
mindful7 <- cfa(mindful6, mfdatal)  # fit the model
summary(mindful7,fit.measures=T,standardized=T)


semPaths(mindful7,"model","stand",style="LISREL",rotation=1,
         edge.color="black",edge.label.cex=1,mar=c(10,1,2,1)) # Graphic of the model

#Better to work Mindfulness as a Manifest variable, this will decrease the number of paths
# In order to decrease the number of paths, use role ambiguity and emotional demands as two latent variables
# Theoretically these variables measure job hindrance.


#Full sem: Job hindrance, mindfulness, engagement and exhaustion. Mediation model. 

# Hindrance
full.sem3 <- 'RA =~ rrola1 + rrola2 + rrola3
ED =~ emo1 + emo2 + emo3
Mindfulness ~ RA
Mindfulness ~ ED
Engagement ~ Mindfulness
Exhaustion ~ Mindfulness
Engagement ~ RA
Engagement ~ ED
Exhaustion ~ RA
Exhaustion ~ ED'
fit3 <- cfa(full.sem3,data=jhlimpio)
summary(fit32,fit.measures=T,standardized=T)
fit32 <- sem(full.sem3,data=jhlimpio)

semPaths(fit3,"model","stand",style="LISREL",rotation=1, layout="circle",
         edge.color="black",edge.label.cex=.5,mar=c(5,2,5,2)) # Graphic of

mi<-inspect(fit3,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

full.sem4 <- 'RA =~ rrola1 + rrola2 + rrola3
ED =~ emo1 + emo2 + emo3
Mindfulness ~ RA
Mindfulness ~ ED
Engagement ~ Mindfulness
Exhaustion ~ Mindfulness
Engagement ~ RA
Engagement ~ ED
Exhaustion ~ RA
Exhaustion ~ ED
emo2 ~~ emo3'
fit4 <- cfa(full.sem4,data=jhlimpio)
summary(fit4,fit.measures=T,standardized=T)

semPaths(fit4,"model","stand",style="LISREL",rotation=1, layout="circle",
         edge.color="black",edge.label.cex=.5,mar=c(5,2,5,2)) # Graphic of

mi<-inspect(fit4,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]

full.sem5 <- 'RA =~ rrola1 + rrola2 + rrola3
ED =~ emo1 + emo2 + emo3
Mindfulness ~ RA
Mindfulness ~ ED
Engagement ~ Mindfulness
Exhaustion ~ Mindfulness
Engagement ~ RA
Engagement ~ ED
Exhaustion ~ RA
Exhaustion ~ ED
emo2 ~~ emo3
emo1 ~~ emo2'
fit5 <- cfa(full.sem5,data=jhlimpio)
summary(fit5,fit.measures=T,standardized=T)

semPaths(fit5,"model","stand",style="LISREL",rotation=1, layout="circle",
         edge.color="black",edge.label.cex=.5,mar=c(5,2,5,2)) # Graphic

mi<-inspect(fit5,"mi") # modification indices
mi.sorted<-mi[order(-mi$mi),]
mi.sorted[1:5,]
anova(fit4,fit5)

full.sem6 <- 'RA =~ rrola1 + rrola2 + rrola3
ED =~ emo1 + emo2 + emo3
Mindfulness ~ RA
Mindfulness ~ ED
Engagement ~ Mindfulness
Exhaustion ~ Mindfulness
Engagement ~ RA
Engagement ~ ED
Exhaustion ~ RA
Exhaustion ~ ED
emo2 ~~ emo3
emo1 ~~ emo2
rrola1 ~~ rrola2'
fit6 <- cfa(full.sem6,data=jhlimpio)
summary(fit6,fit.measures=T,standardized=T)

semPaths(fit6,"model","stand",style="LISREL",rotation=1, layout="circle",
         edge.color="black",edge.label.cex=.5,mar=c(5,2,5,2)) # Graphic

# Put on the paper values for models 3,4,5,6


# Calculating indirect effect inside the best fitting model

full.sem6.1 <- 'RA =~ rrola1 + rrola2 + rrola3
ED =~ emo1 + emo2 + emo3
Engagement ~ (b)*Mindfulness + (c)*RA
Engagement ~ (g)*ED
Mindfulness ~ (a)*RA
Mindfulness ~ (d)*ED
Exhaustion ~ (e)*Mindfulness + (f)*RA
Exhaustion ~ (h)*ED
emo2 ~~ emo3
emo1 ~~ emo2
rrola1 ~~ rrola2

indirect := a * b
total := (a*b)+c

indirect1 := a * e
total1 := (a*e)+f

indirect2 := d * b
total2 := (d*b)+g

indirect3 := d * e
total3 := (d*e)+h'

fit6.1 <- sem(full.sem6.1,data=jhlimpio)
summary(fit6.1,fit.measures=TRUE,standardized=TRUE)

semPaths(fit6.1,"model","stand",style="LISREL",rotation=1, layout="circle",
         edge.color="black",edge.label.cex=.5,mar=c(5,2,5,2)) # Graphic
anova(fit5,fit6.1)
