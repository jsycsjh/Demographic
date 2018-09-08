setwd('C:/Users/sjhuv/Desktop/Misc/system engeering research/Project with Mehdi')

#Used packages
library(tidyr)
library(dplyr)
library(mice)
library(pan)
library(mitml)
library(nlme)
# install.packages('lavaan')
# library(lavaan)
library(mediation)
library(lme4)
library(ggplot2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

writeresults <- function(x,path){
  print(paste0("Current working dir: ",path))
  sink(file=path) 
  print(summary(lme(posExpBiasScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(negExpBiasScale ~ condition+session_int+condition*session_int, random = ~session_int|participantId, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(depressionScale ~ condition*session_int, random = ~1+session_int|participantId, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(anxietyScale ~ condition*session_int, random = ~1+session_int|participantId, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(selfEffScale ~ condition+session_int+condition:session_int, random = ~1+session_int|participantId, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(growthMindScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(optimismScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId, data=x, method="ML",na.action=na.omit)))
  sink(file = NULL)
}

preparedata<-function(x){
  x <- subset(x, scenarioIndex==40 | is.na(x$scenarioIndex))
  #Remove the following participants
  x<-x[which(!x$participantId %in% c(1307,138,200,392,412,453,495,496,577,582,627,634,788,942,961)), ]
  #Groupe Eligibility and Pretest into one session named baseline
  x$session <- gsub('Eligibility','Baseline',  x$session)
  x$session <- gsub('preTest','Baseline',  x$session)
  x$session <- factor(x$session, levels=c("Baseline", "firstSession","secondSession","thirdSession","fourthSession","PostFollowUp"))
  x$session_int <- as.integer(x$session)
  x<-aggregate(x=x[c("posExpBiasScale", "negExpBiasScale","depressionScale","anxietyScale","selfEffScale","growthMindScale","optimismScale")], by=list(participantId=x$participantId,session=x$session,condition=x$condition,session_int=x$session_int), mean, na.rm = TRUE)
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  x[is.nan(x)] <- NA
  return(x)
}
############################################################################################################
############################################################################################################
###Longitudunal analysis with 3 conditions


x <- read.csv("FTdataScored.csv")
x <- preparedata(x)
x$condition <- gsub('FIFTYFIFTYBLOCKED','FIFTYFIFTY',  x$condition)
x$condition <- gsub('FIFTYFIFTYRANDOM','FIFTYFIFTY',  x$condition)
x$condition <- gsub('POSITIVENEGATION','POSITIVE',  x$condition)

#Get a global view on our data
summary(x)
md.pattern(x)

#Multiple imputation  for multilevel data Using the pan package
fml <- posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale  ~ condition + session_int + condition*session_int + (1+session_int|participantId)
imp <- panImpute(x, formula=fml, n.burn=5000, n.iter=100, m=100,seed = 1234)
x <- mitmlComplete(imp, 1)
#This si to summurize and plot the results of the imputation procedure
summary(imp)
plot(imp, trace="all", print="psi",pos=c(1,2))

### make the x$condition a factor with desirable ordering factor levels
x$condition <- as.factor(x$condition)
levels(x$condition)<-c('FIFTYFIFTY','FIFTYFIFTY','NEUTRAL','POSITIVE','POSITIVE')
print(levels(x$condition))
### reorder to make the neutral be baseline
levels(x$condition)<-levels(x$condition)[c(2,1,3)]
### Now neutral has become baseline
writeresults(x,"Results/Longitudinal Outcome/2conditions_vs_Neutral.txt")


### make the x$condition a factor with desirable ordering factor levels
print(levels(x$condition))
### reorder to make the 50_50 be baseline
levels(x$condition)<-levels(x$condition)[c(2,1,3)]
writeresults(x,"Results/Longitudinal Outcome/2conditions_vs_50_50.txt")



############################################################################################################
############################################################################################################
###Longitudunal analysis with 5 conditions
x <- read.csv("FTdataScored.csv")
x <- preparedata(x)

#Multiple imputation  for multilevel data Using the pan package
fml <- posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale  ~ condition + session_int + condition*session_int + (1+session_int|participantId)
imp <- panImpute(x, formula=fml, n.burn=5000, n.iter=100, m=100,seed = 1234)
x <- mitmlComplete(imp, 1)


### Make 50_50_random as baseline

x$condition <- factor(x$condition,levels=c("FIFTY_FIFTY_RANDOM","POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_BLOCKED"))
writeresults(x,'results/Longitudinal Outcome/4conditions_vs_50_50Random.txt')

### Make Positive as baseline
x$condition <- factor(x$condition,levels=c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"))
writeresults(x,'results/Longitudinal Outcome/4conditions_vs_Positive.txt')



############################################################
################### Diagnostic #############################
############################################################


######### Model I: 3 conditions with NEUTRAL Baseline


x <- mitmlComplete(imp, 1)
### make the x$condition a factor with desirable ordering factor levels
x$condition <- as.factor(x$condition)
levels(x$condition)<-c('FIFTYFIFTY','FIFTYFIFTY','NEUTRAL','POSITIVE','POSITIVE')
print(levels(x$condition))
### reorder to make the neutral be baseline
levels(x$condition)<-levels(x$condition)[c(2,1,3)]
### Now neutral has become baseline

### Take the model with posExpBiasScale as an example
posExpBiasScale.model<-lme(posExpBiasScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId, data=x, method="ML",na.action=na.omit)
par(mfrow=c(1,2))
a <- plot(posExpBiasScale.model,main='Standardized Residuals vs Fitted Values')
b <- qqnorm(posExpBiasScale.model,main='QQ plot of Standardized Residuals')
print(a, position = c(0, 0, 0.5, 1), more = TRUE)
### This residual plot does not indicate any deviations from a linear form. 
### It also shows relatively constant variance across the fitted range. 
### The slight reduction in apparent variance on the right and left of the graph are likely a result of there being fewer observation in these predicted areas.
print(b, position = c(0.5, 0, 1, 1))
### This residual plot does not show any non-normality


### Models are assumed to be linear in each of the independent variables. 
### This assumption can be checked with plots of the residuals versus each of the variables.
attach(x)
c<-ggplot(data.frame(x1=condition,pearson=residuals(posExpBiasScale.model,type="pearson")),
       aes(x=x1,y=pearson)) +ggtitle("Plot of Residuals vs Condition") +
  xlab("Condition") + ylab("Residuals")+geom_point() +
  theme_bw()
d<-ggplot(data.frame(x2=session,pearson=residuals(posExpBiasScale.model,type="pearson")),
       aes(x=x2,y=pearson)) +ggtitle("Plot of Residuals vs Session") +
  xlab("Session") + ylab("Residuals")+
  geom_point() +
  theme_bw()
multiplot(c,d,cols=2)
### These plot do not raise any concern about a non linear relationship
detach(x)

### ICC is a measure of how much of the variation in the response variable, 
### which is not attributed to fixed effects, is accounted for by a random effect. 
### It is the ratio of the variance of the random effect to the total random variation. 
VarCorr(posExpBiasScale.model)
intercept.Var <- 0.6052
session_int.Var <- 0.053
residVar <- 0.4923
intercept.Var/(intercept.Var+session_int.Var+residVar)
(session_int.Var+intercept.Var)/(intercept.Var+session_int.Var+residVar)
