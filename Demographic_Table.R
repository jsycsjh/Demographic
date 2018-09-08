setwd('C:/Users/sjhuv/Desktop/Misc/system engeering research/Project with Mehdi')

writeresults <- function(x,path){
  print(paste0("Current working dir: ",path))
  sink(file=path) 
  print(summary(lme(posExpBiasScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(negExpBiasScale ~ condition+session_int+condition*session_int, random = ~session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(depressionScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(anxietyScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(selfEffScale ~ condition+session_int+condition:session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(growthMindScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(optimismScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  sink()
}
preparedata<-function(x){
  x <- subset(x, scenarioIndex==40 | is.na(x$scenarioIndex))
  #Remove the following participants
  x<-x[which(!x$participantId %in% c(1307,138,200,392,412,453,495,496,577,582,627,634,788,942,961)), ]
  #Group Eligibility and Pretest into one session named baseline
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

### Demographic Table
Demographic<-read.csv("Demographics_recovered_Mar_27_2018.csv")
### Note that the demographic table has double recordings for same participantRSA which are not exact replicates


### x is 4590*11 and demographic is 1212*18
### merge x and demographic based on id
### first only keep baseline of each subject in x
x.ordered<-x[order(x$session),]
### check whether there is participant with multiple baseline
participant.num<-length(unique(x.ordered[x.ordered$session=='Baseline',"participantId"]))
# 765 unique id in baseline
baseline.num<-length(x.ordered[x.ordered$session=='Baseline',"participantId"])
# 765 baseline recordings, consistent with participant number, there is no repeated recordings for one subject

### only keep x with 'baseline' session
x.baseline<-x.ordered[x.ordered$session=='Baseline',]

############################ new id in demographic
length(unique(Demographic$participantRSA))
# 1176 unique participant id in demographic data, we only keep those with baseline recordings


######################## NEED to change the id in demographic

x.Demographic<-merge(x.baseline,Demographic,by.x="participantId",by.y="participantRSA",all.x = FALSE,all.y = FALSE,sort = TRUE)
# merged dataframe has 781 observations, which means there are 781-765=16 double recordings

### only keep demographic variables that we are interested
x.Demographic<-x.Demographic[,c("participantId","condition","birthYear","country","education","employmentStat","income","maritalStat","race","genderId","ethnicity",'date','id')]

### check who is experiencing double recordings
dim(x.Demographic[duplicated(x.Demographic$participantId),])
print(x.Demographic[duplicated(x.Demographic$participantId),"participantId"])

duplicate.id<-x.Demographic[duplicated(x.Demographic$participantId),"participantId"]

### see what these participants information look like
print(x.Demographic[x.Demographic$participantId==137,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==223,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==239,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==261,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==352,],row.names = FALSE)

print(x.Demographic[x.Demographic$participantId==584,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==711,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==760,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==814,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==834,],row.names = FALSE)

print(x.Demographic[x.Demographic$participantId==862,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==883,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==947,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==950,],row.names = FALSE)
print(x.Demographic[x.Demographic$participantId==1130,],row.names = FALSE)


### keep only the latest response
x.Demographic<-x.Demographic[!(x.Demographic$id %in% c(112,191,216,228,306,521,640,691,737,756,759,
                                                       1017,804,867,870,1077)),]
### then it has 765 different participants without double recordings

### return each summary statistics in demographic table

### Note: there is a very odd subject in the study, id=779, his answer to most questions is "???"
### I will delete that participant for now. so there are 764 participants.
x.Demographic<-x.Demographic[x.Demographic$participantId != 779,]


### number of subjects in each condition
group.size<-summary(as.factor(x.Demographic$condition))
### CBM-PP: 109 CBM-N+PP: 133 50/50 Blocked: 278 50/50 Random: 134 Control Neutral: 111
pie(group.size,labels = as.factor(x.Demographic$condition),main = 'Number of Participants in each Condition')


x.Demographic$condition<-as.factor(x.Demographic$condition)
print(levels(x.Demographic$condition))
x.Demographic$condition=factor(x.Demographic$condition,levels(x.Demographic$condition)[c(4,5,1,2,3)])
print(levels(x.Demographic$condition))


### Age
### First check whether there is any odd number reported in the survey
nrow(x.Demographic[(x.Demographic$birthYear>2008)|(x.Demographic$birthYear<=1900),])
#1 people reported an odd birthyear
x.Demographic[(x.Demographic$birthYear>2008)|(x.Demographic$birthYear<=1900),"birthYear"]
#'91967' seems odd
#I will regard the second odd birthyear as 1967
x.Demographic[(x.Demographic$birthYear>2008)|(x.Demographic$birthYear<=1900),"birthYear"]<-1967
x.Demographic$age<-2017-x.Demographic$birthYear


### histogram of age
hist(x.Demographic$age,main="Histogram of Participants' Ages",xlab = 'Age')

### return the mean/std of age within each group
tapply(x.Demographic$age,x.Demographic$condition,mean)
tapply(x.Demographic$age,x.Demographic$condition,sd)
### CBM-PP: 40.93284 (13.00503) 
### CBM-N+PP: 42.00901 (13.96000)
### 50/50 Blocked: 40.79817 (12.94989) 
### 50/50 Random: 40.96241 (14.07147)
### Control Neutral: 41.05755 (13.20555)

boxplot(x.Demographic$age~x.Demographic$condition,main='Boxplot of Age',xlab='condition',ylab='age')

### annual income

### the order of income levels is inconsistent with the order in manuscript demographic table, reorder levels here for better look
#Note participant 779 has a response ????? $5000

print(levels(x.Demographic$income))
x.Demographic[x.Demographic$income=='??????, ??? $5000',]

x.Demographic$income=factor(x.Demographic$income,levels(x.Demographic$income)[c(12,9,2,4,6,8,10,11,1,3,5,7,13,15)])
print(levels(x.Demographic$income))
levels(x.Demographic$income)[1]<-"Don't know"
x.Demographic$income=factor(x.Demographic$income,levels(x.Demographic$income)[c(2:12,1,13)])
print(levels(x.Demographic$income))


# In the sequence of:
# Less than $5,000 # $5,000-$11,999 # $12,000-$15,999 # $16,000-$24,999 # $25,000-$34,999
# $35,000-$49,999 # $50,000-$74,999 # $75,000-$99,999 # $100,000-$149,999 # $150,000-$199,999
# $200,000-$249,999 # $250,000 or greater # Unknown # Prefer not to answer

# Income Distribution In General: number (percentage)
table(x.Demographic$income)
income.group.size<-summary(x.Demographic$income)
prop.table(table(x.Demographic$income))*100
pie(income.group.size,labels = x.Demographic$income)


# Income Distribution In separate group: number (percentage)
### Note that the percentage is calculated based on the whole population (instead of the subpopulation within each condition)

tapply(x.Demographic$income,x.Demographic$condition,table)
prop.table(table(x.Demographic$condition,x.Demographic$income))*100

# Income Distribution In separate group
### Note that the percentage is calculated based on each condition (instead of the whole population)
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE',"income"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE_NEGATION',"income"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_BLOCKED',"income"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_RANDOM',"income"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='NEUTRAL',"income"]))*100


### Gender
print(levels(x.Demographic$genderId))
x.Demographic$genderId<-factor(x.Demographic$genderId,levels(x.Demographic$genderId)[c(2,3,6,4,1,5)])
print(levels(x.Demographic$genderId))
levels(x.Demographic$genderId)[5]<-'Other'

### percentage with respect to the total population
tapply(x.Demographic$genderId,x.Demographic$condition,table)
prop.table(table(x.Demographic$condition,x.Demographic$genderId))*100

### percentage with respect to each condition
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE',"genderId"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE_NEGATION',"genderId"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_BLOCKED',"genderId"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_RANDOM',"genderId"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='NEUTRAL',"genderId"]))*100


### Race
print(levels(x.Demographic$race))
x.Demographic$race<-factor(x.Demographic$race,levels(x.Demographic$race)[c(2,3,4,5,8,9,6,7,1)])
levels(x.Demographic$race)[9]<-'Prefer not to answer'
print(levels(x.Demographic$race))

### percentage with respect to each condition
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE',"race"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE_NEGATION',"race"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_BLOCKED',"race"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_RANDOM',"race"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='NEUTRAL',"race"]))*100

### Ethnicity
print(levels(x.Demographic$ethnicity))
x.Demographic$ethnicity<-factor(x.Demographic$ethnicity,levels(x.Demographic$ethnicity)[c(2,3,5,4)])
print(levels(x.Demographic$ethnicity))
### percentage with respect to each condition
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE',"ethnicity"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE_NEGATION',"ethnicity"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_BLOCKED',"ethnicity"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_RANDOM',"ethnicity"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='NEUTRAL',"ethnicity"]))*100

### Country
length(unique(x.Demographic$country))-1
#participants from 35 countries (subtract 'NoAnswer')
#Note that participant 710 is from Russian Republic, and there are 4 others from Russia

### find the top 5 countries
nationality<-rbind(as.character(levels(x.Demographic$country)),table(x.Demographic$country))
nationality<-as.data.frame(t(nationality))
colnames(nationality)<-c('country','number')
#type of nationality$number is factor,transform to numeric
nationality$number<-as.numeric(levels(nationality$number))[nationality$number]

rownames(nationality)<-NULL
nationality<-nationality[order(nationality$number),]
nationality

### top 5 is United States (647), United Kingdom (23), Canada (16), Australia (13), and New Zealand (5)
### US accounts for 84.7%, top 5 accounts for 92.1%
### NoAnswer also has 5 participants

sum(nationality$number[-c(43:47)])
### Others have 60 people
# reorganize the country
levels(x.Demographic$country)<-c(rep('Other',3),'Australia','Other','Other','Canada',rep('Other',24),
                                 'New Zealand','No Answer',rep('Other',12),
                                 'United Kingdom','United States')
print(levels(x.Demographic$country))
x.Demographic$country<-factor(x.Demographic$country,levels(x.Demographic$country)[c(7,6,3,2,4,1,5)])

country.size<-summary(x.Demographic$country)

pie(country.size,labels=levels(x.Demographic$country))

prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE',"country"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE_NEGATION',"country"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_BLOCKED',"country"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_RANDOM',"country"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='NEUTRAL',"country"]))*100


### Education
print(levels(x.Demographic$education))
### change the levels of some odd degree
levels(x.Demographic$education)<-c('Unknown','Some college','College graduate','No high school',
                                   'High school graduate','Advanced degree','No high school','Advanced degree',
                                   'Advanced degree','Advanced degree','Advanced degree','Advanced degree','Prefer not to answer',
                                   'Some college','Some graduate school','Some high school')

print(levels(x.Demographic$education))
x.Demographic$education<-factor(x.Demographic$education,
                                levels(x.Demographic$education)[c(4,9,5,2,3,8,6,1,7)])
print(levels(x.Demographic$education))

pie(summary(x.Demographic$education),labels = levels(x.Demographic$education))


prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE',"education"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE_NEGATION',"education"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_BLOCKED',"education"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_RANDOM',"education"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='NEUTRAL',"education"]))*100

### Employment
print(levels(x.Demographic$employmentStat))
levels(x.Demographic$employmentStat)<-c('Unknown','Homemaker','Unemployed','Other','Prefer not to answer',
                                        'Retired','Student','Unemployed','Working full time','Working part time')
x.Demographic$employmentStat<-factor(x.Demographic$employmentStat,
                                levels(x.Demographic$employmentStat)[c(7,2,3,9,8,6,4,1,5)])

print(levels(x.Demographic$employmentStat))

prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE',"employmentStat"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE_NEGATION',"employmentStat"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_BLOCKED',"employmentStat"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_RANDOM',"employmentStat"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='NEUTRAL',"employmentStat"]))*100

pie(summary(x.Demographic$employmentStat),labels = levels(x.Demographic$employmentStat))


### MaritalStatus
print(levels(x.Demographic$maritalStat))
levels(x.Demographic$maritalStat)<-c('Unknown','Divorced','In a domestic/civil union','Married',
                                        'Other','Prefer not to answer','Separated','Single','Dating',
                                        'Engaged','in a marriage-like relationship','widow/widower')
print(levels(x.Demographic$maritalStat))


x.Demographic$maritalStat<-factor(x.Demographic$maritalStat,
                                     levels(x.Demographic$maritalStat)[c(8,9,10,11,4,3,7,2,12,5,1,6)])

prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE',"maritalStat"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='POSITIVE_NEGATION',"maritalStat"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_BLOCKED',"maritalStat"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='FIFTY_FIFTY_RANDOM',"maritalStat"]))*100
prop.table(table(x.Demographic[x.Demographic$condition=='NEUTRAL',"maritalStat"]))*100

pie(summary(x.Demographic$maritalStat),labels = levels(x.Demographic$maritalStat))

### Test the significance of all variables in demographic table

# Age
### we conduct an one-way ANOVA F test
age.test<-aov(age~condition,data=x.Demographic)
anova(age.test)
### F-statistic=0.1467,df=4/759,P=0.9645, no significant difference

# Gender
### we conduct a Fisher exact test instead of Chi square test since there are cells having very small sample sizes
levels(x.Demographic$genderId)<-c('Female','Male','Female','Other','Prefer not to answer')
gender.table<-table(x.Demographic$genderId,x.Demographic$condition)
### based on our relatively large sample size, fisher.test may not be very appropriate 
### since R will intrinsicly use some approximation to calculate the p value
### every time it will return a slightly different p-value, most of the time, p-value is larger than 0.05
fisher.test(gender.table,simulate.p.value=TRUE)
### p value 0.8121


### better to use chi-square test for larger data set. but the rule of thumb to use chi-square test is each cell should have at least 5 samples
### which in our case, data are sparse, thus, the chi-square test is not reliable, in fact, when several levels only have 1-2 samples
### the test will tend to be significant
chisq.test(gender.table)
### chi-square statistics: 9.4569, df=12, p-value=0.6635


### we could also conduct a chi-square test for the subset data
gender.subset<-x.Demographic[(x.Demographic$genderId=='Female')|(x.Demographic$genderId=='Male'),]
#change the factor levels
levels(gender.subset$genderId)<-c('Female','Male','Female','Female','Female','Female')
gender.table.subset<-table(gender.subset$genderId,gender.subset$condition)
gender.chi=chisq.test(gender.table.subset)
gender.chi
### chi-square statistics: 5.0967, df=4, p-value=0.2775
fisher.test(gender.table.subset,simulate.p.value=TRUE)
### fisher-exact test p value: 0.5542


# Race
race.table<-table(x.Demographic$race,x.Demographic$condition)
fisher.test(race.table,simulate.p.value=TRUE)
### fisher test p value: 0.7226
chisq.test(race.table)
### chi-square statistics: 23.959, df=28, p-value=0.6837

# Ethnicity
ethnicity.table<-table(x.Demographic$ethnicity,x.Demographic$condition)
fisher.test(ethnicity.table,simulate.p.value=TRUE)
### fisher test p value: 0.9775
chisq.test(ethnicity.table)
### chi-square statistics: 4.6706, df=12, p-value=0.9681

# Country

country.table<-table(x.Demographic$country,x.Demographic$condition)
fisher.test(country.table,simulate.p.value=TRUE)
### fisher test p value: 0.3078
chisq.test(country.table)
### chi-square statistics: 23.53, df=24, p-value=0.4887

# Employment
employment.table<-table(x.Demographic$employmentStat,x.Demographic$condition)
#8-th row is a null row
chisq.test(employment.table[-8,])
### chi-square statistics: 26.318, df=28, p-value=0.5556
fisher.test(employment.table[-8,],simulate.p.value = TRUE)
### 0.5242

# Marital Status
marital.table<-table(x.Demographic$maritalStat,x.Demographic$condition)
#11-th row is a null row
chisq.test(marital.table[-11,])
### chi-square statistics: 34.547, df=40, p-value=0.7137
fisher.test(marital.table[-11,],simulate.p.value = TRUE)

# Education
# Kruskal-Wallis should be used since Mann-Whitney U Test only compares two groups
levels(x.Demographic$education)
# no sense to order the last two levels, delete them
education.subset<-x.Demographic[(x.Demographic$education != "Don't know") & (x.Demographic$education != "Prefer not to answer"),]
kruskal.test(education.subset$education~education.subset$condition)
### Kruskal-Wallis chi-squared = 1.9479, df = 4, p-value = 0.7453

# Income
levels(x.Demographic$income)
income.subset<-x.Demographic[(x.Demographic$income != "Don't know") & (x.Demographic$income != "Prefer not to answer"),]
kruskal.test(x.Demographic$income~x.Demographic$condition)
### Kruskal-Wallis chi-squared = 2.6974, df = 4, p-value = 0.6097