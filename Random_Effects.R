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

length(unique(Demographic$id))
# 1212 unique participant id in demographic data, we only keep those with baseline recordings
x.Demographic<-merge(x.baseline,Demographic,by.x="participantId",by.y="id",all.x = FALSE,all.y = FALSE,sort = TRUE)
# merged dataframe has 751 observations, which means 14 subjects in x does not have demographic information

### only keep demographic variables that we are interested
x.Demographic<-x.Demographic[,c("participantId","condition","birthYear","country","education","employmentStat","income","maritalStat","race","genderId","ethnicity")]

### return each summary statistics in demographic table

### Note: there is a very odd subject in the study, id=710, his answer to most questions is "???"
### I will delete that participant for now. so there are 750 participants.
x.Demographic<-x.Demographic[x.Demographic$participantId != 710,]


### number of subjects in each condition
summary(as.factor(x.Demographic$condition))
### CBM-PP: 129 CBM-N+PP: 107 50/50 Blocked: 109 50/50 Random: 130 Control Neutral: 275

x.Demographic$condition<-as.factor(x.Demographic$condition)
print(levels(x.Demographic$condition))
x.Demographic$condition=factor(x.Demographic$condition,levels(x.Demographic$condition)[c(4,5,1,2,3)])
print(levels(x.Demographic$condition))


### Age
### First check whether there is any odd number reported in the survey
nrow(x.Demographic[(x.Demographic$birthYear>2008)|(x.Demographic$birthYear<1900),])
#3 people reported an odd birthyear
x.Demographic[(x.Demographic$birthYear>2008)|(x.Demographic$birthYear<1900),"birthYear"]
#the three birthyears are '6031966' '10167' '14041982'
#I will regard the second odd birthyear as 1967
x.Demographic[(x.Demographic$birthYear>2008)|(x.Demographic$birthYear<1900),"birthYear"]<-c(1966,1967,1982)
x.Demographic$age<-2018-x.Demographic$birthYear

#I noted that 2 subjects (subject 472 and subject 1202) have age 118, they claimed their birth year as 1900
#may need to check whether they randomly write a number.
x.Demographic[x.Demographic$age==max(x.Demographic$age),"participantId"]

### return the mean/std of age within each group
tapply(x.Demographic$age,x.Demographic$condition,mean)
tapply(x.Demographic$age,x.Demographic$condition,sd)
### CBM-PP: 44.20155 (14.52120) 
### CBM-N+PP: 40.71963 (14.06776)
### 50/50 Blocked: 40.93578 (13.14187) 
### 50/50 Random: 42.35385 (13.58000)
### Control Neutral: 43.18909 (14.16459)

### annual income

### the order of income levels is inconsistent with the order in manuscript demographic table, reorder levels here for better look
#Note participant 710 has a response ????? $5000
print(levels(x.Demographic$income))
x.Demographic$income=factor(x.Demographic$income,levels(x.Demographic$income)[c(12,9,2,4,6,8,10,11,1,3,5,7,13,15)])
print(levels(x.Demographic$income))
# In the sequence of:
# Less than $5,000 # $5,000-$11,999 # $12,000-$15,999 # $16,000-$24,999 # $25,000-$34,999
# $35,000-$49,999 # $50,000-$74,999 # $75,000-$99,999 # $100,000-$149,999 # $150,000-$199,999
# $200,000-$249,999 # $250,000 or greater # Unknown # Prefer not to answer

# Income Distribution In General: number (percentage)
table(x.Demographic$income)
prop.table(table(x.Demographic$income))*100


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
#participants from 33 countries (subtract 'NoAnswer')
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

### top 5 is United States (635), United Kingdom (20), Canada (15), Australia (14), and Germany (7)
### US accounts for 84.7%, top 5 accounts for 91.2%
### NoAnswer also has 7 participants

sum(nationality$number[-c(43:47)])
### Others have 59 people

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


### Test the significance of all variables in demographic table

# Age
### we conduct an one-way ANOVA F test
age.test<-aov(age~condition,data=x.Demographic)
anova(age.test)
### F-statistic=1.424,df=4,P=0.2242, no significant difference

# Gender
### we conduct a Fisher exact test instead of Chi square test since there are cells having very small sample sizes
levels(x.Demographic$genderId)<-c('Female','Male','Transgender','Other','Female','Prefer not to answer')
gender.table<-table(x.Demographic$genderId,x.Demographic$condition)
### based on our relatively large sample size, fisher.test may not be very appropriate 
### since R will intrinsicly use some approximation to calculate the p value
### every time it will return a slightly different p-value, most of the time, p-value is larger than 0.05
fisher.test(gender.table,simulate.p.value=TRUE)
### better to use chi-square test for larger data set. but the rule of thumb to use chi-square test is each cell should have at least 5 samples
### which in our case, data are sparse, thus, the chi-square test is not reliable, in fact, when several levels only have 1-2 samples
### the test will tend to be significant
chisq.test(gender.table)

### we could also conduct a chi-square test for the subset data
gender.subset<-x.Demographic[(x.Demographic$genderId=='Female')|(x.Demographic$genderId=='Male'),]
#change the factor levels
levels(gender.subset$genderId)<-c('Female','Male','Female','Female','Female','Female')
gender.table.subset<-table(gender.subset$genderId,gender.subset$condition)
gender.chi=chisq.test(gender.table.subset)
gender.chi
### chi-square statistics: 5.0967, df=4, p-value=0.2775


# Race
race.table<-table(x.Demographic$race,x.Demographic$condition)
chisq.test(gender.table)
### chi-square statistics: 27.211, df=16, p-value=0.03919

# Ethnicity
ethnicity.table<-table(x.Demographic$ethnicity,x.Demographic$condition)
chisq.test(ethnicity.table)
### chi-square statistics: 11.327, df=12, p-value=0.5011

# Country
# reorganize the country
levels(x.Demographic$country)<-c('Other','Other','Other','Australia','Other','Other','Canada','Other',
                                 'Other','Other','Other','Other','Other','Other','Other','Germany',
                                 'Other','Other','Other','Other','Other','Other','Other','Other',
                                 'Other','Other','Other','Other','Other','Other','Other','Other','No Answer',
                                 'Other','Other','Other','Other','Other','Other','Other','Other','Other','Other','Other','Other',
                                 'United Kingdom','United States')

country.table<-table(x.Demographic$country,x.Demographic$condition)
chisq.test(country.table)
### chi-square statistics: 23.421, df=24, p-value=0.4951

# Employment
employment.table<-table(x.Demographic$employmentStat,x.Demographic$condition)
#8-th row is a null row
chisq.test(employment.table[-8,])
### chi-square statistics: 22.159, df=28, p-value=0.7739

# Marital Status
marital.table<-table(x.Demographic$maritalStat,x.Demographic$condition)
#11-th row is a null row
chisq.test(marital.table[-11,])
### chi-square statistics: 29.671, df=40, p-value=0.8842

# Education
# Kruskal-Wallis should be used since Mann-Whitney U Test only compares two groups
levels(x.Demographic$education)
# no sense to order the last two levels, delete them
education.subset<-x.Demographic[(x.Demographic$education != "Don't know") & (x.Demographic$education != "Prefer not to answer"),]
kruskal.test(education.subset$education~education.subset$condition)
### Kruskal-Wallis chi-squared = 3.909, df = 4, p-value = 0.4185

# Income
levels(x.Demographic$income)
income.subset<-x.Demographic[(x.Demographic$income != "Don't know") & (x.Demographic$income != "Prefer not to answer"),]
kruskal.test(x.Demographic$income~x.Demographic$condition)
### Kruskal-Wallis chi-squared = 0.8446, df = 4, p-value = 0.9324