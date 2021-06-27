##################################################
### PROG8430                                    ##
### Assignment 1            ## 
##################################################
#                                               ##
##################################################
# Emerson Landin
# Student ID: 8710791
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory to an appropriate location
setwd ("C:/CONESTOGA/Data Analysis Math/Assignment 1 - ExploratoryDescriptive")
getwd()

options(scipen=9)

##################################################
### Load  R DATA                        ##
##################################################

load("PROG8430_Assign_Explore.Rdata")
Dataset_EL <- PROG8430_Assign_Explore
str(Dataset_EL)
head(Dataset_EL,20)

############################################################################################################
### 1. a. Create a table to show the total income by each category of political affiliation                 ##
############################################################################################################

Table_income_EL <- aggregate(Dataset_EL$income, by=list(Dataset_EL$political), FUN=sum, na.rm=TRUE)
colnames(Table_income_EL)[1] <- "Political" 
colnames(Table_income_EL)[2] <- "Income" 
Table_income_EL


############################################################################################################
### 1. b. Which status has the highest total income?               ##
############################################################################################################

Hi_Total_EL <- aggregate(Dataset_EL$income, by=list(Dataset_EL$m.status),FUN=sum)
colnames(Hi_Total_EL)[1] <- "Status" 
colnames(Hi_Total_EL)[2] <- "Income" 
str(Hi_Total_EL)
Hi_Total_EL


############################################################################################################
### 2. a. Calculate the mean age of respondents born in Asia.                ##
############################################################################################################

Age_mean_EL <- aggregate(Dataset_EL$age, by=list(Dataset_EL$nation),FUN=mean)
colnames(Age_mean_EL)[1] <- "Nation" 
colnames(Age_mean_EL)[2] <- "Age" 
Age_mean_EL

############################################################################################################
### 2. b. Calculate the mean age of respondents born in Asia weighted by the number of children they have.##
############################################################################################################

weighted.mean(Dataset_EL$age, w=Dataset_EL$n.child)


############################################################################################################
### 3. a. Create a table to show the mean score for males compared to females.##
############################################################################################################


Table_MF_EL <- tapply (Dataset_EL$score,Dataset_EL$gender, mean)
Table_MF_EL


############################################################################################################
### 3. b. Which has a higher score?##
############################################################################################################

max(Table_MF_EL)


############################################################################################################
### 4.Calculate the 31st and 61st percentiles of percentage of time taken on the test.##
############################################################################################################

quantile(Dataset_EL$time1, c(.31,.61))


############################################################################################################
### 1a. Pie Chart- Create a pie chart showing the number of respondents by marital status.##
############################################################################################################

str(Dataset_EL)

Status_MR_EL <- aggregate(Dataset_EL$id, by=list(Dataset_EL$m.status), FUN=sum, na.rm=TRUE)
colnames(Status_MR_EL)[1] <- "M.Status" 
colnames(Status_MR_EL)[2] <- "Qty"
Status_MR_EL
M_Status_EL <- Status_MR_EL$Qty
names(M_Status_EL) <- Status_MR_EL$M.Status 
pie(M_Status_EL, main="Total Respondents by Marital Status")

############################################################################################################
### 1b. Pie Chart- Which marital status contains the most respondents.##
############################################################################################################
Status_MR_EL
max(Status_MR_EL$Qty)

############################################################################################################
### 1c. Pie Chart- Which marital status has the fewest respondents? ##
############################################################################################################
Status_MR_EL
min(Status_MR_EL$Qty)


############################################################################################################
### 2a. Summary table - Create a table that shows the percentage of respondents from each Region that graduated high school. ##
############################################################################################################


Tab_GR_EL <- table(Dataset_EL$hs.grad, Dataset_EL$nation)
margin.table(Tab_GR_EL,2)
rownames(Tab_GR_EL)[1] <- "No Graduated" 
rownames(Tab_GR_EL)[2] <- "Graduated"
prop.table(Tab_GR_EL,1)
Tab_GR_EL

############################################################################################################
### 2b. Summary table - Which region has the highest percentage of high school graduates? ##
############################################################################################################

# MAx on row 2 - Graduate / North America

max(prop.table(Tab_GR_EL[2,]))
prop.table(Tab_GR_EL,1)

############################################################################################################
### 2c. Summary table- Which region has the lowest percentage of high school graduates? ##
############################################################################################################

# MIN on row 2 - Graduate / Southern

min(prop.table(Tab_GR_EL[2,]))
prop.table(Tab_GR_EL,1)


############################################################################################################
### 3a. Create a bar chart showing the mean Standardized Test Score on the Political Awareness Test for each Region. ##
############################################################################################################

#Calculate the summaries
Scor_Re_EL <- tapply(Dataset_EL$scr, Dataset_EL$nation, mean)

#Create the bar plot
barplot(Scor_Re_EL, 
        main="Test for each Region", 
        ylab="Standardized Score Test")

############################################################################################################
### 3b. Which Region has the lowest mean score? ##
############################################################################################################

Scor_Re_EL
min(Scor_Re_EL)


############################################################################################################
### 3c. Which Region has the highest mean score? ##
############################################################################################################

Scor_Re_EL
max(Scor_Re_EL)

############################################################################################################
### 4a. Create a histogram with 5 bins showing the distribution of the percentage of household income going to housing. ##
############################################################################################################

#Histogram of Percent Support 

hist(Dataset_EL$housing, main="Pct of Income to Housing")

#Forcing Breaks

hist(Dataset_EL$housing, breaks=5, main="Pct of Income to Housing")

#Style type = (count, density, percent)

hist(Dataset_EL$housing, breaks=5, prob=TRUE,
     main="Pct of Income to Housing")

############################################################################################################
### 4.c. Which range of values has the highest frequency? ##
############################################################################################################


#Histogram Frequency
hist(Dataset_EL$housing, breaks=5, main="Pct of Income to Housing")


############################################################################################################
### 5.a. Create a sequence of box plots showing the distribution of income separated by marital status. ##
############################################################################################################

boxplot(income ~ m.status, data=Dataset_EL, 
        main="Distribution of income by Marital Status",
        xlab="Married",  pch = 20)


############################################################################################################
### 5.b. According to the charts, which martial status has the highest average income? ##
############################################################################################################

# to confirm the high value
Tab_BP_EL <- table(Dataset_EL$income, Dataset_EL$m.status)
margin.table(Tab_BP_EL,2)

Tab_BP_EL2 <- tapply (Dataset_EL$income, Dataset_EL$m.status, max)
Tab_BP_EL2 



############################################################################################################
### 5.c. Which marital status has the lowest average income? ##
############################################################################################################

# to confirm the high value
Tab_BP_EL <- table(Dataset_EL$income, Dataset_EL$m.status)
margin.table(Tab_BP_EL,2)

############################################################################################################
### 5.d. Which marital status has the greatest variability in income? ##
############################################################################################################

# verify the range min and max
Tab_GV_EL <- tapply (Dataset_EL$income, Dataset_EL$m.status, range)
Tab_GV_EL 

# just test
Status_MAX_EL <- aggregate(Dataset_EL$income, by=list(Dataset_EL$m.status), FUN=max, na.rm=TRUE)
colnames(Status_MAX_EL)[1] <- "M.Status" 
colnames(Status_MAX_EL)[2] <- "Income"
Status_MAX_EL 

Status_MIN_EL <- aggregate(Dataset_EL$income, by=list(Dataset_EL$m.status), FUN=min, na.rm=TRUE)
colnames(Status_MIN_EL)[1] <- "M.Status" 
colnames(Status_MIN_EL)[2] <- "Income"
Status_MIN_EL

range(Tab_BP_EL)

############################################################################################################
### 6.a. Create a histogram for income. ##
############################################################################################################
#Histogram of Percent Support 

hist(Dataset_EL$income, main="Income")

#Forcing Breaks

hist(Dataset_EL$income, breaks=10, main="Income")

#Style type = (count, density, percent)

hist(Dataset_EL$income, breaks=12, prob=TRUE,
     main="Income")
############################################################################################################
### 6.b. Create a histogram for standardized score. ##
############################################################################################################
#Histogram of Percent Support 

hist(Dataset_EL$scr, main="Standardized Score")

#Forcing Breaks

hist(Dataset_EL$scr, breaks=10, main="Standardized Score")

#Style type = (count, density, percent)

hist(Dataset_EL$scr, breaks=12, prob=TRUE,
     main="Standardized Score")
############################################################################################################
### 6.c. Create a scatter plot showing the relationship between the income and standardized score. ##
############################################################################################################

# Basic chart
plot(income  ~ scr, data=Dataset_EL, main="Income and Standardized Score")

#You can change some formating

plot(income  ~ scr, data=Dataset_EL, col=2, pch=20,
     main="Income and Standardized Score")

############################################################################################################
### 6.d. What conclusions, if any, can you draw from the chart? . ##
############################################################################################################
#word doc
############################################################################################################
### 6.e. Calculate a correlation coefficient between these two variables. What conclusion you draw from it?  ##
############################################################################################################
# Correlation Coefficient

cor.test(Dataset_EL$income, Dataset_EL$scr)


############################################################################################################
### 1.a. Inference - Create a QQ Normal plot of the Political Awareness Test Score.   ##
############################################################################################################

qqnorm(Dataset_EL$score, main="Score on Political Awareness Test")
qqline(Dataset_EL$score)


############################################################################################################
### 1.b. Conduct a statistical test for normality on the Political Awareness Test Score.  ##
############################################################################################################

shapiro.test(Dataset_EL$score)

############################################################################################################
### 1.c. Inference - Are the Political Awareness Test Scores normally distributed?   ##
############################################################################################################

# yes #word doc

##################################################################################################################################################
### 2.a. Inference - Compare Political Awareness Test Scores between the treatment and control group using a suitable hypothesis test. .  ##
##################################################################################################################################################

# p-value > 0.05, therefore no significant difference in variances.

# T-Test 

res_EL <- t.test(score ~ , group = Dataset_EL, var.equal = TRUE)
res_EL

plot(score  ~ group, data=Dataset_EL, main="Income and Standardized Score")


##################################################################################################################################################
### 2.b. Explain why you chose the test you did.   ##
##################################################################################################################################################

#word doc

############################################################################################################
### 2.c. Do you have strong evidence that the average votes are different?   ##
############################################################################################################
#word doc

############################################################################################################
### 3.a. Determine if the Score on the Political Awareness Test varies by nation using ANOVA (statistical) and a sequence of boxplots (graphical).  ##
############################################################################################################


#One-Way ANOVA

summary(aov(score ~ nation, data=Dataset_EL))

ANOVA_EL <- aov(score~nation, data=Dataset_EL)
summary(ANOVA_EL)

TukeyHSD(ANOVA_EL)

boxplot(score ~ nation, data=Dataset_EL,
        main="Score on the Political Awareness Test ",
        range=0)


############################################################################################################
### 3.b. Determine if the Measure of Political Involvement (Pol) varies by Political Affiliation using ANOVA and a sequence of boxplots.  ##
############################################################################################################

summary(aov(Pol~political, data=Dataset_EL))

ANOVAB_EL <- aov(Pol~political, data=Dataset_EL)
summary(ANOVAB_EL)

TukeyHSD(ANOVAB_EL)

boxplot(Pol~political, data=Dataset_EL,
        main="Political Affiliation  ",
        range=0)


