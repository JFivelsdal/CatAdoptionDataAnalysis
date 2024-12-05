
#### ShortMedHairCatNew.R" #############################################################################

#Program: Analyze the shelter characteristics of domestic shorthair and domestic mediumhair cats #####


######################################################################################################


#Animal Shelter Data from Open Data Louisville Kentucky


#Domestic Shorthair cat and Domestic Mediumhair cat



#The commented line below is to install the countreg package
#install.packages("countreg", repos="http://R-Forge.R-project.org") 



###### Redo in terms of other code using Dplyr ####

setwd("C:/Users/jonat/OneDrive/Desktop")

library(tidyverse) #includes ggplot2 and dplyr among other packages

library(lubridate) #package for date formatting 

library(MASS) #includes the negative binomial regression function glm.nb

library(car) #includes a function to test for outliers

library(vcd) #Categorical Data Analysis package

library(vcdExtra) #Another Categorical Data Analysis package

#ibrary(countreg) #contains rootgram and other diagnostic tools
 
library(stargazer) #to format tables and regression output

library(RColorBrewer) #Create custom palette (this will be used for the interaction plots)

library(broom) #includes the augment function

#library(extracat) #Provides more visualization techniques for categorical data


#library(xtable) #create HTML tables 


animalShelterDat <- read.csv("DSHandDMHCatData.csv",stringsAsFactors = FALSE)

#Convert it into a tibble (tibble is like a dataframe)

animalShelterTable <- as.tibble(animalShelterDat)

#convert Intake Date,Date of Birth (DOB) and Outcome Date into Date objects

animalShelterTable <- mutate(animalShelterTable,IntakeDate = mdy_hm(IntakeDate),
                             DOB = mdy_hm(DOB),OutcomeDate = mdy_hm(OutcomeDate),CatType=PrimaryBreed)
  

#select just the domestic shorthair cats and domestic medium hair cats that entered the shelter in 2017


catData <- filter(animalShelterTable, AnimalType == "CAT" & 
                     (CatType == "DOMESTIC SHORTHAIR") | (CatType == "DOMESTIC MEDIUMHAIR"))  %>%
          filter(IntakeDate > "2017-01-01 00:00:00" & IntakeDate < "2018-01-01 00:00:00")

           
rm(animalShelterDat,animalShelterTable) #remove the shelter data frame and table created earlier

#Create a Binary factor Adopt which has a value of 1 for "Yes" if a cat was adopted and 0 for "No" otherwise



catData<- mutate(catData,Adopt = ifelse(catData$OutcomeType == "ADOPTION","Yes","No"))


#The next line of code after this comment section creates the LOS and IntakeAge variables
#LOS is the number of days (length of stay) that a cat stays in the shelter (measured in days)
#IntakeAge is the age of the cat when the cat arrived at the shelter (measured in years and rounded to 2 decimal places)

catData <- catData %>% mutate(LOS = as.numeric(round(difftime(OutcomeDate,IntakeDate,units="days"),0)),
                             IntakeAge = as.numeric(round(difftime(IntakeDate,DOB,units="days")/365.25,
                                                          2)))
                              

#Provide kitten  are < 1 years,  1 <= Young Adult < 7, Age > 7 Older Adult

catData <- catData %>% mutate(Age = ifelse(IntakeAge < 1,"Kitten",ifelse(IntakeAge >=1 & IntakeAge < 7,
                                          "Young Adult","Older Adult")))
            


#Only include cats with a valid intake age and length of stay value

catData <- catData %>% filter(!is.na(IntakeAge) & !is.na(LOS)) %>%
 
                       filter((IntakeAge <= 25 & IntakeAge >= 0)) %>%
  
                       filter(LOS >= 0) %>%
  
                       filter(Gender != "UNKNOWN" & !is.na(Gender))



#The code line below was to use with truncated negative binomial regression but this did not work so don't use
#catData <- catData %>% mutate(LOS = ifelse(LOS < 1,1,LOS))

#genderTable is a lookup table to create a seperate variable that indicates spay or neuter status

genderTable <-  catData %>% dplyr::select(Gender)


#Create a separate variable for the Spay or Neuter status of each cat (variable called SorN)

#This loops through the original gender column and searches for the text neutered or spayed in each entry
# if an entry in the gender column includes the word "neutered" or "spayed", the SorN variable receives a 
# value of 1, otherwise the SorN variable receives a value of 0.

catData <- catData %>% mutate(SorN = ifelse(sapply(1:nrow(genderTable), function(i){
    str_detect(genderTable[i,],"NEUTERED") | str_detect(genderTable[i,],"SPAYED")}),1,0))



#Creates a new gender factor that just contains Male or Female
# Returns a 0 for female and 1 for male.
catData <- catData %>% mutate(GenderFactor = ifelse(sapply(1:nrow(genderTable), function(i){
  str_detect(genderTable[i,],"FEMALE")}),0,1))


#genderTable is a lookup table to create a seperate variable that indicates spay or neuter status

colorTable <-  catData %>% dplyr::select(PrimaryColor)


#Create a separate variable for the Spay or Neuter status of each cat (variable called SorN)

#This loops through the original gender column and searches for the text neutered or spayed in each entry
# if an entry in the gender column includes the word "neutered" or "spayed", the SorN variable receives a 
# value of 1, otherwise the SorN variable receives a value of 0.

catData <- catData %>% mutate(Black = ifelse(sapply(1:nrow(colorTable), function(i){
  str_detect(colorTable[i,],"BLACK")}),1,0))




# Returns a 1 for female and 0 for male.
#catData <- catData %>% mutate(StrayStatus = ifelse(IntakeType == "STRAY",1,0))



catData <- catData %>% mutate(Stray = ifelse(IntakeType == "STRAY" | IntakeSubtype == "STRAY","Yes","No"))



#Creates the week variable which indicates whether or not a cat stays for more than 1 week

#Did the cat stay more than 2 weeks

catData <- catData %>% mutate(Week = ifelse(LOS > 14,0,1)) 

#week is defined that if a cat stays more than 1 weeks assign 0 to it, if a cat stays two weeks or less, assign 1 to it.

#then assign it to 1.


# Subset of the data that is created for Domestic Shorthair cats
subsetDSH <- catData %>% filter(CatType == "DOMESTIC SHORTHAIR")


#Subset of the data that is created for Domestic Mediumhair cats
subsetDMH <- catData %>% filter(CatType == "DOMESTIC MEDIUMHAIR")


###

#The negative value is that zero counts are included in the lowest interval < 1 week (0 - 6 days) interval

#Intervals < 1 week, 1 week <= x < 2 week, 2 week <= x < 3 week, x >  3 weeks 
#weekLOS =cut(catData$LOS, breaks=c(-0.01,seq(6, 26,by=7),Inf))

#numWeekCases=table(weekLOS)


####

#Data Analysis




#adoptDSH: Total number of domestic shorthair cats that were adopted in 2017

adoptDSH <- sum(catData$CatType == "DOMESTIC SHORTHAIR" & catData$Adopt == "Yes") #919 DSH cats adopted

#totalDSH: Total number of domestic shorthair cats that were admitted in 2017

totalDSH <-  sum(catData$CatType == "DOMESTIC SHORTHAIR") #2945 DSH cats admitted 

#adoptDMH: Total number of domestic medium hair cats that were adopted in 2017

adoptDMH <- sum(catData$CatType == "DOMESTIC MEDIUMHAIR" & catData$Adopt == "Yes") #117 DMH cats adopted


#totalDMH: Total number of domestic shorthair cats that were admitted in 2017

totalDMH <- sum(catData$CatType == "DOMESTIC MEDIUMHAIR") #330 DMH cats admitted


#Difference in Proportions Test
prop.test(c(adoptDSH,adoptDMH),c(totalDSH,totalDMH),correct=FALSE) #p-value 0.1155 (not sig. at 95% confidence level)

#############################


#Produced a graph of Length of Stay 
# catData %>% ggplot(aes(LOS)) + geom_boxplot()
  

# Create stray/adopt contingency tables to create a fourfold plot 
# showing relationships for the adoption of stray DSH cats and non stray DSH cats 

StrayAdoptDSH <- ftable(subsetDSH[c("Adopt","Stray")])

StrayAdoptDSH <- as.table(StrayAdoptDSH)

fourfold(StrayAdoptDSH) #Fourfold plot for adopt vs. stray for DSH cats



# Create stray/adopt contingency tables to create a fourfold plot 
# showing relationships for the adoption of stray DMH cats and non stray DMH cats 

StrayAdoptDMH <- ftable(subsetDMH[c("Adopt","Stray")])

StrayAdoptDMH <- as.table(StrayAdoptDMH)

fourfold(StrayAdoptDMH) #Fourfold plot for adopt vs. stray for DMH cats




# Create stray/adopt contingency tables to create a fourfold plot 
# showing relationships for the adoption of stray and non stray cats


StrayAdopt <- ftable(catData[c("Adopt","Stray")])

StrayAdopt <- as.table(StrayAdopt )

fourfold(StrayAdopt)


#xtabs(Adopt~as.factor(Stray),data=catData) #Number of cats adopted by stray status only



adoptSA <-  ftable(Adopt~Stray,data=catData) #contingency table just for Age and Stray



#Table of Adoption by Stray and Age

stargazer(adoptSA,covariate.labels = c("Stray","Age","Number Adopted"),rownames = FALSE,out="adoptSA.html")




#Create a histogram for Length of Stay

#catData %>% ggplot(aes(x=LOS)) + geom_histogram(binwidth=5,fill="blue",colour="purple") +
 # scale_x_continuous(breaks = seq(from=0,to=250,by=10))  +
  #ggtitle("Length of Stay Distribution")+
  #theme(plot.title=element_text(hjust=.5),panel.grid.minor= element_blank(),panel.grid.major=element_blank()) +
  #xlab("Length of Stay (days)") +
  #ylab("Frequency")






### Use this Negative Binomial Regression ########

#catData$Age <- as.factor(catData$Age) 

#catData <- within(catData,Age <- relevel(Age, ref = "Older Adult")) #change the reference level to Older Adult




#groupCatDataLog <- groupCatDataLog %>% spread(Adopt,n) #Separate the "Yes" and "No" from the Adopt column

#groupCatDataLog[which(is.na(groupCatDataLog$No)),"No"] <- 0

#groupCatDataLog[which(is.na(groupCatDataLog$Yes)),"Yes"] <- 0






  
 # catDataAdopt %>% ggplot(aes(x = as.factor(GenderFactor),y=LOS)) + geom_boxplot() 
  
  
  #qplot(as.factor(Black),mean(LOS),data = catDataAdopt,geom="dotplot",fill=as.factor(Black))
  
 # structable
  
  # plotLOS <- catDataAdopt %>% ggplot(aes(x=LOS))
  
  # plotLOS + geom_histogram(aes(color=Black),fill=c("white"),binwidth=5,position="dodge")

  

 # ggpairs(data=catDataAdopt[,c("Stray","Black","CatType","GenderFactor","LOS")])
  

#Below is the command to create HTML output of the Negative Binomial Regression


#Interaction Plot color scheme

interactPal <- c(brewer.pal(5,"YlOrRd")[4],brewer.pal(9,"Greys")[9])


#Code for the interaction plot between fur color and breed for length of stay (LOS) (related to the Negative Binomial Regression)
#catDataAdopt%>% ggplot() +
 # aes(x = CatType,colour=as.factor(Black),group = as.factor(Black),y = LOS)+
  #stat_summary(fun.y=mean,geom="point") +
#  stat_summary(fun.y=mean,geom="line") + 
 # scale_colour_manual(name = "Fur Color",labels=c("Not Black","Black"),values = interactPal) +
#  theme(legend.text = element_text(size = 18)) +  #set the legend text to size 14
 # theme(legend.title = element_text(size=20)) +   #set the legend title to size 14
#  theme(axis.title.x = element_text(size=20),  #set the x-axis title to size 16
 #       axis.text.x  = element_text(size=18)) +  #set the x-axis text to size 14 (text by tick marks)
#  theme(axis.title.y = element_text(size=20), #set the y-axis title to size 16
 #       axis.text.y  = element_text(size=18)) +  #set the y-axis text to size 14 (text by tick marks)
#   scale_y_continuous(name="Mean LOS (days)") +  #Give the y-axis title a name
 # scale_x_discrete(name="Cat Type")   #Give the x-axis title a name
######################################################







#Grouped Form of the data for the Logistic Regression



#catDataAdopt$Age <- as.factor(catDataAdopt$Age)


#Grouped Form of the data for the Negative Binomial Regression (LOS)



groupCatDataLog <- catData %>% count(Age,Stray,GenderFactor,Week,CatType,Adopt)







groupCatDataLog <- groupCatDataLog %>% spread(Adopt,n) #Separate the "Yes" and "No" from the Adopt column

groupCatDataLog[which(is.na(groupCatDataLog$No)),"No"] <- 0

groupCatDataLog[which(is.na(groupCatDataLog$Yes)),"Yes"] <- 0


groupedDSHData <- groupCatDataLog %>% filter(CatType == "DOMESTIC SHORTHAIR")

ageOrder <- c("Kitten","Young Adult","Older Adult") #this is to order the ages for the barplot



library(ggthemes) #includes the Economist theme used by the barplots 


#barplot for Number of cats Adopted by stray Category for DSH cats
StrayAdoptDSHPlot <- groupedDSHData %>% ggplot(aes(x=Stray,y=Yes,fill=Stray )) + geom_bar(stat="identity",width=0.5) +
  ylim(0,600)  +  scale_y_continuous(name = "Number of DSH Cats Adopted\n",breaks=seq(0,600,by=50)) + 
  scale_x_discrete(name = "\nStray Status") + 
  theme(legend.position="none") + theme_economist(base_size=20)


plot(StrayAdoptDSHPlot)



#barplot for Number of cats Adopted by Age Category for DSH cats
AgeAdoptDSHPlot <- groupedDSHData %>% ggplot(aes(x=Age,y=Yes,fill=Age )) + geom_bar(stat="identity",width=0.5) +
                ylim(0,600)  +  scale_y_continuous(name = "Number of DSH Cats Adopted\n",breaks=seq(0,600,by=50)) + 
                scale_x_discrete(name = "\nAge",limits =ageOrder ) +scale_fill_discrete(breaks=ageOrder) + 
                theme(legend.position="none") + theme_economist(base_size=20)


plot(AgeAdoptDSHPlot)

  
weekOrder <- c("1","0")  #changes the order of the weeks

#barplot for week by Age Category for DSH cats
WeekAdoptDSHPlot <- groupedDSHData %>% ggplot(aes(x=as.factor(Week),y=Yes,fill=as.factor(Week) )) + geom_bar(stat="identity",width=0.5) +
  ylim(0,600) +  scale_y_continuous(name = "Number of DSH Cats Adopted\n",breaks=seq(0,600,by=100)) + 
  scale_x_discrete(name="\nWeeks", labels=c("0" = "More Than 2 Weeks", "1" = "2 Weeks or Less"),limits=weekOrder) +
  scale_fill_brewer(palette="Set2") + theme_economist(base_size=20)

WeekAdoptDSHPlot <- WeekAdoptDSHPlot + theme(legend.position="none") 

  
plot(WeekAdoptDSHPlot)


groupedLogisticDSH1 <- glm(cbind(Yes,No)~Age + Stray  + Week ,family="binomial",data=groupedDSHData)

summary(groupedLogisticDSH1) #BIC 158.97 (Use the quasibinomial version of model 1 which as an intermediate BIC value and

#is more parsimonious than model 3 although model 3 has a slightly lower BIC value)





#linearP <- predict(groupedLogisticDSH1)

#lpSQ <- linearP^2

#groupedlpDSH <- groupedDSHData %>% mutate(squarePred = lpSQ)
#linkModDSH <-  glm(cbind(Yes,No)~Age + Stray  + Week + squarePred,family="binomial",data=groupedlpDSH)

#summary(linkModDSH)


library(aods3)

gof(groupedLogisticDSH1) #goodness of fit applied to the model before adjusting for overdispersion

modelEstimateDSH <- predict(groupedLogisticDSH1)

newGroupDSH <- groupedDSHData %>% mutate(LP = modelEstimateDSH)

linkDSH <- glm(cbind(Yes,No)~Age + Stray  + Week + (LP)^2,family="binomial",data=newGroupDSH)

summary(linkDSH)

#Sum of the squared pearson redisuals

chiSqmodelDSH <- sum((residuals(groupedLogisticDSH1,type="pearson"))^2)

#anovaUnadjDSH.Logistic<- xtable(anova(groupedLogisticDSH1),out="unadjustedLogisticANOVA",type="html")

#library(xtable)
#options(xtable.floating = FALSE)
#options(xtable.timestamp = "")


#anovaDSHLog <- anova(groupedLogisticDSH1)


#anovaUnadjDSH.Logistic<- xtable(anovaDSHLog,sanitize.text=TRUE,auto=TRUE)

#anovaUnadjDSH.Logistic <- autoformat(anovaUnadjDSH.Logistic)


#print.xtable(anovaUnadjDSH.Logistic, type="html",file="")
             
             

#Output for the Model Fit Appendix for the logsitic regression model for DSH (the model not yet adjusted for dispersion)
stargazer(groupedLogisticDSH1,type = "html",style="qje",
          ci=TRUE, dep.var.caption="",dep.var.labels.include = FALSE,digits=3,omit.stat="n",nobs=FALSE,
          table.layout="s",
          covariate.labels = c("Age  (Age = Older Adult)","Age  (Age = Young Adult)","Stray (Stray = Yes)",
                               "Weeks (Weeks = More Than 2 Weeks)"),
          title = "Results",align=TRUE,out="UnadjustedLogisticDSH.htm")



groupedLogisticDSH2 <- glm(cbind(Yes,No)~Age +Stray  + Week  + Stray*Age,family="binomial",data=groupedDSHData)

summary(groupedLogisticDSH2) #BIC 159.43



groupedLogisticDSH3 <- glm(cbind(Yes,No)~Age +  Stray  + Week  + Stray*Week,family="binomial",data=groupedDSHData)

summary(groupedLogisticDSH3) #BIC 153.44

groupedLogisticDSH4 <- glm(cbind(Yes,No)~Age +  Stray  + Week  +  Stray*Age +  Stray*Week,family="binomial",data=groupedDSHData)

summary(groupedLogisticDSH4) #BIC 156.14

bicTableDSH <-round(BIC(groupedLogisticDSH1,groupedLogisticDSH2,groupedLogisticDSH3,groupedLogisticDSH4),2)

rownames(bicTableDSH) <- c("Age + Stray + Week","Age + Stray + Week + Stray*Age","Age + Stray + Week + Stray*Week",
                           "Age + Stray + Week + Stray*Age + Stray*Week")


bicTableDSH <- bicTableDSH %>% dplyr::select(BIC)

print(bicTableDSH)




groupedLogisticDSH1DispModel <- glm(cbind(Yes,No)~Age + Stray  + Week,family="quasibinomial",data=groupedDSHData)

summary(groupedLogisticDSH1DispModel)  #Use the dispersion adjusted version of model 1 with quasibinomial link

groupedLogisticDSH2DispModel <- glm(cbind(Yes,No)~Age + Stray  + Week + Stray*Age ,family="quasibinomial",data=groupedDSHData)

summary(groupedLogisticDSH2DispModel)

groupedLogisticDSH3DispModel  <- glm(cbind(Yes,No)~Age + Stray  + Week + Stray*Week ,family="quasibinomial",data=groupedDSHData)

summary(groupedLogisticDSH3DispModel )

groupedLogisticDSH4DispModel <- glm(cbind(Yes,No)~Age +  Stray  + Week  +  Stray*Age +  Stray*Week,family="quasibinomial",data=groupedDSHData)

summary(groupedLogisticDSH4) #This model which contains two interaction terms is the global model



library(AICcmodavg) #contains the c_hat function to calculate the dispersion parameter

#Estimate the c-hat value for QAICc based on the dispersion parameter of the global model
#The global model is the model that contains both the stray*age and stray*week interaction terms

dispGlobalDSH  <- c_hat(groupedLogisticDSH4)
dispGlobalDSH <-  round(as.numeric(dispGlobalDSH),2)

library(MuMIn) # contains the QAIC function

dshQAICc1 <- round(QAICc(groupedLogisticDSH1,chat=dispGlobalDSH ),2)

dshQAICc2 <- round(QAICc(groupedLogisticDSH2,chat=dispGlobalDSH ),2)

dshQAICc3 <- round(QAICc(groupedLogisticDSH3,chat=dispGlobalDSH ),2)


dshQAICc4 <- round(QAICc(groupedLogisticDSH4,chat=dispGlobalDSH ),2)

QAICctableDSH <-data.frame(c(dshQAICc1,dshQAICc2,dshQAICc3,dshQAICc4))

colnames(QAICctableDSH) <- c("QAICc")


rownames(QAICctableDSH) <- c("Age + Stray + Week","Age + Stray + Week + Stray*Age","Age + Stray + Week + Stray*Week",
                            "Age + Stray + Week + Stray*Age + Stray*Week")


print(QAICctableDSH) #display the table of QAICc values



library(dispmod)

#dispDSH is not used as a model, but is just used to get the goodness of fit statistics for the adjusted model

dispDSH <- glm.binomial.disp(groupedLogisticDSH1 , verbose = FALSE)

#Only use the chi-square value (the bottom value, not the Deviance value)

gofDSH <- gof(dispDSH) #chi-square value of  19 with 19 degrees of freedom, p-value = 0.457

print(gofDSH)

gof(groupedLogisticDSH1) #chi-square value of 79.7 with 19 degrees of freedom and p= 2.12*10^-9

library(visreg) #provides some exploratory plots that can be used with different types of regression models

#Some plots that display bivariate relationships between each predictor in the model vs. the outcome (adoption)
visreg(groupedLogisticDSH1DispModel)


#Residuals adjusted by the dispersion parameter (divide the residuals by the square root of the disp. parameter)
adjModelEstimatesDSH <- augment(groupedLogisticDSH1DispModel,groupedDSHData) %>%
                        mutate(adjustRes = .resid / sqrt(summary(groupedLogisticDSH1DispModel)$dispersion))



adjChiSQ <- sum((adjModelEstimatesDSH$adjustRes)^2)

#Palette for DSH residuals plot
residualPlotColorDSH <- brewer.pal(n = 12, name = "Spectral")

#Resiudal Plot for DSH Cat Model after adjusting for Overdispersion


residualPlotDSH <- adjModelEstimatesDSH %>% ggplot(aes(x=1:length(adjustRes),y=adjustRes)) +
  geom_point(aes(color=adjustRes),show.legend=FALSE) + 
  expand_limits(y=c(-4,4)) +
  scale_y_continuous(name="Residuals",breaks = seq(-4,4,1)) +
  scale_x_continuous(name="",breaks=NULL,labels=NULL) +
  theme_calc(base_size=16)
  
  residualPlotDSH  <- residualPlotDSH + scale_color_gradientn(colours = residualPlotColorDSH[c(3,10,11)] )

plot(residualPlotDSH)


# Output for final logistic regression model for DSH cats
stargazer(groupedLogisticDSH1DispModel,type = "html",style="qje",
         ci=TRUE, dep.var.caption="",dep.var.labels.include = FALSE,digits=3,omit.stat="n",nobs=FALSE,
        covariate.labels = c("Age  (Age = Older Adult)","Age  (Age = Young Adult)","Stray (Stray = Yes)",
                             "Weeks (Weeks = More Than 2 Weeks)"),
        title = "Results",align=TRUE,out="LogisticModelDSH.htm")


#Creating the Reciprocal Odds Ratio Table for the Logistic Regression Model for DSH cats
logistic.oddsRatioTableDSH <- 1/exp(cbind(OR.T=coef(groupedLogisticDSH1DispModel),confint(groupedLogisticDSH1DispModel)))

logistic.oddsRatioTableDSH <- logistic.oddsRatioTableDSH[,c(1,3,2)] #change the rows of the 2.5% and 97.5% CI columns

#Odds Ratio Table for the Logistic Regression Model for DSH cats
logistic.oddsRatioTableDSH <- round(logistic.oddsRatioTableDSH,digits=2)


#Change the order of the variables to match the output for the logistic regression table for DSH cats
logistic.oddsRatioTableDSH <- logistic.oddsRatioTableDSH[c(2,3,4,5,1),]


#Remove the last row of the odds ratio table which contains the 
#odds ratio and confidence interval for the intercept odds ratio

logistic.oddsRatioTableDSH <- logistic.oddsRatioTableDSH[-c(nrow(logistic.oddsRatioTableDSH)),]

#Change the column names of the last two columns to Lower for the lower bound of the 95% CI
# and Upper for the upper bound of the 95% CI for the odds ratios.

colnames(logistic.oddsRatioTableDSH)[c(2,3)] <- c("Lower","Upper") 

rownames(logistic.oddsRatioTableDSH) <- c("Kitten vs. Older Adult","Kitten vs. Younger Adult",
                                          "Non-Stray vs. Stray","At Most 2 Weeks vs. More Than 2 Weeks")



#Table of reciprocal odds ratio for the logistic model for DSH cats

stargazer(logistic.oddsRatioTableDSH,colnames=TRUE,
          rownames = TRUE,out="LogisticOddsRatioDSH.html", 
          summary=FALSE,digits=2,font.size="small",no.space=FALSE,align=TRUE)



#Residuals adjusted by the disperson parameter

adjustedResidualsDSHModel <- rstandard(groupedLogisticDSH1,type="deviance")/sqrt(4.19297)





# groupedDSHChiSq <- sum((residuals(groupedLogisticDSH3,type="pearson"))^2)


1-pchisq( groupedDSHChiSq,groupedLogisticDSH3$df.residual ) #chi-square goodness of fit p-value = 0.019



#Chosen Logistic Model adjusted for dispersion

#groupedLogisticDSH3DispModel <- glm(cbind(Yes,No)~Age + Stray  + Week + Stray*Week,family="quasibinomial",data=groupedDSHData)

#summary(groupedLogisticDSH3DispModel )

#groupedLogisticDSH3DispModel <- glm(cbind(Yes,No)~Age +  GenderFactor + Stray  + Week  + Stray*Week,family="quasibinomial",data=groupedDSHData)

summary(groupedLogisticDSH3DispModel) #Lowest AIC 138.07 choose this mode for Domestic Shorthair Cats

#groupedLogisticDSH4 <- glm(cbind(Yes,No)~Age +  GenderFactor + Stray  + Week + Stray*GenderFactor,family="binomial",data=groupedDSHData)


#summary(groupedLogisticDSH4)


AIC(groupedLogisticDSH1,groupedLogisticDSH2,groupedLogisticDSH3,groupedLogisticDSH4)



groupedLogisticDSH1 <- glm(cbind(Yes,No)~Age + Stray  + Week ,family="binomial",data=groupedDSHData)


library(xtable)

print(xtable(anova(groupedLogisticDSH1),hline.after=c(1),type=html))

#stargazer(groupedLogisticDSH1,colnames=TRUE,
 #         rownames = TRUE,out="SummaryLogisticDSH.html", 
  #        summary=FALSE,digits=2,font.size="small",no.space=FALSE,align=TRUE)




groupedLogisticDSH2 <- glm(cbind(Yes,No)~Age + Stray  + Week + Stray*Age,family="binomial",data=groupedDSHData)

summary(groupedLogisticDSH2)

groupedLogisticDSH3 <- glm(cbind(Yes,No)~Age + Stray  + Week + Stray*Week,family="binomial",data=groupedDSHData)

summary(groupedLogisticDSH3)

AIC(groupedLogisticDSH1,groupedLogisticDSH2,groupedLogisticDSH3)





logisticDSH.OddsRatioTable <- exp(cbind(OR=coef(groupedLogisticDSH3),confint(groupedLogisticDSH3)))


################### DMH Logistic Regression #########################



groupedDMHData <- groupCatDataLog %>% filter(CatType == "DOMESTIC MEDIUMHAIR")




#barplot for Number of cats Adopted by stray Category for DSH cats
StrayAdoptDMHPlot <- groupedDMHData %>% ggplot(aes(x=Stray,y=Yes,fill=Stray )) + geom_bar(stat="identity",width=0.5) +
  ylim(0,80)  +  scale_y_continuous(name = "Number of DMH Cats Adopted\n",breaks=seq(0,80,by=10)) + 
  scale_x_discrete(name = "\nStray Status") + 
  theme(legend.position="none") + theme_economist(base_size=20)


plot(StrayAdoptDMHPlot)



ageOrder


#barplot for Number of cats Adopted by Age Category for DMH cats
AgeAdoptDMHPlot <- groupedDMHData %>% ggplot(aes(x=Age,y=Yes,fill=Age )) + geom_bar(stat="identity",width=0.5) +
  ylim(0,100)  +  scale_y_continuous(name = "Number of DMH Cats Adopted\n",breaks=seq(0,100,by=10)) + 
  scale_x_discrete(name = "\nAge",limits =ageOrder ) +scale_fill_discrete(breaks=ageOrder) + 
  theme(legend.position="none") + theme_economist(base_size=20)


plot(AgeAdoptDMHPlot)





weekOrder <- c("1","0")  #changes the order of the weeks

#barplot for week by Age Category for DMH cats
WeekAdoptDMHPlot <- groupedDMHData %>% ggplot(aes(x=as.factor(Week),y=Yes,fill=as.factor(Week) )) + geom_bar(stat="identity",width=0.5) +
  ylim(0,100) +  scale_y_continuous(name = "Number of DMH Cats Adopted\n",breaks=seq(0,100,by=10)) + 
  scale_x_discrete(name="\nWeeks", labels=c("0" = "More Than 2 Weeks", "1" = "2 Weeks or Less"),limits=weekOrder) +
  scale_fill_brewer(palette="Set2") + theme_economist(base_size=20)

WeekAdoptDMHPlot <- WeekAdoptDMHPlot + theme(legend.position="none") 


plot(WeekAdoptDMHPlot)




### Logistic Regression For Domestic Mediumhair Cats (DMH cats) ###


groupedLogisticDMH1 <- glm(cbind(Yes,No)~Age + Stray  + Week,family="binomial",data=groupedDMHData)

summary(groupedLogisticDMH1) #AIC 62.435 ; BIC 67.41



gof(groupedLogisticDMH1)
#confint(exp(coef(groupedLogisticDMH1)))


groupedLogisticDMH2 <- glm(cbind(Yes,No)~Age + Stray  + Week+ Stray*Age,family="binomial",data=groupedDMHData)

summary(groupedLogisticDMH2) #AIC 63.019  ; BIC 69.99


groupedLogisticDMH3 <- glm(cbind(Yes,No)~Age + Stray  + Week  + Stray*Week,family="binomial",data=groupedDMHData)

summary(groupedLogisticDMH3) #AIC 62.735 ; BIC 68.71

groupedLogisticDMH4 <- glm(cbind(Yes,No)~Age +  Stray  + Week  +  Stray*Age +  Stray*Week,family="binomial",data=groupedDMHData)

summary(groupedLogisticDSH4) #BIC

bicTableDMH <- round(BIC(groupedLogisticDMH1,groupedLogisticDMH2,groupedLogisticDMH3),2) #BIC values rounded to decimal places

rownames(bicTableDMH) <- c("Age + Stray + Week","Age + Stray + Week + Stray*Age","Age + Stray + Week + Stray*Week")

bicTableDMH <- bicTableDMH %>% dplyr::select(BIC)

print(bicTableDMH)

#residuals(groupedLogisticDMH1,type="pearson")


#stargazer(groupedLogisticDSH1DispModel,type = "html",style="qje",
 #         ci=TRUE, dep.var.caption="",dep.var.labels.include = FALSE,digits=3,omit.stat="n",nobs=FALSE,
  #        covariate.labels = c("Age  (Age = Older Adult)","Age  (Age = Young Adult)","Stray (Stray = Yes)",
   #                            "Weeks (Weeks = More Than 2 Weeks)"),
    #      title = "Results",align=TRUE,out="LogisticModelDSH.htm")




#Palette for DSH residuals plot
residualPlotColorDMH <- brewer.pal(n = 12, name = "Spectral")

#Resiudal Plot for DSH Cat Model after adjusting for Overdispersion

#residuals for DMH logistic model
residDMH <- residuals(groupedLogisticDMH1,type="pearson")

#convert this into a dataframe
residDMH <- data.frame(residDMH)

residualPlotDMH <- residDMH %>% ggplot(aes(x=1:length(residDMH),y=residDMH)) +
  geom_point(aes(color=residDMH),show.legend=FALSE) + 
  expand_limits(y=c(-4,4)) +
  scale_y_continuous(name="Residuals",breaks = seq(-4,4,1)) +
  scale_x_continuous(name="",breaks=NULL,labels=NULL) +
  theme_calc(base_size=16)

residualPlotDMH  <- residualPlotDMH + scale_color_gradientn(colours = residualPlotColorDMH[c(3,10,11)] )

plot(residualPlotDMH)

library(car)

influencePlot(groupedLogisticDMH1,id.col="blue")



stargazer(groupedLogisticDMH1,type = "html",style="qje",
          ci=TRUE, dep.var.caption="",dep.var.labels.include = FALSE,digits=3,nobs=FALSE,
          covariate.labels = c("Age  (Age = Older Adult)","Age  (Age = Young Adult)","Stray (Stray = Yes)",
                               "Weeks (Weeks = More Than 2 Weeks)"),
          title = "Results",align=TRUE,out="LogisticModelDMH.htm",omit.stat=c("ll","aic","n"))




#Creating the Reciprocal Odds Ratio Table for the Logistic Regression Model for DSH cats
logistic.oddsRatioTableDMH <- 1/exp(cbind(OR.T=coef(groupedLogisticDMH1),confint(groupedLogisticDMH1)))

logistic.oddsRatioTableDMH <- logistic.oddsRatioTableDMH[,c(1,3,2)] #change the rows of the 2.5% and 97.5% CI columns

#Odds Ratio Table for the Logistic Regression Model for DSH cats
logistic.oddsRatioTableDMH <- round(logistic.oddsRatioTableDMH,digits=2)


#Change the order of the variables to match the output for the logistic regression table for DSH cats
logistic.oddsRatioTableDMH <- logistic.oddsRatioTableDMH[c(2,3,4,5,1),]


#Remove the last row of the odds ratio table which contains the 
#odds ratio and confidence interval for the intercept odds ratio

logistic.oddsRatioTableDMH <- logistic.oddsRatioTableDMH[-c(nrow(logistic.oddsRatioTableDMH)),]

#Change the column names of the last two columns to Lower for the lower bound of the 95% CI
# and Upper for the upper bound of the 95% CI for the odds ratios.

colnames(logistic.oddsRatioTableDMH)[c(2,3)] <- c("Lower","Upper") 

rownames(logistic.oddsRatioTableDMH) <- c("Kitten vs. Older Adult","Kitten vs. Younger Adult",
                                          "Non-Stray vs. Stray","At Most 2 Weeks vs. More Than 2 Weeks")


#Table of reciprocal odds ratio for the logistic model for DSH cats

stargazer(logistic.oddsRatioTableDMH,colnames=TRUE,
          rownames = TRUE,out="LogisticOddsRatioDMH.html", 
          summary=FALSE,digits=2,font.size="small",no.space=FALSE,align=TRUE)






catData$CatType <- as.factor(catData$CatType)


catData$Stray <- as.factor(catData$Stray)


######################

library(vcd)





####### Keep this in comment for now (don't delete yet, might do something similar for the logistic models created) #######

#names(adoptPredMat) <- c("CatType","GenderFactor","Stray","Age")



#adoptPredMat <- expand.grid(CatType = c("DOMESTIC SHORTHAIR","DOMESTIC MEDIUMHAIR"), #expand.grid contains all the levels
 #                           GenderFactor = as.factor(c(0,1)), Stray = c("Yes","No"),
  #                          Age = c("Kitten","Young Adult","Older Adult"))


#adoptProb <- predict(logisticModel2,newdata=
 #         adoptPredMat,type="response")


#adoptPredTable <- cbind(adoptPredMat,adoptProb)


#adoptPredTable$GenderFactor <- ifelse(adoptPredTable$GenderFactor == "1","Male","Female") #change 0 and 1's back to male and female

#adoptPredTable$adoptProb <- adoptPredTable$adoptProb * 100 #convert to percentages


#adoptPredTable <- adoptPredTable[order(adoptPredTable$adoptProb,decreasing=TRUE),] #order by the estimated chance of adoption in descending order

# Create a table of esitmated probabilites for all of the different categories
#stargazer(adoptPredTable,covariate.labels = c("Cat Type","Gender","Stray", "Age","Estimated Chance of Adoption (%)")
 #         ,rownames = FALSE,out="adoptPredTable.html", 
  #        summary=FALSE,digits=2)


######################




#CatType is the new name for the variable PrimaryBreed





#Create a boxplot using ggboxplot


#Create a scatter plot using geom_point
ggplot(data=catData) +
  geom_point(mapping=aes(x=IntakeAge,y=LOS)) +
  ggtitle("Length of Stay  Vs. Intake Age")+
  theme(plot.title=element_text(hjust=.5))+
  xlab("Intake Age (years)")+
  ylab("Length of Stay (days)") +
  scale_y_continuous(breaks = seq(from=0,to=300,by=10)) 


#Create a smooth line plot (shows intake age continuous versus length of stay)
#ggplot(data=catData) +
 # geom_smooth(mapping=aes(x=IntakeAge,y=LOS)) +
  #ggtitle("Length of Stay  Vs. Intake Age")+
  #theme(plot.title=element_text(hjust=.5))+
  #xlab("Intake Age (years)")+
  #ylab("Length of Stay (days)") 
  






