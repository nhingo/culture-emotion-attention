# SET UP -----------------------------------------------
#load the MoodEye df (need to replace the var names when re-running since it has changed, also need to be careful of NAs)
#general dir for GRF1314, as other data files are not stored in the R project folder
#for administrative purposes
#because the working dir for an R project, when loaded into Rstudio, is always the folder
#with the R project file, we go up one level to get to the general dir
gendir <- dirname(getwd())
mooddir <- "/mood_data"

stimdir <- "/stim_data"

eyedir <- "/eye_data"

load("MoodEye.Rda")

# FURTHER CLEAN AND PREPARE DATA -----------------------------------------------
#check for duplicate data
dups <- which(duplicated(sortedMoodEye)==TRUE)
#drop the last 2 rows because they're the "duplicates"- turns out they are just empty rows
sortedMoodEye <- sortedMoodEye[c(1:(dups-2)), ]

#follow instructions from Linear mixed effects models using R book
#convert id vars into factor
sortedMoodEye <-  within(sortedMoodEye,
                          {Subject.f <- factor(Subject)
                          picID.f <- factor(picID)
                          TrialNum.f <- factor(TrialNum)})
library(nlme)
#there are NAs in the data -> examine the rows and eliminate if necessary
sortedMoodEye <- sortedMoodEye[-c(which(is.na(sortedMoodEye$picID.f)==TRUE),which(is.na(sortedMoodEye$TrialNum.f)==TRUE)),]
#identify subject- and trial- level variables
#variables that are invariant by a certain level (level of subj or pic or trial)
nmsj <- names(gsummary(sortedMoodEye,
                       form = ~Subject.f, inv = TRUE))
nmpic <- names(gsummary(sortedMoodEye,
                       form = ~picID.f, inv = TRUE))
nmtrial <- names(gsummary(sortedMoodEye,
                        form = ~TrialNum.f, inv = TRUE))

#check unique values
length(unique(sortedMoodEye$Subject))
order(unique(sortedMoodEye$picID)) #order works for lists with strings
sort(unique(sortedMoodEye$TrialNum)) # sort works for integer
tab1 <- xtabs(~Subject + picID, data = sortedMoodEye) #picture by subject table
all(tab1 > 0) #all counts > 0? if all >0, factors are fully crossed
range(tab1) #range of count, should be just 1 (1 pic for each picID for each subj)
#range returns 2, so some subjects have 2 trials with the same picID
which(tab1 == 2, arr.ind=TRUE) #show both row and column (without arr.ind = TRUE will only show index of element)
#make tab1 into a df for easy examination
x <- data.frame(tab1)
str(x)
xdup <- x[which(x$Freq == 2),] #examine the duplicates
#remove duplicate in original df
rmrow <- which((sortedMoodEye$Subject == xdup$Subject) & (sortedMoodEye$picID == xdup$picID))
sortedMoodEye <-sortedMoodEye[-rmrow[1],]

#explore DV to see if there's any outliers
plottedvar <- sortedMoodEye$ValenceRating
varname <- "Mood Task Valence Rating"
summary(plottedvar)
hist(plottedvar, breaks = 20, col = rgb(0,0,1,0.5), main = varname)
boxplot(plottedvar, col = rgb(0,0,1,0.5), main = varname)
qqnorm(plottedvar, main = varname)
qqline(plottedvar, col = "red")

#grand-mean center continous vars (not centering LZPicArea ratio because a 0 doesnt make sense)
sortedMoodEye$picVlnCent <- scale(sortedMoodEye$picValence, center = TRUE, scale = FALSE)
sortedMoodEye$picArslCent <- scale(sortedMoodEye$picArousal, center = TRUE, scale = FALSE)
sortedMoodEye$FixDurPercent_AOIs_cent <- scale(sortedMoodEye$FixDurPercent_AOIs, center = TRUE, scale = FALSE)

#recode so that chinese pics are relevant to Chinese subj but not US subj,
#and us pics are relevant to US subj but not Chinese subj
picculrelevance <- factor(ifelse(((sortedMoodEye$subjculture == "HK") & (sortedMoodEye$picculture == "HK"))|
                            ((sortedMoodEye$subjculture == "US") & (sortedMoodEye$picculture == "US")),"selfRelevant","otherRelevant"))

#picture valence category based on the file that was made prior to running the study (unsure where the categorization is from)
sortedMoodEye$picValcat <- ifelse(sortedMoodEye$picIDNum < 11,"positive",
                                  ifelse(((10 <sortedMoodEye$picIDNum)&(sortedMoodEye$picIDNum<27))
                                         |((sortedMoodEye$picIDNum >= 39)),"neutral","negative"))

#pic valence based on pilot rating
sortedMoodEye$picValcat2 <- ifelse(sortedMoodEye$picValence < 4,"negative",
                                   ifelse((sortedMoodEye$picValence < 6)&(sortedMoodEye$picValence >=4),
                                          "neutral","positive"))

# MODEL AND PLOT WITH MOOD AS DV WITH TIME AS A FACTOR -----------------------------------------------
##H1: does mood change from beginning to end of the experiment? are there group differences? (difference at the beginning < difference at the end)
#use mood rating from first and last trial
library(dtplyr) #for data.table and dplyr
library(dplyr)
library(data.table)
#subset data so only the first and last trials for each subject are included
####this dataset includes the neutral filler trials
mood.subset<-setDT(sortedMoodEye)[, .SD[c(which.max(TrialNum),which.min(TrialNum))], by=Subject]
#another method:
#mood.subset2 <- mood.data %>% group_by(Subject) %>% filter(TrialNum == max(TrialNum) | TrialNum == min(TrialNum))
#convert first and last trial to "beginning" and "end" of session mood

mood.subset <- within(mood.subset, {
                      Time <- round(TrialNum / TrialNum[which.max(TrialNum)])
})
mood.subset$Time <- factor(mood.subset$Time,
                           labels = c("begin","end"))

#plot mood trajectory
library(lattice)
xy1 <- xyplot(ValenceRating ~ Time | moodcond * age,
               groups = Subject.f,
               data = mood.subset,
               type = "l", lty = 1)

update(xy1,
       xlab = "Time point in exp",
       ylab = "Valence Rating of Mood",
       grid = "h")
flst <- list(mood.subset$Time, mood.subset$moodcond, mood.subset$age, mood.subset$subjculture) # list of "by" factors
tN <- tapply(mood.subset$ValenceRating,flst, function(x) length(x[!is.na(x)])) #table of non NA values

tMn <- tapply(mood.subset$ValenceRating,flst, mean)

bwUS <- bwplot(ValenceRating ~ Time | moodcond * age, #box-whisker plot of means
               data = subset(mood.subset,subjculture == "US"))
update(bwUS,
       xlab = "Time point in exp",
       ylab = "Valence Rating US group")

bwHK <- bwplot(ValenceRating ~ Time | moodcond * age, #box-whisker plot of means
               data = subset(mood.subset,subjculture == "HK"))
update(bwHK,
       xlab = "Time point in exp",
       ylab = "Valence Rating HK group")
              

summ_timemood <- by(subset(mood.subset,select = c(FixDurPercent_AOIs,FixDurPercent_OUTSIDE,HRCulMean,
                                 OwnCulMean,OtherCulMean,ArousalRating,ValenceRating)),flst,summary)

#________________________________________________________________
#6/23/2017: was up to here, need to run the rest
library(lme4)
library(lmerTest)
#unconditional model
uncondMT <- lmer(ValenceRating ~ 1 + (1|Subject), data = mood.subset)
summary(uncondMoodTime)
plot(uncondMoodTime)
#unconditional growth model
uncondgrowthMT <- lmer(ValenceRating ~ Time + (Time|Subject), data = mood.subset)

#uncondgrowthMT had an error because there are as many random effects as there are observations
#random slope + random intercept = one random effect for each observation

#unconditional growth model dropping random slope 
uncondgrowthMT <- lmer(ValenceRating ~ Time + (1|Subject), data = mood.subset)
summary(uncondgrowthMT)
#conditional growth model
condgrowthMT <- lmer(ValenceRating ~ Time*moodcond + subjculture*age + (1|Subject), data = mood.subset)
summary(condgrowthMT)

condgrowthMT1 <- lmer(ValenceRating ~ Time*moodcond + Time*subjculture*age + (1|Subject), data = mood.subset)
summary(condgrowthMT1)

#no random slopes for the models above because not enough observations (only 2 observations per subject)

#ICC calculated from between-group (group = subj) variance over betweein + within group variance indicates that
#there is not enough variance between group to warrant incorporating random effect, so it will be dropped
#conditional growth model with picture/trial predictor

condgrowthMT2 <- lmer(ValenceRating ~ Time*moodcond + age*subjculture 
                      + picVlnCent + picculture + picArslCent
                      + (1|Subject), data = mood.subset)
summary(condgrowthMT2)

lsmip(condgrowthMT1,age~USCul|subjculture, at = list(USCul=c(2,6)))


#plot general trend of mood for all participants across all 45 trials
meanAllVlnc <- tapply(sortedMoodEye$ValenceRating,sortedMoodEye$TrialNum,mean)
library(ggplot2)
allMood <- data.frame(as.numeric(unlist(dimnames(meanAllVlnc))),unname(meanAllVlnc))
colnames(allMood) <- c("Trial","meanMood")
ggplot(data=allMood, aes(x=Trial, y=meanMood, group=1)) +
  geom_line(color="red")+
  geom_point()
#plot by group
facls <- list(sortedMoodEye$TrialNum,sortedMoodEye$moodcond)
meanMoodgrp <- tapply(sortedMoodEye$ValenceRating,facls,mean)
allMoodGrp <- data.frame(as.numeric(unlist(dimnames(meanMoodgrp)[1])),
                         unname(meanMoodgrp))
colnames(allMoodGrp) <- c("Trial","meanMoodControl","meanMoodNegative")
ggplot(data=allMoodGrp, aes(x=Trial, y=meanMoodNegative, group=1)) +
  geom_line(color="red")+
  geom_point()
#melt the data for easier plotting
library(reshape2)
allMoodGrp_long <- melt(allMoodGrp,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars="Trial",
                  # The source columns
                  measure.vars=c("meanMoodControl","meanMoodNegative"),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="condition",
                  value.name="meanMood")

ggplot(allMoodGrp_long, aes(x=Trial, y=meanMood, group=condition)) +
  geom_line(aes(color=condition)) +
  geom_point(aes(color=condition))

# MODEL WITH MOOD AS DV, WITHOUT TIME -----------------------------------------------
#unconditional model
MoodbyTrial1 <- lmer(ValenceRating ~ 1 + (1|Subject) + (1|picID), data = sortedMoodEye)
summary(MoodbyTrial1)

#models with predictors
#random intercepts only, full model
MoodbyTrial2 <- lmer(ValenceRating ~ picVlnCent*picculture*FixDurPercent_AOIs_cent*age*moodcond*subjculture
                     + picArslCent + LZPicArea
                       + (1|Subject) + (1|picID), data = sortedMoodEye)
summary(MoodbyTrial2)


##from full model (Model2), instead of dropping nonsig terms, drop terms that are non-central to hypothesis
#drop moodcond from interaction; random intercepts only
MoodbyTrial2b <- lmer(ValenceRating ~ picVlnCent*picculture*FixDurPercent_AOIs_cent*age*subjculture
                     + moodcond + picArslCent + LZPicArea
                     + (1|Subject) + (1|picID), data = sortedMoodEye)
summary(MoodbyTrial2b)

#add random slopes
#not adding picArousal random slope because 
MoodbyTrial2c <- lmer(ValenceRating ~ picVlnCent*picculture*FixDurPercent_AOIs_cent*age*subjculture
                      + moodcond + picArslCent + LZPicArea
                      + (picVlnCent + picArslCent|Subject) 
                      + (age + subjculture + moodcond|picID), data = sortedMoodEye)
summary(MoodbyTrial2c)

MoodbyTrial2c1 <- lmer(ValenceRating ~ picVlnCent*picculture*FixDurPercent_AOIs_cent*age*subjculture
                      + moodcond + picArslCent + LZPicArea
                      + (1|Subject) + (1|picID), data = sortedMoodEye)
summary(MoodbyTrial2c1)

MoodbyTrial2d <- lmer(ValenceRating ~ picVlnCent*picculture*FixDurPercent_AOIs_cent*age*subjculture*moodcond
                      + picArslCent + LZPicArea
                      + (picVlnCent+ picArslCent|Subject) 
                      + (age + subjculture + moodcond|picID), data = sortedMoodEye)
summary(MoodbyTrial2d)
#simple slope analysis - run models by picculture

MoodbyTrial3a <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*subjculture
                      + moodcond + picArslCent + LZPicArea
                      + (picVlnCent+ picArslCent|Subject) 
                      + (age + subjculture + moodcond|picID),
                      data = sortedMoodEye[sortedMoodEye$picculture %in% "HK",])
summary(MoodbyTrial3a)

MoodbyTrial3ai <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*subjculture
                      + moodcond + picArslCent + LZPicArea
                      + (1|Subject) + (1|picID),
                      data = sortedMoodEye[sortedMoodEye$picculture %in% "HK",])
summary(MoodbyTrial3ai)

MoodbyTrial3b <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*subjculture
                      + moodcond + picArslCent + LZPicArea
                      + (picVlnCent+ picArslCent|Subject) 
                      + (age + subjculture + moodcond|picID), 
                      data = sortedMoodEye[sortedMoodEye$picculture =="US",]) #another way of subsetting
summary(MoodbyTrial3b)

#simple slope analysis - run models by subject culture
MoodbyTrial3c <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*picculture
                      + moodcond + picArslCent + LZPicArea
                      + (picVlnCent+ picArslCent|Subject) 
                      + (age + moodcond|picID),
                      data = sortedMoodEye[sortedMoodEye$subjculture %in% "HK",])
summary(MoodbyTrial3c)
lsmip(MoodbyTrial3c,age~picculture)

MoodbyTrial3d <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*picculture
                      + moodcond + picArslCent + LZPicArea
                      + (picVlnCent+ picArslCent|Subject) 
                      + (age + moodcond|picID),
                      data = sortedMoodEye[sortedMoodEye$subjculture %in% "US",])
summary(MoodbyTrial3d)
#graph the model at +-1SD of each var
sd(sortedMoodEye$FixDurPercent_AOIs_cent,na.rm=TRUE)
sd(sortedMoodEye$picVlnCent,na.rm=TRUE)
library(lsmeans)
lsmip(MoodbyTrial3b,age~FixDurPercent_AOIs_cent|picVlnCent,
      at=list(picVlnCent=c(-1.3,1.3),FixDurPercent_AOIs_cent=c(-24,24)))

#from model 2d, decompose interactions with mood condition
#break down by mood condition
MoodbyTrial3e <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*subjculture*picculture
                      + picArslCent + LZPicArea
                      + (picVlnCent|Subject) + (age + subjculture|picID),
                      data = sortedMoodEye[sortedMoodEye$moodcond == "negative",])
summary(MoodbyTrial3e)

MoodbyTrial3f <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*subjculture*picculture
                       + picArslCent + LZPicArea
                       + (picVlnCent|Subject) + (age + subjculture|picID),
                       data = sortedMoodEye[sortedMoodEye$moodcond == "control",])
summary(MoodbyTrial3f)

MoodbyTrial3e2 <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*subjculture
                      + picArslCent + LZPicArea
                      + (picVlnCent|Subject) + (age + subjculture|picID),
                      data = sortedMoodEye[sortedMoodEye$moodcond == "negative"
                                           & sortedMoodEye$picculture == "US",])
summary(MoodbyTrial3e2)

summary(MoodbyTrial3e1)

MoodbyTrial3z <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*moodcond
                       + picArslCent + LZPicArea
                       + (picVlnCent + picArslCent|Subject) + (age + moodcond|picID),
                       data = sortedMoodEye[sortedMoodEye$subjculture == "HK"
                                            &sortedMoodEye$picculture == "HK",])
summary(MoodbyTrial3z)

MoodbyTrial3y <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*moodcond
                      + picArslCent + LZPicArea
                      + (picVlnCent + picArslCent|Subject) + (age + moodcond|picID),
                      data = sortedMoodEye[sortedMoodEye$subjculture == "US"
                                           &sortedMoodEye$picculture == "US",])
summary(MoodbyTrial3y)

MoodbyTrial3zz <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*moodcond*picculture
                      + picArslCent + LZPicArea
                      + (picVlnCent + picArslCent|Subject) + (age + moodcond|picID),
                      data = sortedMoodEye[sortedMoodEye$subjculture == "HK",])
summary(MoodbyTrial3zz)

MoodbyTrial3yy <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*moodcond*picculture
                      + picArslCent + LZPicArea
                      + (picVlnCent + picArslCent|Subject) + (age + moodcond|picID),
                      data = sortedMoodEye[sortedMoodEye$subjculture == "US",])
summary(MoodbyTrial3yy)

MoodbyTrial3z1 <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*picculture
                       + picArslCent + LZPicArea
                       + (picVlnCent + picArslCent|Subject) + (age|picID),
                       data = sortedMoodEye[sortedMoodEye$subjculture == "HK"
                                            & sortedMoodEye$moodcond == "negative",])
summary(MoodbyTrial3z1)
MoodbyTrial3z2 <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age*picculture
                       + picArslCent + LZPicArea
                       + (picVlnCent + picArslCent|Subject) + (age|picID),
                       data = sortedMoodEye[sortedMoodEye$subjculture == "HK"
                                            & sortedMoodEye$moodcond == "control",])
summary(MoodbyTrial3z2)

MoodbyTrial3z4 <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*age
                       + picArslCent + LZPicArea
                       + (picVlnCent + picArslCent|Subject) + (age|picID),
                       data = sortedMoodEye[sortedMoodEye$subjculture == "HK"
                                            & sortedMoodEye$moodcond == "negative"
                                            & sortedMoodEye$picculture == "US",])
summary(MoodbyTrial3z4)

MoodbyTrial3z5 <- lmer(ValenceRating ~ picVlnCent*FixDurPercent_AOIs_cent*picculture
                       + picArslCent + LZPicArea
                       + (picVlnCent + picArslCent|Subject) + (1|picID),
                       data = sortedMoodEye[sortedMoodEye$subjculture == "HK"
                                            & sortedMoodEye$moodcond == "negative"
                                            & sortedMoodEye$age == "old",])
summary(MoodbyTrial3z5)

lsmip(MoodbyTrial3z5,picculture~FixDurPercent_AOIs_cent|picVlnCent,
      at=list(picVlnCent=c(-1.3,1.3),FixDurPercent_AOIs_cent=c(-24,24)))

# MODEL WITH EYE DATA AS DV -----------------------------------------------
#for HK participants, use heritage culture score if their heritage is not HK but somewhere else in China
unique(sortedMoodEye$heritageCulture[sortedMoodEye$subjculture=="HK"])
#1 pp identified as having "England" heritage culture
#create a var that identifies pp with heritage culture as "not HK" (this includes also US pps)
sortedMoodEye$HK<-grepl("Hong Kong",sortedMoodEye$heritageCulture,ignore.case=TRUE)
sortedMoodEye$HK <- factor(sortedMoodEye$HK)
#create a HK/Chinese culture score
#for non-HK Chinese, it'd be their heritage culture
#for US pp, it'd be the "OtherCul" score
#for HK Chinese, copy over their "OwnCul" score
sortedMoodEye$ChiCul <- ifelse(sortedMoodEye$subjculture == "HK",
                               ifelse(sortedMoodEye$HK == FALSE,sortedMoodEye$HRCulMean,sortedMoodEye$OwnCulMean),
                               sortedMoodEye$OtherCulMean)

sortedMoodEye$USCul <- ifelse(sortedMoodEye$subjculture == "HK",
                               sortedMoodEye$OtherCulMean,sortedMoodEye$OwnCulMean)

#check for group differences in these VIA scores
t.test(USCul ~ subjculture, data = sortedMoodEye)
t.test(ChiCul ~ subjculture, data = sortedMoodEye)

#unconditional model
FixMod1 <- lmer(FixDurPercent_AOIs ~ 1 + (1|Subject) + (1|picID), data = sortedMoodEye)
summary(FixMod1)

#models with predictors
#random intercepts only, full model
#method 1: update the model formula; doing it this way we won't be able to get p-value from lmerTest however
FixMod2 <- update(FixMod1,.~.+ picVlnCent*picculture*age*moodcond*subjculture + picArslCent + LZPicArea)
  
summary(FixMod2)

#normal method; get p value from lmerTest
FixMod2a <- lmer(FixDurPercent_AOIs ~ picVlnCent*picculture*age*moodcond*subjculture + picArslCent + LZPicArea
                 + (1|Subject) + (1|picID), data = sortedMoodEye)
summary(FixMod2a)

#random slope and intercept, full model
FixMod2b <- lmer(FixDurPercent_AOIs ~ picVlnCent*picculture*age*moodcond*subjculture + picArslCent + LZPicArea
                 + (1 + picVlnCent + picculture + picArslCent|Subject) + (1 + age + subjculture + moodcond|picID),
                 data = sortedMoodEye)
summary(FixMod2b)

#dropping fixed effect of mood, and random slope of mood over picID, and picArousal random slope (did not explain
#much variance/non-sig & non-central to hypothesis)
FixMod2c <- lmer(FixDurPercent_AOIs ~ picVlnCent*picculture*age*subjculture + picArslCent + LZPicArea
                 + (1 + picVlnCent + picculture |Subject) + (1 + age + subjculture |picID),
                 data = sortedMoodEye)
summary(FixMod2c)

#same as full model with random slope and intercept, but take moodcond out of the interaction
FixMod2c1 <- lmer(FixDurPercent_AOIs ~ picVlnCent*picculture*age*subjculture 
                  + moodcond + picArslCent + LZPicArea
                 + (1 + picVlnCent + picculture + picArslCent |Subject) + (1 + age + subjculture |picID),
                 data = sortedMoodEye)
summary(FixMod2c1)

#model 2c1 has very small variance for random slopes of picculture and picarousal, so remove them:
FixMod2c2 <- lmer(FixDurPercent_AOIs ~ picVlnCent*picculture*age*subjculture 
                  + moodcond + picArslCent + LZPicArea
                  + (1 + picVlnCent |Subject) + (1 + age + subjculture |picID),
                  data = sortedMoodEye)
summary(FixMod2c2)

FixMod2d <- lmer(FixDurPercent_AOIs ~ picVlnCent*picculture*age*subjculture + picArslCent + LZPicArea
                 + (1 + picVlnCent*picculture |Subject) + (1 + age*subjculture |picID),
                 data = sortedMoodEye)
summary(FixMod2d)
#compare deviance 2c is not different from 2d so will go with 2c
#compare deviance 2b not sig different from 2c so will go with 2c

lsmip(FixMod2c2,age~picVlnCent|subjculture,at=list(picVlnCent=c(-1.3,1.3)))

#control for VIA scores (familiarity with Chinese & US cultures)
FixMod3 <- lmer(FixDurPercent_AOIs ~ subjculture*picVlnCent*picculture*age 
                + picVlnCent*age*subjculture*picculture*USCul
                +  picVlnCent*age*subjculture*picculture*ChiCul 
                + picArslCent + LZPicArea
                + (1 + picVlnCent + picculture |Subject) + (1 + age + subjculture|picID),
                 data = sortedMoodEye)
summary(FixMod3)

lsmip(FixMod3,age~USCul|subjculture, at = list(USCul=c(2,6)))

plot(fitted(FixMod2c2), residuals(FixMod2c2), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(FixMod2c2), residuals(FixMod2c2)))
# ADDITIONAL DEMOGRAPHIC ANALYSIS ------------------------------------------------------------------------
#load eye.df 
hksamp <- eye.df[eye.df$Subject<3000,]
length(unique(hksamp$Subject[hksamp$Subject<2000]))
length(unique(hksamp$Subject[hksamp$Subject>1999]))
ussamp <- eye.df[eye.df$Subject>3000,]
# number of OAs
length(unique(ussamp$Subject[ussamp$Subject<76199])) + length(unique(ussamp$Subject[ussamp$Subject<76399 & ussamp$Subject>76299]))
#number of YAs
length(unique(ussamp$Subject[ussamp$Subject>76399])) + length(unique(ussamp$Subject[ussamp$Subject>76199 & ussamp$Subject<76299]))

