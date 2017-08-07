###SET UP####
#general dir for GRF1314, as other data files are not stored in the R project folder
#for administrative purposes
#because the working dir for an R project, when loaded into Rstudio, is always the folder
#with the R project file, we go up one level to get to the general dir
gendir <- dirname(getwd())
mooddir <- "/mood_data"

stimdir <- "/stim_data"

eyedir <- "/eye_data"

#no need to set working directory because if we open Rproj for GRF1314 it'll be in the Rproj directory
#but do setwf("put your dir here") if you need

#load the necessary packages
library(readxl)
library(plyr)
library(foreign)

### MOOD DURING VIDEO PLAY####
#this is to exclude participants who still did not feel sad after 2nd mood induction (sad music & sad thoughts)
moodvid <- paste0(gendir,mooddir,"/VidPlay_Merged_cleaned.xlsx")
VidTask.df = read_excel(moodvid, sheet = "VidPlay_Merged_cleaned", col_names = TRUE)
#subset to use only the vars we need
#include only: participants in the neg mood condition; take out 1st mood rating (after video) & 2nd (after musics)
VidMood <- subset(VidTask.df,Procedure == "MoodProcExp", select = c(Subject,Mood.Valence,Mood2ndMan.Valence))
#participants who did not feel sad after video and have to go to 2nd mood induction
notsad1st <- VidMood[which(VidMood$Mood.Valence >= 3), ]
#make sure that they did have 2nd mood induction
Did2ndMood <- VidMood$Subject[!is.na(VidMood$Mood2ndMan.Valence)]
Did2ndMood == notsad1st$Subject
#check to see who's not sad after 2nd mood induction
notsad2nd <- VidMood[which(VidMood$Mood2ndMan.Valence >= 3), ]

### MOOD RATING DURING PICTURE VIEWING#### 
##read the file with the main task data
mt.path <- paste0(gendir,mooddir,"/GRF1314MoodDataAll_cleaned.txt")
#read in sheet 2, which only has scores for identification with Chinese culture
MainTask.df = read.table(mt.path, header = TRUE)

##review your data; if you convert spss files into csv the var
#labels will be lost to make sure you know what your vars mean
str(MainTask.df)

#change var names to be more comprehensible
names(MainTask.df)[names(MainTask.df) == 'Block'] <- 'TrialNum'
names(MainTask.df)[names(MainTask.df) == 'Stim.Arousal'] <- 'ArousalRating'
names(MainTask.df)[names(MainTask.df) == 'Stim.Valence'] <- 'ValenceRating'
names(MainTask.df)[names(MainTask.df) == 'StimuliList'] <- 'picID'

#fix an ID issue (don't use double quote, would turn var into string)
MainTask.df[MainTask.df == 6172] <- 76172

##assign subject age and culture and mood condition

MainTask.df$age <- ifelse((MainTask.df$Subject >= 1001 & MainTask.df$Subject <= 1999)|
                                (MainTask.df$Subject >= 76100 & MainTask.df$Subject <= 76199)|
                                  (MainTask.df$Subject >= 76300 & MainTask.df$Subject <= 76399),1,
                                ifelse((MainTask.df$Subject >= 1 & MainTask.df$Subject <= 100)|
                                (MainTask.df$Subject >= 76200 & MainTask.df$Subject <= 76299)|
                                (MainTask.df$Subject >= 76400 & MainTask.df$Subject <= 76499),0,"NA"))

#the levels part of this command makes sure "young" goes first (reference category for analysis later)
#or "factor" will just use alphabetical order
MainTask.df$age = factor(MainTask.df$age,
                         levels = c("0","1"),
                         labels = c("young","old"))

#subject culture
MainTask.df$culture <- ifelse((MainTask.df$Subject >= 1 & MainTask.df$Subject <= 1999),0,
                          ifelse(MainTask.df$Subject >= 76100 & MainTask.df$Subject <= 76499,1,"NA"))

table(MainTask.df$culture)
MainTask.df$culture = factor(MainTask.df$culture,
                             levels = c("0","1"),
                             labels = c("HK","US"))

#change "culture" to "subjculture" for clarit, not to confuse with picture culture
colnames(MainTask.df)[grep("^culture$", colnames(MainTask.df))] <- "subjculture"

#mood condition (even number (%% = division modulus) is control)
MainTask.df$moodcond <- ifelse(MainTask.df$Subject %% 2 == 0, "control","negative")
MainTask.df$moodcond <- factor(MainTask.df$moodcond)

table(MainTask.df$moodcond)

#exclude participants who did not feel sad after 2nd manipulation
MainTask.full <- MainTask.df
MainTask.df <- MainTask.full[!(MainTask.full$Subject %in% notsad2nd$Subject),]

### ADD STIM RATING DATA ####
#add the picture rating variables (valence and arousal) to this data file (rated by a separate group of sjs)
pic.db = paste0(gendir,stimdir,"/ChineseIAPS_final.sav")
#R will give you a warning, but it's fine; check your data if it was imported ok
#can use the pkg memisc but for some reason that package can't import string variables with labels (turned into NA)
mySPSSData <- read.spss(pic.db,to.data.frame=TRUE)

#even pictures are US pics, odd pics are Chinese
picculture <- ifelse(mySPSSData$Study2PicID %% 2 == 0, "1","0")
picculture <- factor(picculture,
                     levels = c("0","1"),
                     labels = c("HK","US"))

pic.df.subset <- mySPSSData[,c("Study2PicID","Avg_Rating_Arousal","Avg_Rating.Valence",
                               "Avg_Rating_CulturalRelevance","H","W","LZPixel",
                               "PicArea","TotalArea","LZPicArea","LZTotal")]

pic.df.subset[,"picculture"] <- picculture

#merge the picture data (wide format) with the mood data (long format)
#explicitly, can do: merge(pic.df.subset, MainTask.df, by = "Study2PicID", all = TRUE, sort = FALSE)
#this merged dataset doesn't have data from the filler pictures because the argument "all" wasn't specified
#that means the extra rows (from fillers, pic 39-45) were dropped from the merged data
MoodRating.df <- merge(pic.df.subset,MainTask.df,by.x = "Study2PicID",by.y = "picID",all = TRUE)

#rename to clarify which ratings were from the independent raters for the pics
names(MoodRating.df)[names(MoodRating.df) == 'Avg_Rating_Arousal'] <- 'picArousal'
names(MoodRating.df)[names(MoodRating.df) == 'Avg_Rating.Valence'] <- 'picValence'
names(MoodRating.df)[names(MoodRating.df) == 'Avg_Rating_CulturalRelevance'] <- 'picCultrCtnuous'

#make sure nominal variables are strings and not numeric
MoodRating.df$picID <- sprintf('%d',MoodRating.df$Study2PicID)

### ADD VIA DATA ####
#this is the VIA file with problematic subject IDs already edited
VIApath <- paste0(gendir,"/qualtrics_data/VIA/VIA_all_March2017.xlsx")
VIA.df <- read_excel(VIApath,sheet = "cleaned_data")

names(VIA.df)[names(VIA.df) == 'Q5 - Subject ID'] <- 'Subject'

#before merging, find out which subjects are missing from which dataset
MoodRating.sjlist <- unique(MoodRating.df$Subject)
VIA.sjlist <- unique(VIA.df$Subject)
diff <- MoodRating.sjlist %in% VIA.sjlist
#find which IDs were in MoodRating but not in VIA
MoodRating.sjlist[which(diff == FALSE)]

#calculate culture scores
CulScoreMean <- function(df, culscoretype){
  Cultype <- df[c(grep(culscoretype,names(df),value=TRUE))]
  Culmean <- rowMeans(Cultype,na.rm = TRUE)
  return(Culmean)
}

#heritage culture = HR
HRCulMean <- CulScoreMean(VIA.df,"HR")
OwnCulMean <- CulScoreMean(VIA.df,"own")
OtherCulMean <- CulScoreMean(VIA.df,"other")

#create the df with just the means
VIAmeans <- data.frame(VIA.df[c("Subject","heritageCulture")],HRCulMean,OwnCulMean,OtherCulMean, stringsAsFactors=FALSE)

#add VIA data to MoodRating data
MoodVIA.df <- merge(VIAmeans,MoodRating.df,by=c("Subject"), all = TRUE)

###ADD FIXATION DATA####
# (to use fixation as a predictor)
load("cleaned_eyedata.Rda")
names(cleaned_data)[names(cleaned_data) == 'Study2PicID'] <- 'picID'

#because I added "2" to HK YA's ID,which were just "001", "002" etc. in the eye data
#and the numbers were read as "1","2",etc. in the mood data
#so we need to change both to make them the same
#add the character 2 if there are more than 5 digits
add2 <- function(x) ifelse(nchar(x) <4,paste0("2",x),x)
MoodVIA.df$Subject <- sprintf("%03d",MoodVIA.df$Subject)
MoodVIA.df$Subject <- sapply(MoodVIA.df$Subject, add2)

#find out which IDs are not in the eyedata 
eye.sjlist <- unique(cleaned_data$Subject)
MoodVIA.sjlist <- unique(MoodVIA.df$Subject)

eyemoodDiff <- MoodVIA.sjlist %in% eye.sjlist
MoodVIA.sjlist[which(eyemoodDiff == FALSE)]

#even though merge didn't need the "by" argument, if we don't specify it, it returns an empty dataframe
#with the eyedata that had 80% OUTSIDE fixation excluded
MoodEye.df <- merge(cleaned_data,MoodVIA.df,by=c("Subject","picID","LZPicArea"),all = TRUE)

MoodEye.df$picIDNum <- as.numeric(MoodEye.df$picID)
# sort by subject and picID
sortedMoodEye <- MoodEye.df[order(MoodEye.df$Subject, MoodEye.df$picIDNum),]

#save this df
save(sortedMoodEye,file="MoodEye.Rda")


