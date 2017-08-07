###CLEAN EYETRACKING DATA####

#general dir for GRF1314, as other data files are not stored in the R project folder
#for administrative purposes
#because the working dir for an R project, when loaded into Rstudio, is always the folder
#with the R project file, we go up one level to get to the general dir
gendir <- dirname(getwd())

##import the eye data file
# this data file was already somewhat processed by SPSS, and has picture-level data added by SPSS
#this data file was pre-processed with SPSS; next time should start with raw data
datafile = read.csv(paste0(gendir,"/eye_data/spss/eyedata_AOI_restructured_all_4.csv"), header=TRUE)
#you can also just put file names in the argument space for the function read.csv

##review the names of your vars; if you convert spss files into csv the var
#labels will be lost to make sure you know what your vars mean
str(datafile)

#for some reason the first var name always has a "i.." in front of it, so we
#need to change the names ourselves (editing in spss or notepad wont help)
names(datafile)[grep('SubID',names(datafile))]<-'SubID'

#relabel var names for ease of typing!For this dataset, vars end in 00 are for OUTSIDE fixation
#vars end in 1.00 are for AOI fixation
names(datafile)[names(datafile)=='TotalFixDur_Percent_sum..00']<-'FixDurPercent_OUTSIDE'
names(datafile)[names(datafile)=='TotalFixDur_Percent_sum.1.00']<-'FixDurPercent_AOIs'

## data screening
## accuracy & missing data
summary(datafile)

##factor our categorical IV
datafile$picvalenceCAT = factor(datafile$Pic_valence,
                               levels = c(1,2,3),
                               labels = c("positive", "neutral", "negative"))
#create a new data frame with just the variables you care about (eliminate some vars that will already be in the Mood
#dataset)
eye.df = data.frame(datafile[c("SubID","EventNo","Study2PicID","FixDurPercent_AOIs","FixDurPercent_OUTSIDE","LZPicArea","picvalenceCAT")])

names(eye.df)[names(eye.df)=='SubID']<-'Subject'

#find subjects with more than 80% of total fixation duration outside of AOIs; 
#this is a high criterion because AOIs are sometimes small
#keep rows that have OUTSIDE fixation less than 80%
includetrials <- eye.df$FixDurPercent_OUTSIDE<80
#index into these rows for the entire dataframe
cleaned_data = eye.df[includetrials,]
#save this dataset for use later
save(cleaned_data,file="cleaned_eyedata.Rda")
save(eye.df,file="all_eyedata.Rda")

