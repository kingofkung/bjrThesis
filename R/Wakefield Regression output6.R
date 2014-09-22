# Wakefield Modeling Project Master's Thesis Edition:
# A Ben Rogers Joint
# Started 4/9/2014
# Last Edited 9/15/2014
 

rm(list = ls())
ptr <- proc.time()
#Go to correct file location.  

inputwd <- "/Users/bjr/Google Drive/Activate/Wakefield Modelling Project/Wakefield Input 2"
setwd(inputwd)
outputwd <- "/Users/bjr/Google Drive/Activate/Wakefield Modelling Project/Wakefield Output 2/Wakefield Regression Outputs"
dateUnformed <-  date()
dateFormed <-  strsplit(dateUnformed, split = ' ')
dateFormed <- paste(dateFormed[[1]][c(2,4)], collapse = ' ')


#Load relevant packages

library(car)
library(xlsx)
library(gdata)
library(rockchalk)
# library(betareg)
library(caret) #loads ggplot2
# library(nnet)
# library(e1071)
# library(randomForest)


#Function Section 

 
 
 
 #Read in the files:
#Bit of a problem here. Mark wants us to read in files from two separate segments of surveying. Now we know which segments are 

# maindf <-  read.csv("FivePrimaryVoterInfo.csv") #Appears to be the main dataset, includes the DV

mdf1 <- read.csv("Survey Round 1.csv")
data.frame(colnames(mdf1)) #Note: mdf question columns start at sp02 and end at sp08. The two that vary are sp02 and sp07
#To deal with this, we separate out the questions that are different between surveys, and add a couple of new columns on for new questions
mdf1qs <- mdf1[, c('voterid', 'firstname', 'lastname','sp02', 'sp07')]
# mdf1qs$sp02.2 <- factor('')
# mdf1qs$sp07.2 <- factor('')
colnames(mdf1qs)[2] <- 'firstnameq1'
colnames(mdf1qs)[3] <- 'lastnameq1'

length(rownames(mdf1qs))
head(mdf1qs)

mdf2 <- read.csv("Survey Round 2.csv") #Then, we place the new questions in the new columns, and clear the former sp02 and sp07 columns. 
mdf2qs <- mdf2[, c('voterid', 'firstname', 'lastname','sp02', 'sp07')]
str(mdf2qs)
mdf2qs$sp02.2 <- mdf2qs$sp02
mdf2qs$sp07.2 <- mdf2qs$sp07
colnames(mdf2qs)[2] <- 'firstnameq2'
colnames(mdf2qs)[3] <- 'lastnameq2'

maindf <- rbind(mdf1[,c(1:33, 35:38, 40:41)], mdf2[, c(1:33, 35:38, 40:41)]) #This grabs  everything that stays the same in the two datasets. Right now that's all but sp02, sp07, but this can change when we get new data. 
maindf <-  cbind(maindf, mdf1qs[match(maindf$voterid, mdf1qs$voterid), c('sp02', 'sp07', 'lastnameq1')]  )
maindf <-  cbind(maindf, mdf2qs[match(maindf$voterid, mdf2qs$voterid), c('sp02.2', 'sp07.2', 'lastnameq2')]  )

str(maindf)

set.seed(12345)
maindf[sample(1:nrow(maindf), size = 25),c('lastname', 'lastnameq1', 'lastnameq2')] #This is a consistency check. It checks out.

length(rownames(maindf))

otherdat <- read.csv('wakefield_prelim_data_pull.csv') #bring in an IV Dataset
Vandat <- read.csv('IVDat2.csv')
Vandensinc  <- read.csv("wakefield_data_export.csv")
head(Vandensinc)
#purge commas and Match density and income data to Vandat

str(Vandensinc)

Vandensinc$votebuilder_identifier2 <- lapply(Vandensinc$votebuilder_identifier,FUN = as.character)
Vandensinc$votebuilder_identifier2 <- gsub(',', '', Vandensinc$votebuilder_identifier2)
Vandensinc$votebuilder_identifier2 <- as.numeric(Vandensinc$votebuilder_identifier2)

Vandensinc$census_2000_medianincome2 <- lapply(Vandensinc$census_2000_medianincome,FUN = as.character)
Vandensinc$census_2000_medianincome2 <- gsub(',', '', Vandensinc$census_2000_medianincome2)
Vandensinc$census_2000_medianincome2 <- as.numeric(Vandensinc$census_2000_medianincome2)


Vandensinc$density_land_sq_km2 <- lapply(Vandensinc$density_land_sq_km,FUN = as.character)
Vandensinc$density_land_sq_km2 <- gsub(',', '', Vandensinc$density_land_sq_km2)
Vandensinc$density_land_sq_km2 <- as.numeric(Vandensinc$density_land_sq_km2)




#Match IV data to the main dataset
otherdatarows <- match(maindf$voterid, otherdat$votebuilder_identifier)
Vandatrows <- match(maindf$voterid, Vandat$VANID)
# otherdatarows2 <- match(maindf$voterid, otherdat2$VANID)
colnames(otherdat)
maindf <- cbind(maindf, Vandat[Vandatrows,])
maindf <- cbind(maindf, otherdat[otherdatarows,])
maindf$namecheck <- otherdat[otherdatarows, 'votebuilder_identifier']
 # na.omit(data.frame(maindf$voterid, maindf$namecheck)) #Checks out.
# data.frame(maindf[which(maindf$sp03 != ""), ])[1521,]

#Match IV data to Vandat
VandatMatcherrows <- match(Vandat$VANID, Vandensinc$votebuilder_identifier2)

Vandat <- cbind(Vandat, Vandensinc[VandatMatcherrows,])
head(Vandat[, c('VANID','votebuilder_identifier2') ])

#rename relevant columns to correct names 'cen10_densityRec', 'cen00_medianincomeRec'
colnames(Vandat)[ which(colnames( Vandat) == 'census_2000_medianincome2')] <- 'cen00_medianincomeRec'

colnames(Vandat)[ which(colnames( Vandat) == 'density_land_sq_km2')] <- 'cen10_densityRec'


#general guess: Are you a heavy 
#match in data

#Get Jake ID Numbers of everyone we want 'full' IV data for.


as.character(levels(mdf1qs$sp02))

# print(mdf1qs$sp02)

topullVANS <- c(maindf[  which(maindf$sp02 != "" & maindf$sp02 != " "),'voterid'], maindf[  which(maindf$sp02.2 != "" & maindf$sp02.2 != " "),'voterid'])

topullVANS == 1544185
length(topullVANS)

colnames(maindf)[33]

# maindf[which( maindf$voterid %in% topullVANS ), c('lastname', 'lastnameq1', 'sp02', 'sp02.2', 'sp03')]

# write.csv(topullVANS, file = 'topull completed2.csv')

 


# Recoding section,  
#In data for wakefield, need to recode sp02-sp08
#Mark would like to see 'movement' variables, in that we determine how people's opinions change between one question to the next. I'm going to try and put together a set that includes 
# a. how people moved on a question compared to the first question with a numeric option (sp03, first ID), which We will refert to as the average movement.
# b. how people moved on a question compared to the question preceeding it, which we will call instantaneous movement.
# c. how people moved from a question to the final iD
# recode sp02
maindf$sp02Rec <- maindf$sp02 #save value in new variable, so I can redo this without issue
levels(maindf$sp02Rec) <- c(levels(maindf$sp02Rec), "Higher than $10.10", "Raise it to $10.10", "Raise it to $9.00", "Leave it as is", "Eliminate the Minimum Wage")

maindf$sp02Rec[which(maindf$sp02Rec %in% c("6 - Would not answer ", "5 - Not sure/no opinion"))] <- NA
maindf$sp02Rec[which(maindf$sp02Rec %in% "0 - Higher than $10.10")] <- "Higher than $10.10"
maindf$sp02Rec[which(maindf$sp02Rec %in% "1 - Raise to $10.10")] <- "Raise it to $10.10"
maindf$sp02Rec[which(maindf$sp02Rec %in% "2 - Raise to $9.00")] <- "Raise it to $9.00"
maindf$sp02Rec[which(maindf$sp02Rec %in% "3 - Leave it as is")] <- "Leave it as is"
maindf$sp02Rec[which(maindf$sp02Rec %in% " 4 - Eliminate the minimum wage")] <- "Eliminate the Minimum Wage"


 # " 4 - Eliminate the minimum wage" "0 - Higher than $10.10"         
 # "1 - Raise to $10.10"             "2 - Raise to $9.00"             
 # "3 - Leave it as is"              "5 - Not sure/no opinion"        
 # "6 - Would not answer "          



maindf$sp02Rec[which(maindf$sp02Rec %in% c('', " "))] <- NA
maindf$sp02Rec <- factor(maindf$sp02Rec) #eliminate the extra levels
levels(maindf$sp02Rec)


# recode sp02.2
maindf$sp02.2Rec <- maindf$sp02.2


#I decided to separate the final output of the titles from the variable inputs in the recoded function, as 
passopt <- "Pass the Paycheck Fairness Act"
noneedopt <- "No, a Federal Law is Not Needed"
noprobopt <- "No Problems with Gender Equity"

levels(maindf$sp02.2Rec) <- c(levels(maindf$sp02.2Rec), passopt, noneedopt, noprobopt)
maindf$sp02.2Rec[which(maindf$sp02.2Rec %in% c("3 - Not sure/no opinion", " 4 - Wouldn't answer "))] <- NA 
maindf$sp02.2Rec[which(maindf$sp02.2Rec %in% c("0 - Yes- pass the Paycheck Fairness Act"))] <- passopt
maindf$sp02.2Rec[which(maindf$sp02.2Rec %in% c("1 - No- a federal law is not needed"))] <- noneedopt
maindf$sp02.2Rec[which(maindf$sp02.2Rec %in% c("2 -No- there are no problems with gender equity"))] <-  noprobopt









#Recode sp03
specificrecode <- function(varRec){
#This function will take our special field variables and return them as recoded in the specific manner described below. Please note that for this function, the levels have to be exactly the same, so 

levels(varRec) <- c(levels(varRec), "Supports Wakefield",  "Undecided","Supports Jenkins") #add the new values as categories

#Recode level
varRec[which(varRec %in% c('', " 0 - Decline to answer ", " 0 - Decline to answer  " ))] <- NA
varRec[which(varRec %in% c("1 - Supports Lynn", "2", "3", "4"))] <- "Supports Jenkins"
varRec[which(varRec %in% "5")] <- "Undecided"
varRec[which(varRec %in% c("6", "7","8","9", "10 - Supports Margie"))] <- "Supports Wakefield"

rts <- factor(varRec)

return(rts)

}

specificrecode2 <- function(varRec){
#This function will take our special field variables and return them as recoded in the specific manner described below. Please note that for this function, the levels have to be exactly the same, so 

levels(varRec) <- c(levels(varRec),'10', '1','0') #add the new values as categories

#Recode level
varRec[which(varRec %in% c('', " 0 - Decline to answer ", " 0 - Decline to answer  " ))] <- NA
varRec[which(varRec %in% c("1 - Supports Lynn"))] <- "1"

varRec[which(varRec %in% c("10 - Supports Margie"))] <- "10"



# varRec[which(varRec %in% c('1','2','3','4','5'))] <- '0'
# varRec[which(varRec %in% c('6','7','8','9','10'))] <- '1'

 rts <- as.numeric(as.character((varRec)))

return(rts)

}





binRec <- function(varRec, lvlsToZero, lvlsToOne, lvlsToNa){
#This function will take our special field variables and return them as recoded as a binary value. These values are Please note that for this function, the levels have to be exactly the same, so 

levels(varRec) <- c(levels(varRec), '1','0', '999') #add the new values as categories

#Recode level
varRec[which(varRec %in% lvlsToOne)] <- "1"

varRec[which(varRec %in% lvlsToZero)] <- "0"
varRec[which(varRec %in%  lvlsToNa)] <- '999'


rts <- as.numeric(as.character((varRec))) #if this starts returning an NA's introduced by coercion, really watch out. It's very likely that we've forgotten a level, or mistyped it into the function. 

# rts <- as.character(varRec)

rts[which(rts == '999')] <- NA

return(rts)

}

# Levels for binary recode
zeroLvls <- c("1 - Supports Lynn", "2", "3", "4", "5",  "6")
oneLvls <- c('7', '8', '9', "10 - Supports Margie")
naLvls <- c("", " 0 - Decline to answer ", " 0 - Decline to answer  " )

maindf$sp03Rec <- specificrecode(maindf$sp03)
# maindf$sp03Rec[!is.na(maindf$sp03Rec)]

maindf$sp03Rec2 <- specificrecode2(maindf$sp03)


levels(maindf$sp03)
maindf$sp03SuppRec <- binRec(maindf$sp03, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)


 # data.frame(maindf$sp03, maindf$sp03SuppRec)[!is.na(maindf$sp03SuppRec),]




maindf$sp04Rec <- specificrecode(maindf$sp04)
# maindf$sp04Rec[which(maindf$sp04Rec %in% " 0 - Decline to answer  ")] <- NA #As I mentioned earlier, the specific recode only recodes very specific variables, and since the 0 level changes slightly between  these factors, I have to go through and recode this as a separate command. .
# maindf$sp04Rec <- factor(maindf$sp04Rec)
# print(maindf$sp04Rec[!is.na(maindf$sp04Rec)]) #I always forget how to make sure that we're not looking at NA responses, but this does the trick.

maindf$sp04Rec2 <- specificrecode2(maindf$sp04)
maindf$sp04avgmove <- maindf$sp04Rec2 - maindf$sp03Rec2
# maindf$sp04instmove <- maindf$sp04avgmove #in this case, average movement == instantaneous movement 
levels(maindf$sp04)
maindf$sp04SuppRec <- binRec(maindf$sp04, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)


# data.frame(maindf$sp04, maindf$sp04SuppRec)[is.na(maindf$sp04SuppRec),]


maindf$sp05Rec <- specificrecode(maindf$sp05)
levels(maindf$sp05Rec)
# maindf$sp05Rec[which(maindf$sp05Rec %in% " 0 - Decline to answer  " )] <- NA
# maindf$sp05Rec <- factor(maindf$sp05Rec)
# print( maindf$sp05Rec[!is.na(maindf$sp05Rec)])

maindf$sp05SuppRec <- binRec(maindf$sp05, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)


maindf$sp05Rec2 <- specificrecode2(maindf$sp05)
maindf$sp05avgmove <- maindf$sp05Rec2 - maindf$sp03Rec2
# maindf$sp05instmove <- maindf$sp05Rec2 - maindf$sp04Rec2  #in this case, average movement == instantaneous movement 



maindf$sp06Rec <- specificrecode(maindf$sp06)
levels(maindf$sp06Rec)

# maindf$sp06Rec[which(maindf$sp06Rec %in% " 0 - Decline to answer  " )] <- NA

# maindf$sp06Rec <- factor(maindf$sp06Rec)
maindf$sp06Rec[!is.na(maindf$sp06Rec)]

maindf$sp06Rec2 <- specificrecode2(maindf$sp06)
# print(maindf[!is.na(maindf$sp06Rec2),c('sp03Rec2','sp04Rec2','sp05Rec2','sp06Rec2')])

maindf$sp06Rec2 <- specificrecode2(maindf$sp06)
maindf$sp06avgmove <- maindf$sp06Rec2 - maindf$sp03Rec2
# maindf$sp06instmove <- maindf$sp06Rec2 - maindf$sp05Rec2  #in this case, average movement == instantaneous movement 

maindf$sp06SuppRec <- binRec(maindf$sp06, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)




#Koch bros question. We'll need to reconsider how we recode this one. 
maindf$sp07Rec <- maindf$sp07
levels(maindf$sp07Rec) <- c(levels(maindf$sp07Rec), "Kochs Negative Impact","Uncertain about Kochs","Kochs Positive Impact") #Add levels to cross tabulation


maindf$sp07Rec[which(maindf$sp07Rec %in% c("", " 0 - Decline to answer  ") )] <- NA

maindf$sp07Rec[which(maindf$sp07Rec %in% c("1 - Positive Impact", "2", "3" ) )] <- "Kochs Positive Impact"
maindf$sp07Rec[which(maindf$sp07Rec %in% c("4", "5", "6" ) )] <- "Uncertain about Kochs"
maindf$sp07Rec[which(maindf$sp07Rec %in% c("7", "8", "9", "10 - Negative Impact" ) )] <- "Kochs Negative Impact"

maindf$sp07Rec <- factor(maindf$sp07Rec)
# print(maindf$sp07Rec[!is.na(maindf$sp07Rec)])



#recodesp07.2


maindf$sp07.2Rec <- maindf$sp07.2


lowregpref <-  "Less Regulation"
medregpref <-  "Some Regulation"
highregpref <- "More Regulation"
levels(maindf$sp07.2Rec) <- c(levels(maindf$sp07.2Rec), lowregpref, medregpref, highregpref) #Need to add levels
maindf$sp07.2Rec[which(maindf$sp07.2Rec %in% c("", " 0 - Decline to answer  ") )] <- NA
maindf$sp07.2Rec[which(maindf$sp07.2Rec %in% c("1 - Less regulation", "2", "3"))] <- lowregpref
maindf$sp07.2Rec[which(maindf$sp07.2Rec %in% c("4", "5", "6"))] <- medregpref
maindf$sp07.2Rec[which(maindf$sp07.2Rec %in% c("7", "8", "9", "10 - More regulation" ) )] <- highregpref


maindf$sp07.2Rec <- factor(maindf$sp07.2Rec)
#print( maindf$sp07.2Rec[!is.na(maindf$sp07.2Rec)])



maindf$sp08Rec <- specificrecode(maindf$sp08)
levels(maindf$sp08Rec)

maindf$sp08Rec2 <- specificrecode2(maindf$sp08)

maindf$sp08avgmove <- maindf$sp08Rec2 - maindf$sp03Rec2

maindf$sp08SuppRec <- binRec(maindf$sp08, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)






# Note to self: parenthesis means everything more or less than, while square bracket means equal to when creating breaks



Agebreaks <- c(17, 30, 45, 64, 200)
# data.frame("age" = maindf$Age, "cat_age" = cut(maindf$Age, breaks = Agebreaks)) #Look at ages
 Agelabels <- c('18-30', '31-45', '46-64', '65+')
# clear(Agelabels)

maindf$cat_age <- cut(maindf$Age, breaks = Agebreaks , labels = Agelabels)
Vandat$cat_age <- cut(Vandat$Age, breaks = Agebreaks , labels = Agelabels)

#Use one of these to create a more Demarcated (hence the De) set of agebreaks 
AgebreaksDe <- c(17, 25,30, 35,40, 45, 50, 55,60, 65, 200)
AgelabelsDe <- c('18-25', '26-30', '31-35','36-40','41-45', '46-50', '51-55', '56-60', '61-64', '65+')

maindf$cat_ageDe <- cut(maindf$Age, breaks = AgebreaksDe, labels = AgelabelsDe)


summary(maindf$Party)

#remove all green and libertarian predicted
maindf$PartyRec <- maindf$Party
levels(maindf$PartyRec) <- c(levels(maindf$PartyRec), "Democratic", "Republican", "Unaffiliated")
maindf$PartyRec[which(maindf$PartyRec %in% c("L", "U") )] <- "Unaffiliated" #get rid of the odd parties and unknowns
maindf$PartyRec[which(maindf$PartyRec %in% "D") ] <- "Democratic"
maindf$PartyRec[which(maindf$PartyRec %in% "R") ] <- "Republican" 
maindf$PartyRec <- factor(maindf$PartyRec) #Remove the odd parties levels



#repeat for Vandat
Vandat$PartyRec <- Vandat$Party
levels(Vandat$PartyRec) <- c(levels(Vandat$PartyRec), "Democratic", "Republican", "Unaffiliated")
Vandat$PartyRec[which(Vandat$PartyRec %in% c("L", "U") )] <- "Unaffiliated" #get rid of the odd parties and unknowns
Vandat$PartyRec[which(Vandat$PartyRec %in% "D") ] <- "Democratic"
Vandat$PartyRec[which(Vandat$PartyRec %in% "R") ] <- "Republican" 
Vandat$PartyRec <- factor(Vandat$PartyRec) #Remove the odd parties levels


maindf$Sex[which(maindf$Sex == 'U')] <- NA
maindf$Sex <- factor(maindf$Sex)
levels(maindf$Sex)


#Recode sex in Vandat to match maindf

Vandat$Sex[which(Vandat$Sex == 'U')] <- NA
Vandat$Sex <- factor(Vandat$Sex)
levels(Vandat$Sex)


 #Recode factors as numeric
is.na( maindf$cen00_medianincome)

maindf$cen00_medianincomeRec <- lapply(maindf$cen00_medianincome, as.character)
maindf$cen00_medianincomeRec <- gsub(',', '', maindf$cen00_medianincomeRec)

maindf$cen00_medianincomeRec <- as.numeric(maindf$cen00_medianincomeRec)



maindf$cen10_densityRec <- lapply(maindf$cen10_density, as.character)
maindf$cen10_densityRec <- gsub(',', '', maindf$cen10_densityRec)

maindf$cen10_densityRec <- as.numeric(maindf$cen10_densityRec)


summary(maindf$cons_ppi)

maindf$cons_ppiRec <- lapply(maindf$cons_ppi, as.character)
maindf$cons_ppiRec <- gsub(",", '',maindf$cons_ppiRec)
maindf$cons_ppiRec <- as.numeric(maindf$cons_ppiRec)


maindf$reg_earliest_yearRec <- lapply(maindf$reg_earliest_year, as.character)
maindf$reg_earliest_yearRec <- gsub(",", '', maindf$reg_earliest_yearRec)
maindf$reg_earliest_yearRec <- as.numeric(maindf$reg_earliest_yearRec)

maindf$donate_max_demRec <- lapply(maindf$donate_max_dem, as.character)
maindf$donate_max_demRec <- gsub(",", '', maindf$donate_max_demRec)
maindf$donate_max_demRec <- as.numeric(maindf$donate_max_demRec)



na.omit(data.frame(maindf$cons_ppi, maindf$cons_ppiRec))

#
# recode datasets information (proxy for region)
maindf$dsnRec <- maindf$datasource_name #Shortened, since this is pretty long


levels(maindf$dsnRec)


Douglevels <- levels(maindf$dsnRec)[ grep('Douglas', levels(maindf$dsnRec))]
Leavenlevels <- levels(maindf$dsnRec)[ grep('Leavenworth', levels(maindf$dsnRec))]
Shawnlevels <- levels(maindf$dsnRec)[ grep('Shawnee', levels(maindf$dsnRec))]
SElevels <- levels(maindf$dsnRec)[ grep('Southeast', levels(maindf$dsnRec))]
SElevels <-c(SElevels, levels(maindf$dsnRec)[ grep('SESampleCellExport', levels(maindf$dsnRec))])

NElevels <- levels(maindf$dsnRec)[ grep('Northeast', levels(maindf$dsnRec))]
NElevels <-c(NElevels, levels(maindf$dsnRec)[ grep('NESampleCellExport', levels(maindf$dsnRec))])


levels(maindf$dsnRec) <- c(levels(maindf$dsnRec), "Northeast", "Southeast", "Douglas", "Leavenworth", "Shawnee") #add simplified data to the region


#recode. this just seemed easiest, since the recodes were 1-1
maindf$dsnRec[ which(maindf$dsnRec %in% Douglevels)] <- "Douglas"
maindf$dsnRec[ which(maindf$dsnRec %in% Leavenlevels)] <- "Leavenworth"
maindf$dsnRec[ which(maindf$dsnRec %in% Shawnlevels)] <- "Shawnee"
maindf$dsnRec[ which(maindf$dsnRec %in% SElevels)] <- "Southeast"
maindf$dsnRec[ which(maindf$dsnRec %in% NElevels)] <- "Northeast"

maindf$dsnRec <- factor(maindf$dsnRec) #and remove old levels
 levels(maindf$dsnRec)

maindf$dougDummy <-0
maindf$dougDummy[which(maindf$dsnRec == 'Douglas')] <- 1

maindf$leavenDummy <-0
maindf$leavenDummy[which(maindf$dsnRec == 'Leavenworth')] <- 1

maindf$shawnDummy <- 0
maindf$shawnDummy[which(maindf$dsnRec == 'Shawnee')] <- 1

maindf$neDummy <- 0
maindf$neDummy[which(maindf$dsnRec == 'Northeast')] <- 1

maindf$seDummy <- 0
maindf$seDummy[which(maindf$dsnRec == 'Southeast')] <- 1

# #separate maindf into three dataframes: 
# # maindf which contains dv and Iv information
# # matchdf, which only has IVs for unmatched people
# # and controldf, which lets us find out how well our model is performing
# maindf$rownum <- as.integer(rownames(maindf))
# maindfrows <- match( dvdf$sp01,  maindf$VANID) #get the rows that match vanids
as.character(levels(maindf$sp03))

maindf2 <- maindf[which(is.na(maindf$votebuilder_identifier) == F & is.na(maindf$sp03) == F & maindf$sp03 != '' ),]

maindf2$sp03
#Way to check % of records that are NA's
length(na.omit(maindf2[, 'cen10_densityRec']))/length(maindf2[, 'cen10_densityRec'])
complete.cases(maindf2[,'cen00_medianincomeRec'])


nrow(maindf2) 

paste("'", paste(colnames(Vandat), collapse = "', '"), "'", sep = '') #OK, so check it out. These nested paste statements allow me to not only paste the colnames of Vandat without resorting to that cumbersome do.call command, but it also allows me to add on a set of quotations to the outside, making copy and pasting the ones we want removed really easy. 

#get rid of categorical columns
colstoelim <- c("project_name",'lastnameq2', 'dispositionid','datasource_name',  'agent_name', 'datelastcalled', 'firstname', 'lastname', 'lastnameq1','lastnameq2', 'address1', 'address2', 'country', 'countrycode', 'activephone', 'phonenumber1', 'phonenumber2','fips', 'fkey', 'pd01', 'pd02', 'pd03', 'pd04', 'pd05', 'pd06', 'pd07', 'pd08', 'pd09','sp02','sp02.2', 'sp02.2Rec', 'sp07', 'sp07.2Rec','sp07.2', 'sp09', 'sp03', 'sp01','sp04','sp05','sp06','sp08', 'cen00_medianincome', 'cen10_density', 'sp02Rec' ,"sp03Rec", "sp04Rec", "sp05Rec", "sp06Rec", "sp07Rec", "sp08Rec", 'cons_ppi', 'city', 'reg_earliest_year', 'state', 'donate_max_dem', 'mAddress', 'mCity', 'mState', 'mZip5', 'mZip4', 'vAddress', 'City', 'State', 'Zip5', 'Zip4', 'LastName', 'FirstName', 'MiddleName', 'Suffix', 'Email')

novarcols <- ('voted_10p')

colstoelim <- c(colstoelim, novarcols)

for(i in 1:length(colstoelim)) maindf2[,colstoelim[i]] <- NULL
#Create correlation matrix, remove anything that correlates too highly with prior IVs

#There's a problem though. Correlation matrices require numeric predictors, and maindf2 is partially factor, mostly integer values

#we need to create dummy vars for party, cat_age, and Sex

# # maindf2$SexF <- NA
# maindf2[which(maindf2[,'Sex'] == 'F'), 'SexF' ] <- 1
# maindf2[which(maindf2[,'Sex'] == 'M'), 'SexF' ] <- 0


# levels( maindf2$cat_age) #"18-30" "31-45" "46-64" "65+
# maindf2$cat_age18_30 <- NA
# maindf2[which(maindf2[,'cat_age'] == '18-30'), 'cat_age18_30' ] <- 1
# maindf2[which(maindf2[,'cat_age'] != '18-30' & is.na(maindf2[,'cat_age']) == F), 'cat_age18_30' ] <- 0
# # maindf2[,c('cat_age', 'cat_age18_30')]

# maindf2$cat_age31_45 <- NA
# maindf2[which(maindf2[,'cat_age'] == '31-45'), 'cat_age31_45' ] <- 1
# maindf2[which(maindf2[,'cat_age'] != '31-45' & is.na(maindf2[,'cat_age']) == F), 'cat_age31_45' ] <- 0
# # maindf2[,c('cat_age', 'cat_age31_45')]


# maindf2$cat_age46_64 <- NA
# maindf2[which(maindf2[,'cat_age'] == '46-64'), 'cat_age46_64' ] <- 1
# maindf2[which(maindf2[,'cat_age'] != '46-64' & is.na(maindf2[,'cat_age']) == F), 'cat_age46_64' ] <- 0
# # maindf2[,c('cat_age', 'cat_age46-64')]

# maindf2$cat_age65up <- NA
# maindf2[which(maindf2[,'cat_age'] == '65+'), 'cat_age65up' ] <- 1
# maindf2[which(maindf2[,'cat_age'] != '65+' & is.na(maindf2[,'cat_age']) == F), 'cat_age65up' ] <- 0
# # maindf2[,c('cat_age', 'cat_age65up')]

# # levels(maindf2$PartyRec)
# maindf2$PartyRecR <- NA
# maindf2[which(maindf2[,'PartyRec'] == 'Republican'), 'PartyRecR' ] <- 1
# maindf2[which(maindf2[,'PartyRec'] != 'Republican' & is.na(maindf2[, 'PartyRec']) == F), 'PartyRecR' ] <- 0
# maindf2[, c('PartyRec', 'PartyRecR')]

# maindf2$PartyRecD <- NA
# maindf2[which(maindf2[,'PartyRec'] == 'Democratic'), 'PartyRecD' ] <- 1
# maindf2[which(maindf2[,'PartyRec'] != 'Democratic' & is.na(maindf2[, 'PartyRec']) == F), 'PartyRecD' ] <- 0
# maindf2[, c('PartyRec', 'PartyRecD')]

# maindf2$PartyRecU <- NA
# maindf2[which(maindf2[,'PartyRec'] == 'Unaffiliated'), 'PartyRecU' ] <- 1
# maindf2[which(maindf2[,'PartyRec'] != 'Unaffiliated' & is.na(maindf2[, 'PartyRec']) == F), 'PartyRecU' ] <- 0
# maindf2[, c('PartyRec', 'PartyRecU')]


# droppr <-  c('PartyRec','Party', 'cat_age', 'cat_ageDe','Sex')
# savr <- maindf2[,droppr]
# maindf2 <-  maindf2[,!names(maindf2) %in% droppr] 
# summary(maindf2)



# maindfcorgettr <- data.frame(as.numeric(maindf2[,1]))
# rownames(maindfcorgettr) <- rownames(maindf2)

# for(i in 1:ncol(maindf2)){
	
	# dfrowtonum <- data.frame(as.numeric(maindf2[,i]))
	# rownames(dfrowtonum) <- rownames(maindf2)
	# colnames(dfrowtonum) <- colnames(maindf2)[i]
	# maindfcorgettr <-  cbind(maindfcorgettr, dfrowtonum)
	# rm(dfrowtonum)	
	# }  

# maindfcorgettr <- maindfcorgettr[,-1]

# which( cor(maindfcorgettr, use = 'pairwise.complete.obs') >.75, arr.ind = T)[, 'col' ]
# which( cor(maindfcorgettr, use = 'pairwise.complete.obs') >.75, arr.ind = T)[, 'row' ]



# head(, n = 30L)

# maindf2 <- cbind(maindf2, savr)


# create lastN scores

 # Collect fields to use for scoring
 metaFields <- c('General08','General12', 'General10')
 
 #create two dummy variables based on the following
  # Those who voted in 2008, 2012, but not 2010,
 # Those who voted in 2010
 
maindf2$SpecVotes <- 0
maindf2[ which(maindf2$General08 %in% c('Y','A') & maindf2$General12 %in% c('Y','A')), 'SpecVotes'] <- 1
maindf2[maindf2$General10 %in% c('Y','A'), 'SpecVotes'] <- 0

maindf2$TenDummy <- 0
maindf2[maindf2$General12 %in% c('Y','A'), 'TenDummy'] <- 1
 
Vandat$SpecVotes <- 0
Vandat[ which(Vandat$General08 %in% c('Y','A') & Vandat$General12 %in% c('Y','A')), 'SpecVotes'] <- 1
Vandat[Vandat$General10 %in% c('Y','A'), 'SpecVotes'] <- 0

Vandat$TenDummy <- 0
Vandat[maindf2$General12 %in% c('Y','A'), 'TenDummy'] <- 1
 
 
 
 
 
 
 
# # for(i in 1:length(metaFields)) maindf2[ which(maindf2[,metaFields[i]] == 0), metaFields[i]] <-'' # Whenever you see a zero in column i, replace it with a blank space, repeat for all columns i. 
 
 # maindf2$combineMeta <- do.call("paste", c(maindf2[metaFields], sep = '')) #Nee to figure out why do.call works when paste alone won't
 # # maindf2$combineMeta <- strsplit( maindf2$combineMeta, split = '')
 # maindf2$LastN <- nchar(maindf2$combineMeta)

# # for(i in 1:length(metaFields)) Vandat[ which(Vandat[,metaFields[i]] == 0), metaFields[i]] <-'' # Whenever you see a zero in column i, replace it with a blank space, repeat for all columns i. 
 
 # Vandat$combineMeta <- do.call("paste", c(Vandat[metaFields], sep = '')) #Nee to figure out why do.call works when paste alone won't
 # # maindf2$combineMeta <- strsplit( maindf2$combineMeta, split = '')
 # Vandat$LastN <- nchar(Vandat$combineMeta)




# pull out a tenth of the data for control purposes

tenperc <-  createFolds(complete.cases(maindf2[,'sp03SuppRec']), k = 10)

maindf2 <- maindf2[-tenperc$Fold06,]
controldf2 <- maindf2[tenperc$Fold06,]






#BEGIN LOOP HERE!
 i <- 1
  L <- 1
NIVs <- 1 #The number of IVs, this will eventually serve as the building block of the outer loop
MAXIVs <- 10 # Number of Go rounds for IV selection

# nFold <- 2 #number of folds to use for kfold cross validation method of IV selection


deevlist <- c('sp03SuppRec', 'sp04SuppRec','sp05SuppRec','sp06SuppRec','sp08SuppRec') 
 # deevlist <- 'sp03SuppRec' #for when we want to test a feature, but are overwhelmed by the number of regressions
 #Select things that we don't want in our IV list below
 dvcols <- c("sp03Rec2", "sp04Rec2", "sp04avgmove",  "sp05Rec2", "sp05avgmove", "sp06Rec2", "sp06avgmove",  "sp08Rec2", "sp08avgmove", 'sp03SuppRec', 'sp04SuppRec','sp05SuppRec','sp06SuppRec','sp08SuppRec',  'deevdiv', 'deevfac') #anything that deals with a DV
 rescols <- c('Preds_for_sp03Rec2', 'Recoded_sp03Rec2','Preds_for_sp04Rec2', 'Recoded_sp04Rec2', 'Preds_for_sp05Rec2', 'Recoded_sp05Rec2', 'Preds_for_sp06Rec2', 'Recoded_sp06Rec2', 'Preds_for_sp08Rec2', 'Recoded_sp08Rec2') #Any thing that resembles a dv or is a stored dv. 
extraagevars <-  colnames(maindf2)[grep("age_", colnames(maindf2))]
gendervars <- colnames(maindf2)[grep("male", colnames(maindf2))]

redundant <- c('reg_party_dem','others_num_dem', 'others_num_rep', 'reg_party_ind', 'clarity_party', 'cons_dbi_educ_nohs', 'cat_ageDe', extraagevars, gendervars,'Age','Sex','Party') #Age sex and Party aren't actually redundant, but since we hard code them into the loop, this is a good place to tell the code, hey we don't need you included in the boostrapping selection process 
zerovar <- c('voted_12p_dem', 'voted_10p_dem')
mostlyNAs <- c('cons_on_outside_list')
ActivateVars <- c('prospectid', 'PersonID', 'zipcode')
 # VANVars <- c('VANID')
 VANVars <-  colnames(Vandat)
clarityvars <- colnames(maindf2)[grep('clarity', colnames(maindf2))]

colsnottouse <- c(dvcols,"sp07Rec", "sp07.2Rec", "sp08Rec", 'reg_party_rep','attemptcount', 'voterid', 'votebuilder_identifier', 'dsnRec', 'cen10_asian', 'namecheck',zerovar, redundant, mostlyNAs, ActivateVars, clarityvars, VANVars, rescols, colnames(maindf2)[nearZeroVar(maindf2)])

#End IV Removal

# maindf2[,rescols] <- NULL #make sure the recursive elements can't harm reruns by nullifying them pre-loop. 
 
# age, gender party registration, county, and income, if we have it on everyone. 

Rprof('bensprof.txt')
for(L in 1:length(deevlist)) { #begin DV loop
deev <- deevlist[L] 
maindf2$deevdiv <- maindf2[,deev] #for this one, we don't need to do anything to deevdiv to make it work. Unfortunately, it's pretty much everywhere instead of deev, so I'm just passing it on here.
controldf2$deevdiv <- controldf2[,deev] #ditto.
# colnames(maindf2)



#chose columns for our sample
numsforsample <- 1:ncol(maindf2)

initlooplength <-  ncol(maindf2) - length( colsnottouse)

# colnames(maindf2)

colsnumstoavoid <-  which(colnames(maindf2) %in% colsnottouse)
colnumstouse <- which(!colnames(maindf2) %in% colsnottouse)  # get all the columns we want to use as IVs


#create variable selecting loop
# This loop will select a single variable and regress it against deev
currentReg <- NULL # This will hold the regression of the moment
bestReg <- NULL
bestVarResSt <- NULL
# priorIVs <- c('cat_age', 'Sex', 'PartyRec', 'cen00_medianincomeRec', "CountyName")
# priorIVs <- c('cat_age', 'Sex', 'PartyRec', 'CountyName','cen10_densityRec','cen00_medianincomeRec')

 priorIVs <- c('cat_age', 'Sex', 'PartyRec', 'CountyName', 'SpecVotes', 'TenDummy', 'cen10_densityRec', 'cen00_medianincomeRec') #Mark's IV's


  # priorIVs <- NULL #For when we want to do something without prior IVs

for(NIVs in 1:MAXIVs){ #Outer Loop Begins
metabreaker <- 999 #set metabreaker to some nonzero value. 999, in honor of the current greatest troll of all time. 
# print( paste( 'NIVs =', NIVs))
print(paste('dv =',deev))
# if(NIVs == 1) priorIVs <- c('cat_age', 'Sex', 'Party')


# Need to figure out how to make it so that If at no point does something happen in the loop below, break out of the outermost loop

for(i in 1:initlooplength) {#Inner Loop Begins
# for(i in 1:length) {#Inner Loop Begins

iloopbreaker <- 1 #iloopbreaker begins as 1

 ivstouse <- c(priorIVs, colnames(maindf2)[colnumstouse[i]])

# ivstouse <- priorIVs

# if(NIVs == 1) ivformed <-  ivstouse  else ivformed <-  do.call(paste,c(as.list( ivstouse), sep = ' + ')) # if NIVs is not one, we need to form a list of IVs to place into formed eqn. If it isn't then we can just use the text from ivstouse
# ivformed <-  do.call(paste,c(as.list( ivstouse), sep = ' + ')) #Great if you want to use do.call, but I found something better below
ivformed <- paste(ivstouse, collapse = ' + ')
formedeqn <- as.formula(paste('deevdiv', " ~ ", ivformed)) #Form our equation. In this version, we're going to need to figure out the DV's structure before we start these loops


traindf2 <- maindf2[complete.cases(maindf2$deevdiv), c('deevdiv', ivstouse)]

# KFold cross-validation method of iv selection
# trainerFolds <- createFolds(traindf2[, 'deevdiv'], k = nFold, list = T)

# for(r in 1:nFold){ #Begin Kfold cross-validation loop
 # traindf3 <- na.omit(traindf2[-trainerFolds[[r]],]) #Create training data

# testdf3 <-  na.omit(traindf2[trainerFolds[[r]],]) #create test data
 
# currentReg <-  multinom(formula = formedeqn, data = traindf3) #perform a beta regression and store it in currentReg

# testdf3$testpreds <-  predict(currentReg, testdf3) #predict the test data's results, and store it in the test data's dataframe
# testdf3$testpreds <- as.numeric(levels(testdf3$testpreds)[testdf3$testpreds])


# if (r == 1) currentVarResidST <-  var(na.omit( testdf3$deevdiv - testdf3$testpreds)) else currentVarResidST <-  c(currentVarResidST, var(na.omit( testdf3$deevdiv - testdf3$testpreds)) ) #store the variance of the residuals; if it's the first go-round, put it them into a new variable called currentVarResidST. If it isn't, store them there using the c() function


# }
# currentVarResid <- mean(currentVarResidST)


# currentReg <-  betareg(formula = formedeqn, data = traindf2, link = 'logit') #perform a beta regression and store it in currentReg

currentReg <-  glm(formula = formedeqn, data = traindf2, family = 'binomial') #perform a regression and store it in currentReg
# summary(currentReg)


 # bestReg <-  glm(formula = formedeqn, data = maindf2, family = 'binomial') #perform a logit regression and store it in currentReg


 # currentVarResid <-  var( currentReg$resid)  #Get currentReg's residuals, and store their variance
traindf2$currentpreds <- predict(currentReg, traindf2, 'response') 
controldf2$currentpreds <- predict(currentReg, controldf2, 'response') 

rtPredRat <-  (length(which(traindf2$deevdiv == 0 & traindf2$currentpreds <.5)) + length(which(traindf2$deevdiv == 1 & traindf2$currentpreds >.5)) )/length(traindf2$deevdiv)   #rtPredRat gives us the ratio of the number of predictions which are less than 50 and equal zero and the number greater than 50 that actually equal 1, and
 
 
controldf3 <- controldf2[which( is.na(controldf2$deevdiv) == F), c('deevdiv', 'currentpreds')] 

onOnePreds <- length(which(controldf3$deevdiv == 1 & controldf3$currentpreds >=.5)) #Number of times the control group predicts ones correctly
onZeroPreds <- length(which(controldf3$deevdiv == 0 & controldf3$currentpreds <.5)) #Number of times controlgroup predicts zeros correctly
Npreds <- length(controldf3$deevdiv) #number of values that can be predicted


contPredRat <-  (onOnePreds + onZeroPreds )/Npreds  #contPredRat gives us the ratio of the number of predictions which are less than 50 and equal zero and the number greater than 50 that actually equal 1 as a ratio to the total number of possible values that can be predicted in the control group. 
 

 
 
# # # print(currentVarResid)

if(i == 1 & NIVs == 1 ){ #On the very first pass,
	bestReg <- currentReg #The First regression is the best regression.
	bestRtPredRat <- contPredRat
	# bestVarResid <- currentVarResid #Same with the residual variance
	# bestVarResST <- bestVarResid #and thus we put it in storage
	print(paste('Number of correctly predicted Zeros =', onZeroPreds))
	print(paste('Number of correctly predicted Ones =', onOnePreds))

	print(paste('Rate to beat =',round( bestRtPredRat, 6)))
	bestIV <- colnames(maindf2)[colnumstouse[i]]
	 } #keep them always if i == 1

# print( paste('currentVarResid =', currentVarResid ))
# print(paste('bestVarResid =', bestVarResid))

# residdif <- currentVarResid - bestVarResid
# # print(paste('currentVarResid - bestVarResid = ', round(currentVarResid, 4), '-', round(bestVarResid, 4), '≈', round(residdif, 4 )) )

	# if(bestVarResid > currentVarResid){ #if the currentVarResid is lower than the bestVarresid
		
		if(contPredRat > bestRtPredRat){ #if the current prediction ratio is bigger than the best one so far
		bestReg <- currentReg #put the current reg as best reg
		# bestVarResid <- currentVarResid #put the currentVariance of the residuals as best
		# bestVarResST <- c(bestVarResST, bestVarResid) #Put the new best VarResid in storage
		
		bestRtPredRat <- contPredRat # Put the current right prediction ratio as the best one, 
		bestIV <- colnames(maindf2)[colnumstouse[i]] #and save the bestIV for storage in priorIVs
		print(paste('Best Ratio of prediction rates =',round( bestRtPredRat,6)))
		# print(paste('Control Prediction rates =',round( contPredRat,4)))
		metabreaker <- 0 #make the metabreaker variable = 0
		# print(paste('bestVarStDev =', round(sqrt(bestVarResid), 6)))
		} # if the best regressions residual variance is bigger than the current regression's residual variance, the current regression replaces the prior best regression. 	
	

# print(paste('i =', i))
rm(rtPredRat)
rm(contPredRat)

if(metabreaker != 0) iloopbreaker <- 0 #if metabreaker is still equal to its original nonzero value, break the loop.

}# Inner Loop Ends
# 
# print( paste( 'NIVs =', NIVs))
priorIVs <- c(priorIVs, bestIV) #The bestIV now becomes part of the prior IVs
print(priorIVs)

#Executive Decision: This program must become more aggressive at countering multicollinearity, if only because the code runs at a snail's pace. 
#Also, how to tell whether the IV's that survive the KFold cross validation are actually liable to reduce overall variance of residuals, or just lucky? Preferably without adding on to computational time? 



 #get the names of all IVs in the best Regression (excluding the intercept)
 length(priorIVs) == NIVs
 initlooplength <- initlooplength - 1 # since one of our variables is lost to the best regression, reduce the length of our internal loop by one

#Now, we need to figure out how to make sure it's not going to repeatedly grab the same variable, eliminating it from colnumstouse



 IVcols <-  which(colnames(maindf2) %in% priorIVs) #get numbers of columns that have been used so far



 colnumstouse <- colnumstouse[ !colnumstouse %in% IVcols] #get rid of the columns used previously by taking them out of colnums to use. 
 
 if(iloopbreaker == 0) break #if I loopbreaker is 0, break out of the iv loop and move onto the next dv
} #Outer Loop ends
# print(data.frame(controldf3$deevdiv, controldf3$currentpreds))
#Print outer loop output
print('')
print('')
print(deev)
print( summary(bestReg))
# str(bestReg)
print(paste('nVal =', length(bestReg$residuals)))
# summary(bestReg$residuals)
# hist(bestReg$residuals)

# print(var(bestReg$residuals))
print('')


#note to self. What if it doesn't select any after a certain point? Is there a danger that it might start arbitrarily pulling stuff out? I'll say it does, and this is something we have to watch for. 


# summarize(maindf2[,IVcols])

deevpreds <-  predict(bestReg, maindf2, 'response')
contpreds <- predict(bestReg, controldf2, 'response')
# finpreds <- predict(bestReg, Vandat, 'response')
# str(bestReg)


#create titles for our new variables, and add them to maindf2
predtitler <- paste('Scoring on ', deev, sep = '') #title deevpreds for our export
rectitler <- paste('Voter choice of ',deev, sep = '')
# data.frame(c(names(deevpreds)), c( rownames(maindf2)))
maindf2 <-  cbind(maindf2,  deevpreds)
controldf2 <- cbind(controldf2, contpreds)
# Vandat <- cbind(Vandat, 'torep' = finpreds)

colnames(maindf2)[colnames(maindf2) == 'deevdiv'] <- rectitler
colnames(maindf2)[colnames(maindf2) == 'deevpreds'] <- predtitler
colnames(controldf2)[colnames(controldf2) == 'deevdiv'] <- rectitler
colnames(controldf2)[colnames(controldf2) == 'contpreds'] <- predtitler

# colnames(Vandat)[colnames(Vandat) == 'torep'] <- predtitler

#Fix our predictions to the scale of the original queries

 maindf2[, rectitler] <- round(maindf2[,rectitler], 2) #
 maindf2[, predtitler] <- round(maindf2[,predtitler]*100, 5)

 controldf2[, rectitler] <- round(controldf2[, rectitler], 2)
 controldf2[, predtitler] <- round(controldf2[, predtitler] *100, 5)


# Vandat[, predtitler] <- round(Vandat[, predtitler] *100, 2)




# plot(log(1:length(bestVarResSt), 10), log(bestVarResSt, base = 10), type = 'l')
# plot(1:length(bestVarResSt), bestVarResSt, type = 'l')

 if(L == 1) bestRegST <- bestReg else bestRegST <- c(bestRegST, bestReg)

#Clean up before next iteration

rm( deev, bestVarResSt, colnumstouse, bestRtPredRat)
} # End DV Loop

Rprof(NULL)

summaryRprof('bensprof.txt')


valsToPred <- with(maindf2, list(PartyRec = levels(PartyRec), cat_age = levels(cat_age), Sex = levels(Sex)))

print(newdata(bestReg, predVals = valsToPred ))


# # #Creating movement score loop

# # #Isolate the question number from predtitler 
# zeroloc <-  which(strsplit(predtitler, split ='')[[1]] == '0') #find the location of the zero in predtitler
 # strsplit(predtitler, split = '')[[1]][zeroloc + 1] #returns predtitler question number.

# numstor <- c(3,4,5,6) #the numeric value of the special fields for which we seek movement information
# for(i in 1:length(numstor)){ #Begin Movement creation loop
# n <- numstor[i] #n becomes the number of the storage, and the question number we'll use to create the movement score

# tosubtitler <-  paste(substr(predtitler, 1, 14), n, substr(predtitler, 16, nchar(predtitler)) , sep = '') #'grab' the name of the question to measure movement on by pasting together parts of it, and the question number we want movement for.


# Movementtitle <-  paste("Movement scoring on sp0",n, sep ='')  #create title for movement scoring on questions.  
# maindf2[, Movementtitle] <-  maindf2[,predtitler] - maindf2[, tosubtitler] #The movement score begins as difference of final prediction and initial prediction...

# # maindf2[, Movementtitle] <- (maindf2[, Movementtitle] - min(na.omit(maindf2[,Movementtitle])))# ...and then we normalize over the intervals by subtracting out the minimum of the values ...
# # maindf2[, Movementtitle] <- round(maindf2[, Movementtitle] /max(na.omit(maindf2[,Movementtitle])) * 100, 2) #... finishing up by dividing by the maximum and rounding to 2 digits.

# #We repeat all of this for the control data frame. 
# controldf2[, Movementtitle] <-  controldf2[,predtitler] - controldf2[, tosubtitler]
# # controldf2[, Movementtitle] <- (controldf2[, Movementtitle] - min(na.omit(controldf2[,Movementtitle])))
# # controldf2[, Movementtitle] <- round(controldf2[, Movementtitle] /max(na.omit(controldf2[,Movementtitle])) * 100, 2)

# #and do it for vandat

# Vandat[, Movementtitle] <-  Vandat[,predtitler] - Vandat[, tosubtitler]
# # Vandat[, Movementtitle] <- (Vandat[, Movementtitle] - min(na.omit(Vandat[,Movementtitle])))
# # Vandat[, Movementtitle] <- round(Vandat[, Movementtitle] /max(na.omit(Vandat[,Movementtitle])) * 100, 2)

# if(i == 1) MovementtitleST <- Movementtitle else MovementtitleST <- c(MovementtitleST, Movementtitle)
# } #end movement creation loop



# setwd(outputwd)
# getwd()

 # write.csv(maindf2, paste('modelpredslogit',' ', dateFormed,'.csv', sep = ''), row.names = F, na = '')
 # controldf2$currentpreds <- NULL

 # write.csv(controldf2, paste('controlpredslogit',' ', dateFormed,'.csv', sep = ''), row.names = F, na = '')

# # system("say 	-v Hysterical I am finished.")

# # for(i in 1:2) system("afplay -v 15 '/System/Library/Sounds/Sosumi.aiff/'")


# # # Prep vandat for export


# notforfinVan <- c('mAddress', 'mCity', 'mState', 'mZip5', 'mZip4', 'CD', 'SD', 'HD', 'X2012.ClarityTurnout','Caucus08', 'Municipal11', 'Municipal09', 'Municipal08', 'Municipal07', 'Municipal05', 'Municipal03', 'MunicipalPrimary11', 'MunicipalPrimary05', 'MunicipalPrimary03', 'Primary12', 'Primary10', 'Primary10Party', 'Primary08', 'Primary06', 'Primary04', 'Primary02', 'Primary00', 'Primary98', 'Primary96', 'Special09', 'Special08', 'Special07', 'Special06', 'Special05', 'Special04', 'Special03', 'Special02', 'Special01', 'Special00', 'BoardOfEducationCode', 'block_group', 'PollingAddress', 'PollingLocation', 'Email') #select columns that seem irrelevant 

# Vandat2 <- Vandat[, which(!colnames(Vandat) %in% notforfinVan)] # and take the ones we want and put them in a smaller dataframe, called vandat2


# setwd(inputwd)
# advDat <-  read.csv('Wakefield Advocacy Export.csv')
# head(advDat)
# advDat$voteridnum <- as.character(advDat$voterid)
# advDat$voteridnum[which(advDat$voteridnum == '')] <- NA
# advDat$voteridnum <- as.numeric(advDat$voteridnum)
# head(data.frame(advDat$voterid, advDat$voteridnum))


# advocateRows <-  match(Vandat2$VANID, advDat$voteridnum)
# # length(advocateRows)

# Vandat2 <- cbind(Vandat2, advDat[advocateRows, c('voterid','sp03', 'sp04', 'sp05', 'sp06', 'sp07', 'sp08', 'sp09','sp12','sp13') ])

# # as.numeric(advDat$voteridnum)

# # Vandat2$VANID

# setwd(outputwd)
 # write.csv(Vandat2, paste('VanPredslogitSupporters ', dateFormed,'.csv', sep = ''), row.names = F, na = '')

# fintime <- (proc.time() - ptr)/60 #Number of minutes the process takes.
# print(fintime)

system('say Done!')
