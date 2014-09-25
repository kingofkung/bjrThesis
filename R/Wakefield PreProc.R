#Wakefield Modelling data pre-processing
# A Ben Rogers Joint
# Started 4/9/2014
# Last Edited

rm(list = ls())
ptr <- proc.time()
#Go to correct file location.  

inputwd <- "/Users/bjr/Google Drive/Activate/Wakefield Modelling Project/Wakefield Input 2"
setwd(inputwd)
outputwd <- "/Users/bjr/Google Drive/Activate/Wakefield Modelling Project/Wakefield Output 2/Wakefield Regression Outputs"
dateUnformed <-  date()
dateFormed <-  strsplit(dateUnformed, split = ' ')
dateFormed <- paste(dateFormed[[1]][c(2,4)], collapse = ' ')

 maindf <-  readRDS('maindf.RDS')
 Vandat <- readRDS('Vandat.RDS')


#Load relevant packages

library(car)
library(xlsx)
library(gdata)
library(rockchalk)
library(caret) #loads ggplot2
library(leaps)
library(glmnet)

 
 

# # # #  
 # #Read in the files:
# #Bit of a problem here. Mark wants us to read in files from two separate segments of surveying. Now we know which segments are 

# # maindf <-  read.csv("FivePrimaryVoterInfo.csv") #Appears to be the main dataset, includes the DV

# mdf1 <- read.csv("Survey Round 1.csv", stringsAsFactors = F)
# data.frame(colnames(mdf1)) #Note: mdf question columns start at sp02 and end at sp08. The two that vary are sp02 and sp07
# #To deal with this, we separate out the questions that are different between surveys, and add a couple of new columns on for new questions
# mdf1qs <- mdf1[, c('voterid', 'firstname', 'lastname','sp02', 'sp07')]
# # mdf1qs$sp02.2 <- factor('')
# # mdf1qs$sp07.2 <- factor('')
# colnames(mdf1qs)[2] <- 'firstnameq1'
# colnames(mdf1qs)[3] <- 'lastnameq1'

# length(rownames(mdf1qs))
# head(mdf1qs)

# mdf2 <- read.csv("Survey Round 2.csv", stringsAsFactors = F) #Then, we place the new questions in the new columns, and clear the former sp02 and sp07 columns. 
# mdf2qs <- mdf2[, c('voterid', 'firstname', 'lastname','sp02', 'sp07')]
# str(mdf2qs)
# mdf2qs$sp02.2 <- mdf2qs$sp02
# mdf2qs$sp07.2 <- mdf2qs$sp07
# colnames(mdf2qs)[2] <- 'firstnameq2'
# colnames(mdf2qs)[3] <- 'lastnameq2'

# maindf <- rbind(mdf1[,c(1:33, 35:38, 40:41)], mdf2[, c(1:33, 35:38, 40:41)]) #This grabs  everything that stays the same in the two datasets. Right now that's all but sp02, sp07, but this can change when we get new data. 
# maindf <-  cbind(maindf, mdf1qs[match(maindf$voterid, mdf1qs$voterid), c('sp02', 'sp07', 'lastnameq1')]  )
# maindf <-  cbind(maindf, mdf2qs[match(maindf$voterid, mdf2qs$voterid), c('sp02.2', 'sp07.2', 'lastnameq2')]  )

# str(maindf)

# set.seed(12345)
# maindf[sample(1:nrow(maindf), size = 25),c('lastname', 'lastnameq1', 'lastnameq2')] #This is a consistency check. It checks out.

# length(rownames(maindf))

# otherdat <- read.csv('wakefield_prelim_data_pull.csv', stringsAsFactors = F) #bring in an IV Dataset
# Vandat <- read.csv('IVDat2.csv', stringsAsFactors = F)
# Vandensinc  <- read.csv("wakefield_data_export.csv", stringsAsFactors = F)
# head(Vandensinc)
# #purge commas and Match density and income data to Vandat

# str(Vandensinc)

# # Vandensinc$votebuilder_identifier2 <- lapply(Vandensinc$votebuilder_identifier,FUN = as.character)
# Vandensinc$votebuilder_identifier <- gsub(',', '', Vandensinc$votebuilder_identifier)
# Vandensinc$votebuilder_identifier <- as.numeric(Vandensinc$votebuilder_identifier)

# # Vandensinc$census_2000_medianincome2 <- lapply(Vandensinc$census_2000_medianincome,FUN = as.character)
# Vandensinc$census_2000_medianincome <- gsub(',', '', Vandensinc$census_2000_medianincome)
# Vandensinc$census_2000_medianincome <- as.numeric(Vandensinc$census_2000_medianincome)


# # Vandensinc$density_land_sq_km2 <- lapply(Vandensinc$density_land_sq_km,FUN = as.character)
# Vandensinc$density_land_sq_km <- gsub(',', '', Vandensinc$density_land_sq_km)
# Vandensinc$density_land_sq_km <- as.numeric(Vandensinc$density_land_sq_km)




# #Match IV data to the main dataset
# otherdatarows <- match(maindf$voterid, otherdat$votebuilder_identifier)
# Vandatrows <- match(maindf$voterid, Vandat$VANID)
# # otherdatarows2 <- match(maindf$voterid, otherdat2$VANID)
# colnames(otherdat)
# maindf <- cbind(maindf, Vandat[Vandatrows,])
# maindf <- cbind(maindf, otherdat[otherdatarows,])
# maindf$namecheck <- otherdat[otherdatarows, 'votebuilder_identifier']
 # # na.omit(data.frame(maindf$voterid, maindf$namecheck)) #Checks out.
# # data.frame(maindf[which(maindf$sp03 != ""), ])[1521,]

# #Match IV data to Vandat
# VandatMatcherrows <- match(Vandat$VANID, Vandensinc$votebuilder_identifier)

# Vandat <- cbind(Vandat, Vandensinc[VandatMatcherrows,])
# head(Vandat[, c('VANID','votebuilder_identifier') ])

# #rename relevant columns to correct names 'cen10_densityRec', 'cen00_medianincomeRec'
# colnames(Vandat)[ which(colnames( Vandat) == 'census_2000_medianincome')] <- 'cen00_medianincome'

# colnames(Vandat)[ which(colnames( Vandat) == 'density_land_sq_km')] <- 'cen10_density'


# #general guess: Are you a heavy 
# #match in data

# #Get Jake ID Numbers of everyone we want 'full' IV data for.


# # # # as.character(levels(mdf1qs$sp02))

# # # print(mdf1qs$sp02)

# # # topullVANS <- c(maindf[  which(maindf$sp02 != "" & maindf$sp02 != " "),'voterid'], maindf[  which(maindf$sp02.2 != "" & maindf$sp02.2 != " "),'voterid'])

# # topullVANS == 1544185
# # length(topullVANS)

# # colnames(maindf)[33]

# # # maindf[which( maindf$voterid %in% topullVANS ), c('lastname', 'lastnameq1', 'sp02', 'sp02.2', 'sp03')]

# # # write.csv(topullVANS, file = 'topull completed2.csv')

 


# # Recoding section,  
# #In data for wakefield, need to recode sp02-sp08
# #Mark would like to see 'movement' variables, in that we determine how people's opinions change between one question to the next. I'm going to try and put together a set that includes 
# # a. how people moved on a question compared to the first question with a numeric option (sp03, first ID), which We will refert to as the average movement.
# # b. how people moved on a question compared to the question preceeding it, which we will call instantaneous movement.
# # c. how people moved from a question to the final iD
# # recode sp02
# maindf$sp02Rec <- maindf$sp02 #save value in new variable, so I can redo this without issue
# levels(maindf$sp02Rec) <- c(levels(maindf$sp02Rec), "Higher than $10.10", "Raise it to $10.10", "Raise it to $9.00", "Leave it as is", "Eliminate the Minimum Wage")

# maindf$sp02Rec[which(maindf$sp02Rec %in% c("6 - Would not answer ", "5 - Not sure/no opinion"))] <- NA
# maindf$sp02Rec[which(maindf$sp02Rec %in% "0 - Higher than $10.10")] <- "Higher than $10.10"
# maindf$sp02Rec[which(maindf$sp02Rec %in% "1 - Raise to $10.10")] <- "Raise it to $10.10"
# maindf$sp02Rec[which(maindf$sp02Rec %in% "2 - Raise to $9.00")] <- "Raise it to $9.00"
# maindf$sp02Rec[which(maindf$sp02Rec %in% "3 - Leave it as is")] <- "Leave it as is"
# maindf$sp02Rec[which(maindf$sp02Rec %in% " 4 - Eliminate the minimum wage")] <- "Eliminate the Minimum Wage"


 # # " 4 - Eliminate the minimum wage" "0 - Higher than $10.10"         
 # # "1 - Raise to $10.10"             "2 - Raise to $9.00"             
 # # "3 - Leave it as is"              "5 - Not sure/no opinion"        
 # # "6 - Would not answer "          



# maindf$sp02Rec[which(maindf$sp02Rec %in% c('', " "))] <- NA
# maindf$sp02Rec <- factor(maindf$sp02Rec) #eliminate the extra levels
# levels(maindf$sp02Rec)


# # recode sp02.2
# maindf$sp02.2Rec <- maindf$sp02.2


# #I decided to separate the final output of the titles from the variable inputs in the recoded function, as 
# passopt <- "Pass the Paycheck Fairness Act"
# noneedopt <- "No, a Federal Law is Not Needed"
# noprobopt <- "No Problems with Gender Equity"

# levels(maindf$sp02.2Rec) <- c(levels(maindf$sp02.2Rec), passopt, noneedopt, noprobopt)
# maindf$sp02.2Rec[which(maindf$sp02.2Rec %in% c("3 - Not sure/no opinion", " 4 - Wouldn't answer "))] <- NA 
# maindf$sp02.2Rec[which(maindf$sp02.2Rec %in% c("0 - Yes- pass the Paycheck Fairness Act"))] <- passopt
# maindf$sp02.2Rec[which(maindf$sp02.2Rec %in% c("1 - No- a federal law is not needed"))] <- noneedopt
# maindf$sp02.2Rec[which(maindf$sp02.2Rec %in% c("2 -No- there are no problems with gender equity"))] <-  noprobopt









# #Recode sp03
# # specificrecode <- function(varRec){
# # #This function will take our special field variables and return them as recoded in the specific manner described below. Please note that for this function, the levels have to be exactly the same, so 

# # levels(varRec) <- c(levels(varRec), "Supports Wakefield",  "Undecided","Supports Jenkins") #add the new values as categories

# # #Recode level
# # varRec[which(varRec %in% c('', " 0 - Decline to answer ", " 0 - Decline to answer  " ))] <- NA
# # varRec[which(varRec %in% c("1 - Supports Lynn", "2", "3", "4"))] <- "Supports Jenkins"
# # varRec[which(varRec %in% "5")] <- "Undecided"
# # varRec[which(varRec %in% c("6", "7","8","9", "10 - Supports Margie"))] <- "Supports Wakefield"

# # rts <- factor(varRec)

# # return(rts)

# # }

# # specificrecode2 <- function(varRec){
# # #This function will take our special field variables and return them as recoded in the specific manner described below. Please note that for this function, the levels have to be exactly the same, so 

# # levels(varRec) <- c(levels(varRec),'10', '1','0') #add the new values as categories

# # #Recode level
# # varRec[which(varRec %in% c('', " 0 - Decline to answer ", " 0 - Decline to answer  " ))] <- NA
# # varRec[which(varRec %in% c("1 - Supports Lynn"))] <- "1"

# # varRec[which(varRec %in% c("10 - Supports Margie"))] <- "10"



# # # varRec[which(varRec %in% c('1','2','3','4','5'))] <- '0'
# # # varRec[which(varRec %in% c('6','7','8','9','10'))] <- '1'

 # # rts <- as.numeric(as.character((varRec)))

# # return(rts)

# # }





# binRec <- function(varRec, lvlsToZero, lvlsToOne, lvlsToNa){
# #This function will take our special field variables and return them as recoded as a binary value. These values are Please note that for this function, the levels have to be exactly the same, so 

# levels(varRec) <- c(levels(varRec), '1','0', '999') #add the new values as categories

# #Recode level
# varRec[which(varRec %in% lvlsToOne)] <- "1"

# varRec[which(varRec %in% lvlsToZero)] <- "0"
# varRec[which(varRec %in%  lvlsToNa)] <- '999'


# rts <- as.numeric(as.character((varRec))) #if this starts returning an NA's introduced by coercion, really watch out. It's very likely that we've forgotten a level, or mistyped it into the function. 

# # rts <- as.character(varRec)

# rts[which(rts == '999')] <- NA

# return(rts)

# }

# # Levels for binary recode
# zeroLvls <- c("1 - Supports Lynn", "2", "3", "4", "5",  "6")
# oneLvls <- c('7', '8', '9', "10 - Supports Margie")
# naLvls <- c("", " 0 - Decline to answer ", " 0 - Decline to answer  " )

# # maindf$sp03Rec <- specificrecode(maindf$sp03)
# # maindf$sp03Rec[!is.na(maindf$sp03Rec)]

# # maindf$sp03Rec2 <- specificrecode2(maindf$sp03)


# levels(maindf$sp03)
# maindf$sp03 <- binRec(maindf$sp03, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)


 # # data.frame(maindf$sp03, maindf$sp03SuppRec)[!is.na(maindf$sp03SuppRec),]




# # maindf$sp04Rec <- specificrecode(maindf$sp04)
# # maindf$sp04Rec[which(maindf$sp04Rec %in% " 0 - Decline to answer  ")] <- NA #As I mentioned earlier, the specific recode only recodes very specific variables, and since the 0 level changes slightly between  these factors, I have to go through and recode this as a separate command. .
# # maindf$sp04Rec <- factor(maindf$sp04Rec)
# # print(maindf$sp04Rec[!is.na(maindf$sp04Rec)]) #I always forget how to make sure that we're not looking at NA responses, but this does the trick.

# # maindf$sp04Rec2 <- specificrecode2(maindf$sp04)
# # maindf$sp04avgmove <- maindf$sp04Rec2 - maindf$sp03Rec2
# # maindf$sp04instmove <- maindf$sp04avgmove #in this case, average movement == instantaneous movement 
# levels(maindf$sp04)
# maindf$sp04 <- binRec(maindf$sp04, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)


# # data.frame(maindf$sp04, maindf$sp04SuppRec)[is.na(maindf$sp04SuppRec),]


# # maindf$sp05Rec <- specificrecode(maindf$sp05)
# # levels(maindf$sp05Rec)
# # maindf$sp05Rec[which(maindf$sp05Rec %in% " 0 - Decline to answer  " )] <- NA
# # maindf$sp05Rec <- factor(maindf$sp05Rec)
# # print( maindf$sp05Rec[!is.na(maindf$sp05Rec)])

# maindf$sp05 <- binRec(maindf$sp05, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)


# # maindf$sp05Rec2 <- specificrecode2(maindf$sp05)
# # maindf$sp05avgmove <- maindf$sp05Rec2 - maindf$sp03Rec2
# # # maindf$sp05instmove <- maindf$sp05Rec2 - maindf$sp04Rec2  #in this case, average movement == instantaneous movement 



# # # maindf$sp06Rec <- specificrecode(maindf$sp06)
# # levels(maindf$sp06Rec)

# # # maindf$sp06Rec[which(maindf$sp06Rec %in% " 0 - Decline to answer  " )] <- NA

# # # maindf$sp06Rec <- factor(maindf$sp06Rec)
# # maindf$sp06Rec[!is.na(maindf$sp06Rec)]

# # maindf$sp06Rec2 <- specificrecode2(maindf$sp06)
# # # print(maindf[!is.na(maindf$sp06Rec2),c('sp03Rec2','sp04Rec2','sp05Rec2','sp06Rec2')])

# # maindf$sp06Rec2 <- specificrecode2(maindf$sp06)
# # maindf$sp06avgmove <- maindf$sp06Rec2 - maindf$sp03Rec2
# # maindf$sp06instmove <- maindf$sp06Rec2 - maindf$sp05Rec2  #in this case, average movement == instantaneous movement 

# maindf$sp06 <- binRec(maindf$sp06, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)




# #Koch bros question. We'll need to reconsider how we recode this one. 
# maindf$sp07Rec <- maindf$sp07
# levels(maindf$sp07Rec) <- c(levels(maindf$sp07Rec), "Kochs Negative Impact","Uncertain about Kochs","Kochs Positive Impact") #Add levels to cross tabulation


# maindf$sp07Rec[which(maindf$sp07Rec %in% c("", " 0 - Decline to answer  ") )] <- NA

# maindf$sp07Rec[which(maindf$sp07Rec %in% c("1 - Positive Impact", "2", "3" ) )] <- "Kochs Positive Impact"
# maindf$sp07Rec[which(maindf$sp07Rec %in% c("4", "5", "6" ) )] <- "Uncertain about Kochs"
# maindf$sp07Rec[which(maindf$sp07Rec %in% c("7", "8", "9", "10 - Negative Impact" ) )] <- "Kochs Negative Impact"

# maindf$sp07Rec <- factor(maindf$sp07Rec)
# # print(maindf$sp07Rec[!is.na(maindf$sp07Rec)])



# #recodesp07.2


# maindf$sp07.2Rec <- maindf$sp07.2


# lowregpref <-  "Less Regulation"
# medregpref <-  "Some Regulation"
# highregpref <- "More Regulation"
# levels(maindf$sp07.2Rec) <- c(levels(maindf$sp07.2Rec), lowregpref, medregpref, highregpref) #Need to add levels
# maindf$sp07.2Rec[which(maindf$sp07.2Rec %in% c("", " 0 - Decline to answer  ") )] <- NA
# maindf$sp07.2Rec[which(maindf$sp07.2Rec %in% c("1 - Less regulation", "2", "3"))] <- lowregpref
# maindf$sp07.2Rec[which(maindf$sp07.2Rec %in% c("4", "5", "6"))] <- medregpref
# maindf$sp07.2Rec[which(maindf$sp07.2Rec %in% c("7", "8", "9", "10 - More regulation" ) )] <- highregpref


# maindf$sp07.2Rec <- factor(maindf$sp07.2Rec)
# #print( maindf$sp07.2Rec[!is.na(maindf$sp07.2Rec)])



# # maindf$sp08Rec <- specificrecode(maindf$sp08)
# # levels(maindf$sp08Rec)

# # maindf$sp08Rec2 <- specificrecode2(maindf$sp08)

# # maindf$sp08avgmove <- maindf$sp08Rec2 - maindf$sp03Rec2

# maindf$sp08 <- binRec(maindf$sp08, lvlsToZero = zeroLvls, lvlsToOne = oneLvls, lvlsToNa = naLvls)






# # Note to self: parenthesis means everything more or less than, while square bracket means equal to when creating breaks



# Agebreaks <- c(17, 30, 45, 64, 200)
# # data.frame("age" = maindf$Age, "cat_age" = cut(maindf$Age, breaks = Agebreaks)) #Look at ages
 # Agelabels <- c('18-30', '31-45', '46-64', '65+')
# # clear(Agelabels)

# maindf$cat_age <- cut(maindf$Age, breaks = Agebreaks , labels = Agelabels)
# Vandat$cat_age <- cut(Vandat$Age, breaks = Agebreaks , labels = Agelabels)

# # #Use one of these to create a more Demarcated (hence the De) set of agebreaks 
# # AgebreaksDe <- c(17, 25,30, 35,40, 45, 50, 55,60, 65, 200)
# # AgelabelsDe <- c('18-25', '26-30', '31-35','36-40','41-45', '46-50', '51-55', '56-60', '61-64', '65+')

# # maindf$cat_ageDe <- cut(maindf$Age, breaks = AgebreaksDe, labels = AgelabelsDe)


# summary(maindf$Party)

# #remove all green and libertarian predicted
# maindf$Party <- factor(maindf$Party)
# levels(maindf$Party) <- c(levels(maindf$Party), "Democratic", "Republican", "Unaffiliated")
# maindf$Party[which(maindf$Party %in% c("L", "U") )] <- "Unaffiliated" #get rid of the odd parties and unknowns
# maindf$Party[which(maindf$Party %in% "D") ] <- "Democratic"
# maindf$Party[which(maindf$Party %in% "R") ] <- "Republican" 
# maindf$Party <- factor(maindf$Party) #Remove the odd parties levels



# #repeat for Vandat
# Vandat$Party <- factor(Vandat$Party)
# levels(Vandat$Party) <- c(levels(Vandat$Party), "Democratic", "Republican", "Unaffiliated")
# Vandat$Party[which(Vandat$Party %in% c("L", "U") )] <- "Unaffiliated" #get rid of the odd parties and unknowns
# Vandat$Party[which(Vandat$Party %in% "D") ] <- "Democratic"
# Vandat$Party[which(Vandat$Party %in% "R") ] <- "Republican" 
# Vandat$Party <- factor(Vandat$Party) #Remove the odd parties levels


# maindf$Sex[which(maindf$Sex == 'U')] <- NA
# maindf$Sex <- factor(maindf$Sex)
# levels(maindf$Sex)


# #Recode sex in Vandat to match maindf

# Vandat$Sex[which(Vandat$Sex == 'U')] <- NA
# Vandat$Sex <- factor(Vandat$Sex)
# levels(Vandat$Sex)


 # #Recode factors as numeric
# # is.na( maindf$cen00_medianincome)

# # maindf$cen00_medianincomeRec <- lapply(maindf$cen00_medianincome, as.character)
# maindf$cen00_medianincome <- gsub(',', '', maindf$cen00_medianincome)

# maindf$cen00_medianincome <- as.numeric(maindf$cen00_medianincome)



# # maindf$cen10_densityRec <- lapply(maindf$cen10_density, as.character)
# maindf$cen10_density <- gsub(',', '', maindf$cen10_density)

# maindf$cen10_density <- as.numeric(maindf$cen10_density)


# summary(maindf$cons_ppi)

# # maindf$cons_ppiRec <- lapply(maindf$cons_ppi, as.character)
# maindf$cons_ppi <- gsub(",", '',maindf$cons_ppi)
# maindf$cons_ppi <- as.numeric(maindf$cons_ppi)


# # maindf$reg_earliest_yearRec <- lapply(maindf$reg_earliest_year, as.character)
# maindf$reg_earliest_year <- gsub(",", '', maindf$reg_earliest_year)
# maindf$reg_earliest_year <- as.numeric(maindf$reg_earliest_year)

# # maindf$donate_max_demRec <- lapply(maindf$donate_max_dem, as.character)
# maindf$donate_max_dem <- gsub(",", '', maindf$donate_max_dem)
# maindf$donate_max_dem <- as.numeric(maindf$donate_max_dem)



 # #create two dummy variables based on the following
  # # Those who voted in 2008, 2012, but not 2010,
 # # Those who voted in 2010
 
 # maindf$General08 <- factor(maindf$General08)
 # maindf$General10 <- factor(maindf$General10)
 # maindf$General12 <- factor(maindf$General12)
 
 
# maindf$SpecVotes <- 0
# maindf[ which(maindf$General08 %in% c('Y','A') & maindf$General12 %in% c('Y','A')), 'SpecVotes'] <- 1
# maindf[maindf$General10 %in% c('Y','A'), 'SpecVotes'] <- 0

# maindf$TenDummy <- 0
# maindf[maindf$General12 %in% c('Y','A'), 'TenDummy'] <- 1
 
# Vandat$SpecVotes <- 0
# Vandat[ which(Vandat$General08 %in% c('Y','A') & Vandat$General12 %in% c('Y','A')), 'SpecVotes'] <- 1
# Vandat[Vandat$General10 %in% c('Y','A'), 'SpecVotes'] <- 0

# Vandat$TenDummy <- 0
# Vandat[Vandat$General12 %in% c('Y','A'), 'TenDummy'] <- 1
 

# saveRDS(maindf, 'maindf.rds')

# maindf2 <- maindf[which(is.na(maindf$votebuilder_identifier) == F & is.na(maindf$sp03) == F & maindf$sp03 != '' ),]

# maindf2$sp03
# #Way to check % of records that are NA's
# length(na.omit(maindf2[, 'cen10_density']))/length(maindf2[, 'cen10_density'])
# complete.cases(maindf2[,'cen00_medianincome'])


# nrow(maindf2) 

# paste("'", paste(colnames(Vandat), collapse = "', '"), "'", sep = '') #OK, so check it out. These nested paste statements allow me to not only paste the colnames of Vandat without resorting to that cumbersome do.call command, but it also allows me to add on a set of quotations to the outside, making copy and pasting the ones we want removed really easy. 

# # # get rid of categorical columns
# colstoelim <- c("project_name",'lastnameq2', 'dispositionid','datasource_name',  'agent_name', 'datelastcalled', 'firstname', 'lastname', 'lastnameq1','lastnameq2', 'address1', 'address2', 'country', 'countrycode', 'activephone', 'phonenumber1', 'phonenumber2','fips', 'fkey', 'pd01', 'pd02', 'pd03', 'pd04', 'pd05', 'pd06', 'pd07', 'pd08', 'pd09','sp02','sp02.2', 'sp02.2Rec', 'sp07', 'sp07.2Rec','sp07.2', 'sp09', 'sp01',  'sp02Rec' ,"sp03Rec", "sp04Rec", "sp05Rec", "sp06Rec", "sp07Rec", "sp08Rec", 'cons_ppi', 'city', 'reg_earliest_year', 'state', 'donate_max_dem', 'mAddress', 'mCity', 'mState', 'mZip5', 'mZip4', 'vAddress', 'City', 'State', 'Zip5', 'Zip4', 'LastName', 'FirstName', 'MiddleName', 'Suffix', 'Email')

# novarcols <- ('voted_10p')

# colstoelim <- c(colstoelim, novarcols)

# maindf2 <-  maindf2[, !(colnames(maindf2) %in% colstoelim)]
# saveRDS(maindf2, 'maindf2.rds')
# # # create lastN scores

 # # Collect fields to use for scoring
 # # metaFields <- c('General08','General12', 'General10')
 
 
 
# for(i in 1:length(metaFields)) maindf2[ which(maindf2[,metaFields[i]] == 0), metaFields[i]] <-'' # Whenever you see a zero in column i, replace it with a blank space, repeat for all columns i. 
 
 # maindf2$combineMeta <- do.call("paste", c(maindf2[metaFields], sep = '')) #Nee to figure out why do.call works when paste alone won't
 # maindf2$combineMeta <- strsplit( maindf2$combineMeta, split = '')
 # maindf2$LastN <- nchar(maindf2$combineMeta)

# for(i in 1:length(metaFields)) Vandat[ which(Vandat[,metaFields[i]] == 0), metaFields[i]] <-'' # Whenever you see a zero in column i, replace it with a blank space, repeat for all columns i. 
 
 # Vandat$combineMeta <- do.call("paste", c(Vandat[metaFields], sep = '')) #Nee to figure out why do.call works when paste alone won't
 # maindf2$combineMeta <- strsplit( maindf2$combineMeta, split = '')
 # Vandat$LastN <- nchar(Vandat$combineMeta)
