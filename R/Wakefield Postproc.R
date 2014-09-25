
#Processing needed for writing


# valsToPred <- with(maindf2, list(PartyRec = levels(Party), cat_age = levels(cat_age), Sex = levels(Sex)))

# # print(newdata(bestReg, predVals = valsToPred ))


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
