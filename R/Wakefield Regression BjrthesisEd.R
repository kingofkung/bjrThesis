# Wakefield Modeling Project Master's Thesis Edition:
# A Ben Rogers Joint
# Started 4/9/2014
# Last Edited 9/28/2014
 

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
library(mice)
 
 
maindf2 <-  readRDS('maindf2.rds')
str(maindf2)

# app


varsToNotInclude <- c("prospectid", 'attemptcount', 'zipcode', 'county', 'voterid','VANID', 'Notes','PollingAddress', 'PollingLocation', 'PrecinctName', 'block_group', 'X2012.ClarityTurnout','Phone','PersonID', 'votebuilder_identifier', 'BoardOfEducationCode','namecheck')

maindf2 <- maindf2[, !colnames(maindf2) %in% varsToNotInclude]

maindf2[, lapply(maindf2, is.character)==T] <- lapply(maindf2[, lapply(maindf2, is.character)==T], as.factor)
head(maindf2)



# pull out a tenth of the data for control purposes
set.seed(12345)
tenperc <-  createFolds(complete.cases(maindf2[,'sp03']), k = 10)

maindf2 <- maindf2[-tenperc$Fold06,]
controldf2 <- maindf2[tenperc$Fold06,]


# junkermod <-  glm(sp03 ~. , family ='binomial',  data = maindf2)
# model.matrix(maindf2)




#BEGIN LOOP HERE!
 i <- 1
  L <- 1
NIVs <- 1 #The number of IVs, this will eventually serve as the building block of the outer loop
MAXIVs <- 10 # Number of Go rounds for IV selection

# nFold <- 2 #number of folds to use for kfold cross validation method of IV selection


deevlist <- c('sp03', 'sp04','sp05','sp06','sp08') 
 # deevlist <- 'sp03SuppRec' #for when we want to test a feature, but are overwhelmed by the number of regressions
 #Select things that we don't want in our IV list below
 dvcols <- c('sp03', 'sp04','sp05','sp06','sp08',"sp03Rec2", "sp04Rec2", "sp04avgmove",  "sp05Rec2", "sp05avgmove", "sp06Rec2", "sp06avgmove",  "sp08Rec2", "sp08avgmove", 'sp03SuppRec', 'sp04SuppRec','sp05SuppRec','sp06SuppRec','sp08SuppRec',  'deevdiv', 'deevfac') #anything that deals with a DV
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

colsnottouse <- c(dvcols,"sp07Rec", "sp07.2Rec", "sp08Rec", 'reg_party_rep','attemptcount', 'voterid', 'votebuilder_identifier', 'dsnRec', 'cen10_asian', 'namecheck', 'phone_primary_cell',zerovar, redundant, mostlyNAs, ActivateVars, clarityvars, VANVars, rescols, colnames(maindf2)[nearZeroVar(maindf2)])


#Imputation section
 impdata <- maindf2[,c(deevlist, colnames(maindf2)[ !colnames(maindf2) %in% colsnottouse],  colnames(maindf2)[colnames(maindf2) %in% c('Age','Sex', 'Party')])] #Put data to impute in a variable
# colnames(impdata)
apply( apply(impdata, 2, is.na), 2, sum)
# colnames(impdata)[ unique(which(is.na(impdata[,1:20] ) == T, arr.ind = T)[,2])]

str(maindf2, list.len = ncol(maindf2))
# lastn <- 225
# print(colnames(maindf2)[lastn])

impdf <- maindf2[,c(deevlist, colnames(maindf2)[colnames(maindf2) %in% redundant], colnames(maindf2)[ !colnames(maindf2) %in% colsnottouse])]
head(impdf)

# library(parallel)
# clus <-  makeCluster(6, type = "PSOCK")
# getOption('mc.cores', 2L)
# for(u in 1:5){
	# mickey <-  mice(impdf)
	# summary(mickey)
	# saveRDS(object = mickey, file = paste('imputation', u ,'.rds', sep = '' ))
	# rm(mickey)
# }
 
 
# mickey <- readRDS(file = 'imputation.rds')
colnames(impdata)
impdata$myAge <- impdata$Age
impdata$age_years[606] <- NA
morty <-  mice(impdata, m = 2, maxit = 5) # so it looks like some columns are just getting switched under the radar. Not sure why, but a full run ignores 3 columns; Age, Sex, and cons_dbi_travel_vacation_3plusplanetrips
plot(Age ~ age_years, data = impdata)

fit <-  with(data = morty, exp = lm(sp03 ~ Sex))
fitpool <- pool(fit)

View(complete(morty, action = 1)[606:607,])
# impdata[, c('Sex', 'Age', 'cons_dbi_travel_vacation_3plusplanetrips')]

# saveRDS(object = morty, file = 'imputationtest.rds')
 
 
ifilename <-  'imputation.rds' #Name of file where imputation is stored:
mickey <- readRDS(file = ifilename)

str(mickey)
#Dataframe appears to have been saved in mickey$pad$data
# But isn't imputed...
# We can get that using complete(mickey)
# str(mickey)
 # complete(mickey)[, !colnames(complete(mickey)) %in% c('sp03', 'sp04', 'sp05', 'sp06', 'sp08')]

xdata <- model.matrix(sp08 ~ . - sp04 - sp05 -sp06 -sp03 - clarity_party - others_num_dem -others_num_rep -reg_party_dem -reg_earliest_month - gender_female - gender_male - others_num_male - cons_childcnt, data = complete(mickey))
# colnames(xdat) 
# length( xdat)
str(xdata) #why do 130 rows seem to just disappear? Identical values elsewhere? No. There are still some NAs in our data
dim(xdata)
head(xdata)

#Until we've got a perfect xdat, we'll need to make certain x and y data line up. We can do this by
ydata <- complete(mickey)$sp08[ 	-which( is.na(complete(mickey)) == T, arr.ind = T)[,1]]

grid <- 10^seq(10, -2, length = 100) #borrowing this directly from islr



testglmnet <-  glmnet( 
x = xdata,
y = ydata,
family = 'binomial',
alpha = 1, #Perform the lasso!
lambda = grid
) 
cvtest <- cv.glmnet(x = xdata, y = ydata, alpha = 1) #Find best value of penalty for our imputed data
plot(cvtest) #plot the value, because it looks cool. 

bestlasso<-  glmnet( 
x = xdata,
y = ydata,
family = 'binomial',
alpha = 1, #Perform the lasso!
lambda = cvtest$lambda.min
) 

coef(bestlasso)
library(glmnetcr) 
print(ifilename)
data.frame(coefs = coef(bestlasso)[which(coef(bestlasso) != 0)], odds.ratios = exp( coef(bestlasso)[which(coef(bestlasso) != 0)]), row.names = rownames(coef(bestlasso))[which(coef(bestlasso) != 0)])

# teedat <-  data.frame(y =c(1,1,1,1), x1 = c(1,1,1,1), x2 = c(0,0,0,1))

y = ydata,
family = 'binomial',
alpha = 1, #Perform the lasso!
lambda = grid
) 
cvtest <- cv.glmnet(x = xdata, y = ydata, alpha = 1) #Find best value of penalty for our imputed data
plot(cvtest) #plot the value, because it looks cool. 

bestlasso<-  glmnet( 
x = xdata,
y = ydata,
family = 'binomial',
alpha = 1, #Perform the lasso!
lambda = cvtest$lambda.min
) 

coef(bestlasso)
library(glmnetcr) 
print(ifilename)
data.frame(coefs = coef(bestlasso)[which(coef(bestlasso) != 0)], odds.ratios = exp( coef(bestlasso)[which(coef(bestlasso) != 0)]), row.names = rownames(coef(bestlasso))[which(coef(bestlasso) != 0)])

# teedat <-  data.frame(y =c(1,1,1,1), x1 = c(1,1,1,1), x2 = c(0,0,0,1))

# model.matrix(y~., data = teedat)

#End IV Removal

# maindf2[,rescols] <- NULL #make sure the recursive elements can't harm reruns by nullifying them pre-loop. 
 
# age, gender party registration, county, and income, if we have it on everyone. 
colnames(maindf2)
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

 priorIVs <- c('cat_age', 'Sex', 'Party', 'CountyName', 'SpecVotes', 'TenDummy', 'cen10_density', 'cen00_medianincome') #Mark's IV's


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
		ivstouse %in% colnames(maindf2)
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
		print(paste('Num of obs used =', nrow(traindf2)))
		# print(paste('Control Prediction rates =',round( contPredRat,4)))
		metabreaker <- 0 #make the metabreaker variable = 0
		# print(paste('bestVarStDev =', round(sqrt(bestVarResid), 6)))
		} # if the best regressions residual variance is bigger than the current regression's residual variance, the current regression replaces the prior best regression. 	
	

# print(paste('i =', i))
rm(rtPredRat)
rm(contPredRat)

if(metabreaker != 0){ iloopbreaker <- 0; bestIV <- NULL} #if metabreaker is still equal to its original nonzero value, break the loop.

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


system('say Done!')
