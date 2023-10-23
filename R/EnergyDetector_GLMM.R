
library(glmmADMB)
library(lme4)

data <- read.csv("../Data/EnergyDetector_GLMM.csv", stringsAsFactors=F)

data$Dscaled <- scale(data$Distance)

#12 models in total

###########
#Full models: 2 models
###########

#Distance^2 + Weekday + Major
NB_D2WM=glmer.nb(Gunshots ~ offset(log(Hours)) + poly(Dscaled,2) + Weekday + Major + (1 | Station), data = data)
#save(NB_D2WM, file='NB_D2WM.RData')

#Distance + Weekday + Major

NB_DWM=glmer.nb(Gunshots ~ offset(log(Hours)) + Dscaled + Weekday + Major + (1 | Station), data = data)
#save(NB_DWM, file='NB_DWM.RData')

###########
#Two variables: 5 models
###########

#Distance^2 + weekday
NB_D2W=glmer.nb(Gunshots ~ offset(log(Hours)) + poly(Dscaled,2) + Weekday + (1 | Station), data = data)
#save(NB_D2W, file='NB_D2W.RData')


#Distance + weekday
NB_DW=glmer.nb(Gunshots ~ offset(log(Hours)) + Dscaled + Weekday + (1 | Station), data = data)
#save(NB_DW, file='NB_DW.RData')


#Distance^2 + Major
NB_D2M=glmer.nb(Gunshots ~ offset(log(Hours)) + poly(Dscaled,2) + Major + (1 | Station), data = data)
#save(NB_D2M, file='NB_D2M.RData')


#Distance + Major
NB_DM=glmer.nb(Gunshots ~ offset(log(Hours)) + Dscaled + Major + (1 | Station), data = data)
#save(NB_DM, file='NB_DM.RData')


#Weekday + Major
NB_WM=glmer.nb(Gunshots ~ offset(log(Hours)) + Weekday + Major + (1 | Station), data = data)
#save(NB_WM, file='NB_WM.RData')

############
#Single Variables: 4
############

#Distance^2
NB_D2=glmer.nb(Gunshots ~ offset(log(Hours)) + poly(Dscaled,2) + (1 | Station), data = data)
#save(NB_D2, file='NB_D2.RData')
#load('NB2.RData')

#Distance
NB_D=glmer.nb(Gunshots ~ offset(log(Hours)) + Dscaled + (1 | Station), data = data)
#save(NB_D, file='NB_D.RData')
#load('NB2.RData')

#Weekday
NB_W =glmer.nb(Gunshots ~ offset(log(Hours)) + Weekday + (1 | Station), data = data)
#save(NB_W, file='NB_W.RData')

#Major
NB_M =glmer.nb(Gunshots ~ offset(log(Hours)) + Major + (1 | Station), data = data)
#save(NB_M, file='NB_M.RData')

###########
#Null
###########

NB_Null =glmer.nb(Gunshots ~ offset(log(Hours)) + (1 | Station), data = data)
#save(NB_Null, file='NB_Null.RData')



#Full
summary(NB_D2WM)$AICtab #Best model.
summary(NB_DWM)$AICtab 
#Two vars.
summary(NB_D2W)$AICtab
summary(NB_DW)$AICtab
summary(NB_D2M)$AICtab
summary(NB_DM)$AICtab
summary(NB_WM)$AICtab
#One var.
summary(NB_D2)$AICtab
summary(NB_D)$AICtab
summary(NB_W)$AICtab
summary(NB_M)$AICtab
#Null
summary(NB_Null)$AICtab




