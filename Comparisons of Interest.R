#Clearing all existing variables
rm(list = ls(all.names = TRUE))

#Loading packages to be used
install.packages("readxl")
install.packages("dplyr")
install.packages("survminer")
library(readxl)
library(survival)
library(survminer)

#Pulling data file and defining final graph array
NCDBMeningioma_1 <- read_excel("/Users/rohitsatish48gmail.com/Downloads/02-JHU/06_Mukherjee Lab/Meningioma/NCDBMeningioma 1.xlsx")
splots <- list()

#Isolating group with grade 2 and grade 3 meningiomas
g23group <- NCDBMeningioma_1[NCDBMeningioma_1$GRADE %in% c(1,2,3), ]

#Isolating group within g23group that has either been assigned to Palliative Care Only(PC==4) or treated with Curative Intent(PC==0)
g23groupPCCI <- g23group[g23group$PALLIATIVE_CARE %in% c(0,4), ]
g23groupPC <- g23group[g23group$PALLIATIVE_CARE == 4, ]

g23grouppCI <- g23group[g23group$PALLIATIVE_CARE == 0, ]



#Forming Kaplan Meier Curve for those with grade 2/3 meningioma and PC==0,4 for academic vs non academic --------
  #Need to find those that died from the palliative care/curative intent group
    #Exclude people we don't have any death data for
    g23groupdeadPCCI <- g23groupPCCI[!is.na(g23groupPCCI$PUF_90_DAY_MORT_CD), ]
    #Isolate those that are dead or alive for sure
    g23groupdeadPCCI <- g23groupdeadPCCI[g23groupdeadPCCI$PUF_90_DAY_MORT_CD %in% c(0,1), ]
    #Exclude those in the group for whom we don't know the months till death
    g23groupdeadPCCI <- g23groupdeadPCCI[!is.na(g23groupdeadPCCI$DX_LASTCONTACT_DEATH_MONTHS), ]
    
    
  #Defining the parameters for the survival object
    #Status
    g23groupdeadPCCI$status <- factor(ifelse(g23groupdeadPCCI$PUF_90_DAY_MORT_CD == 0 , "0", ifelse(g23groupdeadPCCI$PUF_90_DAY_MORT_CD == 1, "1", NA)))
    #Academic or Non-Academic Facility Type
    g23groupdeadPCCI$acadnonacad <- factor(ifelse(g23groupdeadPCCI$FACILITY_TYPE_CD == 3, "Academic" , ifelse(g23groupdeadPCCI$FACILITY_TYPE_CD %in% c(1,2,4) , "Non-Academic", NA )))

    
  #Creating Survival Object to be graphed
    acadnonacacpc04survobj <- Surv(g23groupdeadPCCI$DX_LASTCONTACT_DEATH_MONTHS, as.numeric(g23groupdeadPCCI$status))
  #Plotting Survival Object check
    par("mar")
    par(mar=c(1,1,1,1))
    plot(acadnonacacpc04survobj)
  #Fitting Survival Object
    acadnonacadpc04fit <- survfit(acadnonacacpc04survobj ~ acadnonacad, data = g23groupdeadPCCI)
  #Graphing better with ggsurvplot
    splots[[1]] = ggsurvplot(acadnonacadpc04fit,conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
                              legend.title="Facility Type",  
                            palette=c("dodgerblue2", "orchid2"), 
                            title="Kaplan Meier Curve Showing Survival of Patients with Grade 2 and Grade 3 Meningiomas")
    
#Forming the Kaplan Meier Curve for those with grade 2/3 meningioma and PC==4 only for academic vs non-academic --------
  #Need to find those that died from the palliative care/curative intent group
    #Exclude people we don't have any death data for. Note that we use those that are PC==4 only.
    g23groupdeadPC <- g23groupPC[!is.na(g23groupPC$PUF_90_DAY_MORT_CD), ]
    #Isolate those that are dead or alive for sure
    g23groupdeadPC <- g23groupdeadPC[g23groupdeadPC$PUF_90_DAY_MORT_CD %in% c(0,1), ]
    #Exclude those in the group for whom we don't know the months till death
    g23groupdeadPC <- g23groupdeadPC[!is.na(g23groupdeadPC$DX_LASTCONTACT_DEATH_MONTHS), ]
    
    
    #Defining the parameters for the survival object
    #Status
    g23groupdeadPC$status <- factor(ifelse(g23groupdeadPC$PUF_90_DAY_MORT_CD == 0 , "0", ifelse(g23groupdeadPC$PUF_90_DAY_MORT_CD == 1, "1", NA)))
    #Academic or Non-Academic Facility Type
    g23groupdeadPC$acadnonacad <- factor(ifelse(g23groupdeadPC$FACILITY_TYPE_CD == 3, "Academic" , ifelse(g23groupdeadPC$FACILITY_TYPE_CD %in% c(1,2,4) , "Non-Academic", NA )))
    
    
    #Creating Survival Object to be graphed
    acadnonacacpc4survobj <- Surv(g23groupdeadPC$DX_LASTCONTACT_DEATH_MONTHS, as.numeric(g23groupdeadPC$status))
    #Plotting Survival Object check
    par("mar")
    par(mar=c(1,1,1,1))
    plot(acadnonacacpc4survobj)
    #Fitting Survival Object
    acadnonacadpc4fit <- survfit(acadnonacacpc4survobj ~ acadnonacad, data = g23groupdeadPC)
    #Graphing better with ggsurvplot
    splots[[2]] = ggsurvplot(acadnonacadpc4fit,conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
                             legend.labs = c("Academic", "Non-Academic"),   legend.title="Facility Type",  
                             palette=c("dodgerblue2", "orchid2"), 
                             title="Kaplan Meier Curve Showing Survival of Patients with Grade 2 and Grade 3 Meningiomas")
    
    
    
    
    
    
    
#Graphing ------ 
    arrange_ggsurvplots(splots, print = TRUE,ncol = 2, nrow = 1, risk.table.height = 0.4)
    


    