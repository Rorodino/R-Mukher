#Clearing All Existing Variables------
rm(list=ls(all.names = TRUE))


#Loading Packages and Loading------
install.packages("readxl")
install.packages("dplyr")
library(readxl)
NCDBMeningioma_1 <- read_excel("/Users/rohitsatish48gmail.com/Downloads/02_JHU/06_Mukherjee Lab/Meningioma/NCDBMeningioma 1.xlsx")

#Separtaing out those that received treatment with curative intent. Palliative_Care !=0------
citrue <- NCDBMeningioma_1[NCDBMeningioma_1$PALLIATIVE_CARE == 0, ]
#General Use Vars------
numrows = nrow(citrue)
numcol = ncol(citrue)
print(sum(NCDBMeningioma_1$PALLIATIVE_CARE == 0)+266)
#Separating out those that received Palliative Care Column 4------
pctrue <- NCDBMeningioma_1[NCDBMeningioma_1$PALLIATIVE_CARE == 4, ]
ciandpctrue <- NCDBMeningioma_1[NCDBMeningioma_1$PALLIATIVE_CARE %in% c(0,4), ]
#Palliative Care Recoding------
plcrecode <- factor(ifelse(ciandpctrue$PALLIATIVE_CARE == 4,"1","0"))
#Base Stats-------
  #Age Statistics------
    #Find age column and isolating
      agecolumn <- na.exclude(citrue$AGE)
      
    #Finding Mean of Age and Formatting for curative intent the hard way just for practice with loops and access
      sum = 0
      for(i in 1:numrows)
        sum = sum + agecolumn[i]
      ageMean = sum/numrows
      ageMean <- format(round(ageMean), digits=2)
      
    #Finding Standard Deviation and Formatting for curative intent patients
      agestddev = sd(agecolumn)
      agestddev <- format(round(agestddev), digits=2)
      
    #Finding Mean Age for PC=4 Patients
      sum =0
      pcagecolumn <- na.exclude(pctrue$`AGE`)
      pcageMean = mean(na.exclude(pcagecolumn))
      pcageMean<- format(pcageMean, digits = 3)
      
    #Finding SD for PC=4 patients
      pcagestddev <- sd(na.exclude(pcagecolumn))
      pcagestddev<- format(pcagestddev, digits = 3)
      
    #P-Value through Mann-Whitney U test
    # Our grouping factor/independent variable is whether or not PC==4 and our dependent variable is age. Age in the categories of PC==4 or not requires recode
    # plcrecode creates 2 categorical and age is continuous. So age is DV and plcrecode is IV
      agep <- wilcox.test(citrue$AGE,pctrue$AGE, exact = FALSE)
      
    #Final Output Format
      output <- data.frame(Characteristics=c("Age"),Curative_Treatment = c(paste(ageMean,"±", agestddev)), Pain_Management = c(paste(pcageMean,"±", pcagestddev)), P_Value=format(agep$p.value,digits = 3) )
      
  #Sex Statistics------
    #All people
      sexcolumn <- na.exclude(citrue$SEX)
      femalecount <- sum(sexcolumn == 1, na.rm = TRUE)
      malecount <- sum(sexcolumn == 2, na.rm = TRUE)
      femalepercent <- femalecount/numrows*100
      femalepercent<- format(round(femalepercent, digits=1))
      malepercent <- malecount/numrows*100
      malepercent<- format(round(malepercent, digits=1))
      
    
    #Now for the Palliative_Care non-zero population
      pcsexcolumn <- na.exclude(pctrue$`SEX`)
      pcfemalecount <- sum(pcsexcolumn == 1, na.rm = TRUE)
      pcmalecount <- sum(pcsexcolumn == 2, na.rm = TRUE)
      pcfemalepercent <- pcfemalecount/length(pcsexcolumn)*100
      pcfemalepercent<- format(round(pcfemalepercent, digits=1))
      pcmalepercent <- pcmalecount/length(pcsexcolumn)*100
      pcmalepercent<- format(round(pcmalepercent, digits=1))
    
    #P-Value thorugh Fisher Exact
      #Contingency Table is needed which is going to be inputted to fisher test
      sexcontingency <- data.frame("PC==4" = c(pcfemalecount,pcmalecount), "PC==0" = c(femalecount - pcfemalecount,malecount - pcmalecount),row.names=c("Female","Male"), stringsAsFactors = FALSE)
      colnames(sexcontingency) <- c("PC==4", "PC!=4")
      sexp <- fisher.test(sexcontingency)
    #Final output formatting
      output <- rbind(output, list("Sex","","",format(sexp$p.value,digits = 3)))
      output <- rbind(output, list("Female",paste(femalecount,"(",femalepercent,"%)"),paste(pcfemalecount,"(",pcfemalepercent,"%)"),""))
      output <- rbind(output, list("Male",paste(malecount,"(",malepercent,"%)"),paste(pcmalecount,"(",pcmalepercent,"%)"),""))
  
  #Race Statistics------
    #All People
      racecolumn <- na.exclude(citrue$RACE)
      racecolsize <- length(racecolumn)
      whitecount <- sum(racecolumn == 1, na.rm = TRUE)
      blackcount <- sum(racecolumn == 2, na.rm = TRUE)
      aspicount <- sum(racecolumn %in% c(4,5,6,7,8,10,11,12,13,14,15,16,17,20,21,22,25,26,27,28,30,31,32,96,97), na.rm = TRUE) 
      aindcount <- sum(racecolumn == 3, na.rm = TRUE)
      aspicount
      whitepercent <- whitecount/racecolsize*100
      blackpercent <- blackcount/racecolsize*100
      aspipercent <- aspicount/racecolsize*100
      aindpercent <- aindcount/racecolsize*100
      whitepercent <- format(whitepercent, digits =3)
      blackpercent <- format(blackpercent, digits =3)
      aspipercent <- format(aspipercent, digits =3)
      aindpercent <- format(aindpercent, digits =3)
      output <- rbind(output, list("Race","","",""))
    
    #Now for the Palliative_Care non-zero population
      pcracecolumn <- na.exclude(pctrue$`RACE`)
      pcracecolsize <- length(pcracecolumn)
      pcwhitecount <- sum(pcracecolumn == 1, na.rm = TRUE)
      pcblackcount <- sum(pcracecolumn == 2, na.rm = TRUE)
      pcaspicount <- sum(pcracecolumn %in% c(4,5,6,7,8,10,11,12,13,14,15,16,17,20,21,22,25,26,27,28,30,31,32,96,97), na.rm = TRUE)
      pcaindcount <- sum(pcracecolumn == 3, na.rm = TRUE)
      pcwhitepercent <- pcwhitecount/pcracecolsize*100
      pcblackpercent <- pcblackcount/pcracecolsize*100
      pcaspipercent <- pcaspicount/pcracecolsize*100
      pcaindpercent <- pcaindcount/pcracecolsize*100
      pcwhitepercent <- format(pcwhitepercent, digits =3)
      pcblackpercent <- format(pcblackpercent, digits =3)
      pcaspipercent <- format(pcaspipercent, digits =3)
      pcaindpercent <- format(pcaindpercent, digits =3)
      
    #P-Value with white as reference
      Racetype <- factor(ifelse(citrue$RACE==1,"1",ifelse(citrue$RACE == 2,"2",ifelse(citrue$RACE == 3,"3",ifelse(citrue$RACE %in% c(98,99),NA,"4")))))
      whiteblackcontingency <- data.frame("PC==4" = c(pcwhitecount, pcblackcount), "PC!=4" = c(whitecount - pcwhitecount, blackcount - pcblackcount), row.names = c("White", "Black"), stringsAsFactors = FALSE)
      whiteaspicontingency <-  data.frame("PC==4" = c(pcwhitecount, pcaspicount), "PC!=4" = c(whitecount - pcwhitecount, aspicount - pcaspicount), row.names = c("White", "ASPI"), stringsAsFactors = FALSE)
      whiteaindcontingency <-  data.frame("PC==4" = c(pcwhitecount, pcaindcount), "PC!=4" = c(whitecount - pcwhitecount, aindcount - pcaindcount), row.names = c("White", "AIND"), stringsAsFactors = FALSE)
      whiteblackp <- fisher.test(whiteblackcontingency)
      whiteaspip <- fisher.test(whiteaspicontingency)
      whiteaindp <- fisher.test(whiteaindcontingency)
      
    #Final output formatting
      output <- rbind(output, list("White",paste(whitecount,"(",whitepercent,"%)"),paste(pcwhitecount,"(",pcwhitepercent,"%)"),"Ref"))
      output <- rbind(output, list("Black",paste(blackcount,"(",blackpercent,"%)"),paste(pcblackcount,"(",pcblackpercent,"%)"),format(whiteblackp$p.value,digits = 3)))
      output <- rbind(output, list("Asian or Pacific Islander",paste(aspicount,"(",aspipercent,"%)"),paste(pcaspicount,"(",pcaspipercent,"%)"),format(whiteaspip$p.value,digits = 3)))
      output <- rbind(output, list("American Indian/Alaskan Native",paste(aindcount,"(",aindpercent,"%)"),paste(pcaindcount,"(",pcaindpercent,"%)"),format(whiteaindp$p.value,digits = 3)))
    
  # Ethnicity Statistics------
    # All People
      ethcolumn <- na.exclude(citrue$SPANISH_HISPANIC_ORIGIN)
      ethcolsize <- length(racecolumn)
      hisptruecount <- sum(ethcolumn == 1, na.rm = TRUE)
      hispfalsecount <- sum(ethcolumn == 0, na.rm = TRUE)
      hisptruepercent <- hisptruecount/ethcolsize*100
      hispfalsepercent <- hispfalsecount/ethcolsize*100
      hisptruepercent <- format(hisptruepercent, digits=3)
      hispfalsepercent <- format(hispfalsepercent, digits =3)
     
    
    #Now for the Palliative_Care non-zero population
      pcethcolumn <- na.exclude(pctrue$`SPANISH_HISPANIC_ORIGIN`)
      pcethcolsize <- length(pcethcolumn)
      pchisptruecount <- sum(pcethcolumn == 1, na.rm = TRUE)
      pchispfalsecount <- sum(pcethcolumn == 0, na.rm = TRUE)
      pchisptruepercent <- pchisptruecount/pcethcolsize*100
      pchispfalsepercent <- pchispfalsecount/pcethcolsize*100
      pchisptruepercent <- format(pchisptruepercent, digits=3)
      pchispfalsepercent <- format(pchispfalsepercent, digits =3)
    
    #P-Value thorugh fishers exact
      ethnicitycontigency <- data.frame("PC==4" = c(pchisptruecount,pchispfalsecount) ,"PC!=4" = c(hisptruecount - pchisptruecount, hispfalsecount - pchispfalsecount), row.names = c("Hispanic","Not Hispanic"), stringsAsFactors = FALSE)
      ethp <- fisher.test(ethnicitycontigency)
    #Final output formatting
      output <- rbind(output, list("Ethnicity","","",format(ethp$p.value,digits = 3)))
      output <- rbind(output, list("Hispanic",paste(hisptruecount,"(",hisptruepercent,"%)"),paste(pchisptruecount,"(",pchisptruepercent,"%)"),""))
      output <- rbind(output, list("White",paste(hispfalsecount,"(",hispfalsepercent,"%)"),paste(pchispfalsecount,"(",pchispfalsepercent,"%)"),""))
  
  # Median Household Income Statistics------
    #2020 Income All People
      medincq20 <- na.exclude(citrue$MED_INC_QUAR_16)
      inccount = length(medincq20)
      incq1count <- sum(medincq20 == 1)
      incq2count <- sum(medincq20 == 2)
      incq3count <- sum(medincq20 == 3)
      incq4count <- sum(medincq20 == 4)
      incq1percent <- incq1count/inccount*100
      incq2percent <- incq2count/inccount*100
      incq3percent <- incq3count/inccount*100
      incq4percent <- incq4count/inccount*100
      incq1percent <- format(incq1percent, digits = 3)
      incq2percent <- format(incq2percent, digits = 3)
      incq3percent <- format(incq3percent, digits = 3)
      incq4percent <- format(incq4percent, digits = 3)
      output <- rbind(output, list("2020 Income","","",""))
  
    #Palliative Care
      pcmedincq20 <- na.exclude(pctrue$MED_INC_QUAR_16)
      pcinccount = length(pcmedincq20)
      pcincq1count <- sum(pcmedincq20 == 1)
      pcincq2count <- sum(pcmedincq20 == 2)
      pcincq3count <- sum(pcmedincq20 == 3)
      pcincq4count <- sum(pcmedincq20 == 4)
      pcincq1percent <- pcincq1count/pcinccount*100
      pcincq2percent <- pcincq2count/pcinccount*100
      pcincq3percent <- pcincq3count/pcinccount*100
      pcincq4percent <- pcincq4count/pcinccount*100
      pcincq1percent <- format(pcincq1percent, digits = 3)
      pcincq2percent <- format(pcincq2percent, digits = 3)
      pcincq3percent <- format(pcincq3percent, digits = 3)
      pcincq4percent <- format(pcincq4percent, digits = 3)
      
    #P-Value using Fishers Exact
      lowmedlowcontingency <- data.frame("PC==4" = c(pcincq1count, pcincq2count),"PC!=4" = c(incq1count - pcincq1count, incq2count - pcincq4count), row.names = c("< $46,277","$46,277 - $57,856"), stringsAsFactors = FALSE)
      lowmedhighcontingency <- data.frame("PC==4" = c(pcincq1count, pcincq3count),"PC!=4" = c(incq1count - pcincq1count, incq3count - pcincq4count), row.names = c("< $46,277","$57,857 - $74,062"), stringsAsFactors = FALSE)
      lowhighcontingency <- data.frame("PC==4" = c(pcincq1count, pcincq4count),"PC!=4" = c(incq1count - pcincq1count, incq4count - pcincq4count), row.names = c("< $46,277","$74,063 +"), stringsAsFactors = FALSE)
      lowmedlowp <- fisher.test(lowmedlowcontingency)
      lowmedhighp  <- fisher.test(lowmedhighcontingency)
      lowhighp <- fisher.test(lowhighcontingency)
      
    #Output Formatting
      output <- rbind(output, list("< $46,277",paste(incq1count,"(",incq1percent,")"),paste(pcincq1count,"(",pcincq1percent,")"),"Ref"))
      output <- rbind(output, list("$46,277 - $57,856",paste(incq2count,"(",incq2percent,")"),paste(pcincq2count,"(",pcincq2percent,")"),format(lowmedlowp$p.value,digits = 3)))
      output <- rbind(output, list("$57,857 - $74,062",paste(incq3count,"(",incq3percent,")"),paste(pcincq3count,"(",pcincq3percent,")"),format(lowmedhighp$p.value,digits = 3)))
      output <- rbind(output, list("$74,062 +",paste(incq4count,"(",incq4percent,")"),paste(pcincq4count,"(",pcincq4percent,")"),format(lowhighp$p.value,digits = 3)))
      
  # Residence Statistics------
    #All people
      rescolumn <- na.exclude(citrue$UR_CD_13)
      reslength <- length(rescolumn)
      metrocount <- sum(rescolumn == 1) + sum(rescolumn == 2) + sum(rescolumn == 3)
      urbcount <- sum(rescolumn == 4) + sum(rescolumn == 5) + sum(rescolumn == 6) + sum(rescolumn == 7)
      rurcount <- sum(rescolumn == 8) + sum(rescolumn == 9)
      metropercent <- format(metrocount/reslength*100, digits = 3)
      urbpercent <- format(urbcount/reslength*100, digits = 3)
      rurpercent <- format(rurcount/reslength*100, digits = 3)
    #Palliative Care Group
      pcrescolumn <- na.exclude(pctrue$UR_CD_13)
      pcreslength <- length(pcrescolumn)
      pcmetrocount <- sum(pcrescolumn == 1) + sum(pcrescolumn == 2) + sum(pcrescolumn == 3)
      pcurbcount <- sum(pcrescolumn == 4) + sum(pcrescolumn == 5) + sum(pcrescolumn == 6) + sum(pcrescolumn == 7)
      pcrurcount <- sum(pcrescolumn == 8) + sum(pcrescolumn == 9)
      pcmetropercent <- format(pcmetrocount/pcreslength*100, digits = 3)
      pcurbpercent <- format(pcurbcount/pcreslength*100, digits = 3)
      pcrurpercent <- format(pcrurcount/pcreslength*100, digits = 3)
      
    #P-Value through Fisher's Exact Test
      metrourbancontingency <- data.frame("PC==4" = c(pcmetrocount, pcurbcount),"PC!=4" = c(metrocount - pcmetrocount, urbcount - pcurbcount), row.names = c("Metro","Urban"), stringsAsFactors = FALSE)
      metroruralcontingency <- data.frame("PC==4" = c(pcmetrocount, pcrurcount),"PC!=4" = c(metrocount - pcmetrocount, rurcount - pcrurcount), row.names = c("Metro","Rural"), stringsAsFactors = FALSE)
      metrourbanp <- fisher.test(metrourbancontingency)
      metroruralp <- fisher.test(metroruralcontingency)
      
    #Output Formatting
      output <- rbind(output, list("Residence","","",""))
      output <- rbind(output, list("Metro",paste(metrocount,"(",metropercent,")"),paste(pcmetrocount,"(",pcmetropercent,")"),"Ref"))
      output <- rbind(output, list("Urban",paste(urbcount,"(",urbpercent,")"),paste(pcurbcount,"(",pcurbpercent,")"),format(metrourbanp$p.value,digits = 3)))
      output <- rbind(output, list("Rural",paste(rurcount,"(",rurpercent,")"),paste(pcrurcount,"(",pcrurpercent,")"),format(metroruralp$p.value,digits = 3)))
      
  # Percent No High School Diploma Statistics------
    #All People
      nhdcol <- na.exclude(citrue$NO_HSD_QUAR_16)
      nhdlen <- length(nhdcol)
      highnhdcount <- sum(nhdcol == 1)
      sechighnhdcount <- sum(nhdcol == 2)
      seclownhdcount <- sum(nhdcol == 3)
      lownhdcount <- sum(nhdcol == 4)
      highnhdpercent <- format(highnhdcount/nhdlen*100, digits = 3)
      sechighnhdpercent  <- format(sechighnhdcount/nhdlen*100, digits = 3)
      seclownhdpercent  <- format(seclownhdcount/nhdlen*100, digits = 3)
      lownhdpercent  <- format(lownhdcount/nhdlen*100, digits = 3)
      
    #Palliative Care Group
      pcnhdcol <- na.exclude(pctrue$NO_HSD_QUAR_16)
      pcnhdlen <- length(pcnhdcol)
      pchighnhdcount <- sum(pcnhdcol == 1)
      pcsechighnhdcount <- sum(pcnhdcol == 2)
      pcseclownhdcount <- sum(pcnhdcol == 3)
      pclownhdcount <- sum(pcnhdcol == 4)
      pchighnhdpercent <- format(pchighnhdcount/pcnhdlen*100, digits = 3)
      pcsechighnhdpercent  <- format(pcsechighnhdcount/pcnhdlen*100, digits = 3)
      pcseclownhdpercent  <- format(pcseclownhdcount/pcnhdlen*100, digits = 3)
      pclownhdpercent  <- format(pclownhdcount/pcnhdlen*100, digits = 3)
      
    #P-Value through Fishers Exact test
      highsechighcontingency <- data.frame("PC==4" = c(pchighnhdcount, pcsechighnhdcount),"PC!=4" = c(highnhdcount - pchighnhdcount, sechighnhdcount - pcsechighnhdcount), row.names = c("15.3%+","9.1 15.2"), stringsAsFactors = FALSE)
      highseclowcontingency <- data.frame("PC==4" = c(pchighnhdcount, pcseclownhdcount),"PC!=4" = c(highnhdcount - pchighnhdcount, seclownhdcount - pcseclownhdcount), row.names = c("15.3%+","5.0 9.0"), stringsAsFactors = FALSE)
      highlowcontingency <- data.frame("PC==4" = c(pchighnhdcount, pclownhdcount),"PC!=4" = c((highnhdcount - pchighnhdcount), (lownhdcount - pclownhdcount)), row.names = c("15.3%+","< 5.0%"), stringsAsFactors = FALSE)
      highsechighp <- fisher.test(highsechighcontingency)
      highseclowp <- fisher.test(highseclowcontingency)
      highlowp <- fisher.test(highlowcontingency)
    #Output formatting
      output <- rbind(output, list("Percent No High School Degree 2020","","",""))
      output <- rbind(output, list("15.3%+",paste(highnhdcount,"(",highnhdpercent,")"),paste(pchighnhdcount,"(",pchighnhdpercent,")"),"Ref"))
      output <- rbind(output, list("9.1% - 15.2%",paste(sechighnhdcount,"(",sechighnhdpercent,")"),paste(pcsechighnhdcount,"(",pcsechighnhdpercent,")"),format(highsechighp$p.value,digits = 3)))
      output <- rbind(output, list("5.0% - 9.0%",paste(seclownhdcount,"(",seclownhdpercent,")"),paste(pcseclownhdcount,"(",pcseclownhdpercent,")"),format(highseclowp$p.value,digits = 3)))
      output <- rbind(output, list("< 5.0%",paste(lownhdcount,"(",lownhdpercent,")"),paste(pclownhdcount,"(",pclownhdpercent,")"),format(highlowp$p.value,digits = 3)))
      
  # Insurance Status Statistics------
    #All People
      inscolumn <- na.exclude(citrue$INSURANCE_STATUS[citrue$INSURANCE_STATUS != 9])
      inslen <- length(inscolumn)
      noinscount <- sum(inscolumn == 0)
      privinscount <- sum(inscolumn == 1)
      pubinscount <- sum(inscolumn == 2) + sum(inscolumn == 3) + sum(inscolumn == 4)
      noinspercent <- format(noinscount/inslen*100, digits = 3)
      privinspercent <- format(privinscount/inslen*100, digits = 3)
      pubinspercent <- format(pubinscount/inslen*100, digits = 3)
    
    #Palliative Care Group
      pcinscolumn <- na.exclude(pctrue$INSURANCE_STATUS[pctrue$INSURANCE_STATUS != 9])
      pcinslen <- length(pcinscolumn)
      pcnoinscount <- sum(pcinscolumn == 0)
      pcprivinscount <- sum(pcinscolumn == 1)
      pcpubinscount <- sum(pcinscolumn == 2) + sum(pcinscolumn == 3) + sum(pcinscolumn == 4)
      pcnoinspercent <- format(pcnoinscount/pcinslen*100, digits = 3)
      pcprivinspercent <- format(pcprivinscount/pcinslen*100, digits = 3)
      pcpubinspercent <- format(pcpubinscount/pcinslen*100, digits = 3)
      
    #P-Value thorugh Fishers Exact test
      noneprivcontingency <- data.frame("PC==4" = c(pcnoinscount, pcprivinscount), "PC!=4" = c(noinscount - pcnoinscount, privinscount - pcprivinscount), row.names = c("No Insurance", "Private Insurance"), stringsAsFactors = FALSE) 
      nonepubcontingency <- data.frame("PC==4" = c(pcnoinscount, pcpubinscount), "PC!=4" = c(noinscount - pcnoinscount, pubinscount - pcpubinscount), row.names = c("No Insurance", "Public"), stringsAsFactors = FALSE)
      noneprivp <- fisher.test(noneprivcontingency)
      nonepubp <- fisher.test(nonepubcontingency)
    #Output formatting
      output <- rbind(output,list("Insurance Status","","",""))
      output <- rbind(output,list("No Insurance",paste(noinscount,"(",noinspercent,")"),paste(pcnoinscount,"(",pcnoinspercent,")"),"Ref"))
      output <- rbind(output,list("Private Provider",paste(privinscount,"(",privinspercent,")"),paste(pcprivinscount,"(",pcprivinspercent,")"),format(noneprivp$p.value,digits = 3)))
      output <- rbind(output,list("Public Provider",paste(pubinscount,"(",pubinspercent,")"),paste(pcpubinscount,"(",pcpubinspercent,")"),format(nonepubp$p.value,digits = 3)))
  
  # Charlson Deyo Score Statistics------
    #All People
      chardeycolumn <- na.exclude(citrue$CDCC_TOTAL_BEST)
      chardeylen <- length(chardeycolumn)
      zerocount <- sum(chardeycolumn == 0)
      onecount <- sum(chardeycolumn == 1)
      twocount <- sum(chardeycolumn == 2)
      threecount <- sum(chardeycolumn == 3)
      zeropercent <- format(zerocount/chardeylen*100, digits =3)
      onepercent <- format(onecount/chardeylen*100, digits =3)
      twopercent <- format(twocount/chardeylen*100, digits =3)
      threepercent <- format(threecount/chardeylen*100, digits =3)
    
      #Palliative Care Group
      pcchardeycolumn <- na.exclude(pctrue$CDCC_TOTAL_BEST)
      pcchardeylen <- length(pcchardeycolumn)
      pczerocount <- sum(pcchardeycolumn == 0)
      pconecount <- sum(pcchardeycolumn == 1)
      pctwocount <- sum(pcchardeycolumn == 2)
      pcthreecount <- sum(pcchardeycolumn == 3)
      pczeropercent <- format(pczerocount/pcchardeylen*100, digits =3)
      pconepercent <- format(pconecount/pcchardeylen*100, digits =3)
      pctwopercent <- format(pctwocount/pcchardeylen*100, digits =3)
      pcthreepercent <- format(pcthreecount/pcchardeylen*100, digits =3)
      pczeropercent
      pconepercent
      #P-Value from Fisher's Exact Test
      o1contingency <- data.frame("PC==4" = c(pczerocount, pconecount), "PC!=4" = c(zerocount - pczerocount, onecount - pconecount), row.names = c("0", "1"), stringsAsFactors = FALSE)
      o2contingency <- data.frame("PC==4" = c(pczerocount, pctwocount), "PC!=4" = c(zerocount - pczerocount, twocount - pctwocount), row.names = c("0", "2"), stringsAsFactors = FALSE)
      o3contingency <- data.frame("PC==4" = c(pczerocount, pcthreecount), "PC!=4" = c(zerocount - pczerocount, threecount - pcthreecount), row.names = c("0", "3"), stringsAsFactors = FALSE)
      o1p <- fisher.test(o1contingency)
      o2p <- fisher.test(o2contingency)
      o3p <- fisher.test(o3contingency)
      #Output Formatting
      output <- rbind(output, list("Charlson-Deyo Score","","",""))
      output <- rbind(output, list("0",paste(zerocount,"(",zeropercent,")"),paste(pczerocount,"(",pczeropercent,")"),"Ref"))
      output <- rbind(output, list("1",paste(onecount,"(",onepercent,")"),paste(pconecount,"(",pconepercent,")"), format(o1p$p.value,digits = 3)))
      output <- rbind(output, list("2",paste(twocount,"(",twopercent,")"),paste(pctwocount,"(",pctwopercent,")"), format(o2p$p.value,digits = 3)))
      output <- rbind(output, list("3",paste(threecount,"(",threepercent,")"),paste(pcthreecount,"(",pcthreepercent,")"), format(o3p$p.value,digits = 3)))
      
  # Facility Type Statistics------
    # All People
      factypecolumn <- citrue$FACILITY_TYPE_CD
      factypecolumn <- na.omit(factypecolumn)
      factypelen <- length(factypecolumn)
      ccpcount <- sum(factypecolumn == 1)
      cccpcount <- sum(factypecolumn == 2)
      acadcount <- sum(factypecolumn == 3)
      incpcount <- sum(factypecolumn == 4)
      nonacadcount <- incpcount+cccpcount+ccpcount
      ccppercent <- ccpcount/factypelen*100
      cccppercent <- cccpcount/factypelen*100
      acadpercent <- acadcount/factypelen*100
      incppercent <- incpcount/factypelen*100
      nonacadpercent <- nonacadcount/factypelen*100
      ccppercent <- format(ccppercent, digits =3)
      cccppercent <- format(cccppercent, digits =3)
      acadpercent <- format(acadpercent, digits =3)
      incppercent <- format(incppercent, digits =3)
      nonacadpercent <- format(nonacadpercent, digits = 3)
      output <- rbind(output, list("Facility Type","","",""))
    
    #Now for the Palliative_Care non-zero population
      pc_factypecolumn <- na.exclude(pctrue$`FACILITY_TYPE_CD`)
      pc_factypelen <- length(pc_factypecolumn)
      pc_ccpcount <- sum(pc_factypecolumn == 1)
      pc_cccpcount <- sum(pc_factypecolumn == 2)
      pc_acadcount <- sum(pc_factypecolumn == 3)
      pc_incpcount <- sum(pc_factypecolumn == 4)
      pc_nonacadcount <- pc_ccpcount+pc_cccpcount+pc_incpcount
      pc_ccppercent <- pc_ccpcount/pc_factypelen*100
      pc_cccppercent <- pc_cccpcount/pc_factypelen*100
      pc_acadpercent <- pc_acadcount/pc_factypelen*100
      pc_incppercent <- pc_incpcount/pc_factypelen*100
      pc_ccppercent <- format(pc_ccppercent, digits =3)
      pc_cccppercent <- format(pc_cccppercent, digits =3)
      pc_acadpercent <- format(pc_acadpercent, digits =3)
      pc_incppercent <- format(pc_incppercent, digits =3)
      pc_nonacadpercent <- format(pc_nonacadcount/pc_factypelen*100, digits =3)
    
    #P-Value through Fisher's Test
      dubtripcontingency <- data.frame("PC==4" =c(pc_ccpcount, pc_cccpcount), "PC!=4" = c(ccpcount - pc_ccpcount, cccpcount - pc_cccpcount), row.names =  c("CCP","CCCP"), stringsAsFactors = FALSE)
      dubaccontingency <- data.frame("PC==4" =c(pc_ccpcount, pc_acadcount), "PC!=4" = c(ccpcount - pc_ccpcount, acadcount - pc_acadcount), row.names =  c("CCP","Academic"), stringsAsFactors = FALSE)
      dubincontingency <- data.frame("PC==4" =c(pc_ccpcount, pc_incpcount), "PC!=4" = c(ccpcount - pc_ccpcount, incpcount - pc_incpcount), row.names =  c("CCP","INCP"), stringsAsFactors = FALSE)
      dubtripp <- fisher.test(dubtripcontingency)
      dubacp <- fisher.test(dubaccontingency)
      dubinp <- fisher.test(dubincontingency)
    #Final output formatting
      output <- rbind(output, list("CCP",paste(ccpcount,"(",ccppercent,"%)"),paste(pc_ccpcount,"(",pc_ccppercent,"%)"),"Ref"))
      output <- rbind(output, list("CCCP",paste(cccpcount,"(",cccppercent,"%)"),paste(pc_cccpcount,"(",pc_cccppercent,"%)"),format(dubtripp$p.value,digits = 3)))
      output <- rbind(output, list("Academic",paste(acadcount,"(",acadpercent,"%)"),paste(pc_acadcount,"(",pc_acadpercent,"%)"),format(dubacp$p.value,digits = 3)))
      output <- rbind(output, list("INCP",paste(incpcount,"(",incppercent,"%)"),paste(pc_incpcount,"(",pc_incppercent,"%)"),format(dubinp$p.value,digits = 3)))
  
      
  # Distance to Hospital Statistics------
    #All People
    disthospcolumn <- na.exclude(citrue$CROWFLY)
    meandist <- format(mean(disthospcolumn), digits = 3)
    stddist <- format(sd(disthospcolumn), digits = 3)
  
    #Palliative Care
    pcdisthospcolumn <- na.exclude(pctrue$CROWFLY)
    pcmeandist <- format(mean(pcdisthospcolumn), digits = 3)
    pcstddist <- format(sd(pcdisthospcolumn), digits = 3)
    
    #P-Value
    distp = wilcox.test(citrue$CROWFLY, pctrue$CROWFLY, exact= FALSE)
    
    #Output formatting
    output <- rbind(output, list("Distance to Hospital (miles)",paste(meandist,"±",stddist), paste(pcmeandist,"±",pcstddist), format(distp$p.value,digits = 3)))
  
  # Laterality Statistics------
    #Unsure how to interpret numbers
      latcolumn <- na.exclude(citrue$LATERALITY)
      latlen <- length(latcolumn)
      notpaircount <- sum(latcolumn == 0) + sum(latcolumn == 3) + sum(latcolumn == 4) + sum(latcolumn == 5) + sum(latcolumn == 9)
      rightcount <- sum(latcolumn == 1)
      leftcount <- sum(latcolumn == 2)
      notpairpercent <- format(notpaircount/latlen*100, digits =3)
      rightpairpercent <- format(rightcount/latlen*100, digits =3)
      leftpairpercent <- format(leftcount/latlen*100, digits =3)
      
    #Palliative Care group
      pclatcolumn <- na.exclude(pctrue$LATERALITY)
      pclatlen <- length(pclatcolumn)
      pcnotpaircount <- sum(pclatcolumn == 0) + sum(pclatcolumn == 3) + sum(pclatcolumn == 4) + sum(pclatcolumn == 5) + sum(pclatcolumn == 9)
      pcrightcount <- sum(pclatcolumn == 1)
      pcleftcount <- sum(pclatcolumn == 2)
      pcnotpairpercent <- format(pcnotpaircount/pclatlen*100, digits =3)
      pcrightpairpercent <- format(pcrightcount/pclatlen*100, digits =3)
      pcleftpairpercent <- format(pcleftcount/pclatlen*100, digits =3)
      
    #P-Value through Fisher's Exact Test
      notpairleftcontingency <- data.frame("PC==4" = c(pcnotpaircount, pcleftcount), "PC!=4" = c(notpaircount - pcnotpaircount, leftcount - pcleftcount), row.names = c("Not paired", "Left"), stringsAsFactors = False )
      notpairrightcontingency <- data.frame("PC==4" = c(pcnotpaircount, pcrightcount), "PC!=4" = c(notpaircount - pcnotpaircount, rightcount - pcrightcount), row.names = c("Not paired", "Right"), stringsAsFactors = False )
      notpairleftp <- fisher.test(notpairleftcontingency)
      notpairrightp <- fisher.test(notpairrightcontingency)
      
    #Output Formatting
      output <- rbind(output, list("Laterality","","",""))
      output <- rbind(output, list("Not paired",paste(notpaircount,"(",notpairpercent,")"),paste(pcnotpaircount,"(",pcnotpairpercent,")"),"Ref"))
      output <- rbind(output, list("Left",paste(leftcount,"(",leftpairpercent,")"),paste(pcleftcount,"(",pcleftpairpercent,")"),format(notpairleftp$p.value,digits = 3)))   
      output <- rbind(output, list("Right",paste(rightcount,"(",rightpairpercent,")"),paste(pcrightcount,"(",pcrightpairpercent,")"),format(notpairrightp$p.val,digits = 3)))
      
  # Hospital Volume Statistics
    #Could Not Find Column
      #volcolumn <- citrue$
      
  # Tumor Size (mm) Statistics------
    # All People
      tumsizecolumn <- citrue$TUMOR_SIZE
      tumsizecolumn <- tumsizecolumn[! tumsizecolumn %in% (999)]
      tumsizemean <- mean(tumsizecolumn, na.rm = TRUE)
      tumsizemean <- format(tumsizemean, digits= 3)
      tumsizestddev <- sd(tumsizecolumn,na.rm = TRUE)
      tumsizestddev <- format(tumsizestddev, digits = 3)
      print(paste("Tumor Size(mm): ",tumsizemean,"±", tumsizestddev))
      
    #Same Stats for those that are non-zero Palliative_Care value
      sum =0
      pctumsizecolumn <- na.exclude(pctrue$`TUMOR_SIZE`)
      pctumsizeMean = mean(na.exclude(pctumsizecolumn))
      pctumsizeMean<- format(pctumsizeMean, digits = 3)
      
    #Finding SD for PM patients
      pctumsizestddev <- sd(na.exclude(pctumsizecolumn))
      pctumsizestddev<- format(pctumsizestddev, digits = 3)
      
    #P-Value
      tumsizep<- wilcox.test(citrue$TUMOR_SIZE, pctrue$TUMOR_SIZE)
    
    #Output
      output <- rbind(output, list(format("Tumor Size(mm)",justify="left"),paste(tumsizemean,"±", tumsizestddev),paste(pctumsizeMean,"±", pctumsizestddev),format(tumsizep$p.value,digits = 3)))
  
  #Tumor Grade
      
      
  #Survival Months------
      #All People
      surmoncolumn <- na.exclude(citrue$DX_LASTCONTACT_DEATH_MONTHS)
      surmonmean <- format(mean(surmoncolumn), digits = 3)
      surmonstd <- format(sd(surmoncolumn), digits = 3)
      
      #Palliative Care
      pcsurmoncolumn <- na.exclude(pctrue$DX_LASTCONTACT_DEATH_MONTHS)
      pcsurmonmean <- format(mean(pcsurmoncolumn), digits = 3)
      pcsurmonstd <- format(sd(pcsurmoncolumn), digits = 3)
      
      #P-Value
      surmonp = wilcox.test(citrue$DX_LASTCONTACT_DEATH_MONTHS, pctrue$DX_LASTCONTACT_DEATH_MONTHS, exact= FALSE)
      
      #Output formatting
      output <- rbind(output, list("Survival Months","","",""))
      output <- rbind(output, list("",paste(surmonmean,"±",surmonstd),paste(pcsurmonmean,"±",pcsurmonstd),format(surmonp$p.value,digits = 3)))
  
  #Academic vs Non Academic------
      #Academic vs Non Academic
      acadnonacadcontingency <- data.frame("PC==4" = c(pc_acadcount, pc_nonacadcount),"PC!=4" = c(acadcount - pc_acadcount, nonacadcount - pc_nonacadcount), row.names = c("Academic","Non-Academic"), stringsAsFactors = FALSE)
      acadnonacadp <- fisher.test(acadnonacadcontingency)
      
      output <- rbind(output, list("Academic vs Non Academic","","",""))
      output <- rbind(output, list("Academic",paste(acadcount,"(",acadpercent,")"),paste(pc_acadcount,"(",pc_acadpercent,")"),"Ref"))
      output <- rbind(output, list("Non Academic",paste(nonacadcount,"(",nonacadpercent,")"),paste(pc_nonacadcount,"(",pc_nonacadpercent,")"),format(acadnonacadp$p.value,digits = 3)))   

      
      
      
      
      
#Univariate Analyses-------
    #Age------
    Age_model <- glm(plcrecode ~ ciandpctrue$AGE , family = binomial)
    
    summary_Age_model <- summary(Age_model)
    Age_coefficients <- summary_Age_model$coefficients
    Age_odds_ratios <- exp(summary(Age_model)$coefficients[, "Estimate"])
    Age_conf_int <- exp(confint(Age_model))  # 95% confidence intervals for odds ratios
    Age_p_values <- summary(Age_model)$coefficients[, "Pr(>|z|)"]
    Age_Odd_results <- data.frame(
      Estimate = Age_coefficients[, "Estimate"],
      OR = Age_odds_ratios,
      `2.5 %` = Age_conf_int[, 1],
      `97.5 %` = Age_conf_int[, 2],
      `P-value` = Age_p_values
    )
    print(Age_Odd_results)
    
    #Sex------
    sexrecode <- factor(ifelse(ciandpctrue$SEX == 1,"Female",ifelse(ciandpctrue$SEX == 2, "Male",NA)))
    sexrecode <- relevel(sexrecode, ref = "Female")
    Sex_model <- glm(plcrecode ~ sexrecode, family = binomial)
    summary_Sex_model <- summary(Sex_model)
    Sex_coefficients <- summary_Sex_model$coefficients
    Sex_odds_ratios <- exp(summary(Sex_model)$coefficients[, "Estimate"])
    Sex_conf_int <- exp(confint(Sex_model))  # 95% confidence intervals for odds ratios
    Sex_p_values <- summary(Sex_model)$coefficients[, "Pr(>|z|)"]
    Sex_Odd_results <- data.frame(
      Estimate = Sex_coefficients[, "Estimate"],
      OR = Sex_odds_ratios,
      `2.5 %` = Sex_conf_int[, 1],
      `97.5 %` = Sex_conf_int[, 2],
      `P-value` = Sex_p_values
    )
    print(Sex_Odd_results)
    
    #Race------
    Racetype <- factor(ifelse(ciandpctrue$RACE==1,"White",ifelse(ciandpctrue$RACE == 2,"Black",ifelse(ciandpctrue$RACE == 3,"American Indian",ifelse(ciandpctrue$RACE %in% c(98,99),NA,"Asian/Pacific Islander")))))
    Racetype <- relevel(Racetype, ref = "White")
    Race_model <- glm(plcrecode ~ Racetype, family = binomial)
    summary_Race_model <- summary(Race_model)
    Race_coefficients <- summary_Race_model$coefficients
    Race_odds_ratios <- exp(Race_coefficients[, "Estimate"])
    Race_conf_int <- exp(confint.default(Race_model))  # 95% confidence intervals for odds ratios
    Race_p_values <- Race_coefficients[, "Pr(>|z|)"]
    Race_Odd_results <- data.frame(
      Estimate = Race_coefficients[, "Estimate"],
      OR = Race_odds_ratios,
      `2.5 %` = Race_conf_int[, 1],
      `97.5 %` = Race_conf_int[, 2],
      `P-value` = Race_p_values
    )
    print(Race_Odd_results)
    
    #Ethnicity------
    ethnicitytype <-  factor(ifelse(ciandpctrue$SPANISH_HISPANIC_ORIGIN == 0,"0",ifelse(ciandpctrue$SPANISH_HISPANIC_ORIGIN == 1,"1",NA)))
    ethnicitytype <- relevel(ethnicitytype, ref = "0")
    His_model <- glm(plcrecode ~ ethnicitytype, family = binomial)
    summary_His_model <- summary(His_model)
    His_coefficients <- summary_His_model$coefficients
    His_odds_ratios <- exp(His_coefficients[, "Estimate"])
    His_conf_int <- exp(confint(His_model))  # 95% confidence intervals for odds ratios
    His_p_values <- His_coefficients[, "Pr(>|z|)"]
    His_Odd_results <- data.frame(
      Estimate = His_coefficients[, "Estimate"],
      OR = His_odds_ratios,
      `2.5 %` = His_conf_int[, 1],
      `97.5 %` = His_conf_int[, 2],
      `P-value` = His_p_values
    )
    print(His_Odd_results)
    
    #Median Household Income------
    ciandpctrue$MED_INC_QUAR_16 <- factor(ciandpctrue$MED_INC_QUAR_16)
    ciandpctrue$MED_INC_QUAR_16 <- relevel(ciandpctrue$MED_INC_QUAR_16, ref = "1")
    
    
    MHI_model <- glm(plcrecode ~ ciandpctrue$MED_INC_QUAR_16, family = binomial)
    summary_MHI_model <- summary(MHI_model)
    MHI_coefficients <- summary_MHI_model$coefficients
    MHI_odds_ratios <- exp(MHI_coefficients[, "Estimate"])
    MHI_conf_int <- exp(confint(MHI_model))  # 95% confidence intervals for odds ratios
    MHI_p_values <- MHI_coefficients[, "Pr(>|z|)"]
    MHI_Odd_results <- data.frame(
      Estimate = MHI_coefficients[, "Estimate"],
      OR = MHI_odds_ratios,
      `2.5 %` = MHI_conf_int[, 1],
      `97.5 %` = MHI_conf_int[, 2],
      `P-value` = MHI_p_values
    )
    print(MHI_Odd_results)
    
    #Residence Statistics------
    restype <-  factor(ifelse(ciandpctrue$UR_CD_13 %in% c(1,2,3),"Metro",ifelse(ciandpctrue$UR_CD_13 %in% c(4,5,6,7),"Urban",ifelse(ciandpctrue$UR_CD_13 %in% c(8,9),"Rural",NA))))
    restype <- relevel(restype, ref = "Metro")
    Residence_model <- glm(plcrecode ~ restype, family = binomial)
    summary_Residence_model <- summary(Residence_model)
    Residence_coefficients <- summary_Residence_model$coefficients
    Residence_odds_ratios <- exp(Residence_coefficients[, "Estimate"])
    Residence_conf_int <- exp(confint(Residence_model))  # 95% confidence intervals for odds ratios
    Residence_p_values <- Residence_coefficients[, "Pr(>|z|)"]
    Residence_Odd_results <- data.frame(
      Estimate = Residence_coefficients[, "Estimate"],
      OR = Residence_odds_ratios,
      `2.5 %` = Residence_conf_int[, 1],
      `97.5 %` = Residence_conf_int[, 2],
      `P-value` = Residence_p_values
    )
    print(Residence_Odd_results)
    
    #Percent No High School Diploma------
    nhdtype <- factor(ifelse(ciandpctrue$NO_HSD_QUAR_16 == 1, "1",ifelse(ciandpctrue$NO_HSD_QUAR_16 == 2,"2",ifelse(ciandpctrue$NO_HSD_QUAR_16 == 3,"3",ifelse(ciandpctrue$NO_HSD_QUAR_16 == 4,"4",NA) ))))
    nhdtype <- relevel(nhdtype, ref = "1")
    NHD_model <- glm(plcrecode ~ nhdtype, family = binomial)
    summary_NHD_model <- summary(NHD_model)
    NHD_coefficients <- summary_NHD_model$coefficients
    NHD_odds_ratios <- exp(NHD_coefficients[, "Estimate"])
    NHD_conf_int <- exp(confint(NHD_model))  # 95% confidence intervals for odds ratios
    NHD_p_values <- NHD_coefficients[, "Pr(>|z|)"]
    NHD_Odd_results <- data.frame(
      Estimate = NHD_coefficients[, "Estimate"],
      OR = NHD_odds_ratios,
      `2.5 %` = NHD_conf_int[, 1],
      `97.5 %` = NHD_conf_int[, 2],
      `P-value` = NHD_p_values
    )
    print(NHD_Odd_results)
    
    #Insurance Status------
    instype <- factor(ifelse(ciandpctrue$INSURANCE_STATUS == 0, "Not Insured",ifelse(ciandpctrue$INSURANCE_STATUS == 1, "Private", ifelse(ciandpctrue$INSURANCE_STATUS %in% c(2,3,4),"Public",NA))))
    instype <- relevel(instype, ref = "Not Insured")
    INS_model <- glm(plcrecode ~ instype, family = binomial)
    summary_INS_model <- summary(INS_model)
    INS_coefficients <- summary_INS_model$coefficients
    INS_odds_ratios <- exp(INS_coefficients[, "Estimate"])
    INS_conf_int <- exp(confint(INS_model))  # 95% confidence intervals for odds ratios
    INS_p_values <- INS_coefficients[, "Pr(>|z|)"]
    INS_Odd_results <- data.frame(
      Estimate = INS_coefficients[, "Estimate"],
      OR = INS_odds_ratios,
      `2.5 %` = INS_conf_int[, 1],
      `97.5 %` = INS_conf_int[, 2],
      `P-value` = INS_p_values
    )
    print(INS_Odd_results)
    
    #Charlson Deyo------
    cdtype <- factor(ifelse(ciandpctrue$CDCC_TOTAL_BEST == 0, "0",ifelse(ciandpctrue$CDCC_TOTAL_BEST == 1, "1",ifelse(ciandpctrue$CDCC_TOTAL_BEST == 2, "2", ifelse(ciandpctrue$CDCC_TOTAL_BEST == 3,"3",ifelse(ciandpctrue$CDCC_TOTAL_BEST == 4,"4",NA))))))
    cdtype <- relevel(cdtype, ref = "0")
    CD_model <- glm(plcrecode ~ cdtype, family = binomial)
    summary_CD_model <- summary(CD_model)
    CD_coefficients <- summary_CD_model$coefficients
    CD_odds_ratios <- exp(CD_coefficients[, "Estimate"])
    CD_conf_int <- exp(confint(CD_model))  # 95% confidence intervals for odds ratios
    CD_p_values <- CD_coefficients[, "Pr(>|z|)"]
    CD_Odd_results <- data.frame(
      Estimate = CD_coefficients[, "Estimate"],
      OR = CD_odds_ratios,
      `2.5 %` = CD_conf_int[, 1],
      `97.5 %` = CD_conf_int[, 2],
      `P-value` = CD_p_values
    )
    print(CD_Odd_results)
    
    #Facility Type------
    factype <- factor(ifelse(ciandpctrue$FACILITY_TYPE_CD == 1, "CCP", ifelse(ciandpctrue$FACILITY_TYPE_CD == 2, "CCCP", ifelse(ciandpctrue$FACILITY_TYPE_CD == 3, "Academic", ifelse(ciandpctrue$FACILITY_TYPE_CD == 4,"INCP",NA))) ))
    factype <- relevel(factype, ref = "CCP")
    FAC_model <- glm(plcrecode ~ factype, family = binomial)
    summary_FAC_model <- summary(FAC_model)
    FAC_coefficients <- summary_FAC_model$coefficients
    FAC_odds_ratios <- exp(FAC_coefficients[, "Estimate"])
    FAC_conf_int <- exp(confint(FAC_model))  # 95% confidence intervals for odds ratios
    FAC_p_values <- FAC_coefficients[, "Pr(>|z|)"]
    FAC_Odd_results <- data.frame(
      Estimate = FAC_coefficients[, "Estimate"],
      OR = FAC_odds_ratios,
      `2.5 %` = FAC_conf_int[, 1],
      `97.5 %` = FAC_conf_int[, 2],
      `P-value` = FAC_p_values
    )
    print(FAC_Odd_results)
    
    #Distance to Hospital------
    DTH_model <- glm(plcrecode ~ ciandpctrue$CROWFLY, family = binomial)
    summary_DTH_model <- summary(DTH_model)
    DTH_coefficients <- summary_DTH_model$coefficients
    DTH_odds_ratios <- exp(DTH_coefficients[, "Estimate"])
    DTH_conf_int <- exp(confint(DTH_model))  # 95% confidence intervals for odds ratios
    DTH_p_values <- DTH_coefficients[, "Pr(>|z|)"]
    DTH_Odd_results <- data.frame(
      Estimate = DTH_coefficients[, "Estimate"],
      OR = DTH_odds_ratios,
      `2.5 %` = DTH_conf_int[, 1],
      `97.5 %` = DTH_conf_int[, 2],
      `P-value` = DTH_p_values
    )
    print(DTH_Odd_results)
    
    #Laterality------
    lattype <- factor(ifelse(ciandpctrue$LATERALITY == 1,"Right",ifelse(ciandpctrue$LATERALITY == 2,"Left", ifelse(ciandpctrue$LATERALITY %in% c(0,3,4,5,9),"Not Paired",NA))))
    lattype <- relevel(lattype, ref = "Not Paired")
    LAT_model <- glm(plcrecode ~ lattype, family = binomial)
    summary_LAT_model <- summary(LAT_model)
    LAT_coefficients <- summary_LAT_model$coefficients
    LAT_odds_ratios <- exp(LAT_coefficients[, "Estimate"])
    LAT_conf_int <- exp(confint(LAT_model))  # 95% confidence intervals for odds ratios
    LAT_p_values <- LAT_coefficients[, "Pr(>|z|)"]
    LAT_Odd_results <- data.frame(
      Estimate = LAT_coefficients[, "Estimate"],
      OR = LAT_odds_ratios,
      `2.5 %` = LAT_conf_int[, 1],
      `97.5 %` = LAT_conf_int[, 2],
      `P-value` = LAT_p_values
    )
    print(LAT_Odd_results)
    
    #Hospital Volume
    
    #Tumor Size------
    TS_model <- glm(plcrecode ~ ciandpctrue$TUMOR_SIZE, family = binomial)
    summary_TS_model <- summary(TS_model)
    TS_coefficients <- summary_TS_model$coefficients
    TS_odds_ratios <- exp(summary(TS_model)$coefficients[, "Estimate"])
    TS_conf_int <- exp(confint(TS_model))  # 95% confidence intervals for odds ratios
    TS_p_values <- summary(TS_model)$coefficients[, "Pr(>|z|)"]
    TS_Odd_results <- data.frame(
      Estimate = TS_coefficients[, "Estimate"],
      OR = TS_odds_ratios,
      `2.5 %` = TS_conf_int[, 1],
      `97.5 %` = TS_conf_int[, 2],
      `P-value` = TS_p_values
    )
    print(TS_Odd_results)
    #Tumor Grade------
    gradetype <- factor(ifelse(ciandpctrue$GRADE == 1, "Grade1",ifelse(ciandpctrue$GRADE %in% c(2,3), "Grade3", NA)))
    gradetype <- relevel(gradetype, ref = "Grade1")
    Grade_model <- glm(plcrecode ~ gradetype, family = binomial)
    summary_Grade_model <- summary(Grade_model)
    Grade_coefficients <- summary_Grade_model$coefficients
    Grade_odds_ratios <- exp(Grade_coefficients[, "Estimate"])
    Grade_conf_int <- exp(confint(Grade_model))  # 95% confidence intervals for odds ratios
    Grade_p_values <- Grade_coefficients[, "Pr(>|z|)"]
    Grade_Odd_results <- data.frame(
      Estimate = Grade_coefficients[, "Estimate"],
      OR = Grade_odds_ratios,
      `2.5 %` = Grade_conf_int[, 1],
      `97.5 %` = Grade_conf_int[, 2],
      `P-value` = Grade_p_values
    )
    print(Grade_Odd_results)
    #Academic vs Non-Academic------
    factype1 <- factor(ifelse(ciandpctrue$FACILITY_TYPE_CD == 3, "Academic", ifelse(ciandpctrue$FACILITY_TYPE_CD %in% c(1,2,4) , "Non-Academic", NA )))
    factype1 <- relevel(factype1, ref = "Academic")
    FAC1_model <- glm(plcrecode ~ factype1, family = binomial)
    summary_FAC1_model <- summary(FAC1_model)
    FAC1_coefficients <- summary_FAC1_model$coefficients
    FAC1_odds_ratios <- exp(FAC1_coefficients[, "Estimate"])
    FAC1_conf_int <- exp(confint(FAC1_model))  # 95% confidence intervals for odds ratios
    FAC1_p_values <- FAC1_coefficients[, "Pr(>|z|)"]
    FAC1_Odd_results <- data.frame(
      Estimate = FAC1_coefficients[, "Estimate"],
      OR = FAC1_odds_ratios,
      `2.5 %` = FAC1_conf_int[, 1],
      `97.5 %` = FAC1_conf_int[, 2],
      `P-value` = FAC1_p_values
    )
    print(FAC1_Odd_results)
    
#Multivariate Analysis------
    SRMV_model <- glm(plcrecode ~ ciandpctrue$AGE + sexrecode + Racetype + ethnicitytype + ciandpctrue$MED_INC_QUAR_16  + restype + nhdtype + instype + cdtype + factype1 + ciandpctrue$CROWFLY + lattype+ ciandpctrue$TUMOR_SIZE + gradetype , family = binomial)
    summary(SRMV_model)
    summary_SRMV_model <- summary(SRMV_model)
    SRMV_coefficients <- summary_SRMV_model$coefficients
    SRMV_odds_ratios <- exp(SRMV_coefficients[, "Estimate"])
    SRMV_conf_int <- exp(confint(SRMV_model))  # 95% confidence intervals for odds ratios
    SRMV_p_values <- SRMV_coefficients[, "Pr(>|z|)"]
    length(SRMV_coefficients[,"Estimate"])
    length(SRMV_odds_ratios)
    # Combine results into a single data frame for easy viewing
    SRMV_results <- data.frame(
      Estimate = SRMV_coefficients[,"Estimate"],
      OR = SRMV_odds_ratios,
      `2.5 %` = SRMV_conf_int[, 1],
      `97.5 %` = SRMV_conf_int[, 2],
      `P-value` = SRMV_p_values
    )
    
    # Display the results
    print(SRMV_results)

    
#Uni and Multivariate Output Formatting------
    analysesoutput <- data.frame("Characteristics" = c("Age"), "Odds Ratio [95% CI]" = paste(format(Age_Odd_results$OR[2],digits = 3),"[",format(Age_Odd_results$X2.5..[2],digits = 3),"-",format(Age_Odd_results$X97.5..[2], digits = 3),"]"),"P" = format(Age_Odd_results$P.value[2], digits = 3), "Adjusted Odds Ratio [95% CI]" = paste(format(SRMV_results$OR[2], digits =3),"[",format(SRMV_results$X2.5..[2], digits = 3),"-",format(SRMV_results$X97.5..[2], digits = 3),"]"), "P" = format(SRMV_results$P.value[2], digits = 3))   
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Sex","","","",""))
    analysesoutput <- rbind(analysesoutput, list("Female","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("Male",paste(format(Sex_Odd_results$OR[2],digits = 3),"[",format(Sex_Odd_results$X2.5..[2],digits = 3),"-",format(Sex_Odd_results$X97.5..[2], digits = 3),"]"),format(Sex_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[2], digits =3),"[",format(SRMV_results$X2.5..[2], digits = 3),"-",format(SRMV_results$X97.5..[2], digits = 3),"]"),format(SRMV_results$P.value[2], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Race","","","",""))
    analysesoutput <- rbind(analysesoutput, list("White","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("Black",paste(format(Race_Odd_results$OR[4],digits = 3),"[",format(Race_Odd_results$X2.5..[4],digits = 3),"-",format(Race_Odd_results$X97.5..[4], digits = 3),"]"),format(Race_Odd_results$P.value[4], digits = 3),paste(format(SRMV_results$OR[6], digits =3),"[",format(SRMV_results$X2.5..[6], digits = 3),"-",format(SRMV_results$X97.5..[6], digits = 3),"]"),format(SRMV_results$P.value[6], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("Asian/Pacific Islander",paste(format(Race_Odd_results$OR[3],digits = 3),"[",format(Race_Odd_results$X2.5..[3],digits = 3),"-",format(Race_Odd_results$X97.5..[3], digits = 3),"]"),format(Race_Odd_results$P.value[3], digits = 3),paste(format(SRMV_results$OR[5], digits =3),"[",format(SRMV_results$X2.5..[5], digits = 3),"-",format(SRMV_results$X97.5..[5], digits = 3),"]"),format(SRMV_results$P.value[5], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("American Indian",paste(format(Race_Odd_results$OR[2],digits = 3),"[",format(Race_Odd_results$X2.5..[2],digits = 3),"-",format(Race_Odd_results$X97.5..[2], digits = 3),"]"),format(Race_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[4], digits =3),"[",format(SRMV_results$X2.5..[4], digits = 3),"-",format(SRMV_results$X97.5..[4], digits = 3),"]"),format(SRMV_results$P.value[4], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Ethnicity","","","",""))
    analysesoutput <- rbind(analysesoutput, list("Not Hispanic","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("Hispanic",paste(format(His_Odd_results$OR[2],digits = 3),"[",format(His_Odd_results$X2.5..[2],digits = 3),"-",format(His_Odd_results$X97.5..[2], digits = 3),"]"),format(His_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[7], digits =3),"[",format(SRMV_results$X2.5..[7], digits = 3),"-",format(SRMV_results$X97.5..[7], digits = 3),"]"),format(SRMV_results$P.value[7], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Median Household Income","","","",""))
    analysesoutput <- rbind(analysesoutput, list("< $46,277","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("$46,277 - $57,856",paste(format(MHI_Odd_results$OR[2],digits = 3),"[",format(MHI_Odd_results$X2.5..[2],digits = 3),"-",format(MHI_Odd_results$X97.5..[2], digits = 3),"]"),format(MHI_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[8], digits =3),"[",format(SRMV_results$X2.5..[8], digits = 3),"-",format(SRMV_results$X97.5..[8], digits = 3),"]"),format(SRMV_results$P.value[8], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("$57,857 - $74,062",paste(format(MHI_Odd_results$OR[3],digits = 3),"[",format(MHI_Odd_results$X2.5..[3],digits = 3),"-",format(MHI_Odd_results$X97.5..[3], digits = 3),"]"),format(MHI_Odd_results$P.value[3], digits = 3),paste(format(SRMV_results$OR[9], digits =3),"[",format(SRMV_results$X2.5..[9], digits = 3),"-",format(SRMV_results$X97.5..[9], digits = 3),"]"),format(SRMV_results$P.value[9], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("$74,063 +",paste(format(MHI_Odd_results$OR[4],digits = 3),"[",format(MHI_Odd_results$X2.5..[4],digits = 3),"-",format(MHI_Odd_results$X97.5..[4], digits = 3),"]"),format(MHI_Odd_results$P.value[4], digits = 3),paste(format(SRMV_results$OR[10], digits =3),"[",format(SRMV_results$X2.5..[10], digits = 3),"-",format(SRMV_results$X97.5..[10], digits = 3),"]"),format(SRMV_results$P.value[10], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Residence","","","",""))
    analysesoutput <- rbind(analysesoutput, list("Metro","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("Urban",paste(format(Residence_Odd_results$OR[3],digits = 3),"[",format(Residence_Odd_results$X2.5..[3],digits = 3),"-",format(Residence_Odd_results$X97.5..[3], digits = 3),"]"),format(Residence_Odd_results$P.value[3], digits = 3),paste(format(SRMV_results$OR[12], digits =3),"[",format(SRMV_results$X2.5..[12], digits = 3),"-",format(SRMV_results$X97.5..[12], digits = 3),"]"),format(SRMV_results$P.value[12], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("Rural",paste(format(Residence_Odd_results$OR[2],digits = 3),"[",format(Residence_Odd_results$X2.5..[2],digits = 3),"-",format(Residence_Odd_results$X97.5..[2], digits = 3),"]"),format(Residence_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[11], digits =3),"[",format(SRMV_results$X2.5..[11], digits = 3),"-",format(SRMV_results$X97.5..[11], digits = 3),"]"),format(SRMV_results$P.value[11], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Percent No High School Degree 2020","","","",""))
    analysesoutput <- rbind(analysesoutput, list("15.3%+","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("9.1% - 15.2%",paste(format(NHD_Odd_results$OR[2],digits = 3),"[",format(NHD_Odd_results$X2.5..[2],digits = 3),"-",format(NHD_Odd_results$X97.5..[2], digits = 3),"]"),format(NHD_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[13], digits =3),"[",format(SRMV_results$X2.5..[13], digits = 3),"-",format(SRMV_results$X97.5..[13], digits = 3),"]"),format(SRMV_results$P.value[13], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("5.0% - 9.0%",paste(format(NHD_Odd_results$OR[3],digits = 3),"[",format(NHD_Odd_results$X2.5..[3],digits = 3),"-",format(NHD_Odd_results$X97.5..[3], digits = 3),"]"),format(NHD_Odd_results$P.value[3], digits = 3),paste(format(SRMV_results$OR[14], digits =3),"[",format(SRMV_results$X2.5..[14], digits = 3),"-",format(SRMV_results$X97.5..[14], digits = 3),"]"),format(SRMV_results$P.value[14], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("< 5.0%",paste(format(NHD_Odd_results$OR[4],digits = 3),"[",format(NHD_Odd_results$X2.5..[4],digits = 3),"-",format(NHD_Odd_results$X97.5..[4], digits = 3),"]"),format(NHD_Odd_results$P.value[4], digits = 3),paste(format(SRMV_results$OR[15], digits =3),"[",format(SRMV_results$X2.5..[15], digits = 3),"-",format(SRMV_results$X97.5..[15], digits = 3),"]"),format(SRMV_results$P.value[15], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Insurance Status","","","",""))
    analysesoutput <- rbind(analysesoutput, list("No Insurance","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("Private Provider",paste(format(INS_Odd_results$OR[3],digits = 3),"[",format(INS_Odd_results$X2.5..[3],digits = 3),"-",format(INS_Odd_results$X97.5..[3], digits = 3),"]"),format(INS_Odd_results$P.value[3], digits = 3),paste(format(SRMV_results$OR[16], digits =3),"[",format(SRMV_results$X2.5..[16], digits = 3),"-",format(SRMV_results$X97.5..[16], digits = 3),"]"),format(SRMV_results$P.value[16], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("Public Provider",paste(format(INS_Odd_results$OR[2],digits = 3),"[",format(INS_Odd_results$X2.5..[2],digits = 3),"-",format(INS_Odd_results$X97.5..[2], digits = 3),"]"),format(INS_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[17], digits =3),"[",format(SRMV_results$X2.5..[17], digits = 3),"-",format(SRMV_results$X97.5..[17], digits = 3),"]"),format(SRMV_results$P.value[17], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Charlson-Deyo Score","","","",""))
    analysesoutput <- rbind(analysesoutput, list("0","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("1",paste(format(CD_Odd_results$OR[2],digits = 3),"[",format(CD_Odd_results$X2.5..[2],digits = 3),"-",format(CD_Odd_results$X97.5..[2], digits = 3),"]"),format(CD_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[18], digits =3),"[",format(SRMV_results$X2.5..[18], digits = 3),"-",format(SRMV_results$X97.5..[18], digits = 3),"]"),format(SRMV_results$P.value[18], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("2",paste(format(CD_Odd_results$OR[3],digits = 3),"[",format(CD_Odd_results$X2.5..[3],digits = 3),"-",format(CD_Odd_results$X97.5..[3], digits = 3),"]"),format(CD_Odd_results$P.value[3], digits = 3),paste(format(SRMV_results$OR[19], digits =3),"[",format(SRMV_results$X2.5..[19], digits = 3),"-",format(SRMV_results$X97.5..[19], digits = 3),"]"),format(SRMV_results$P.value[19], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("3",paste(format(CD_Odd_results$OR[4],digits = 3),"[",format(CD_Odd_results$X2.5..[4],digits = 3),"-",format(CD_Odd_results$X97.5..[4], digits = 3),"]"),format(CD_Odd_results$P.value[4], digits = 3),paste(format(SRMV_results$OR[20], digits =3),"[",format(SRMV_results$X2.5..[20], digits = 3),"-",format(SRMV_results$X97.5..[20], digits = 3),"]"),format(SRMV_results$P.value[20], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("Academic vs Non-Academic",paste(format(FAC1_Odd_results$OR[2],digits = 3),"[",format(FAC1_Odd_results$X2.5..[2],digits = 3),"-",format(FAC1_Odd_results$X97.5..[2], digits = 3),"]"),format(FAC1_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[21], digits =3),"[",format(SRMV_results$X2.5..[21], digits = 3),"-",format(SRMV_results$X97.5..[21], digits = 3),"]"),format(SRMV_results$P.value[21], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("Distance to Hospital",paste(format(DTH_Odd_results$OR[2],digits = 3),"[",format(DTH_Odd_results$X2.5..[2],digits = 3),"-",format(DTH_Odd_results$X97.5..[2], digits = 3),"]"),format(DTH_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[22], digits =3),"[",format(SRMV_results$X2.5..[22], digits = 3),"-",format(SRMV_results$X97.5..[22], digits = 3),"]"),format(SRMV_results$P.value[22], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Laterality","","","",""))
    analysesoutput <- rbind(analysesoutput, list("Not Paired","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("Left",paste(format(LAT_Odd_results$OR[2],digits = 3),"[",format(LAT_Odd_results$X2.5..[2],digits = 3),"-",format(LAT_Odd_results$X97.5..[2], digits = 3),"]"),format(LAT_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[23], digits =3),"[",format(SRMV_results$X2.5..[23], digits = 3),"-",format(SRMV_results$X97.5..[23], digits = 3),"]"),format(SRMV_results$P.value[23], digits = 3)))
    analysesoutput <- rbind(analysesoutput, list("Right",paste(format(LAT_Odd_results$OR[3],digits = 3),"[",format(LAT_Odd_results$X2.5..[3],digits = 3),"-",format(LAT_Odd_results$X97.5..[3], digits = 3),"]"),format(LAT_Odd_results$P.value[3], digits = 3),paste(format(SRMV_results$OR[24], digits =3),"[",format(SRMV_results$X2.5..[24], digits = 3),"-",format(SRMV_results$X97.5..[24], digits = 3),"]"),format(SRMV_results$P.value[24], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("Tumor Size",paste(format(TS_Odd_results$OR[2],digits = 3),"[",format(TS_Odd_results$X2.5..[2],digits = 3),"-",format(TS_Odd_results$X97.5..[2], digits = 3),"]"),format(TS_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[25], digits =3),"[",format(SRMV_results$X2.5..[25], digits = 3),"-",format(SRMV_results$X97.5..[25], digits = 3),"]"),format(SRMV_results$P.value[25], digits = 3)))

    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Tumor Grade","","","",""))
    analysesoutput <- rbind(analysesoutput, list("Grade1","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("Grade 2 or 3",paste(format(Grade_Odd_results$OR[2],digits = 3),"[",format(Grade_Odd_results$X2.5..[2],digits = 3),"-",format(Grade_Odd_results$X97.5..[2], digits = 3),"]"),format(Grade_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[26], digits =3),"[",format(SRMV_results$X2.5..[26], digits = 3),"-",format(SRMV_results$X97.5..[26], digits = 3),"]"),format(SRMV_results$P.value[26], digits = 3)))
    
    analysesoutput <- rbind(analysesoutput, list("","","","",""),list("Academic vs Non Academic","","","",""))
    analysesoutput <- rbind(analysesoutput, list("Academic","Ref","","Ref",""))
    analysesoutput <- rbind(analysesoutput, list("Non Academic",paste(format(FAC1_Odd_results$OR[2],digits = 3),"[",format(FAC1_Odd_results$X2.5..[2],digits = 3),"-",format(FAC1_Odd_results$X97.5..[2], digits = 3),"]"),format(FAC1_Odd_results$P.value[2], digits = 3),paste(format(SRMV_results$OR[27], digits =3),"[",format(SRMV_results$X2.5..[27], digits = 3),"-",format(SRMV_results$X97.5..[27], digits = 3),"]"),format(SRMV_results$P.value[27], digits = 3)))
    
    
#Call for output + anything else to verify------

output
analysesoutput
print(paste("n=",numrows," for curative_treatment and n=",nrow(pctrue)," for pain management."))
#For some NA values cause the sample size tobe smaller than stated above




#Survival curves------
install.packages("survminer")
install.packages('ggpubr')
library(dplyr)
library(survival)
library(cli)
library(ggpubr)
library(survminer)

splots <- list()

g23groupPCCI <- NCDBMeningioma_1[NCDBMeningioma_1$PALLIATIVE_CARE %in% c(0,4), ]
g23groupPC <- NCDBMeningioma_1[NCDBMeningioma_1$PALLIATIVE_CARE == 4, ]

g23grouppCI <- NCDBMeningioma_1[NCDBMeningioma_1$PALLIATIVE_CARE == 0, ]

#Forming Kaplan Meier Curve for those with  PC==0,4 for academic vs non academic --------
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
                         legend.labs = c("Academic", "Non Academic"),legend.title="Facility Type",  
                         palette=c("dodgerblue2", "orchid2"), 
                         title="KM Curve: Patients with No Prior Treatments")

#Forming the Kaplan Meier Curve for those with PC==4 only for academic vs non-academic --------
#Need to find those that died from the palliative care/curative intent group
  #Exclude people we don't have any death data for. Note that we use those that are PC==4 only.
    g23groupdeadPC <- g23groupPC[!is.na(g23groupPC$PUF_90_DAY_MORT_CD), ]
  #Isolate those that are dead or alive for sure
    g23groupdeadPC <- g23groupdeadPC[g23groupdeadPC$PUF_90_DAY_MORT_CD %in% c(1), ]
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
                         title="KM Curve: Patients with Palliative Care Only")







#Graphing ------ 
arrange_ggsurvplots(splots, print = TRUE,ncol = 2, nrow = 1, risk.table.height = 0.4)
    

    
    