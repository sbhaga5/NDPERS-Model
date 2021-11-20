library("readxl")
library(ggplot2)
library(tidyverse)
setwd(getwd())
rm(list = ls())
#
#User can change these values
StartYear <- 2016
StartProjectionYear <- 2022
EndProjectionYear <- 2051
FileName <- 'Model Inputs 2021.xlsx'
#
#Reading Input File
user_inputs_numeric <- read_excel(FileName, sheet = 'Numeric Inputs')
user_inputs_character <- read_excel(FileName, sheet = 'Character Inputs')
Historical_Data <- read_excel(FileName, sheet = 'Historical Data')
Scenario_Data <- read_excel(FileName, sheet = 'Inv_Returns')
#
##################################################################################################################################################################
#
#Functions for later use
#Function for Present Value for Amortization
PresentValue = function(rate, nper, pmt) {
  PV = pmt * (1 - (1 + rate) ^ (-nper)) / rate * (1 + rate)
  return(PV)
}

NPV = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }
  
  return(NPV)
}

#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  # if(nper == 0){
  #   a <- 0
  # }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period. 
PMT <- function(r, g = 0, nper, pv, t = 1) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}
#
##################################################################################################################################################################
#
#Reading Values from Input input file and assigning values
#Assigning numeric inputs
for(i in 1:nrow(user_inputs_numeric)){
  if(!is.na(user_inputs_numeric[i,2])){
    assign(as.character(user_inputs_numeric[i,2]),as.double(user_inputs_numeric[i,3]))
  }
}
#
#Assigning character inputs
for(i in 1:nrow(user_inputs_character)){
  if(!is.na(user_inputs_character[i,2])){
    assign(as.character(user_inputs_character[i,2]),as.character(user_inputs_character[i,3]))
  }
}

#Create an empty Matrix for the Projection Years
EmptyMatrix <- matrix(0,(EndProjectionYear - StartProjectionYear + 1), 1)
for(j in 1:length(colnames(Historical_Data))){
  TempMatrix <- rbind(as.matrix(Historical_Data[,j]), EmptyMatrix)
  assign(as.character(colnames(Historical_Data)[j]), TempMatrix)
}
#Assign values for Projection Years
FYE <- StartYear:EndProjectionYear
#Get Start Index, since historical data has 3 rows, we want to start at 4
StartIndex <- StartProjectionYear - StartYear + 1
HistoricalIndex <- StartProjectionYear - StartYear

#Initialize the Inflation Adjusted Variables to use later
UAL_AVA_InflAdj <- UAL_AVA
colnames(UAL_AVA_InflAdj) <- 'UAL_AVA_InflAdj'
UAL_MVA_InflAdj <- UAL_MVA
colnames(UAL_MVA_InflAdj) <- 'UAL_MVA_InflAdj'

#Initialize Amortization and Outstnading Base
RowColCount <- (EndProjectionYear - StartProjectionYear + 1)

##################################################################################################################################################################
#Running the model from NC onwards. this gets iterated for different scenarios
# LvDollarorPercent <- 'Lv%'
RunModel <- function(DR_CurrentHires = dis_r_proj_currentHires,           #Discount rate for current hires. Min = 4%. Max = 9%. 0.25% step
                     DR_NewHires = dis_r_proj_newHires,                   #Discount rate for new hires. Min = 4%. Max = 9%. 0.25% step
                     ReturnType = AnalysisType,                           #"Deterministic" or "Stochastic" type of simulated returns.
                     DeSimType = ScenType,                                #Deterministic return scenarios. 
                     StoSimType = SimType,                                #"Assumed" or "Conservative" (for stochastic analysis)
                     FundingPolicy = ER_Policy,                           #"Statutory" or "ADC" funding policy.
                     CostShare_AmoNew = CostSharing_Amo_NewHire,          #"No" or "Yes". "No" means no Amo cost sharing between the employer and new hires.
                     CostShare_NCNew = CostSharing_NC_NewHire,            #"No" or "Yes". "No" means no Normal cost sharing between the employer and new hires.
                     CurrentDebt_period = NoYearsADC_CurrentDebt,         #Amortization period (in years) for current unfunded liability. 
                     NewDebtCurrentHire_period = NoYearsADC_NewDebtCurrentHire,       #Amortization period (in years) for new unfunded liability created under current hire plan
                     NewDebtNewHire_period = NoYearsADC_NewDebtNewHire,               #Amortization period (in years) for new unfunded liability created under new hire plan  
                     AmoMethod_current = AmoMethod_CurrentHire,                       #"Level %" or "Level dollar" amortization method for unfunded liability created under current hire plan
                     AmoMethod_new = AmoMethod_NewHire,                               #"Level %" or "Level dollar" amortization method for unfunded liability created under new hire plan
                     OneTimeInfusion = CashInfusion,                                  #One time cash infusion in 2022.
                     NewHireDC_choice = NewHireDCPct,                                 #Percentage of new hires electing the DC plan. This should be in % unit. 
                     DC_ContRate = DC_Contrib,                                        #DC contribution rate for new hires 
                     BenMult = BenMult_new){
  #Scenario Index for referencing later based on investment return data
  ScenarioIndex <- which(colnames(Scenario_Data) == as.character(DeSimType))
  
  ##Amo period tables
  currentlayer <- seq(CurrentDebt_period, 1)
  futurelayer_currenthire <- seq(NewDebtCurrentHire_period, 1)
  futurelayer_futurehire <- seq(NewDebtNewHire_period, 1)
  n <- max(length(currentlayer), length(futurelayer_currenthire))
  length(currentlayer) <- n
  length(futurelayer_currenthire) <- n
  
  #Amo period table for current hires plan
  OffsetYears_CurrentHires <- rbind(currentlayer, matrix(futurelayer_currenthire, 
                                                         nrow = RowColCount,
                                                         ncol = length(currentlayer),
                                                         byrow = T))
  
  rownames(OffsetYears_CurrentHires) <- NULL         #Remove row names
  
  for (i in 1:ncol(OffsetYears_CurrentHires)) {      #Put the amo periods on diagonal rows
    OffsetYears_CurrentHires[,i] <- lag(OffsetYears_CurrentHires[,i], n = i - 1)
  }
  OffsetYears_CurrentHires[is.na(OffsetYears_CurrentHires)] <- 0    #Turn all NAs in the table to 0s
  
  #Amo period table for future hires plan
  OffsetYears_NewHires <- matrix(futurelayer_futurehire, 
                                 nrow = RowColCount + 1,
                                 ncol = length(futurelayer_futurehire),
                                 byrow = T)
  
  for (i in 1:ncol(OffsetYears_NewHires)) {      #Put the amo periods on diagonal rows
    OffsetYears_NewHires[,i] <- lag(OffsetYears_NewHires[,i], n = i - 1)
  }
  OffsetYears_NewHires[is.na(OffsetYears_NewHires)] <- 0    #Turn all NAs in the table to 0s
  
  ##Amo base and payment tables
  #Default value is Lv% for Amo Base
  #If its Level $, then set to 0
  if(AmoMethod_current == "Level $"){
    AmoBaseInc_CurrentHire <- 0
  }
  
  if(AmoMethod_new == "Level $"){
    AmoBaseInc_NewHire <- 0
  }
  
  #Amo base & payment - current hires initial setup
  OutstandingBase_CurrentHires <- matrix(0, RowColCount + 1, length(currentlayer) + 1)
  Amortization_CurrentHires <- matrix(0, RowColCount + 1, length(currentlayer))
  #Initialize the first UAAL layer and amo payment (current hires)
  OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
  Amortization_CurrentHires[1,1] <- PMT(pv = OutstandingBase_CurrentHires[1,1],
                                       r = NewDR_CurrentHires[HistoricalIndex],
                                       g = AmoBaseInc_CurrentHire, 
                                       nper = OffsetYears_CurrentHires[1,1], 
                                       t = 0.5)
  
  #Amo base & payment - future hires initial setup
  OutstandingBase_NewHires <- matrix(0,RowColCount + 1, length(futurelayer_futurehire) + 1)
  Amortization_NewHires <- matrix(0,RowColCount + 1, length(futurelayer_futurehire))
  
  #intialize this value at 0 for Total ER Contributions
  Total_ER[StartIndex-1] <- 0
  for(i in StartIndex:length(FYE)){
    #ProjectionCount is used because amortization and return scenarios do not start at the same time as start index
    #Because start index includes historical data and thus might be 3 by the time ProjectionCount is 1
    ProjectionCount <- i - StartIndex + 1
    
    #Payroll
    TotalPayroll[i] <- TotalPayroll[i-1]*(1 + Payroll_growth)
    CurrentPayroll[i] <- CurrentPayroll[i-1]*(PayrollTier1Attrition - 0.005*(FYE[i]-NC_StaryYear))
    NewHireDCPayroll[i] <- (TotalPayroll[i] - CurrentPayroll[i])*NewHireDC_choice
    NewHireDBPayroll[i] <- (TotalPayroll[i] - CurrentPayroll[i])*(1 - NewHireDC_choice)
    
    
    #Discount Rate
    OriginalDR_CurrentHires[i] <- dis_r_currentHires
    NewDR_CurrentHires[i] <- DR_CurrentHires
    OriginalDR_NewHires[i] <- dis_r_newHires
    NewDR_NewHires[i] <- DR_NewHires
    #
    #Benefit Payments, Admin Expenses, Refunds, Transfer
    NewHireBenPct[i] <- 0.25/100*(FYE[i] - StartProjectionYear - 1)
    BenPayments_NewHires_prelim[i] <- -1*NewHireBenPct[i]*(NewHireDBPayroll[i] + NewHireDCPayroll[i])
    BenPayments_Total_prelim[i] <- BenPayments_Total_prelim[i-1]*(1 + max(0.0806 - 0.00266*(FYE[i] - StartProjectionYear - 1), BenGrowthMax))
    BenPayments_CurrentHires[i] <- BenPayments_Total_prelim[i] - BenPayments_NewHires_prelim[i]
    BenPayments_NewHires[i] <- -1*NewHireBenPct[i]*NewHireDBPayroll[i]*BenMult/BenMult_current
    BenPayments_Total[i] <- BenPayments_CurrentHires[i] + BenPayments_NewHires[i] 
    
    
    
    #BenPayments_CurrentHires[i] <- as.double(BenefitPayments$CurrentHires[i])
    #BenPayments_NewHires[i] <- as.double(BenefitPayments$NewHires[i])
    AdminExp_CurrentHires[i] <- -1*Admin_Exp_Pct*CurrentPayroll[i]
    AdminExp_NewHires[i] <- -1*Admin_Exp_Pct*NewHireDBPayroll[i]
    Refunds[i] <- Refunds[i-1]*(1 + BenGrowthMax)
    Transfers[i] <- (Refunds[i] + BenPayments_CurrentHires[i])*TransfersPct
    #
    #Accrued Liability, MOY NC - Original DR
    BOYNCExistOrigDR[i] <- NC_CurrentHires_Pct*CurrentPayroll[i]
    BOYNCNewHiresOrigDR[i] <- NC_NewHires_Pct*NewHireDBPayroll[i]
    AccrLiabOrigDR_CurrentHires[i] <- AccrLiabOrigDR_CurrentHires[i-1]*(1+OriginalDR_CurrentHires[i]) + (BOYNCExistOrigDR[i] + BenPayments_CurrentHires[i])*(1+OriginalDR_CurrentHires[i])^PayTimeEOY
    AccrLiabOrigDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i-1]*(1+OriginalDR_CurrentHires[i]) + (BOYNCNewHiresOrigDR[i] + BenPayments_NewHires[i])*(1+OriginalDR_NewHires[i])^PayTimeEOY
    AccrLiabOrigDR_Total[i] <- AccrLiabOrigDR_CurrentHires[i] + AccrLiabOrigDR_NewHires[i]
    #
    #Accrued Liability, MOY NC - New DR
    DRDifference_CurrentHires <- 100*(OriginalDR_CurrentHires[i] - NewDR_CurrentHires[i])
    DRDifference_NewHires <- 100*(OriginalDR_NewHires[i] - NewDR_NewHires[i])
    BOYNCExistNewDR[i] <- BOYNCExistOrigDR[i]*((1+(NCSensDR/100))^(DRDifference_CurrentHires))
    BOYNCNewHiresNewDR[i] <- BOYNCNewHiresOrigDR[i]*((1+(NCSensDR/100))^(DRDifference_NewHires))
    AccrLiabNewDR_CurrentHires[i] <- AccrLiabOrigDR_CurrentHires[i]*((1+(LiabSensDR/100))^(DRDifference_CurrentHires))*((1+(Convexity/100))^((DRDifference_CurrentHires)^2/2))
    AccrLiabNewDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i]*((1+(LiabSensDR/100))^(DRDifference_NewHires))*((1+(Convexity/100))^((DRDifference_NewHires)^2/2))
    AccrLiabNewDR_Total[i] <- AccrLiabNewDR_CurrentHires[i] + AccrLiabNewDR_NewHires[i]
    #
    #NC, EE Contrib
    NC_CurrentHires[i] <- BOYNCExistNewDR[i]/CurrentPayroll[i]  
    NC_NewHires[i] <- BOYNCNewHiresNewDR[i]/NewHireDBPayroll[i]     
    
    if(CostSharing_NC_CurrentHire == 'Yes'){
      EmployeeNC_CurrentHires[i] <- NC_CurrentHires[i]/2    
    } else {
      EmployeeNC_CurrentHires[i] <- EEContribDB_CurrentHires
    }
    
    if(CostShare_NCNew == 'Yes'){
      EmployeeNC_NewHires[i] <- NC_NewHires[i]/2      
    } else {
      EmployeeNC_NewHires[i] <- EEContribDB_NewHires
    }
    
    EmployerNC_CurrentHires[i] <- NC_CurrentHires[i] - EmployeeNC_CurrentHires[i]
    EmployerNC_NewHires[i] <- NC_NewHires[i] - EmployeeNC_NewHires[i]
    
    EE_NC_CurrentHires[i] <- EmployeeNC_CurrentHires[i]*CurrentPayroll[i]
    EE_NC_NewHires[i] <- EmployeeNC_NewHires[i]*NewHireDBPayroll[i]
    # EEPurchase_CurrentHires[i] <- EEPurchases*CurrentPayroll[i]
    # EEPurchase_NewHires[i] <- EEPurchases*NewHireDBPayroll[i]
    
    # if((FYE[i] < 2026) && (ContrFreeze == 'FREEZE')){
    #   ER_NC_CurrentHires[i] <- ER_NC_CurrentHires[i-1]
    #   ER_NC_NewHires[i] <- ER_NC_NewHires[i-1]
    # } else {
      ER_NC_CurrentHires[i] <- EmployerNC_CurrentHires[i]*CurrentPayroll[i] - AdminExp_CurrentHires[i]
      ER_NC_NewHires[i] <- EmployerNC_NewHires[i]*NewHireDBPayroll[i] - AdminExp_NewHires[i]
    # }
    
    if(FYE[i] == 2022){
      ERCashInfusion <- OneTimeInfusion
    } else {
      ERCashInfusion <- 0
    }
    # if(FR_AVA[i] < TransferFRTreshold){
    #   ERSupplemental <- TransferPERS*0
    # } else {
      ERSupplemental <- 0
    # }
    AdditionalER[i] <- ERCashInfusion + ERSupplemental
    
    
    AmoRate_CurrentHires[i] <- sum(Amortization_CurrentHires[ProjectionCount,]) / TotalPayroll[i]
    AmoRate_NewHires[i] <- sum(Amortization_NewHires[ProjectionCount,]) / (NewHireDBPayroll[i] + NewHireDCPayroll[i])
    
    if(CostSharing_Amo_CurrentHire == 'Yes'){
      EEAmoRate_CurrentHires[i] <- AmoRate_CurrentHires[i]/2
    } else {
      EEAmoRate_CurrentHires[i] <- 0
    }
    
    if(CostShare_AmoNew == 'Yes'){
      EEAmoRate_NewHires[i] <- AmoRate_NewHires[i]/2
    } else {
      EEAmoRate_NewHires[i] <- 0
    }
    
    EE_Amo_CurrentHires[i] <- EEAmoRate_CurrentHires[i]*CurrentPayroll[i]
    EE_Amo_NewHires[i] <- EEAmoRate_NewHires[i]*NewHireDBPayroll[i]
    
    # if((FYE[i] < 2026) && (ContrFreeze == 'FREEZE')){
    #   ER_Amo_CurrentHires[i] <- ER_Amo_CurrentHires[i-1]
    #   ER_Amo_NewHires[i] <- ER_Amo_NewHires[i-1]
    # } else {
      if(FundingPolicy == 'Statutory Rate'){
        ER_Amo_CurrentHires[i] <- (ERContrib_CurrentHires - EmployerNC_CurrentHires[i] - Admin_Exp_Pct)*CurrentPayroll[i]
        ER_Amo_NewHires[i] <- (ERContrib_NewHires - EmployerNC_NewHires[i] - Admin_Exp_Pct)*NewHireDBPayroll[i]
      } else {
        ER_Amo_CurrentHires[i] <- max(AmoRate_CurrentHires[i]*TotalPayroll[i] - EE_Amo_CurrentHires[i], -ER_NC_CurrentHires[i])
        ER_Amo_NewHires[i] <- max(AmoRate_NewHires[i]*(NewHireDBPayroll[i] + NewHireDCPayroll[i]) - EE_Amo_NewHires[i], -ER_NC_NewHires[i])
      }
    # }
    
    #Assign values for simulation
    if(StoSimType == 'Assumed'){
      SimReturn <- SimReturnAssumed
    } else if(StoSimType == 'Conservative'){
      SimReturn <- SimReturnConservative
    }
    
    #Return data based on deterministic or stochastic
    if((ReturnType == 'Stochastic') && (i >= StartIndex)){
      ROA_MVA[i] <- rnorm(1,SimReturn,SimVolatility)
    } else if(ReturnType == 'Deterministic'){
      ROA_MVA[i] <- as.double(Scenario_Data[ProjectionCount,ScenarioIndex]) 
    }
    
    #Solvency Contribution
    CashFlows_CurrentHires <- BenPayments_CurrentHires[i] + AdminExp_CurrentHires[i] + Transfers[i] + EE_NC_CurrentHires[i] + EE_Amo_CurrentHires[i] + 
      ER_NC_CurrentHires[i] + EEPurchase_CurrentHires[i] + AdditionalER[i] + ER_Amo_CurrentHires[i]
    CashFlows_NewHires <- BenPayments_NewHires[i] + AdminExp_NewHires[i] + EE_NC_NewHires[i] + EE_Amo_NewHires[i] + 
      ER_NC_NewHires[i] + EEPurchase_NewHires[i] + ER_Amo_NewHires[i]
    CashFlows_Total <- CashFlows_CurrentHires + CashFlows_NewHires + Refunds[i]
    Solv_Contrib_Total[i] <- as.double(max(-(MVA[i-1]*(1+ROA_MVA[i]) + CashFlows_Total*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    Solv_Contrib_CurrentHires[i] <- Solv_Contrib_Total[i]*(AccrLiabNewDR_CurrentHires[i] / AccrLiabNewDR_Total[i])
    Solv_Contrib_NewHires[i] <- Solv_Contrib_Total[i]*(AccrLiabNewDR_NewHires[i] / AccrLiabNewDR_Total[i])
    #
    
    #Net CF, Expected MVA      
    NetCF_CurrentHires[i] <- CashFlows_CurrentHires + Solv_Contrib_CurrentHires[i]
    ExpInvInc_CurrentHires[i] <- MVA_CurrentHires[i-1]*NewDR_CurrentHires[i-1] + NetCF_CurrentHires[i]*((NewDR_CurrentHires[i-1] + 1)*0.5 - 1)
    ExpectedMVA_CurrentHires[i] <- MVA_CurrentHires[i-1] + NetCF_CurrentHires[i] + ExpInvInc_CurrentHires[i]
    MVA_CurrentHires[i] <- MVA_CurrentHires[i-1]*(1+ROA_MVA[i]) + NetCF_CurrentHires[i]*(1+ROA_MVA[i])^PayTimeEOY
    
    NetCF_NewHires[i] <- CashFlows_NewHires + Solv_Contrib_NewHires[i]
    ExpInvInc_NewHires[i] <- MVA_NewHires[i-1]*NewDR_NewHires[i-1] + NetCF_NewHires[i]*((NewDR_NewHires[i-1] + 1)*0.5 - 1)
    ExpectedMVA_NewHires[i] <- MVA_NewHires[i-1] + NetCF_NewHires[i] + ExpInvInc_NewHires[i]
    MVA_NewHires[i] <- MVA_NewHires[i-1]*(1+ROA_MVA[i]) + NetCF_NewHires[i]*(1+ROA_MVA[i])^PayTimeEOY
    #
    
    #Gain Loss, Defered Losses
    GainLoss_CurrentHires[i] <- MVA_CurrentHires[i] - ExpectedMVA_CurrentHires[i]
    CurYearGL_CurrentHires[i] <- GainLoss_CurrentHires[i]*0.2
    Year1GL_CurrentHires[i] <- CurYearGL_CurrentHires[i-1]
    Year2GL_CurrentHires[i] <- Year1GL_CurrentHires[i-1]
    Year3GL_CurrentHires[i] <- Year2GL_CurrentHires[i-1]
    Year4GL_CurrentHires[i] <- Year3GL_CurrentHires[i-1]
    TotalGL_CurrentHires[i] <- CurYearGL_CurrentHires[i] + Year1GL_CurrentHires[i] + Year2GL_CurrentHires[i] + Year3GL_CurrentHires[i] + Year4GL_CurrentHires[i] 
    # 
    
    GainLoss_NewHires[i] <- MVA_NewHires[i] - ExpectedMVA_NewHires[i]
    CurYearGL_NewHires[i] <- GainLoss_NewHires[i]*0.2
    Year1GL_NewHires[i] <- CurYearGL_NewHires[i-1]
    Year2GL_NewHires[i] <- Year1GL_NewHires[i-1]
    Year3GL_NewHires[i] <- Year2GL_NewHires[i-1]
    Year4GL_NewHires[i] <- Year3GL_NewHires[i-1]
    TotalGL_NewHires[i] <- CurYearGL_NewHires[i] + Year1GL_NewHires[i] + Year2GL_NewHires[i] + Year3GL_NewHires[i] + Year4GL_NewHires[i]
    #
    
    #AVA, MVA, UA, FR
    AVA_CurrentHires[i] <- AVA_CurrentHires[i-1] + NetCF_CurrentHires[i] + ExpInvInc_CurrentHires[i] + TotalGL_CurrentHires[i]
    #AVA_CurrentHires[i] <- MVA_CurrentHires[i]
    
    UAL_AVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires[i] - AVA_CurrentHires[i]
    UAL_MVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires[i] - MVA_CurrentHires[i]
    
    AVA_NewHires[i] <- AVA_NewHires[i-1] + NetCF_NewHires[i] + ExpInvInc_NewHires[i] + TotalGL_NewHires[i] 
    
    #AVA_NewHires[i] <- MVA_NewHires[i]
    UAL_AVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - AVA_NewHires[i]
    UAL_MVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - MVA_NewHires[i]
    
    UAL_AVA[i] <- UAL_AVA_CurrentHires[i] + UAL_AVA_NewHires[i]
    UAL_MVA[i] <- UAL_MVA_CurrentHires[i] + UAL_MVA_NewHires[i]
    AVA[i] <- AVA_CurrentHires[i] + AVA_NewHires[i]
    MVA[i] <- MVA_CurrentHires[i] + MVA_NewHires[i]
    FR_AVA[i] <- AVA[i] / AccrLiabNewDR_Total[i]
    FR_MVA[i] <- MVA[i] / AccrLiabNewDR_Total[i]
    UAL_AVA_InflAdj[i] <- UAL_AVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    UAL_MVA_InflAdj[i] <- UAL_MVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    #
    #Employer Contribution  
    #DB Contribution
    Total_Contrib_DB[i] <- EE_NC_CurrentHires[i] + EE_NC_NewHires[i] + EE_Amo_CurrentHires[i] + EE_Amo_NewHires[i] + EEPurchase_CurrentHires[i] + EEPurchase_NewHires[i] +
      AdditionalER[i] + ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i] + Solv_Contrib_Total[i]
    
    Total_ERContrib_DB[i] <- AdditionalER[i] + ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i] + Solv_Contrib_Total[i]
    # if(NewHirePlan == "Hybrid"){
    #   Total_Contrib_DC[i] <- Hybrid_Contrib*NewHiresDBHybridPayroll[i]
    # } else if (NewHirePlan == "DC"){
    #   Total_Contrib_DC[i] <- DC_Contrib*NewHiresDCPayroll[i]
    # }
    
    #DC Contribution
    ERContrib_DC[i] <- DC_ContRate * NewHireDCPayroll[i]
    
    Total_ERContrib[i] <- Total_ERContrib_DB[i] + ERContrib_DC[i]   
    
    ER_InflAdj[i] <- Total_ERContrib[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    ER_Percentage[i] <- Total_ERContrib[i] / TotalPayroll[i]

    #All-in Employer Cost
    Total_ER[i] <- Total_ER[i-1] + ER_InflAdj[i]      #Make sure that the Total_ER number before StartIndex equals 0  
    AllInCost[i] <- Total_ER[i] + UAL_MVA_InflAdj[i]
    
    #Amortization
    #Current Hires
    # BaseCurHires_width = min((ProjectionCount + 1), length(currentlayer) + 1)
    # AmoCurHires_width = min((ProjectionCount + 1), length(currentlayer))
    # BaseNewHires_width = min((ProjectionCount + 1), length(futurelayer_futurehire) + 1)
    # AmoNewHires_width = min((ProjectionCount + 1), length(futurelayer_futurehire))
    
    if(ProjectionCount < nrow(Amortization_CurrentHires)){
      #Oustanding Balance
      OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)] <- OutstandingBase_CurrentHires[ProjectionCount,1:(ncol(OutstandingBase_CurrentHires)-1)]*(1 + NewDR_CurrentHires[i-1]) - (Amortization_CurrentHires[ProjectionCount,1:ncol(Amortization_CurrentHires)]*(1 + NewDR_CurrentHires[i-1])^0.5)
      OutstandingBase_CurrentHires[ProjectionCount+1,1] <- UAL_AVA_CurrentHires[i] - sum(OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)])
      #Amo Layers
      Amortization_CurrentHires[ProjectionCount+1,1:ncol(Amortization_CurrentHires)] <- PMT(r = NewDR_CurrentHires[i],
                                                                                            g = AmoBaseInc_CurrentHire,
                                                                                            nper = pmax(OffsetYears_CurrentHires[ProjectionCount+1,1:ncol(OffsetYears_CurrentHires)],1),
                                                                                            pv = OutstandingBase_CurrentHires[ProjectionCount+1,1:(ncol(OutstandingBase_CurrentHires)-1)],
                                                                                            t = 0.5)
    }
    
    #New Hires
    if(ProjectionCount < nrow(Amortization_NewHires)){
      #Oustanding Balance
      OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)] <- OutstandingBase_NewHires[ProjectionCount,1:(ncol(OutstandingBase_NewHires)-1)]*(1 + NewDR_NewHires[i-1]) - (Amortization_NewHires[ProjectionCount,1:ncol(Amortization_NewHires)]*(1 + NewDR_NewHires[i-1])^0.5)
      OutstandingBase_NewHires[ProjectionCount+1,1] <- UAL_AVA_NewHires[i] - sum(OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)])
      #Amo Layers
      Amortization_NewHires[ProjectionCount+1,1:ncol(Amortization_NewHires)] <- PMT(r = NewDR_NewHires[i],
                                                                                    g = AmoBaseInc_NewHire, 
                                                                                    nper = pmax(OffsetYears_NewHires[ProjectionCount+1,1:ncol(OffsetYears_NewHires)],1),
                                                                                    pv = OutstandingBase_NewHires[ProjectionCount+1,1:(ncol(OutstandingBase_NewHires)-1)],
                                                                                    t = 0.5)
    }
  }
  
  Output <- cbind(FYE,TotalPayroll,CurrentPayroll,NewHireDBPayroll,NewHireDCPayroll,PayrollDCORP,OriginalDR_CurrentHires,NewDR_CurrentHires,
                  OriginalDR_NewHires,NewDR_NewHires,AccrLiabOrigDR_Total,AccrLiabOrigDR_CurrentHires,AccrLiabOrigDR_NewHires,
                  BOYNCExistOrigDR,BOYNCNewHiresOrigDR,AccrLiabNewDR_Total,AccrLiabNewDR_CurrentHires,AccrLiabNewDR_NewHires,
                  BOYNCExistNewDR,BOYNCNewHiresNewDR,AVA,AVA_CurrentHires,AVA_NewHires,MVA,MVA_CurrentHires,MVA_NewHires,ROA_MVA,
                  UAL_AVA,UAL_AVA_InflAdj,UAL_AVA_CurrentHires,UAL_AVA_NewHires,UAL_MVA,UAL_MVA_InflAdj,UAL_MVA_CurrentHires,UAL_MVA_NewHires,
                  FR_AVA,FR_MVA,Impl_FundPeriod,TargetYear100Pct,NC_CurrentHires,NC_NewHires,EmployeeNC_CurrentHires,EmployeeNC_NewHires,
                  EmployerNC_CurrentHires,EmployerNC_NewHires,AmoRate_CurrentHires,AmoRate_NewHires,EEAmoRate_CurrentHires,EEAmoRate_NewHires,
                  BenPayments_CurrentHires,BenPayments_NewHires,Refunds,Transfers,AdminExp_CurrentHires,AdminExp_NewHires,EE_NC_CurrentHires,
                  EE_NC_NewHires,EE_Amo_CurrentHires,EE_Amo_NewHires,EEPurchase_CurrentHires,EEPurchase_NewHires,AdditionalER,ER_NC_CurrentHires,
                  ER_NC_NewHires,ER_Amo_CurrentHires,ER_Amo_NewHires,Solv_Contrib_CurrentHires,Solv_Contrib_NewHires,Solv_Contrib_Total,
                  Total_Contrib_DB, Total_ERContrib_DB, ERContrib_DC, Total_ERContrib, AllInCost, ER_InflAdj,Total_ER,ER_Percentage,NetCF_CurrentHires,ExpInvInc_CurrentHires,
                  ExpectedMVA_CurrentHires,GainLoss_CurrentHires,CurYearGL_CurrentHires,Year1GL_CurrentHires,Year2GL_CurrentHires,Year3GL_CurrentHires,
                  Year4GL_CurrentHires, TotalGL_CurrentHires, NetCF_NewHires,ExpInvInc_NewHires,ExpectedMVA_NewHires,GainLoss_NewHires,CurYearGL_NewHires,Year1GL_NewHires,
                  Year2GL_NewHires,Year3GL_NewHires,Year4GL_NewHires, TotalGL_NewHires)
  
  return(Output)
}

##################################################################################################################################################################

#Test
Test <- RunModel()
Test <- as.data.frame(Test)


#Scenarios
# Scenario_Returns <- as.data.frame(FYE)
# Scenario_UAL <- as.data.frame(FYE)
# Scenario_FR <- as.data.frame(FYE)
# Scenario_ER_Percentage <- as.data.frame(FYE)
# Scenario_ER_InflAdj <- as.data.frame(FYE)
# Scenario_Total_ER <- as.data.frame(FYE)
# Scenario_AllIn_ER <- as.data.frame(FYE)
# 
# #There are 3 types of scenarios here - Recessions, Supplemental and Lv$%
# #We are trying to run all of them outside of a function because we need the data for UAL, FR, etc.
# #If we run them in a function, we can only generate one output
# ScenarioRuns <- 'Return Scenarios'
# #Initialize Max Length, this will be used at the end
# MaxLength <- 0
# if(ScenarioRuns == 'Return Scenarios'){
#   Scenarios <- c('Assumption','Model','Recession','Recurring Recession')
#   for (i in 1:length(Scenarios)){
#     NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'Statutory', Scenarios[i], 'Lv%'))
#     Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
#     Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
#     Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
#     Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
#     Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
#     Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
#     Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
#   }
#   #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
#   ScenarioNames <- Scenarios
#   
# } else if(ScenarioRuns == 'Lv$ vs %'){
#   Scenarios <- c('Assumption','Lv%','Assumption','Lv$','Recurring Recession','Lv%','Recurring Recession','Lv$')
#   MaxLength <- length(Scenarios)/2
#   for (i in 1:MaxLength){
#     NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'ADC', Scenarios[i*2 - 1], Scenarios[i*2]))
#     Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
#     Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
#     Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
#     Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
#     Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
#     Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
#     Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
#   }
#   #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
#   ScenarioNames <- c('Assumption Lv%', 'Assumption Lv$', 'Recurring Recession Lv%', 'Recurring Recession Lv$')
# }
# 
# #MaxLength should in theory be the lenght of the scenarios but because of Lv$%, it may not be
# #Hence we have to do the max function
# if(ScenarioRuns != 'Lv$ vs %'){
#   MaxLength <- length(Scenarios)
# }
# 
# for(i in 1:MaxLength){
#   #Start from StartIndex because thats when the projection is
#   #Total ER is already inflation adjusted
#   TotalERScenario <- sum(Scenario_Total_ER[nrow(Scenario_Total_ER),i+1])/1000
#   #inflation adjusted UAL
#   EndingUAL <- Scenario_UAL[nrow(Scenario_UAL),i+1]/1000
#   AllInER <- Scenario_AllIn_ER[nrow(Scenario_AllIn_ER),i+1]/1000
#   
#   if(i == 1){
#     ERCostTable <- c(TotalERScenario,EndingUAL, AllInER)
#   } else {
#     ERCostTable <- rbind(ERCostTable, c(TotalERScenario,EndingUAL, AllInER))
#   }
# }
# colnames(ERCostTable) <- c('Total ER Contributions','Ending UAL','All in ER Cost')
# rownames(ERCostTable) <- ScenarioNames
# 
# colnames(Scenario_Returns) <- c('FYE',ScenarioNames)
# colnames(Scenario_UAL) <- c('FYE',ScenarioNames)
# colnames(Scenario_FR) <- c('FYE',ScenarioNames)
# colnames(Scenario_ER_Percentage) <- c('FYE',ScenarioNames)
# colnames(Scenario_ER_InflAdj) <- c('FYE',ScenarioNames)
# 
# #library(ggplot2)
# ScenarioPlot <- function(Data, YAxisLabel){
#   ggplot(Data, aes(x = FYE)) +
#     geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
#     geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
#     geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2) +
#     geom_line(aes(y = Data[,5]), color = "#CC0000", size = 2) +
#     labs(y = YAxisLabel, x = 'Year') + ggtitle(YAxisLabel)
#   #scale_linetype_manual(labels = '')
# }
# ScenarioPlot(Scenario_UAL, 'Unfunded Liabilities (MVA)')
# #
# # ##################################################################################################################################################################
# #
# #Simulations
# start_time <- Sys.time()
# #Set seed insures a consistency when simulations are run multiple times
# set.seed((1234))
# NumberofSimulations <- 1000
# #initialize the return simulations based on years and # of simulations
# Returns_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# UAL_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# FR_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# ER_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# 
# #Run the simulations
# for (i in 1:NumberofSimulations){
#   NewData <- as.data.frame(RunModel('Stochastic', SimReturnAssumed, SimVolatility, 'ADC', '', 'Lv%'))
#   Returns_Sims[,i+1] <- NewData$ROA_MVA
#   UAL_Sims[,i+1] <- NewData$UAL_MVA_InflAdj
#   FR_Sims[,i+1] <- NewData$FR_MVA
#   ER_Sims[,i+1] <- NewData$ER_Percentage
# }
# 
# Simulations_Returns <- cbind(FYE,FYE,FYE)
# Simulations_UAL <- cbind(FYE,FYE,FYE)
# Simulations_FR <- cbind(FYE,FYE,FYE)
# Simulations_ER <- cbind(FYE,FYE,FYE)
# 
# #Get the 25th, 50th, 75th percentile
# for(i in 1:length(FYE)){
#   Simulations_Returns[i,] <- t(quantile(Returns_Sims[i,2:ncol(Returns_Sims)],c(0.25,0.5,0.75)))
#   Simulations_UAL[i,] <- t(quantile(UAL_Sims[i,2:ncol(UAL_Sims)],c(0.25,0.5,0.75)))
#   Simulations_FR[i,] <- t(as.data.frame(quantile(FR_Sims[i,2:ncol(FR_Sims)],c(0.25,0.5,0.75))))
#   Simulations_ER[i,] <- t(quantile(ER_Sims[i,2:ncol(ER_Sims)],c(0.25,0.5,0.75)))
# }
# 
# #plot the graphs
# SimulationPlot <- function(Data, FYE){
#   Data <- (as.data.frame(Data))
#   Data <- cbind(FYE, Data)
#   colnames(Data) <- c('FYE','25th Percentile', '50th Percentile', '75th Percentile')
#   ggplot(Data, aes(x = Data[,1])) +
#     geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
#     geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
#     geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2)
# }
# SimulationPlot(Simulations_FR, FYE)
# 
# end_time <- Sys.time()
# print(end_time - start_time)
