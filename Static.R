Static_Func = function(x,y,z,a,b,d,j, m, f) {
  
  withProgress(message = "Calculations in progress, please wait...", {
  
  options(digits = 5)
  
  # -----------------------------------------------------------------------------#
  # Import Data and Fill in Beats                                                #
  # -----------------------------------------------------------------------------#
    Dataset1 = j
    attach(Dataset1)
    
    
    Dataset3 = m
    attach(Dataset3)
  
  
  
  # -----------------------------------------------------------------------------#
  # MNSE Function                                                                #
  # -----------------------------------------------------------------------------#
  
  # Function to compute MNSE
  mnse = function(actual, predicted) {
    if (length(actual) != length(predicted)) {
      stop("Vectors 'actual' and 'predicted' must be the same length.")
    }
    
    mean_actual = mean(actual)
    
    # Prevent divide-by-zero
    if (mean_actual == 0) {
      stop("Mean of actual values is zero; normalization not possible.")
    }
    
    squared_errors = ((actual - predicted) / mean_actual)^2
    mnse_value = mean(squared_errors)
    
    return(mnse_value)
  }
  
  
  #---- Count Beat Number for each data set -----------
  # Dataset 1
  Count = 0
  for (i in 1:length(Dataset1$Beat_Num)){
    if (is.na(Dataset1$Beat_Num[i])){
      
    } else {
      Count = Count + 1
      Dataset1$Beat_Num[i] = Count
    }
  }
  
  
  # Dataset 3
  Count = 0
  for (i in 1:length(Dataset3$Beat_Num)){
    if (is.na(Dataset3$Beat_Num[i])){
      
    } else {
      Count = Count + 1
      Dataset3$Beat_Num[i] = Count
    }
  }
  
  #---- Fill each beat with appropriate number until next beat. ----
  Baseline_Dataset = Dataset1 %>% fill(Beat_Num,.direction="down")
  Stressor_Dataset = Dataset3 %>% fill(Beat_Num,.direction="down")
  
  
  
  
  
  #---- Add timeline to datasets ---- 
  Fs = 1 / f
  
  
  Baseline_Dataset$Time = (1:length(Baseline_Dataset$Beat_Num)) * Fs
  Stressor_Dataset$Time = (1:length(Stressor_Dataset$Beat_Num)) * Fs
  
  
  # -----------------------------------------------------------------------------#
  # signal Processing                                                            #
  # -----------------------------------------------------------------------------#
  
  #---- linear interpolated NA values in data (cut for physiocal or data errors) ---- 
  Baseline_Dataset$SplineMCAv = na.approx(Baseline_Dataset$MCAv, x = Baseline_Dataset$Time)
  Baseline_Dataset$SplineMAP = na.approx(Baseline_Dataset$reBAP, x = Baseline_Dataset$Time)
  
  Stressor_Dataset$SplineMCAv = na.approx(Stressor_Dataset$MCAv, x = Stressor_Dataset$Time)
  Stressor_Dataset$SplineMAP = na.approx(Stressor_Dataset$reBAP, x = Stressor_Dataset$Time)
  
  Baseline_Dataset$Beat_Num = as.numeric(Baseline_Dataset$Beat_Num)
  Stressor_Dataset$Beat_Num = as.numeric(Stressor_Dataset$Beat_Num)
  
  
  #---- Filter MCAv Through Median Filter (3 sample width) ---- 
  
  Base_MedianFilteredMCAv = runmed(Baseline_Dataset$SplineMCAv, k = 3, endrule = "median")
  Baseline_Dataset$MedianFilteredMCAv = Base_MedianFilteredMCAv
  
  Stress_MedianFilteredMCAv = runmed(Stressor_Dataset$SplineMCAv, k = 3, endrule = "median")
  Stressor_Dataset$MedianFilteredMCAv = Stress_MedianFilteredMCAv
  
  
  #---- Filter MCAv and MAP Through Butterworth Filter (0 phase, 8th order) ----
  
  Base_BWFiltered_LMCAv = bw_filter(Baseline_Dataset$MedianFilteredMCAv, n = 8, W = 0.04, zero_lag = TRUE)
  Base_BWFiltered_MAP = bw_filter(Baseline_Dataset$SplineMAP, n = 8, W = 0.04, zero_lag = TRUE)
  
  Baseline_Dataset$FilteredLMCAv = Base_BWFiltered_LMCAv
  Baseline_Dataset$FilteredMAP = Base_BWFiltered_MAP
  
  
  Stress_BWFiltered_LMCAv = bw_filter(Stressor_Dataset$MedianFilteredMCAv, n = 8, W = 0.04, zero_lag = TRUE)
  Stress_BWFiltered_MAP = bw_filter(Stressor_Dataset$SplineMAP, n = 8, W = 0.04, zero_lag = TRUE)
  
  Stressor_Dataset$FilteredLMCAv = Stress_BWFiltered_LMCAv
  Stressor_Dataset$FilteredMAP = Stress_BWFiltered_MAP
  
  incProgress(amount = 0.25)
  # -----------------------------------------------------------------------------#
  # Create Blank Matrix to Receive Newly Calculated Data                         #
  # -----------------------------------------------------------------------------#
  Base_matrix = matrix(, ncol = 11, nrow= max(as.numeric(Baseline_Dataset$Beat_Num)))
  colnames(Base_matrix) = c("Beat", "CrCP", "RAP", "MCAv", "MAP", "Check", "Time", "CPI", "CPP", "CVCi", "PETCO2")
  Base_matrix[1,7] = 0
  
  Stress_matrix = matrix(, ncol = 11, nrow= max(as.numeric(Stressor_Dataset$Beat_Num)))
  colnames(Stress_matrix) = c("Beat", "CrCP", "RAP", "MCAv", "MAP", "Check", "Time", "CPI", "CPP", "CVCi", "PETCO2")
  Stress_matrix[1,7] = 0
  
  # -----------------------------------------------------------------------------#
  #Calculating RAP, CrcP, MCAv, and MAP Values                                   #
  # -----------------------------------------------------------------------------#
  #---- Baseline Calculations ---- 
  
  for(i in 1:max(Baseline_Dataset$Beat_Num)){ 
    
    #i = 7 (for debugging purposes) 
    #----------------------------------------------------------------------------#
    # Pulling each heart beat data and numbering each sample. Additionally,      #
    # calculating each heart beats periods.                                      #
    #----------------------------------------------------------------------------#
    PulledData = subset(Baseline_Dataset, Beat_Num == i)
    PulledData$Sample = seq(1,length(PulledData$Sample))
    CardiacCycleLength = length(PulledData$Sample) * Fs
    n = length(PulledData$Sample)
    Time_Seconds = seq(0, CardiacCycleLength, length.out = n)
    Time_Periods = seq(0, 2 * pi, length.out = n)
    
    Mean_Velocity = mean(PulledData$FilteredLMCAv)
    Mean_MAP = mean(PulledData$FilteredMAP)
    Mean_PET1 = mean(PulledData$PETCO2)
    
    Sys_velocity = max(PulledData$FilteredLMCAv)
    Sys_MAP = max(PulledData$FilteredMAP)
    
    Diastolic_Velocity = min(PulledData$FilteredLMCAv)
    Diastolic_MAP = min(PulledData$FilteredMAP)
    
    
    #Calculate estimated CPP. 
    CPP = (PulledData$FilteredMAP - (0.7355 * d))
    
    
    #Slope determined through 2 point mean methods. 
    Slope = (Mean_Velocity - Diastolic_Velocity) / (Mean_MAP - Diastolic_MAP)
    
    #Add data to matrix for storage. 
    Base_matrix[i, 1] = i
    
    #RAP Calculation
    Base_matrix[i, 3] = 1/Slope
    
    #CrCP Calculation
    Base_matrix[i, 2] = mean(PulledData$FilteredMAP) - (Base_matrix[i,3] * mean(PulledData$FilteredLMCAv))
    
    #Mean MCAv
    Base_matrix[i, 4] = mean(PulledData$FilteredLMCAv)
    
    #Mean MAP
    Base_matrix[i, 5] = mean(PulledData$FilteredMAP)
    
    #"Check" Calculation
    Base_matrix[i, 6] = (Base_matrix[i,5] - Base_matrix[i,2]) / Base_matrix[i,3]
    
    #CPI Calculation
    Base_matrix[i, 8] = (Sys_velocity - Diastolic_Velocity) / Mean_Velocity
    
    #Mean CPP
    Base_matrix[i, 9] = mean(CPP)
    
    #CVCi Calculation
    Base_matrix[i, 10] = mean(PulledData$FilteredLMCAv) / mean(CPP)
    
    #PETCO2 mean 
    Base_matrix[i,11] = Mean_PET1
    
    if (i < length(Base_matrix[,7])) {
      Base_matrix[i + 1, 7] = CardiacCycleLength + Base_matrix[i, 7]
    }
  }
  
  
  Base_Matrix_Df = as.data.frame(Base_matrix)
  
  Base_Matrix_Df[Base_Matrix_Df$CrCP < 0, c("CrCP", "RAP", "MAP", "MCAv", "CPP", "CPI", "CVCi", "PETCO2")] = NA
  
  # -----------------------------------------------------------------------------#
  # Data processing for newly calculated CrCP and RAP                            #
  # -----------------------------------------------------------------------------#
  #---- Resampling Data for Baseline and Steady State ------
  BaseT = seq(min(Base_Matrix_Df$Time), max(Base_Matrix_Df$Time), by = .2)
  
  
  BaseResampled = data.frame(CrCP = spline(Base_Matrix_Df$Time, Base_Matrix_Df$CrCP, xout = BaseT)$y, 
                             RAP = spline(Base_Matrix_Df$Time, Base_Matrix_Df$RAP, xout = BaseT)$y, 
                             CVCi = spline(Base_Matrix_Df$Time, Base_Matrix_Df$CVCi, xout = BaseT)$y,
                             MAP = spline(Base_Matrix_Df$Time, Base_Matrix_Df$MAP, xout = BaseT)$y, 
                             CPP = spline(Base_Matrix_Df$Time, Base_Matrix_Df$CPP, xout = BaseT)$y,
                             CPI = spline(Base_Matrix_Df$Time, Base_Matrix_Df$CPI, xout = BaseT)$y,
                             PETCO2 = spline(Base_Matrix_Df$Time, Base_Matrix_Df$PETCO2, xout = BaseT)$y,
                             MCAv = spline(Base_Matrix_Df$Time, Base_Matrix_Df$MCAv, xout = BaseT)$y,
                             Check = spline(Base_Matrix_Df$Time, Base_Matrix_Df$Check, xout = BaseT)$y,
                             Time = BaseT)
  
  
  # ---- Removing negative CrCP values ---- 
  
  CrCPCorrectBase = BaseResampled[BaseResampled$CrCP > 0,]
  
  
  
  # -----------------------------------------------------------------------------#
  # Calculate Averages of CrCP and RAP (Last -- sec)                             #
  # -----------------------------------------------------------------------------#
  
  Baseline_MCAv_Mean = mean(CrCPCorrectBase$MCAv[CrCPCorrectBase$Time >= (max(CrCPCorrectBase$Time) - y) & CrCPCorrectBase$Time <= max(CrCPCorrectBase$Time)])
  Baseline_MAP_Mean = mean(CrCPCorrectBase$MAP[CrCPCorrectBase$Time >= (max(CrCPCorrectBase$Time) - y) & CrCPCorrectBase$Time <= max(CrCPCorrectBase$Time)])
  Baseline_CrCP_Mean = mean(CrCPCorrectBase$CrCP[CrCPCorrectBase$Time >= (max(CrCPCorrectBase$Time) - y) & CrCPCorrectBase$Time <= max(CrCPCorrectBase$Time)])
  Baseline_RAP_Mean = mean(CrCPCorrectBase$RAP[CrCPCorrectBase$Time >= (max(CrCPCorrectBase$Time) - y) & CrCPCorrectBase$Time <= max(CrCPCorrectBase$Time)])
  Baseline_CPI_Mean = mean(CrCPCorrectBase$CPI[CrCPCorrectBase$Time >= (max(CrCPCorrectBase$Time) - y) & CrCPCorrectBase$Time <= max(CrCPCorrectBase$Time)])
  Baseline_PETCO2_Mean = mean(CrCPCorrectBase$PETCO2[CrCPCorrectBase$Time >= (max(CrCPCorrectBase$Time) - y) & CrCPCorrectBase$Time <= max(CrCPCorrectBase$Time)])
  
  
  
  
  
  
  incProgress(amount = 0.25)
  
  # -----------------------------------------------------------------------------#
  # Calculate Data  (Dynamic Response to Stress)                                 #
  # -----------------------------------------------------------------------------#
  
  for(i in 1:max(Stressor_Dataset$Beat_Num)){ 
    #----------------------------------------------------------------------------#
    # Pulling each heart beat data and numbering each sample. Additionally,      #
    # calculating each heart beats periods.                                      #
    #----------------------------------------------------------------------------#
    PulledData3 = subset(Stressor_Dataset, Beat_Num == i)
    PulledData3$Sample = seq(1,length(PulledData3$Sample))
    CardiacCycleLength = length(PulledData3$Sample) * Fs
    n = length(PulledData3$Sample)
    Time_Seconds = seq(0, CardiacCycleLength, length.out = n)
    Time_Periods = seq(0, 2 * pi, length.out = n)
    
    
    #Calculate Mean MAP and V
    Mean_Velocity = mean(PulledData3$FilteredLMCAv)
    Mean_MAP = mean(PulledData3$FilteredMAP)
    Mean_PET2 = mean(PulledData3$PETCO2)
    
    #Systolic MAP and V
    Sys_velocity = max(PulledData3$FilteredLMCAv)
    Sys_MAP = max(PulledData3$FilteredMAP)
    
    
    #Diastolic MAP and V
    Diastolic_Velocity = min(PulledData3$FilteredLMCAv)
    Diastolic_MAP = min(PulledData3$FilteredMAP)
    
    
    #CPP Calculation
    CPP3 = (PulledData3$FilteredMAP - (0.7355 * d))
    
    #slope determined by 2-point mean method
    Slope = (Mean_Velocity - Diastolic_Velocity) / (Mean_MAP - Diastolic_MAP)
    
    if (i < length(Stress_matrix[,7])) {
      Stress_matrix[i + 1, 7] = CardiacCycleLength + Stress_matrix[i, 7]
    }
    
    
    #Add data to matrix for storage. 
    Stress_matrix[i, 1] = i
    
    #RAP
    Stress_matrix[i, 3] = 1/Slope
    
    #CrCP
    Stress_matrix[i, 2] = mean(PulledData3$FilteredMAP) - (Stress_matrix[i,3] * mean(PulledData3$FilteredLMCAv))
    
    #Mean MCAv
    Stress_matrix[i, 4] = mean(PulledData3$FilteredLMCAv)
    
    #Mean MAP
    Stress_matrix[i, 5] = mean(PulledData3$FilteredMAP)
    
    #Check Calculation
    Stress_matrix[i, 6] = (Stress_matrix[i,5] - Stress_matrix[i,2]) / Stress_matrix[i,3]
    
    #CPI Calculation
    Stress_matrix[i, 8] = (Sys_velocity - Diastolic_Velocity) / Mean_Velocity
    
    #Mean CPP
    Stress_matrix[i, 9] = mean(CPP3)
    
    #CVCi Calculation
    Stress_matrix[i, 10] = mean(PulledData3$FilteredLMCAv) / mean(CPP3)
    
    #PETCO2 mean 
    Stress_matrix[i, 11] = Mean_PET2
    
    
  }
  
  Stress_Matrix_Df = as.data.frame(Stress_matrix)
  
  Stress_Matrix_Df[Stress_Matrix_Df$CrCP < 0, c("CrCP", "RAP", "MAP", "MCAv", "CPP", "CPI", "CVCi", "PETCO2")] = NA
  
  
  # -----------------------------------------------------------------------------#
  # Dynamic Data Processing                                                      #
  # -----------------------------------------------------------------------------#
  #---- remove negative CrCP Values ---- 
  Corrected_Stress = Stress_Matrix_Df
  CrCPCorrectStress = as.data.frame(Corrected_Stress)
  #CrCPCorrectStress = na.omit(CrCPCorrectStress)
  
  
  #---- Resample data at 5 hz ---- 
  StressT = seq(min(CrCPCorrectStress$Time), max(CrCPCorrectStress$Time), by = .2)
  
  StressResampled = data.frame(CrCP = spline(CrCPCorrectStress$Time, CrCPCorrectStress$CrCP, xout = StressT)$y, 
                               RAP = spline(CrCPCorrectStress$Time, CrCPCorrectStress$RAP, xout = StressT)$y, 
                               CVCi = spline(CrCPCorrectStress$Time, CrCPCorrectStress$CVCi, xout = StressT)$y,
                               MAP = spline(CrCPCorrectStress$Time, CrCPCorrectStress$MAP, xout = StressT)$y,
                               CPP = spline(CrCPCorrectStress$Time,CrCPCorrectStress$CPP, xout = StressT)$y,
                               CPI = spline(CrCPCorrectStress$Time, CrCPCorrectStress$CPI, xout = StressT)$y,
                               PETCO2 = spline(CrCPCorrectStress$Time, CrCPCorrectStress$PETCO2, xout = StressT)$y,
                               MCAv = spline(CrCPCorrectStress$Time,CrCPCorrectStress$MCAv, xout = StressT)$y,
                               Check = spline(CrCPCorrectStress$Time, CrCPCorrectStress$Check, xout = StressT)$y,
                               Time = StressT)
  
  # -----------------------------------------------------------------------------#
  # Create Blank Matrix to Receive Newly Calculated Data                         #
  # -----------------------------------------------------------------------------#
  SubComp_Stress_matrix = matrix(, ncol = 7, nrow= length(as.numeric(StressResampled$CrCP)))
  colnames(SubComp_Stress_matrix) = c("Beat", "Sub_CrCP", "Sub_RAP", "Sub_MCAv", "Sub_MAP", "Check", "Time")
  SubComp_Stress_matrix[1,7] = 0
  SubComp_Stress_matrix[,7] = StressResampled$Time
  
  
  # -----------------------------------------------------------------------------#
  # Calculate subcomponent values for dynamic timepoints                         #
  # -----------------------------------------------------------------------------#
  
  for(i in 1:length(StressResampled$CrCP)){ 
    SubComp_Stress_matrix[i, 1] = i
    SubComp_Stress_matrix[i, 3] = -(StressResampled$RAP[i]-Baseline_RAP_Mean)/ Baseline_RAP_Mean
    SubComp_Stress_matrix[i, 2] = -(StressResampled$CrCP[i]-Baseline_CrCP_Mean)/(Baseline_RAP_Mean * Baseline_MCAv_Mean) 
    SubComp_Stress_matrix[i, 4] = (StressResampled$MCAv[i]-Baseline_MCAv_Mean)/ Baseline_MCAv_Mean
    SubComp_Stress_matrix[i, 5] = (StressResampled$MAP[i]-Baseline_MAP_Mean)/(Baseline_RAP_Mean * Baseline_MCAv_Mean)
    SubComp_Stress_matrix[i, 6] = SubComp_Stress_matrix[i,5] + SubComp_Stress_matrix[i,3] + SubComp_Stress_matrix[i,2]
    
  }
  
  SubComp_DF = as.data.frame(SubComp_Stress_matrix)
  
  SubComp_DF2 = data.frame(Beat = SubComp_DF$Beat,
                           CrCP = SubComp_DF$Sub_CrCP *100,
                           RAP = SubComp_DF$Sub_RAP * 100,
                           MAP = SubComp_DF$Sub_MAP *100,
                           MCAv = SubComp_DF$Sub_MCAv *100,
                           Check = SubComp_DF$Check *100,
                           Time = SubComp_DF$Time)
  
  incProgress(amount = 0.25)
  
  # -----------------------------------------------------------------------------#
  # AUC subcomponent CrCP, RAP, MCAv, and MAP                                    #
  # -----------------------------------------------------------------------------#
  AUC_CrCP = auc(x = SubComp_DF$Time, y = SubComp_DF$Sub_CrCP, type = "linear")
  AUC_RAP = auc(x = SubComp_DF$Time, y = SubComp_DF$Sub_RAP, type = "linear")
  AUC_MAP = auc(x = SubComp_DF$Time, y = SubComp_DF$Sub_MAP, type = "linear")
  AUC_MCAv = auc(x = SubComp_DF$Time, y = SubComp_DF$Sub_MCAv, type = "linear")
  
  AUC_Matrix = matrix(nrow = 1, ncol = 4)
  colnames(AUC_Matrix) = c("AUC_CrCP", "AUC_RAP", "AUC_MCAv", "AUC_MAP")
  AUC_Matrix[,1] = AUC_CrCP
  AUC_Matrix[,2] = AUC_RAP
  AUC_Matrix[,3] = AUC_MCAv
  AUC_Matrix[,4] = AUC_MAP
  
  AUC_DF = as.data.frame(AUC_Matrix)
  
  
  
  #---- average last ___ s for baseline, stress, and subcomp ----
  
  Baseline_Ave = BaseResampled %>%
    dplyr::filter(Time >= (max((as.numeric(Time) - z)))) %>%
    dplyr::summarise(across(where(is.numeric), mean))
  
  Baseline_Ave = Baseline_Ave[,-10]
  
  Stress_Ave = StressResampled %>%
    dplyr::filter(Time >= (max((as.numeric(Time) - a)))) %>%
    dplyr::summarise(across(where(is.numeric), mean))
  
  Stress_Ave = Stress_Ave[,-10]
  
  SubComponent = SubComp_DF2 %>%
    dplyr::filter(Time >= (max((as.numeric(Time) - b)))) %>%
    dplyr::summarise(across(where(is.numeric), mean))
  
  SubComponent = SubComponent[,-1]
  SubComponent = SubComponent[,-6]
  
  
  colnames(SubComponent) = c("V CrCP", "V RAP", "V MAP","Δ% MCAv", "Check")
  
  
  # -----------------------------------------------------------------------------#
  # Data Quality Check                                                           #
  # -----------------------------------------------------------------------------#
  
  #---- MSNE Check for Absolute Values ----
  AbsData_MSNE = mnse(StressResampled$MCAv, StressResampled$Check)%>%
    formatC(., format = "e", digits = (2))
  
  Stim_Fit = mnse(StressResampled$MCAv, StressResampled$Check)
  
  
  AbsData_Base_MSNE = mnse(BaseResampled$MCAv, BaseResampled$Check)%>%
    formatC(., format = "e", digits = (2))
  
  Base_Fit = mnse(BaseResampled$MCAv, BaseResampled$Check)
  
  
  #---- MSNE Check for Subcomponent Values ----
  
  SubComp_MNSE = mnse(SubComp_DF$Sub_MCAv, SubComp_DF$Check)
  
  SubComp_Fit = mnse(SubComp_DF$Sub_MCAv, SubComp_DF$Check)
  
  
  
  MNSE_Matrix = matrix(, ncol = 3, nrow= 1)
  colnames(MNSE_Matrix) = c("Abs Baseline Score", "Abs Stimuli Score", "Subcomponent Score")
  MNSE_Matrix[,1] = Base_Fit
  MNSE_Matrix[,2] = Stim_Fit
  MNSE_Matrix[,3] = SubComp_MNSE
  
  MNSEdf = as.data.frame(MNSE_Matrix)
  
  
  #------------------------------------------------------------------------------#
  # Plot creation                                                                #
  #------------------------------------------------------------------------------#
  
  #---- Baseline MNSE Check ---- 
  Abs_Check = ggplot(data = BaseResampled, aes(x = Time)) + 
    geom_line(aes(y = MCAv, color = "Measured MCAv")) + 
    geom_line(aes(y = Check, color = "Estimated MCAv"))+ 
    scale_color_manual(name = "",
                       values = c("Estimated MCAv" = "red", "Measured MCAv" = "black"), 
                       labels = c("Estimated MCAv" = "Estimated MCAv", "Measured MCAv" = "Measured MCAv"))+
    labs(y = "Value (cm/s)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  
  
  tiff("Base_Check.tiff", width = 9, height = 6, units = "in", res = 500)
  Abs_Check
  dev.off()
  
  #---- Stim MNSE Check ---- 
  Stim_Check = ggplot(data = StressResampled, aes(x = Time)) + 
    geom_line(aes(y = MCAv, color = "Measured MCAv")) + 
    geom_line(aes(y = Check, color = "Estimated MCAv"))+ 
    scale_color_manual(name = "",
                       values = c("Estimated MCAv" = "red", "Measured MCAv" = "black"), 
                       labels = c("Estimated MCAv" = "Estimated MCAv", "Measured MCAv" = "Measured MCAv"))+
    labs(y = "Value (cm/s)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  
  
  #---- Subcomp MNSE Check ---- 
  SubComp_Check = ggplot(data = SubComp_DF2, aes(x = Time)) + 
    geom_line(aes(y = MCAv, color = "Measured %MCAv")) + 
    geom_line(aes(y = Check, color = "Estimated %MCAv"))+ 
    scale_color_manual(name = "",
                       values = c("Estimated %MCAv" = "red", "Measured %MCAv" = "black"), 
                       labels = c("Estimated %MCAv" = "Estimated MCAv", "Measured %MCAv" = "Measured %MCAv"))+
    labs(y = "Value (Δ%)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  
  
  #---- Subcomp Master Plot ----
  SubComp_Master_Plot = ggplot(SubComp_DF2, aes(x = Time)) + 
    geom_line(aes(y = MCAv, color = "V MCAv")) + 
    geom_line(aes(y = CrCP, color = "V CrCP")) + 
    geom_line(aes(y = RAP, color = "V RAP")) + 
    geom_line(aes(y = MAP, color = "V MAP"))+ 
    scale_color_manual(name = "",
                       values = c("V MCAv" = "black", "V CrCP" = "deepskyblue", "V RAP" = "red", "V MAP" = "darkolivegreen3"), 
                       labels = c("V MCAv" = "V MCAv", "V CrCP" = "V CrCP", "V RAP" = "V RAP", "V MAP" = "V MAP"))+
    labs(y = "", x = "Time (s)")+
    geom_hline(yintercept = 0, alpha = .3) +
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  
  
  #---- Subcomp Individual Plots ----  
  Sub_CrCP_Plot = ggplot(SubComp_DF2, aes(x = Time, y = CrCP)) + 
    geom_area(fill = "skyblue")+
    geom_line(color = "deepskyblue")+ 
    geom_hline(yintercept = 0, alpha = .3)+
    labs(y = "V CrCP(Δ%)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Sub_RAP_Plot = ggplot(SubComp_DF2, aes(x = Time, y = RAP)) + 
    geom_area(fill = "darkred")+
    geom_line(color = "red")+
    geom_hline(yintercept = 0, alpha = .3)+
    labs(y = "V RAP (Δ%)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Sub_MAP_Plot = ggplot(SubComp_DF2, aes(x = Time, y = MAP)) + 
    geom_area(fill = "darkolivegreen")+
    geom_line(color = "darkolivegreen3")+
    geom_hline(yintercept = 0, alpha = .3)+
    labs(y = "V MAP (Δ%)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Sub_MCAv_Plot = ggplot(SubComp_DF2, aes(x = Time, y = MCAv)) + 
    geom_area(fill = "grey")+
    geom_line()+
    geom_hline(yintercept = 0, alpha = .3)+
    labs(y = " MCAv (Δ%)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  
  
  #---- Baseline Individual Plots ---- 
  Base_MCAv_Plot = ggplot(BaseResampled, aes(x = Time, y = MCAv))+
    geom_line(color = "black")+ 
    labs(y = "MCAv (cm/s)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Base_PETCO2_Plot = ggplot(BaseResampled, aes(x = Time, y = PETCO2))+
    geom_line(color = "black")+ 
    labs(y = "PETCO2 (mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Base_CrCP_Plot = ggplot(BaseResampled, aes(x = Time, y = CrCP))+
    geom_line(color = "deepskyblue")+
    labs(y = "CrCP (mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Base_RAP_Plot = ggplot(BaseResampled, aes(x = Time, y = RAP))+
    geom_line(color = "red")+
    labs(y = "RAP (mmHg·s/cm)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Base_MAP_Plot = ggplot(BaseResampled, aes(x = Time, y = MAP))+
    geom_line(color = "darkolivegreen3")+
    labs(y = "MAP (mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  
  Base_CPP_Plot = ggplot(BaseResampled, aes(x = Time, y = CPP))+
    geom_line(color = "orchid")+ 
    labs(y = "CPP (mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Base_CVCi_Plot = ggplot(BaseResampled, aes(x = Time, y = CVCi))+
    geom_line(color = "goldenrod2")+ 
    labs(y = "CVCi (cm/s/mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Base_CPI_Plot = ggplot(BaseResampled, aes(x = Time, y = CPI))+
    geom_line(color = "grey40")+ 
    labs(y = "CPI (A.U)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  
  incProgress(amount = 0.24)
  #---- Stimulus Individual Plots ----   
  Stim_MCAv_Plot = ggplot(StressResampled, aes(x = Time, y = MCAv)) + 
    geom_line(color = "black")+
    labs(y = "MCAv (cm/s)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Stim_PETCO2_Plot = ggplot(StressResampled, aes(x = Time, y = PETCO2)) + 
    geom_line(color = "black")+
    labs(y = "PETCO2 (mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Stim_CrCP_Plot = ggplot(StressResampled, aes(x = Time, y = CrCP))+
    geom_line(color = "deepskyblue")+
    labs(y = "CrCP (mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Stim_RAP_Plot = ggplot(StressResampled, aes(x = Time, y = RAP))+
    geom_line(color = "red")+
    labs(y = "RAP (mmHg·s/cm)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Stim_MAP_Plot = ggplot(StressResampled, aes(x = Time, y = MAP))+
    geom_line(color = "darkolivegreen3")+
    labs(y = "MAP (mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Stim_CPP_Plot = ggplot(StressResampled, aes(x = Time, y = CPP))+
    geom_line(color = "orchid")+ 
    labs(y = "CPP (mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Stim_CVCi_Plot = ggplot(StressResampled, aes(x = Time, y = CVCi))+
    geom_line(color = "goldenrod2")+ 
    labs(y = "CVCi (cm/s/mmHg)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  Stim_CPI_Plot = ggplot(StressResampled, aes(x = Time, y = CPI))+
    geom_line(color = "grey40")+ 
    labs(y = "CPI (A.U)", x = "Time (s)")+
    theme(axis.title = element_text(face = "bold"), 
          panel.background = element_blank(), 
          axis.line = element_line(color = "black"),
          strip.background = element_blank())
  
  
  # -----------------------------------------------------------------------------#
  # Return List                                                                  #
  # -----------------------------------------------------------------------------#
  MainOutputs = list(Abs_Stress_Values = StressResampled, 
                     Abs_Baseline_Values = BaseResampled,
                     Abs_Check_Plot = Abs_Check,
                     SubComp_Check = SubComp_Check,
                     Abs_Stim_Check = Stim_Check,
                     Abs_Baseline = Baseline_Ave,
                     Abs_Stim = Stress_Ave,
                     SubComp_Master_Plot = SubComp_Master_Plot,
                     Sub_CrCP_Plot = Sub_CrCP_Plot,
                     Sub_RAP_Plot = Sub_RAP_Plot,
                     Sub_MAP_Plot = Sub_MAP_Plot,
                     Sub_MCAv_Plot = Sub_MCAv_Plot,
                     Subcomponent_AUC = AUC_DF,
                     Base_MSNE = AbsData_Base_MSNE, 
                     Stim_MSNE = AbsData_MSNE,
                     SubComp_MSNE = SubComp_MNSE,
                     AUC_MCAv = AUC_DF$AUC_MCAv,
                     AUC_CrCP = AUC_DF$AUC_CrCP,
                     AUC_RAP = AUC_DF$AUC_RAP,
                     AUC_MAP = AUC_DF$AUC_MAP, 
                     Base_MCAv_Plot = Base_MCAv_Plot, 
                     Base_CrCP_Plot = Base_CrCP_Plot,
                     Base_RAP_Plot = Base_RAP_Plot, 
                     Base_MAP_Plot = Base_MAP_Plot, 
                     Base_CPP_Plot = Base_CPP_Plot, 
                     Base_CVCi_Plot = Base_CVCi_Plot, 
                     Base_CPI_Plot = Base_CPI_Plot,
                     Base_PETCO2_Plot = Base_PETCO2_Plot,
                     Stim_MCAv_Plot = Stim_MCAv_Plot, 
                     Stim_CrCP_Plot = Stim_CrCP_Plot, 
                     Stim_RAP_Plot = Stim_RAP_Plot, 
                     Stim_MAP_Plot = Stim_MAP_Plot, 
                     Stim_CPP_Plot = Stim_CPP_Plot,
                     Stim_CVCi_Plot = Stim_CVCi_Plot, 
                     Stim_CPI_Plot = Stim_CPI_Plot,
                     Stim_PETCO2_Plot = Stim_PETCO2_Plot,
                     MNSEdf = MNSEdf,
                     SubCompAve = SubComponent, 
                     AUC_Data = AUC_DF,
                     Stim_Fit = Stim_Fit,
                     Base_Fit = Base_Fit, 
                     SubComp_Fit = SubComp_Fit)
  
  return(MainOutputs)
  
  incProgress(amount = 0.01)
  })
  
}