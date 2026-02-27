R_Detect = function(x, y, z) {

withProgress(message = "Beat detection in progress, please wait...", {  
  
  #------------------------------------------
# Input sample frequency (Fs), calculate nyquist freq, 
# and calculate high and low pass cutoff freq for bandpass 
# filter. Then calculate filter coeffs. 
#------------------------------------------
Fs = z
Nyq = Fs/2
Wn = c(5, 15) / Nyq

Band.Filt.Coef = butter(4, Wn, type = "pass")
Derive.Coef = sgolay(p = 3, n = 5, m = 1)

#------------------------------------------
# Input data from app. Decide which condition.
#------------------------------------------

if(y == 1){
  Dataset_Base = read_excel(x, 
                        sheet = "CondA_Baseline")
  attach(Dataset_Base)
  
  
  Dataset_Stim = read_excel(x, 
                        sheet = "CondA_Stim")
  attach(Dataset_Stim)
  
}

if(y == 2){
  Dataset_Base = read_excel(x, 
                            sheet = "CondB_Baseline")
  attach(Dataset_Base)
  
  
  Dataset_Stim = read_excel(x, 
                            sheet = "CondB_Stim")
  attach(Dataset_Stim)
  
}

incProgress(amount = 0.25)
#------------------------------------------
# For baseline data, find QRS peaks using
# classical technique by Pan and Tompkins (1985)
#------------------------------------------
B.Raw.ECG = Dataset_Base$ECG

#Calculate freq. resolution
B.F.res = Fs/length(B.Raw.ECG)

# filter ecg with bandwith of 5-15 cutoff.
B.Fil.ECG = filtfilt(Band.Filt.Coef$b, Band.Filt.Coef$a, B.Raw.ECG)

# filter ecg again with derivative filter. 
B.Fil2.ECG = signal::filter(Derive.Coef, B.Fil.ECG)

# Square ECG values to remove negatives. 
B.Sqr.ECG = B.Fil2.ECG^2

# Use a moving average filter to smooth squared values. 
B.Move.Av.ECG = zoo::rollmean(B.Sqr.ECG, k = as.integer(0.05*Fs), fill = 0)

#this is for debugging, ignore. 
FINALB = data.frame(ECG = B.Raw.ECG, 
                   Time = 1:length(B.Raw.ECG))

# Find peaks of data and record the height, index of the peak, the peak beginning and end. 
# We care about the indices that have the R wave. 
B.Peaks = data.frame(
  pracma::findpeaks(B.Move.Av.ECG, 
                    minpeakdistance = 0.2*Fs, 
                    minpeakheight = mean(B.Move.Av.ECG)+0.5 * sd(B.Move.Av.ECG))
)

#Set Beat Num to "Beat" based on index of R wave. 
Dataset_Base$Beat_Num[B.Peaks$X2] = "beat"

incProgress(amount = 0.25)
#------------------------------------------
# For Stim data, find QRS peaks using
# classical technique by Pan and Tompkins (1985)
#------------------------------------------
S.Raw.ECG = Dataset_Stim$ECG

#Calculate freq. resolution
S.F.res = Fs/length(S.Raw.ECG)

# filter ecg with bandwith of 5-15 cutoff.
S.Fil.ECG = filtfilt(Band.Filt.Coef$b, Band.Filt.Coef$a, S.Raw.ECG)

# filter ecg again with derivative filter. 
S.Fil2.ECG = signal::filter(Derive.Coef, S.Fil.ECG)

# Square ECG values to remove negatives. 
S.Sqr.ECG = S.Fil2.ECG^2

# Use a moving average filter to smooth squared values. 
S.Move.Av.ECG = zoo::rollmean(S.Sqr.ECG, k = as.integer(0.05*Fs), fill = 0)

#this is for debugging, ignore. 
FINALS = data.frame(ECG = S.Raw.ECG, 
                    Time = 1:length(S.Raw.ECG))

# Find peaks of data and record the height, index of the peak, the peak beginning and end. 
# We care about the indices that have the R wave. 
S.Peaks = data.frame(
  pracma::findpeaks(S.Move.Av.ECG, 
                    minpeakdistance = 0.2*Fs, 
                    minpeakheight = mean(S.Move.Av.ECG)+0.5 * sd(S.Move.Av.ECG))
)

incProgress(amount = 0.25)

#Set Beat Num to "Beat" based on index of R wave. 
Dataset_Stim$Beat_Num[S.Peaks$X2] = "beat"

#write_xlsx(Dataset_Base, "Data.xlsx")


#Pan1 <<- plot(x = 1:length(B.Raw.ECG[1:10000]), y = B.Raw.ECG[1:10000], type = "l", xaxt = "n", yaxt = "n", ann = FALSE, bty = "n")

#Pan2 <<- plot(x = 1:length(B.Fil.ECG[1:10000]), y = B.Fil.ECG[1:10000], type = "l", xaxt = "n", yaxt = "n", ann = FALSE, bty = "n")

#Pan3 <<- plot(x = 1:length(B.Fil2.ECG[1:10000]), y = B.Fil2.ECG[1:10000], type = "l", xaxt = "n", yaxt = "n", ann = FALSE, bty = "n")

#Pan4 <<- plot(x = 1:length(B.Sqr.ECG[1:10000]), y = B.Sqr.ECG[1:10000], type = "l", xaxt = "n", yaxt = "n", ann = FALSE, bty = "n")

#Pan5 <<- plot(x = 1:length(B.Move.Av.ECG[1:10000]), y = B.Move.Av.ECG[1:10000], type = "l", xaxt = "n", yaxt = "n", ann = FALSE, bty = "n")

B.Peak_Plot = ggplot(FINALB, aes(x = Time, y = ECG)) + 
  geom_line() +
  geom_point(data = B.Peaks, aes(y = B.Raw.ECG[X2], x = X2), color = "#cc2a36") +
  labs(x = "Time (ms)", y = "ECG (mV)") +
  theme(axis.title = element_text(face = "bold"), 
      panel.background = element_blank(), 
      axis.line = element_line(color = "black"),
      strip.background = element_blank(), 
      legend.position = "none")

B.Peak_Plot = ggplotly(B.Peak_Plot) %>%
  rangeslider(start = FINALB$Time[1], end = FINALB$Time[50000])

S.Peak_Plot = ggplot(FINALS, aes(x = Time, y = ECG)) + 
  #geom_vline(xintercept = S.Peaks$X2, color = "red", linetype = "dotted") +
  geom_line() +
  geom_point(data = S.Peaks, aes(y = S.Raw.ECG[X2], x = X2), color = "#cc2a36") +
  labs(x = "Time (ms)", y = "ECG (mV)") +
  theme(axis.title = element_text(face = "bold"), 
      panel.background = element_blank(), 
      axis.line = element_line(color = "black"),
      strip.background = element_blank(), 
      legend.position = "none")

incProgress(amount = 0.24)

S.Peak_Plot = ggplotly(S.Peak_Plot) %>%
  rangeslider(start = FINALS$Time[1], end = FINALS$Time[50000])

  
return(list(Base.Plot = B.Peak_Plot, 
            Stim.Plot = S.Peak_Plot, 
            Baseline.Data = Dataset_Base, 
            Stim.Data = Dataset_Stim)
)

incProgress(amount = 0.01)
})

}

