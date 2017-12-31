library(psych)
library(GPArotation)
library(readxl)
library(vcd)
library(ggplot2)
library(grid)
library(dplyr)
library(gridExtra)
library(reshape2)

#Import data and global variables
df <- read_excel("Desktop/CEFR/CEFR Database Simplified 2017I.xlsx")
alpha = 0.01 # Used for hypothesis testing.
options(warn=-1) #suppress warnings


#---------- BEGIN FUNCTIONS ----------

#---------- END FUNCTIONS ----------

# Be sure to copy the data to "data" and then use the Scree plot to find the 
# number of factors (nfact).
data<-df[c(7:19)]
fap<-fa.parallel(data, fm = 'minres', fa = 'fa', plot = TRUE)

# Check for correlation--Kendall.  If there is  correlation, then the fa()
# will use the oblimin or promax method; else, varimax.

ken_cor<-corr.test(data, method = "kendall")
ken_cor$p < alpha

# Read the resulting alpha-level correlational matrix and then run the fa() at
# the number of factors using fap$nfact and fap$nfact+1.

nfact_fa<-fa(data,nfactors = fap$nfact, rotate = "promax",fm = "minres")
nfact1_fa<-fa(data,nfactors = fap$nfact+1, rotate = "promax",fm = "minres")

# We have two models.  Print them using a cutoff.

fa_cutoff <-function(n)
{
  return(4.9123*n^-0.476) 
}
print("----------BEGIN RESULTS----------")
print("----------NFACT RESULTS----------")
print(paste0("Cutoff for this nfact model:", fa_cutoff(nfact_fa$n.obs)))
print(nfact_fa$loadings, cutoff = fa_cutoff(nfact_fa$n.obs))
print("----------NFACT1 RESULTS----------")
print(paste0("Cutoff for this nfact1 model:", fa_cutoff(nfact1_fa$n.obs)))
print(nfact1_fa$loadings, cutoff = fa_cutoff(nfact1_fa$n.obs))
print("----------END RESULTS----------")

# Now validate both models and choose one.

print("----------BEGIN VALIDATION----------")
print("----------NFACT VALIDATION----------")
#print(paste0("RMS Error in Approx::",nfact_fa$RMSEA))
print(paste0("Tucker-Lewis Index (>0.95 is good fit):",nfact_fa$TLI))
print("----------NFACT1 VALIDATION----------")
#print(paste0("RMS Error in Approx::",nfact1_fa$RMSEA))
print(paste0("Tucker-Lewis Index (>0.95 is good fit):",nfact1_fa$TLI))
print("----------COMPARISON VALIDATION----------")
if(nfact1_fa$TLI>nfact_fa$TLI)
  {
  print("TLI finds nfact1 is the better model.")
  png(file="./nfact1_fa.png",width=800,height=800)
  fa.diagram(nfact1_fa, main = "FA Tree for nfact1")
  dev.off()
  } else {
  print("TLI finds nfact is the better model.")
  png(file="./nfact_fa.png",width=800,height=800)
  fa.diagram(nfact_fa, main = "FA Tree for nfact")  
  dev.off()
  }
print("----------END VALIDATION----------")

rm(fap, ken_cor, nfact_fa, nfact1_fa, alpha, fa_cutoff)

# General various summary stats and visualizations.
data2<-df[c(4:19)]

# Box/Violin CEFR - Entire Year and by Term
T18Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for Academic Year") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Term 1
rm(data2)
data2<-filter(df,Term==1)
data2<-data2[c(4:19)]
T1Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for T1") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Term 2
rm(data2)
data2<-filter(df,Term==2)
data2<-data2[c(4:19)]
T2Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for T2") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Term 3
rm(data2)
data2<-filter(df,Term==3)
data2<-data2[c(4:19)]
T3Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for T3") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Term 4
rm(data2)
data2<-filter(df,Term==4)
data2<-data2[c(4:19)]
T4Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for T4") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Term 5
rm(data2)
data2<-filter(df,Term==5)
data2<-data2[c(4:19)]
T5Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for T5") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Term 6
rm(data2)
data2<-filter(df,Term==6)
data2<-data2[c(4:19)]
T6Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for T6") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Term 7
rm(data2)
data2<-filter(df,Term==7)
data2<-data2[c(4:19)]
T7Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for T7") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Term 8
rm(data2)
data2<-filter(df,Term==8)
data2<-data2[c(4:19)]
T8Box<-ggplot(stack(data2[c(4:16)]), aes(x = factor(ind, levels = names(data2[c(4:16)])), y = values, colours=values)) + geom_boxplot()+scale_x_discrete(name="CEFR Category") +scale_y_continuous(name="Scale (A1=1, C2=6)")+geom_jitter(alpha=0.07)+geom_violin(alpha=0.1)+geom_boxplot(alpha = 0.25)+ ggtitle("CEFR Scores for T8") + theme(plot.title = element_text(hjust = 0.5))+theme(text=element_text(size=21))
# Combine all into a stacked plot and render to YearCEFR.png
png(file="./YearCEFR.png",width=4000,height=7000)
grid.draw(rbind(ggplotGrob(T1Box), ggplotGrob(T2Box), ggplotGrob(T3Box), ggplotGrob(T4Box), ggplotGrob(T5Box), ggplotGrob(T6Box), ggplotGrob(T7Box), ggplotGrob(T8Box), ggplotGrob(T18Box), size="first"))
dev.off()

rm(data2, T1Box, T2Box, T3Box, T4Box, T5Box, T6Box, T7Box, T8Box, T18Box)

# Do the summary stats (overall and by term) as to wrap up.
data2<-df[c(6:19)]
print("----------PLOTTING SUMMARY DATA----------")
termsummary<- describeBy(data2[c(2:14)],data2$Term)

# Make a chart of the median CEFRs by term
cefr<-rownames(termsummary$`2`)
t1md<-as.data.frame(termsummary$`1`$median)
t2md<-as.data.frame(termsummary$`2`$median)
# For 2017 ONLY
t1md<-t2md #T1 has no data.
# End For 2017 ONLY
t3md<-as.data.frame(termsummary$`3`$median)
t4md<-as.data.frame(termsummary$`4`$median)
t5md<-as.data.frame(termsummary$`5`$median)
t6md<-as.data.frame(termsummary$`6`$median)
t7md<-as.data.frame(termsummary$`7`$median)
t8md<-as.data.frame(termsummary$`8`$median)
dfmd<-as.data.frame(t(bind_cols(t1md, t2md, t3md, t4md, t5md, t6md, t7md, t8md)))
colnames(dfmd)<-cefr
rownames(dfmd)<-c("T1","T2","T3","T4","T5","T6","T7","T8")
dfmd$Term<-rownames(dfmd)

rm(t1md, t2md, t3md, t4md, t5md, t6md, t7md, t8md, cefr, termsummary, data, df)

#Melt into a stacked table using Term 
long.dfmd<-melt(dfmd,id=c("Term"))
png(file="./YearCEFRmeds.png",width=600,height=600)
ggplot(long.dfmd,aes(x=Term,y=variable,color=value))+geom_point(aes(size=value))+labs(y="Weight")+scale_colour_gradient(low = "red", high="green")
dev.off()
