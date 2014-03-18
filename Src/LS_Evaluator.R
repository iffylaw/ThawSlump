######################################################################
######################################################################
####                                                              ####
####                                                              ####
####             LANDSLIDE SUSCEPTIBILITY EVALUATION              ####
####                     V4.0 - 17/07/2009                        ####
####                           IRPI CNR                           ####
####                    MAURO ROSSI - IRPI CNR                    ####
####                                                              ####
####     Istituto di Ricerca per la Protezione Idrogeologica      ####
####              Consiglio Nazionale delle Ricerche              ####
####                    Gruppo di Geomorfologia                   ####
####                  Via della Madonna Alta, 126                 ####
####                    06128 Perugia (Italia)                    ####
####                       +39 075 5014421                        ####
####                       +39 075 5014420                        ####
####                   mauro.rossi@irpi.cnr.it                    ####
####                  geomorfologia@irpi.cnr.it                   ####
####                                                              ####
####             This script was prepared using R 2.7.2           ####
####         The script requires the following R packages:        ####
####                       1: MASS                                ####
####                       2: vcd                                 ####
####                       3: verification                        ####
####                       4: perturb                             ####
####                       5: Zelig                               ####
####                       6: nnet                                ####
####                                                              ####
####     INPUTS: 1) calibration.txt file (tab delimited)          ####
####                1st column -> identification value            ####
####                2nd column -> grouping variable               ####
####                Other columns -> explanatory variables        ####
####                                                              ####
####             2) validation.txt file (tab delimited)           ####
####                1st column -> identification value            ####
####                2nd column -> validation grouping variable    ####
####                Other columns -> explanatory variables        ####
####                                                              ####
####             3) configuration.txt file (tab delimited)        ####
####                1st column -> model name                      ####
####                2nd column -> model selection (YES or NO)     ####
####                3rd column -> bootstrap sample size           ####
####                4th column -> analysis parameter              ####
####                              For QDA can be:                 ####
####                              SEL (eliminate dummy variables) ####
####                              DUM (maintain dummy variables   ####
####                              trasformed in numeric           ####
####                              introducing a random variation  ####
####                              between -0.1 and +0.1)          ####
####                5th column -> model variability estimation    ####
####                              using a bootstrap procedure     ####
####                              (YES or NO)                     ####
####                6th column -> number of bootstrap samples for ####
####                              the estimation of the model     ####
####                              variability (We selected only   ####
####                              20 run for the neural network   ####
####                              analysis because it requires a  ####
####                              long calculation time)          ####
####                                                              ####
######################################################################
######################################################################



######################################################################
######################################################################
######################################################################
#######################----------------------#########################
##################---------------------------#########################
###############-------------##########################################
#############---------#######################-----####################
###########--------####---------------------------------##############
#########-------####---------------------------------------###########
########-------##--------###########################---------#########
#######-------##-------####------------------##########-------########
#######------##------###---------------------############------#######
######------##------##-----------------------#############------######
######------##------##------##############################------######
######------##------##------##############################------######
######------##------##------##############################------######
#######------##-----##------##############################------######
#######-------##----##------##############################------######
########-------##---##------##############################------######
#########-------###-##------##############################------######
###########--------####-----##############################------######
#############---------####################################------######
###############-----------------------------------------##------######
##################--------------------------------------##------######
#######################---------------------------------##------######
######################################################################
######################################################################
######################################################################



######################################################################
######################################################################
####            _ _     _ _               _ _             _ _     ####
####       /  /    /  /    /  /         /      /\    /  /    /    ####
####      /  /_ _ /  /_ _ /  /   _ _   /      /  \  /  /_ _ /     ####
####     /  /\      /       /         /      /    \/  /\          ####
####    /  /  \    /       /         /_ _   /     /  /  \         ####
####                                                              ####
######################################################################
######################################################################



#--------------------------- SEED SETTING ---------------------------#
seed.value<-NULL
seed.value<-1 # Uncomment this line if you want to fix a seed. If this is the case multiple run of the script will give always the same result.
if (is.numeric(seed.value)){seed.value<-seed.value} else {seed.value<-round(runif(1,min=1,max=10000))}


#--------------------  READ THE CONFIGURATION FILE -------------------#
configuration.table<-read.table("configuration.txt",header = TRUE,dec=".", sep="\t")
model.type<-as.character(configuration.table$MODEL)
model.run.matrix<-as.character(configuration.table$RUN)
bootstrap.sample.values<-as.numeric(configuration.table$BOOTSTRAP_SAMPLES_ROC_CURVE)
analysis.parameter.matrix<-as.character(configuration.table$ANALYSIS_PARAMETER)
bootstrap.model.variability<-as.character(configuration.table$BOOTSTRAP_MODEL_VARIABILITY_RUN)
bootstrap.sample.model<-as.numeric(configuration.table$BOOTSTRAP_SAMPLES_MODEL_VARIABILITY)


#-------------------- READ THE CALIBRATION DATA ----------------------#
data.table<-read.table("calibration.txt",header = TRUE,dec=".", sep="\t")
names(data.table)
dim(data.table)
data.table<-na.omit(data.table)
dim(data.table)

explanatory.variables<-data.table[3:dim(data.table)[2]]
str(explanatory.variables)
range(explanatory.variables)
dim(explanatory.variables)

data.variables<-data.table[,2:dim(data.table)[2]]
#data.variables<-data.table[,2:58]
dim(data.variables)

grouping.variable<-as.factor(data.table[,2])
str(grouping.variable)
identification.value<-data.table[,1]
#str(identification.value)

## Histogram of posterior grouping variable
breaks.histogram.values<-c(0,0.2,0.45,0.55,0.8,1)

windows()
hist(data.table[,2], breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of grouping variable", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
pdf(file = "GroupingVariable_Histogram.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
hist(data.table[,2], breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of grouping variable", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
dev.off()

#---------------------  READ THE VALIDATION DATA ---------------------#
validation.table<-read.table("validation.txt",header = TRUE,dec=".", sep="\t")
dim(validation.table)

validation.table<-na.omit(validation.table)
dim(validation.table)

validation.explanatory.variables<-validation.table[3:dim(validation.table)[2]]
str(validation.explanatory.variables)
range(validation.explanatory.variables)
dim(validation.explanatory.variables)

validation.variables<-validation.table[,2:dim(validation.table)[2]]
dim(validation.variables)

validation.grouping.variable<-as.factor(validation.table[,2])
str(validation.grouping.variable)
validation.identification.value<-validation.table[,1]
#str(validation.identification.value)

## Histogram of validation grouping variable
windows()
hist(validation.table[,2], breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of validation grouping variable", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
pdf(file = "GroupingVariable_Histogram_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
hist(validation.table[,2], breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of validation grouping variable", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
dev.off()


#---------------------  COLLINEARITY EVALUATION ---------------------#

# Colldiag is an implementation of the regression collinearity diagnostic procedures found in
# Belsley, Kuh, and Welsch (1980). These procedures examine the ?onditioning?of the matrix
# of independent variables.

# Colldiag computes the condition indexes of the matrix. If the largest condition index
# (the condition number) is large (Belsley et al suggest 30 or higher), then there may be
# collinearity problems. All large condition indexes may be worth investigating.

# Colldiag also provides further information that may help to identify the source of these problems,
# the variance decomposition proportions associated with each condition index. If a large condition
# index is associated two or more variables with large variance decomposition proportions, these
# variables may be causing collinearity problems. Belsley et al suggest that a large proportion is 50
# percent or more.


#load collinearity package (perturb)
library(perturb)
colnames(explanatory.variables)

collinearity.test<-colldiag(explanatory.variables)

#collinearity.test$condindx 
#collinearity.test$pi 
range(collinearity.test$condindx)

if(range(collinearity.test$condindx)[2] >= 30) {      
  collinearity.value<-"Some explanatory variables are collinear"
  } else {      
  collinearity.value<-"Explanatory variables are not collinear"
  }

print(collinearity.test,fuzz=.5)
collinearity.evaluation.matrix<-print(collinearity.test,fuzz=.5)

write.table("COLLINEARITY ANALYSIS RESULT",file="result_Collinearity_Analysis.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table("",file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table("EXPLANATION",file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table("This analysis was performed with Colldiag an implementation of the regression collinearity diagnostic procedures found in Belsley, Kuh,
and Welsch (1980). These procedures examine the ?onditioning?of the matrix of independent variables. The procedure computes the condition
indexes of the matrix. If the largest condition index (the condition number) is large (Belsley et al suggest 30 or higher), then there may be
collinearity problems. All large condition indexes may be worth investigating. The procedure also provides further information that may help to
identify the source of these problems, the variance decomposition proportions associated with each condition index. If a large condition
index (> 30) is associated with two or more variables with large variance decomposition proportions, these variables may be causing collinearity problems.
Belsley et al suggest that a large proportion is 50 percent or more.",file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t",
row.names=FALSE, col.names=FALSE)
write.table("",file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table("RESULTS",file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table(paste("Largest condition index (the condition number) =",range(collinearity.test$condindx)[2]),file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table("",file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table(collinearity.value,file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table("",file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table("Matrix of the variance decomposition proportions associated with each condition index (1st column)",file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
write.table(rbind(colnames(collinearity.evaluation.matrix),collinearity.evaluation.matrix),file="result_Collinearity_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)


#------------------- LINEAR DISCRIMINANT ANALISYS -------------------#

#load package (MASS)
library(MASS)

##### Linear Discriminant Analisys Run
if(model.run.matrix[1] == "YES")
  {
  #if(class(result.lda[1]) == "NULL") # Other Error selection criteria. In this case the IF istruction must be put at the end of the script
  if (class(try(lda(explanatory.variables, grouping.variable, tol=0.001, method="moment")))=="try-error")
    { 
    lda(explanatory.variables, grouping.variable, tol=0.001, method="moment")
    write.table("Linear Discriminant Analysis was not completed",file="Error_LDA_Analysis.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="Error_LDA_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("Error LOG",file="Error_LDA_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind("Message",rev(1:length(as.vector(.Traceback)))," ->",as.vector(.Traceback)),file="Error_LDA_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    }
    
  ##### Linear Discriminant Analisys using data.frame
  result.lda<-NULL
  result.lda<-lda(explanatory.variables, grouping.variable, tol=0.001, method="moment") 

  # Result Predicted
  predict.result.lda<-predict(result.lda)
  str(predict.result.lda)
  
  ### predict.result.lda$class is obtained rounding the posterior probability associated to 1 (predict.result.lda$posterior[,2]) 
  length(predict.result.lda$class[predict.result.lda$class==0])
  length(data.table[,2][data.table[,2]==0])
  
  cross.classification.lda<-table(grouping.variable,predict.result.lda$class,dnn=c("Observed","Predicted"))
  rownames(cross.classification.lda)<-list("No Landslide","Landslide") # Observed
  colnames(cross.classification.lda)<-list("No Landslide","Landslide") # Predicted    
  str(cross.classification.lda)
  
  # Assignation of a matching code between observed and predicted values
  result.lda.matching.code<-paste(grouping.variable,as.numeric(levels(predict.result.lda$class))[predict.result.lda$class],sep="")
  result.lda.matching.code<-gsub("00","1",result.lda.matching.code)
  result.lda.matching.code<-gsub("01","2",result.lda.matching.code)
  result.lda.matching.code<-gsub("10","3",result.lda.matching.code)
  result.lda.matching.code<-gsub("11","4",result.lda.matching.code)
  result.lda.matching.code<-as.numeric(result.lda.matching.code)


  #Elaboration of Coefficient of association for contingency table
  #load package (vcd)  
  library(vcd)
  
  #help(package=vcd)         
  contingency.table.lda<-table2d_summary(cross.classification.lda)
  test.table.lda<-assocstats(cross.classification.lda)
  #co_table(cross.classification.lda, margin=1)
  #mar_table(cross.classification.lda) 
  #structable(cross.classification.lda)

  #Different plots for contingency table 
  windows()
  fourfold(cross.classification.lda, std="margin", main="LINEAR DISCRIMINANT ANALYSIS MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))


  #Receiver Operating Characteristic (ROC) plots for one or more models.
  #A ROC curve plots the false alarm rate against the hit rate
  #for a probablistic forecast for a range of thresholds. 
  
  #load package (verification)  
  library(verification)

  #verify function
  #Based on the type of inputs, this function calculates a range of verification statistics and skill scores.
  #Additionally, it creates a verify class object that can be further analyzed.

  ##### ROC PLOT OBS - BINARY PREDICTION
  #windows()
  #roc.plot(as.numeric(data.table$FRAXD2[-result.lda$na.action]),as.numeric(predict.result.lda$class),main = "ROC PLOT: LINEAR DISCRIMINANT ANALYSIS MODEL - BINARY PREDICTED", binormal = TRUE, plot = "both")

  ##### ROC PLOT OBS - POSTERIOR PROBABILITY ASSOCIATED TO 1
  ## 1st method
  #windows()
  #roc.plot(data.table[,2],predict.result.lda$posterior[,2],main = "ROC PLOT: LINEAR DISCRIMINANT ANALYSIS MODEL", binormal = TRUE, plot = "both")

  # 2nd method using verify function
  verification.results.lda<-verify(data.table[,2],predict.result.lda$posterior[,2], frcst.type="prob", obs.type="binary")
  summary(verification.results.lda)
  #str(verification.results.lda)
  #windows()
  #roc.plot(verification.results.lda, main = "ROC PLOT: LINEAR DISCRIMINANT ANALYSIS MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  area.under.roc.curve.lda<-roc.area(data.table[,2],predict.result.lda$posterior[,2])

  ## showing confidence intervals.  MAY BE SLOW
  windows()
  roc.plot(verification.results.lda, main = "ROC PLOT: LINEAR DISCRIMINANT ANALYSIS MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[1] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.lda$A,2),";  Sample size = ",area.under.roc.curve.lda$n.total,";  Bootstrap samples = ",bootstrap.sample.values[1], sep=""), side=3, col="red", cex=0.8)

  ## Histogram of posterior probability
  windows()
  hist(predict.result.lda$posterior[,2], breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Linear Disciminant Analysis susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
  pdf(file = "result_LDA_Histogram.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  hist(predict.result.lda$posterior[,2], breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Linear Disciminant Analysis susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
  dev.off()

  # EXPORT OF PLOT FOR LDA MODEL
  pdf(file = "result_LDA_FourfoldPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  fourfold(cross.classification.lda, std="margin", main="LINEAR DISCRIMINANT ANALYSIS MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
  dev.off()
  
  #pdf(file = "result_LDA_ROCPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  #roc.plot(verification.results.lda, main = "ROC PLOT: LINEAR DISCRIMINANT ANALYSIS MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  #dev.off()

  pdf(file = "result_LDA_ROCPlot_bootstrap.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  roc.plot(verification.results.lda, main = "ROC PLOT: LINEAR DISCRIMINANT ANALYSIS MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[1] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.lda$A,2),";  Sample size = ",area.under.roc.curve.lda$n.total,";  Bootstrap samples = ",bootstrap.sample.values[1], sep=""), side=3, col="red", cex=0.8)
  dev.off()
  
  ## BOOTSTRAP PROCEDURE FOR THE ESTIMATION OF MODEL PREDICTION VARIABILITY
  if(bootstrap.model.variability[1] == "YES")
    {
    bootstrap.sample.model.lda<-bootstrap.sample.model[1]

    matrix.bootstrap.model.lda<-matrix(data=NA, nrow=dim(data.table)[1], ncol=(bootstrap.sample.model.lda*3)+1)
    colnames(matrix.bootstrap.model.lda)<-rep("na",(bootstrap.sample.model.lda*3)+1)
    matrix.bootstrap.model.lda[,1]<-identification.value
    colnames(matrix.bootstrap.model.lda)[1]<-"ID"
    name.sel.run<-paste(rep("ID_Selection_Run",bootstrap.sample.model.lda),1:bootstrap.sample.model.lda,sep="_")
    colnames(matrix.bootstrap.model.lda)[seq(2,(bootstrap.sample.model.lda*3)-1,3)]<-name.sel.run
    name.prob.run<-paste(rep("Probability_Run",bootstrap.sample.model.lda),1:bootstrap.sample.model.lda,sep="_")
    colnames(matrix.bootstrap.model.lda)[seq(3,(bootstrap.sample.model.lda*3),3)]<-name.prob.run
    name.pred.run<-paste(rep("Prediction_Run",bootstrap.sample.model.lda),1:bootstrap.sample.model.lda,sep="_")
    colnames(matrix.bootstrap.model.lda)[seq(4,(bootstrap.sample.model.lda*3)+1,3)]<-name.pred.run

    selection.index<-NULL
    library(MASS)
    #Bootstrap procedure
    for (count.boot in 1:bootstrap.sample.model.lda)
        {
        selection.index<-sample(1:dim(data.table)[1], replace=TRUE, prob=NULL)
        matrix.bootstrap.model.lda[as.numeric(names(table(selection.index))),(count.boot*3)-1]<-table(selection.index)
        explanatory.variables.bootstrap.model.lda<-data.table[selection.index,3:dim(data.table)[2]]
        grouping.variable.bootstrap.model.lda<-as.factor(data.table[selection.index,2])
        result.bootstrap.model.lda<-lda(explanatory.variables.bootstrap.model.lda, grouping.variable.bootstrap.model.lda, tol=0.001, method="moment")
        matrix.bootstrap.model.lda[as.numeric(names(table(selection.index))),(count.boot*3)]<-predict(result.bootstrap.model.lda,newdata=explanatory.variables[as.numeric(names(table(selection.index))),])$posterior[,2]
        matrix.bootstrap.model.lda[,(count.boot*3)+1]<-predict(result.bootstrap.model.lda,newdata=explanatory.variables)$posterior[,2]
        }
    # Export of bootstrap sample
    write.table(matrix.bootstrap.model.lda,file="result_LDA_BootstrapSamples.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=TRUE)

    ID.bootstrap.model.lda.count<-numeric(length=dim(data.table)[1])
    #Probability (selected values)
    bootstrap.model.lda.probability.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.probability.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.probability.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.probability.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.probability.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.probability.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    #Prediction (all values)
    bootstrap.model.lda.prediction.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.prediction.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.prediction.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.prediction.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.prediction.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.lda.prediction.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)
    
    for (count.row.variability in 1:dim(data.table)[1])
        {
        # Statistics on boostrapped probability
        ID.bootstrap.model.lda.count[count.row.variability]<-length(na.omit(matrix.bootstrap.model.lda[count.row.variability,seq(2,(bootstrap.sample.model.lda*3)-1,3)]))
        bootstrap.model.lda.probability.mean[count.row.variability]<-mean(na.omit(matrix.bootstrap.model.lda[count.row.variability,seq(3,(bootstrap.sample.model.lda*3),3)]))
        bootstrap.model.lda.probability.sd[count.row.variability]<-sd(na.omit(matrix.bootstrap.model.lda[count.row.variability,seq(3,(bootstrap.sample.model.lda*3),3)]))
        bootstrap.model.lda.probability.min[count.row.variability]<-min(na.omit(matrix.bootstrap.model.lda[count.row.variability,seq(3,(bootstrap.sample.model.lda*3),3)]))
        bootstrap.model.lda.probability.max[count.row.variability]<-max(na.omit(matrix.bootstrap.model.lda[count.row.variability,seq(3,(bootstrap.sample.model.lda*3),3)]))
        bootstrap.model.lda.probability.sderror[count.row.variability]<-bootstrap.model.lda.probability.sd[count.row.variability]/ID.bootstrap.model.lda.count[count.row.variability]
        bootstrap.model.lda.probability.quantiles[count.row.variability,]<-quantile(na.omit(matrix.bootstrap.model.lda[count.row.variability,seq(3,(bootstrap.sample.model.lda*3),3)]),probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        # Statistics on boostrapped prediction
        bootstrap.model.lda.prediction.mean[count.row.variability]<-mean(matrix.bootstrap.model.lda[count.row.variability,seq(4,(bootstrap.sample.model.lda*3)+1,3)])
        bootstrap.model.lda.prediction.sd[count.row.variability]<-sd(matrix.bootstrap.model.lda[count.row.variability,seq(4,(bootstrap.sample.model.lda*3)+1,3)])
        bootstrap.model.lda.prediction.min[count.row.variability]<-min(matrix.bootstrap.model.lda[count.row.variability,seq(4,(bootstrap.sample.model.lda*3)+1,3)])
        bootstrap.model.lda.prediction.max[count.row.variability]<-max(matrix.bootstrap.model.lda[count.row.variability,seq(4,(bootstrap.sample.model.lda*3)+1,3)])
        bootstrap.model.lda.prediction.sderror[count.row.variability]<-bootstrap.model.lda.prediction.sd[count.row.variability]/bootstrap.sample.model.lda
        bootstrap.model.lda.prediction.quantiles[count.row.variability,]<-quantile(matrix.bootstrap.model.lda[count.row.variability,seq(4,(bootstrap.sample.model.lda*3)+1,3)],probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        }

    # Export of bootstrap sample statistics
    write.table(cbind("ID","LDA_NumberSelectedSamples","LDA_Probability_Mean","LDA_Probability_Sd","LDA_Probability_Min","LDA_Probability_Max","LDA_Probability_Sderror","LDA_Probability_Quantiles_0","LDA_Probability_Quantiles_0.05","LDA_Probability_Quantiles_0.25","LDA_Probability_Quantiles_0.5","LDA_Probability_Quantiles_0.75","LDA_Probability_Quantiles_0.95","LDA_Probability_Quantiles_1","LDA_Prediction_Mean","LDA_Prediction_Sd","LDA_Prediction_Min","LDA_Prediction_Max","LDA_Prediction_Sderror","LDA_Prediction_Quantiles_0","LDA_Prediction_Quantiles_0.05","LDA_Prediction_Quantiles_0.25","LDA_Prediction_Quantiles_0.5","LDA_Prediction_Quantiles_0.75","LDA_Prediction_Quantiles_0.95","LDA_Prediction_Quantiles_1"),file="result_LDA_BootstrapStatistics.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind(identification.value,ID.bootstrap.model.lda.count,bootstrap.model.lda.probability.mean,bootstrap.model.lda.probability.sd,bootstrap.model.lda.probability.min,bootstrap.model.lda.probability.max,bootstrap.model.lda.probability.sderror,bootstrap.model.lda.probability.quantiles,bootstrap.model.lda.prediction.mean,bootstrap.model.lda.prediction.sd,bootstrap.model.lda.prediction.min,bootstrap.model.lda.prediction.max,bootstrap.model.lda.prediction.sderror,bootstrap.model.lda.prediction.quantiles),file="result_LDA_BootstrapStatistics.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)

    windows()
    plot(bootstrap.model.lda.probability.mean,bootstrap.model.lda.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="LDA BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lda,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)

    pdf(file = "result_LDA_BootstrapMeansComparison.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(bootstrap.model.lda.probability.mean,bootstrap.model.lda.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="LDA BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lda,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)
    dev.off()

    #windows()
    #double.sd.histogram.variability<-hist(bootstrap.model.lda.probability.sd*2,breaks=seq(0,1,0.05),labels=TRUE)
    #plot(double.sd.histogram.variability$counts, seq(0,0.95,0.05), type="S",ylim=c(0,1),labels=TRUE)

    # BOOTSTRAPPED PROBABILITY - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.probability.lda<-cbind(bootstrap.model.lda.probability.mean,2*bootstrap.model.lda.probability.sd)
    parabola.probability.lda<-na.omit(parabola.probability.lda[order(parabola.probability.lda[,1]),])
    colnames(parabola.probability.lda)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.probability.lda <- nls(parabola.probability.lda[,"ordinate"] ~ coeff.a*(parabola.probability.lda[,"abscissa"]^2) + (-1)*coeff.a*parabola.probability.lda[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.probability.lda<-predict(fit.parabola.probability.lda)
    #coef(fit.parabola.probability.lda)

    windows()
    plot(parabola.probability.lda[,"abscissa"],parabola.probability.lda[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="LDA Model Probability Variability (Bootstrap)")
    lines(parabola.probability.lda[,"abscissa"],value.parabola.probability.lda,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lda,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.lda),3),coeff.b= -round(coef(fit.parabola.probability.lda),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_LDA_BootstrapProbabilityVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.probability.lda[,"abscissa"],parabola.probability.lda[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="LDA Model Probability Variability (Bootstrap)")
    lines(parabola.probability.lda[,"abscissa"],value.parabola.probability.lda,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lda,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.lda),3),coeff.b= -round(coef(fit.parabola.probability.lda),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()
    
    # BOOTSTRAPPED PREDICTION - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.prediction.lda<-cbind(bootstrap.model.lda.prediction.mean,2*bootstrap.model.lda.prediction.sd)
    parabola.prediction.lda<-parabola.prediction.lda[order(parabola.prediction.lda[,1]),]
    colnames(parabola.prediction.lda)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.prediction.lda <- nls(parabola.prediction.lda[,"ordinate"] ~ coeff.a*(parabola.prediction.lda[,"abscissa"]^2) + (-1)*coeff.a*parabola.prediction.lda[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.prediction.lda<-predict(fit.parabola.prediction.lda)
    #coef(fit.parabola.prediction.lda)

    windows()
    plot(parabola.prediction.lda[,"abscissa"],parabola.prediction.lda[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="LDA Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.lda[,"abscissa"],value.parabola.prediction.lda,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lda,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.lda),3),coeff.b= -round(coef(fit.parabola.prediction.lda),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_LDA_BootstrapPredictionVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.prediction.lda[,"abscissa"],parabola.prediction.lda[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="LDA Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.lda[,"abscissa"],value.parabola.prediction.lda,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lda,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.lda),3),coeff.b= -round(coef(fit.parabola.prediction.lda),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()
  }

  ## Sensitivity, Specificity, Cohens kappa plot
  roc.plot.lda.series<-roc.plot(verification.results.lda,binormal=TRUE)
  #str(roc.plot.lda.series)
  #roc.plot.lda.series$plot.data
  #str(roc.plot.lda.series$plot.data)

  contingency.table.matrix.lda<-matrix(nrow=dim(roc.plot.lda.series$plot.data)[1],ncol=8)
  colnames(contingency.table.matrix.lda)<-c("Threshold","TP","TN","FP","FN","TPR","FPR","COHEN_KAPPA")
  contingency.table.matrix.lda[,1]<-roc.plot.lda.series$plot.data[,1,1]
  contingency.table.matrix.lda[,6]<-roc.plot.lda.series$plot.data[,2,1]
  contingency.table.matrix.lda[,7]<-roc.plot.lda.series$plot.data[,3,1]
  values.odserved<-data.table[,2]
  values.predicted<-predict.result.lda$posterior[,2]
  for (threshold.series in 1:dim(roc.plot.lda.series$plot.data)[1])
      {
      value.threshold<-contingency.table.matrix.lda[threshold.series,1]
      values.probability.reclassified<-NULL
      values.probability.reclassified<-numeric(length=length(values.odserved))

      for (length.observed.series in 1:length(values.odserved))
        {
        if (values.predicted[length.observed.series] > value.threshold)
           {
           values.probability.reclassified[length.observed.series]<-1
           } else
           {
           values.probability.reclassified[length.observed.series]<-0
           }
        }
        #sum(values.probability.reclassified-round(values.predicted)) # Check sum: It has to be 0 if threshold is equal to 1
        series.pasted<-paste(values.odserved,values.probability.reclassified,sep="")
        series.pasted<-gsub("00","1",series.pasted)
        series.pasted<-gsub("01","2",series.pasted)
        series.pasted<-gsub("10","3",series.pasted)
        series.pasted<-gsub("11","4",series.pasted)
        series.pasted<-as.numeric(series.pasted)

        TP<-length(series.pasted[series.pasted>=4])                   # True Positive
        FN<-length(series.pasted[series.pasted>=3 & series.pasted<4]) # False Negative
        FP<-length(series.pasted[series.pasted>=2 & series.pasted<3]) # False Positive
        TN<-length(series.pasted[series.pasted>=1 & series.pasted<2]) # True Negative

        #TPR<-TP/(TP+FN) # Hit Rate or True Positive Rate or Sensitivity - Assigned before the for cicle using rocplot data
        #FPR<-FP/(FP+TN) # False Alarm Rate or False Positive Rate or 1-Specificity

        # Cohen's Kappa = (agreement-chance)/(1-chance)  where agreement=(TP+TN)/(TP+TN+FP+FN) and chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
        agreement=(TP+TN)/(TP+TN+FP+FN)
        chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
        cohen.kappa.value<-(agreement-chance)/(1-chance)
        #Other
        #library(vcd)
        #cohen.kappa.value<-Kappa(cross.classification.table)
        contingency.table.matrix.lda[threshold.series,2]<-TP
        contingency.table.matrix.lda[threshold.series,3]<-TN
        contingency.table.matrix.lda[threshold.series,4]<-FP
        contingency.table.matrix.lda[threshold.series,5]<-FN
        contingency.table.matrix.lda[threshold.series,8]<-cohen.kappa.value

      }

  windows()
  plot(roc.plot.lda.series$plot.data[,1,1],roc.plot.lda.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="LDA MODEL EVALUATION PLOT")
  points(roc.plot.lda.series$plot.data[,1,1],1-roc.plot.lda.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
  points(roc.plot.lda.series$plot.data[,1,1], contingency.table.matrix.lda[,8],col="blue",pch=1,cex=0.6)
  mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
  mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
  mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
  pdf(file = "result_LDA_ModelEvaluationPlot_prova.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(roc.plot.lda.series$plot.data[,1,1],roc.plot.lda.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="LDA MODEL EVALUATION PLOT")
  points(roc.plot.lda.series$plot.data[,1,1],1-roc.plot.lda.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
  points(roc.plot.lda.series$plot.data[,1,1], contingency.table.matrix.lda[,8],col="blue",pch=1,cex=0.6)
  mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
  mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
  mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
  dev.off()

  ## VALIDATION OF LDA MODEL (Matching LDA posterior probability results and validation grouping variable)
  cross.classification.temporal.validation.lda<-table(validation.grouping.variable,predict.result.lda$class,dnn=c("Observed","Predicted"))
  rownames(cross.classification.temporal.validation.lda)<-list("No Landslide","Landslide")  #Observed
  colnames(cross.classification.temporal.validation.lda)<-list("No Landslide","Landslide")  #Predicted
  #str(cross.classification.temporal.validation.lda)

  #Elaboration of Coefficient of association for contingency table
  #load package (vcd)
  library(vcd)

  #help(package=vcd)
  contingency.table.temporal.validation.lda<-table2d_summary(cross.classification.temporal.validation.lda)
  test.table.temporal.validation.lda<-assocstats(cross.classification.temporal.validation.lda)

  #Different plots for contingency table
  windows()
  fourfold(cross.classification.temporal.validation.lda, std="margin", main="TEMPORAL VALIDATION LDA MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(170,135,210,max=255), rgb(115,70,155,max=255)))

  #Receiver Operating Characteristic (ROC) plots for one or more models.
  #load package (verification)
  library(verification)

  # 2nd method using verify function
  verification.temporal.validation.lda<-verify(validation.table[,2],predict.result.lda$posterior[,2], frcst.type="prob", obs.type="binary")
  #summary(verification.temporal.validation.lda)

  # showing confidence intervals.  MAY BE SLOW
  area.under.roc.curve.temporal.validation.lda<-roc.area(validation.table[,2],predict.result.lda$posterior[,2])
  windows()
  roc.plot(verification.temporal.validation.lda, main = "ROC PLOT: TEMPORAL VALIDATION LDA MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[1] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.lda$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.lda$n.total,";  Bootstrap samples = ",bootstrap.sample.values[1], sep=""), side=3, col="red", cex=0.8)


  # EXPORT OF PLOT FOR VALIDATION OF LDA MODEL

  pdf(file = "result_LDA_FourfoldPlot_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  fourfold(cross.classification.temporal.validation.lda, std="margin", main="TEMPORAL VALIDATION LDA MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255),  rgb(170,135,210,max=255), rgb(115,70,155,max=255)))
  dev.off()

  #pdf(file = "result_LDA_ROCPlot_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  #roc.plot(verification.temporal.validation.lda, main = "ROC PLOT: TEMPORAL VALIDATION LDA MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  #area.under.roc.curve.temporal.validation.lda<-roc.area(data.table[,2],predict.result.lda$posterior[,2])
  #dev.off()

  pdf(file = "result_LDA_ROCPlot_bootstrap_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  roc.plot(verification.temporal.validation.lda, main = "ROC PLOT: TEMPORAL VALIDATION LDA MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[1] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.lda$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.lda$n.total,";  Bootstrap samples = ",bootstrap.sample.values[1], sep=""), side=3, col="red", cex=0.8)
  dev.off()

  # Assignation of a matching code between observed and predicted values calculated using the validation dataset
  validation.lda.matching.code<-paste(validation.grouping.variable,as.numeric(levels(predict.result.lda$class))[predict.result.lda$class],sep="")
  validation.lda.matching.code<-gsub("00","1",validation.lda.matching.code)
  validation.lda.matching.code<-gsub("01","2",validation.lda.matching.code)
  validation.lda.matching.code<-gsub("10","3",validation.lda.matching.code)
  validation.lda.matching.code<-gsub("11","4",validation.lda.matching.code)
  validation.lda.matching.code<-as.numeric(validation.lda.matching.code)


  # EXPORT OF LDA MODEL RESULTS
  write.table("RESULTS OF LINEAR DISCRIMINANT ANALYSIS",file="result_LDA.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("LDA MODEL OUTPUTS",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Prior Probabilities",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind(names(result.lda$prior),result.lda$prior),file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Total number",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind("N",result.lda$N),file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Counts",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind(names(result.lda$counts),result.lda$counts),file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Means",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(t(rbind(c("",colnames(result.lda$means)),cbind(rownames(result.lda$means),result.lda$means))),file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Discriminant function coefficients",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  #write.table(cbind(rownames(result.lda$scaling),result.lda$scaling),file="result_LDA.tsv", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  scaling.order<-result.lda$scaling[order(result.lda$scaling),]
  scaling.matrix.ordered<-cbind(names(scaling.order),scaling.order)
  write.table(scaling.matrix.ordered,file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("CONTINGENCY TABLE MODEL RESULT",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.lda$table[,1,],contingency.table.lda$table[,2,],contingency.table.lda$table[,3,])),file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("CONTINGENCY TABLE VALIDATION",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.temporal.validation.lda$table[,1,],contingency.table.temporal.validation.lda$table[,2,],contingency.table.temporal.validation.lda$table[,3,])),file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("MATCHING CODE DEFINITION",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind(c("","OBSERVED NO LANDSLIDES: 0","OBSERVED LANDSLIDES: 1"), c("PREDICTED NO LANDSLIDES: 0","00 -> Code 1","10 -> Code 3"), c("PREDICTED LANDSLIDES: 1","01 -> Code 2","11 -> Code 4")),file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("FINAL RESULTS",file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("ID","GROUPING VARIABLE","LDA MODEL POSTERIOR PROBABILITY","LDA MODEL CLASSIFICATION","LDA MODEL RESULT MATCHING CODE","VALIDATION GROUPING VARIABLE","LDA VALIDATION MATCHING CODE"),cbind(identification.value,as.numeric(levels(grouping.variable))[grouping.variable],predict.result.lda$posterior[,2],as.numeric(levels(predict.result.lda$class))[predict.result.lda$class],result.lda.matching.code,as.numeric(levels(validation.grouping.variable))[validation.grouping.variable],validation.lda.matching.code)),file="result_LDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)

  }


#----------------- QUADRATIC DISCRIMINANT ANALISYS ------------------#

if(model.run.matrix[2] == "YES")
  {
  # Changing of Dummy Explanatory Variables in Numeric variable
  if (analysis.parameter.matrix[2] == "DUM")
    {
    print("The Quadratic Discriminant Analsysis (QDA) will be performed using dummy variables, but a random variation in these variables will be introduced")
    for (count.variables in 1:dim(explanatory.variables)[2])
      {
      #print(range(explanatory.variables[,count.variables]))
      if (min(explanatory.variables[,count.variables])==0 & max(explanatory.variables[,count.variables])==1)
          { 
          set.seed(seed.value)
          explanatory.variables[,count.variables]<-explanatory.variables[,count.variables]+runif(dim(explanatory.variables)[1],-0.1,0.1)  
          validation.explanatory.variables[,count.variables]<-explanatory.variables[,count.variables]
          seed.value<-seed.value+1
          }      
      }
    }

    
  if (analysis.parameter.matrix[2] == "SEL")
    {
    print("The Quadratic Discriminant Analsysis (QDA) will be performed excluding dummy variables")
    index.variables.dummy<-NULL
    for (count.variables in 1:dim(explanatory.variables)[2])
      {
      print(range(explanatory.variables[,count.variables]))
      if (min(explanatory.variables[,count.variables])==0 & max(explanatory.variables[,count.variables])==1)  
          { 
          index.variables.dummy<-c(index.variables.dummy,count.variables)
          }      
      }
    explanatory.variables<-explanatory.variables[,-index.variables.dummy]
    validation.explanatory.variables<-validation.explanatory.variables[,-index.variables.dummy]
    #str(explanatory.variables)
    }
  
  
   if (class(try(qda(explanatory.variables, grouping.variable, method="moment")))=="try-error")  
    { 
    #qda(explanatory.variables, grouping.variable, method="moment")
    write.table("Quadratic Discriminant Analysis was not completed",file="Error_QDA_Analysis.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="Error_QDA_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("Error LOG",file="Error_QDA_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind("Message",rev(1:length(as.vector(.Traceback)))," ->",as.vector(.Traceback)),file="Error_QDA_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    } 
  
  ##### Quadratic Discriminant Analisys using data.frame
  result.qda<-NULL
  result.qda<-qda(explanatory.variables, grouping.variable, method="mle")

  # Result Predicted
  predict.result.qda<-predict(result.qda)
  str(predict.result.qda)

  cross.classification.qda<-table(grouping.variable,predict.result.qda$class,dnn=c("Observed","Predicted"))
  rownames(cross.classification.qda)<-list("No Landslide","Landslide") # Observed
  colnames(cross.classification.qda)<-list("No Landslide","Landslide") # Predicted    
  str(cross.classification.qda)
  
  # Assignation of a matching code between observed and predicted values
  result.qda.matching.code<-paste(grouping.variable,as.numeric(levels(predict.result.qda$class))[predict.result.qda$class],sep="")
  result.qda.matching.code<-gsub("00","1",result.qda.matching.code)
  result.qda.matching.code<-gsub("01","2",result.qda.matching.code)
  result.qda.matching.code<-gsub("10","3",result.qda.matching.code)
  result.qda.matching.code<-gsub("11","4",result.qda.matching.code)
  result.qda.matching.code<-as.numeric(result.qda.matching.code)

  #Elaboration of Coefficient of association for contingency table 
  #load package (vcd)  
  library(vcd)
  
  #help(package=vcd)         
  contingency.table.qda<-table2d_summary(cross.classification.qda)
  test.table.qda<-assocstats(cross.classification.qda)

  #Different plots for contingency table 
  windows()
  fourfold(cross.classification.qda, std="margin", main="QUADRATIC DISCRIMINANT ANALYSIS MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))

  #Receiver Operating Characteristic (ROC) plots for one or more models.
  #A ROC curve plots the false alarm rate against the hit rate
  #for a probablistic forecast for a range of thresholds.

  #load package (verification)  
  library(verification)
  
  #verify function
  #Based on the type of inputs, this function calculates a range of verification statistics and skill scores.
  #Additionally, it creates a verify class object that can be further analyzed.


  ##### ROC PLOT OBS - POSTERIOR PROBABILITY ASSOCIATED TO 1                                                                                 
  ## 1st method
  #windows()
  #roc.plot(data.table[,2],predict.result.qda$posterior[,2],main = "ROC PLOT: QUADRATIC DISCRIMINANT ANALYSIS MODEL", binormal = TRUE, plot = "both")

  # 2nd method using verify function
  verification.results.qda<-verify(data.table[,2],predict.result.qda$posterior[,2], frcst.type="prob", obs.type="binary")
  #summary(verification.results.qda)
  #str(verification.results.qda)
  #windows()
  #roc.plot(verification.results.qda, main = "ROC PLOT: QUADRATIC DISCRIMINANT ANALYSIS MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  area.under.roc.curve.qda<-roc.area(data.table[,2],predict.result.qda$posterior[,2])

  ## showing confidence intervals.  MAY BE SLOW
  windows()
  roc.plot(verification.results.qda, main = "ROC PLOT: QUADRATIC DISCRIMINANT ANALYSIS MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[2] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.qda$A,2),";  Sample size = ",area.under.roc.curve.qda$n.total,";  Bootstrap samples = ",bootstrap.sample.values[2], sep=""), side=3, col="red", cex=0.8)
  
  ## Histogram of posterior probability
  windows()
  hist(predict.result.qda$posterior[,2], breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Quadratic Disciminant Analysis susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
  pdf(file = "result_QDA_Histogram.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  hist(predict.result.qda$posterior[,2], breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Quadratic Disciminant Analysis susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
  dev.off()
  
  
  # EXPORT OF PLOT FOR QDA MODEL
  pdf(file = "result_QDA_FourfoldPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  fourfold(cross.classification.qda, std="margin", main="QUADRATIC DISCRIMINANT ANALYSIS MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
  dev.off()
  
  #pdf(file = "result_QDA_ROCPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  #roc.plot(verification.results.qda, main = "ROC PLOT: QUADRATIC DISCRIMINANT ANALYSIS MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  #dev.off()
  
  pdf(file = "result_QDA_ROCPlot_bootstrap.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  roc.plot(verification.results.qda, main = "ROC PLOT: QUADRATIC DISCRIMINANT ANALYSIS MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[2] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.qda$A,2),";  Sample size = ",area.under.roc.curve.qda$n.total,";  Bootstrap samples = ",bootstrap.sample.values[2], sep=""), side=3, col="red", cex=0.8)
  dev.off()
  
  ## BOOTSTRAP PROCEDURE FOR THE ESTIMATION OF MODEL PREDICTION VARIABILITY
  if(bootstrap.model.variability[2] == "YES")
    {
    bootstrap.sample.model.qda<-bootstrap.sample.model[2]

    matrix.bootstrap.model.qda<-matrix(data=NA, nrow=dim(explanatory.variables)[1], ncol=(bootstrap.sample.model.qda*3)+1)
    colnames(matrix.bootstrap.model.qda)<-rep("na",(bootstrap.sample.model.qda*3)+1)
    matrix.bootstrap.model.qda[,1]<-identification.value
    colnames(matrix.bootstrap.model.qda)[1]<-"ID"
    name.sel.run<-paste(rep("ID_Selection_Run",bootstrap.sample.model.qda),1:bootstrap.sample.model.qda,sep="_")
    colnames(matrix.bootstrap.model.qda)[seq(2,(bootstrap.sample.model.qda*3)-1,3)]<-name.sel.run
    name.prob.run<-paste(rep("Probability_Run",bootstrap.sample.model.qda),1:bootstrap.sample.model.qda,sep="_")
    colnames(matrix.bootstrap.model.qda)[seq(3,(bootstrap.sample.model.qda*3),3)]<-name.prob.run
    name.pred.run<-paste(rep("Prediction_Run",bootstrap.sample.model.qda),1:bootstrap.sample.model.qda,sep="_")
    colnames(matrix.bootstrap.model.qda)[seq(4,(bootstrap.sample.model.qda*3)+1,3)]<-name.pred.run

    selection.index<-NULL
    library(MASS)
    #Bootstrap procedure
    for (count.boot in 1:bootstrap.sample.model.qda)
        {
        selection.index<-sample(1:dim(explanatory.variables)[1], replace=TRUE, prob=NULL)
        matrix.bootstrap.model.qda[as.numeric(names(table(selection.index))),(count.boot*3)-1]<-table(selection.index)
        explanatory.variables.bootstrap.model.qda<-explanatory.variables[selection.index,]
        grouping.variable.bootstrap.model.qda<-as.factor(data.table[selection.index,2])
        result.bootstrap.model.qda<-qda(explanatory.variables.bootstrap.model.qda, grouping.variable.bootstrap.model.qda, tol=0.001, method="moment")
        matrix.bootstrap.model.qda[as.numeric(names(table(selection.index))),(count.boot*3)]<-predict(result.bootstrap.model.qda,newdata=explanatory.variables[as.numeric(names(table(selection.index))),])$posterior[,2]
        matrix.bootstrap.model.qda[,(count.boot*3)+1]<-predict(result.bootstrap.model.qda,newdata=explanatory.variables)$posterior[,2]
        }
    # Export of bootstrap sample
    write.table(matrix.bootstrap.model.qda,file="result_QDA_BootstrapSamples.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=TRUE)

    ID.bootstrap.model.qda.count<-numeric(length=dim(data.table)[1])
    #Probability (selected values)
    bootstrap.model.qda.probability.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.probability.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.probability.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.probability.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.probability.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.probability.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    #Prediction (all values)
    bootstrap.model.qda.prediction.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.prediction.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.prediction.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.prediction.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.prediction.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.qda.prediction.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    for (count.row.variability in 1:dim(data.table)[1])
        {
        # Statistics on boostrapped probability
        ID.bootstrap.model.qda.count[count.row.variability]<-length(na.omit(matrix.bootstrap.model.qda[count.row.variability,seq(2,(bootstrap.sample.model.qda*3)-1,3)]))
        bootstrap.model.qda.probability.mean[count.row.variability]<-mean(na.omit(matrix.bootstrap.model.qda[count.row.variability,seq(3,(bootstrap.sample.model.qda*3),3)]))
        bootstrap.model.qda.probability.sd[count.row.variability]<-sd(na.omit(matrix.bootstrap.model.qda[count.row.variability,seq(3,(bootstrap.sample.model.qda*3),3)]))
        bootstrap.model.qda.probability.min[count.row.variability]<-min(na.omit(matrix.bootstrap.model.qda[count.row.variability,seq(3,(bootstrap.sample.model.qda*3),3)]))
        bootstrap.model.qda.probability.max[count.row.variability]<-max(na.omit(matrix.bootstrap.model.qda[count.row.variability,seq(3,(bootstrap.sample.model.qda*3),3)]))
        bootstrap.model.qda.probability.sderror[count.row.variability]<-bootstrap.model.qda.probability.sd[count.row.variability]/ID.bootstrap.model.qda.count[count.row.variability]
        bootstrap.model.qda.probability.quantiles[count.row.variability,]<-quantile(na.omit(matrix.bootstrap.model.qda[count.row.variability,seq(3,(bootstrap.sample.model.qda*3),3)]),probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        # Statistics on boostrapped prediction
        bootstrap.model.qda.prediction.mean[count.row.variability]<-mean(matrix.bootstrap.model.qda[count.row.variability,seq(4,(bootstrap.sample.model.qda*3)+1,3)])
        bootstrap.model.qda.prediction.sd[count.row.variability]<-sd(matrix.bootstrap.model.qda[count.row.variability,seq(4,(bootstrap.sample.model.qda*3)+1,3)])
        bootstrap.model.qda.prediction.min[count.row.variability]<-min(matrix.bootstrap.model.qda[count.row.variability,seq(4,(bootstrap.sample.model.qda*3)+1,3)])
        bootstrap.model.qda.prediction.max[count.row.variability]<-max(matrix.bootstrap.model.qda[count.row.variability,seq(4,(bootstrap.sample.model.qda*3)+1,3)])
        bootstrap.model.qda.prediction.sderror[count.row.variability]<-bootstrap.model.qda.prediction.sd[count.row.variability]/bootstrap.sample.model.qda
        bootstrap.model.qda.prediction.quantiles[count.row.variability,]<-quantile(matrix.bootstrap.model.qda[count.row.variability,seq(4,(bootstrap.sample.model.qda*3)+1,3)],probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        }

    # Export of bootstrap sample statistics
    write.table(cbind("ID","QDA_NumberSelectedSamples","QDA_Probability_Mean","QDA_Probability_Sd","QDA_Probability_Min","QDA_Probability_Max","QDA_Probability_Sderror","QDA_Probability_Quantiles_0","QDA_Probability_Quantiles_0.05","QDA_Probability_Quantiles_0.25","QDA_Probability_Quantiles_0.5","QDA_Probability_Quantiles_0.75","QDA_Probability_Quantiles_0.95","QDA_Probability_Quantiles_1","QDA_Prediction_Mean","QDA_Prediction_Sd","QDA_Prediction_Min","QDA_Prediction_Max","QDA_Prediction_Sderror","QDA_Prediction_Quantiles_0","QDA_Prediction_Quantiles_0.05","QDA_Prediction_Quantiles_0.25","QDA_Prediction_Quantiles_0.5","QDA_Prediction_Quantiles_0.75","QDA_Prediction_Quantiles_0.95","QDA_Prediction_Quantiles_1"),file="result_QDA_BootstrapStatistics.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind(identification.value,ID.bootstrap.model.qda.count,bootstrap.model.qda.probability.mean,bootstrap.model.qda.probability.sd,bootstrap.model.qda.probability.min,bootstrap.model.qda.probability.max,bootstrap.model.qda.probability.sderror,bootstrap.model.qda.probability.quantiles,bootstrap.model.qda.prediction.mean,bootstrap.model.qda.prediction.sd,bootstrap.model.qda.prediction.min,bootstrap.model.qda.prediction.max,bootstrap.model.qda.prediction.sderror,bootstrap.model.qda.prediction.quantiles),file="result_QDA_BootstrapStatistics.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)

    #windows()
    #double.sd.histogram.variability<-hist(bootstrap.model.qda.probability.sd*2,breaks=seq(0,1,0.05),labels=TRUE)
    #plot(double.sd.histogram.variability$counts, seq(0,0.95,0.05), type="S",ylim=c(0,1), labels=TRUE)

    windows()
    plot(bootstrap.model.qda.probability.mean,bootstrap.model.qda.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="QDA BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.qda,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)

    pdf(file = "result_QDA_BootstrapMeansComparison.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(bootstrap.model.qda.probability.mean,bootstrap.model.qda.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="QDA BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.qda,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)
    dev.off()


    # BOOTSTRAPPED PROBABILITY - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.probability.qda<-cbind(bootstrap.model.qda.probability.mean,2*bootstrap.model.qda.probability.sd)
    parabola.probability.qda<-na.omit(parabola.probability.qda[order(parabola.probability.qda[,1]),])
    colnames(parabola.probability.qda)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.probability.qda <- nls(parabola.probability.qda[,"ordinate"] ~ coeff.a*(parabola.probability.qda[,"abscissa"]^2) + (-1)*coeff.a*parabola.probability.qda[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.probability.qda<-predict(fit.parabola.probability.qda)
    #coef(fit.parabola.probability.qda)

    windows()
    plot(parabola.probability.qda[,"abscissa"],parabola.probability.qda[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="QDA Model Probability Variability (Bootstrap)")
    lines(parabola.probability.qda[,"abscissa"],value.parabola.probability.qda,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.qda,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.qda),3),coeff.b= -round(coef(fit.parabola.probability.qda),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_QDA_BootstrapProbabilityVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.probability.qda[,"abscissa"],parabola.probability.qda[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="QDA Model Probability Variability (Bootstrap)")
    lines(parabola.probability.qda[,"abscissa"],value.parabola.probability.qda,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.qda,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.qda),3),coeff.b= -round(coef(fit.parabola.probability.qda),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()

    # BOOTSTRAPPED PREDICTION - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.prediction.qda<-cbind(bootstrap.model.qda.prediction.mean,2*bootstrap.model.qda.prediction.sd)
    parabola.prediction.qda<-parabola.prediction.qda[order(parabola.prediction.qda[,1]),]
    colnames(parabola.prediction.qda)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.prediction.qda <- nls(parabola.prediction.qda[,"ordinate"] ~ coeff.a*(parabola.prediction.qda[,"abscissa"]^2) + (-1)*coeff.a*parabola.prediction.qda[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.prediction.qda<-predict(fit.parabola.prediction.qda)
    #coef(fit.parabola.prediction.qda)

    windows()
    plot(parabola.prediction.qda[,"abscissa"],parabola.prediction.qda[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="QDA Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.qda[,"abscissa"],value.parabola.prediction.qda,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.qda,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.qda),3),coeff.b= -round(coef(fit.parabola.prediction.qda),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_QDA_BootstrapPredictionVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.prediction.qda[,"abscissa"],parabola.prediction.qda[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="QDA Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.qda[,"abscissa"],value.parabola.prediction.qda,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.qda,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.qda),3),coeff.b= -round(coef(fit.parabola.prediction.qda),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()
  }

  ## Sensitivity, Specificity, Cohens kappa plot
  roc.plot.qda.series<-roc.plot(verification.results.qda,binormal=TRUE)
  #str(roc.plot.qda.series)
  #roc.plot.qda.series$plot.data
  #str(roc.plot.qda.series$plot.data)

  contingency.table.matrix.qda<-matrix(nrow=dim(roc.plot.qda.series$plot.data)[1],ncol=8)
  colnames(contingency.table.matrix.qda)<-c("Threshold","TP","TN","FP","FN","TPR","FPR","COHEN_KAPPA")
  contingency.table.matrix.qda[,1]<-roc.plot.qda.series$plot.data[,1,1]
  contingency.table.matrix.qda[,6]<-roc.plot.qda.series$plot.data[,2,1]
  contingency.table.matrix.qda[,7]<-roc.plot.qda.series$plot.data[,3,1]
  values.odserved<-data.table[,2]
  values.predicted<-predict.result.qda$posterior[,2]
  for (threshold.series in 1:dim(roc.plot.qda.series$plot.data)[1])
      {
      value.threshold<-contingency.table.matrix.qda[threshold.series,1]
      values.probability.reclassified<-NULL
      values.probability.reclassified<-numeric(length=length(values.odserved))

      for (length.observed.series in 1:length(values.odserved))
        {
        if (values.predicted[length.observed.series] > value.threshold)
           {
           values.probability.reclassified[length.observed.series]<-1
           } else
           {
           values.probability.reclassified[length.observed.series]<-0
           }
        }
        #sum(values.probability.reclassified-round(values.predicted)) # Check sum: It has to be 0 if threshold is equal to 1
        series.pasted<-paste(values.odserved,values.probability.reclassified,sep="")
        series.pasted<-gsub("00","1",series.pasted)
        series.pasted<-gsub("01","2",series.pasted)
        series.pasted<-gsub("10","3",series.pasted)
        series.pasted<-gsub("11","4",series.pasted)
        series.pasted<-as.numeric(series.pasted)

        TP<-length(series.pasted[series.pasted>=4])                   # True Positive
        FN<-length(series.pasted[series.pasted>=3 & series.pasted<4]) # False Negative
        FP<-length(series.pasted[series.pasted>=2 & series.pasted<3]) # False Positive
        TN<-length(series.pasted[series.pasted>=1 & series.pasted<2]) # True Negative

        #TPR<-TP/(TP+FN) # Hit Rate or True Positive Rate or Sensitivity - Assigned before the for cicle using rocplot data
        #FPR<-FP/(FP+TN) # False Alarm Rate or False Positive Rate or 1-Specificity

        # Cohen's Kappa = (agreement-chance)/(1-chance)  where agreement=(TP+TN)/(TP+TN+FP+FN) and chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
        agreement=(TP+TN)/(TP+TN+FP+FN)
        chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
        cohen.kappa.value<-(agreement-chance)/(1-chance)
        #Other
        #library(vcd)
        #cohen.kappa.value<-Kappa(cross.classification.table)
        contingency.table.matrix.qda[threshold.series,2]<-TP
        contingency.table.matrix.qda[threshold.series,3]<-TN
        contingency.table.matrix.qda[threshold.series,4]<-FP
        contingency.table.matrix.qda[threshold.series,5]<-FN
        contingency.table.matrix.qda[threshold.series,8]<-cohen.kappa.value

      }

  windows()
  plot(roc.plot.qda.series$plot.data[,1,1],roc.plot.qda.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="QDA MODEL EVALUATION PLOT")
  points(roc.plot.qda.series$plot.data[,1,1],1-roc.plot.qda.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
  points(roc.plot.qda.series$plot.data[,1,1], contingency.table.matrix.qda[,8],col="blue",pch=1,cex=0.6)
  mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
  mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
  mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
  pdf(file = "result_QDA_ModelEvaluationPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(roc.plot.qda.series$plot.data[,1,1],roc.plot.qda.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="QDA MODEL EVALUATION PLOT")
  points(roc.plot.qda.series$plot.data[,1,1],1-roc.plot.qda.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
  points(roc.plot.qda.series$plot.data[,1,1], contingency.table.matrix.qda[,8],col="blue",pch=1,cex=0.6)
  mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
  mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
  mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
  dev.off()

  
  ## VALIDATION OF QDA MODEL (Matching QDA posterior probability results and validation grouping variable)
  cross.classification.temporal.validation.qda<-table(validation.grouping.variable,predict.result.qda$class,dnn=c("Observed","Predicted"))
  rownames(cross.classification.temporal.validation.qda)<-list("No Landslide","Landslide")  #Observed
  colnames(cross.classification.temporal.validation.qda)<-list("No Landslide","Landslide")  #Predicted
  #str(cross.classification.temporal.validation.qda)

  #Elaboration of Coefficient of association for contingency table
  #load package (vcd)
  library(vcd)

  #help(package=vcd)
  contingency.table.temporal.validation.qda<-table2d_summary(cross.classification.temporal.validation.qda)
  test.table.temporal.validation.qda<-assocstats(cross.classification.temporal.validation.qda)

  #Different plots for contingency table
  windows()
  fourfold(cross.classification.temporal.validation.qda, std="margin", main="TEMPORAL VALIDATION QDA MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(170,135,210,max=255), rgb(115,70,155,max=255)))

  #Receiver Operating Characteristic (ROC) plots for one or more models.
  #load package (verification)
  library(verification)

  # 2nd method using verify function
  verification.temporal.validation.qda<-verify(validation.table[,2],predict.result.qda$posterior[,2], frcst.type="prob", obs.type="binary")
  #summary(verification.temporal.validation.qda)

  # showing confidence intervals.  MAY BE SLOW
  area.under.roc.curve.temporal.validation.qda<-roc.area(validation.table[,2],predict.result.qda$posterior[,2])
  windows()
  roc.plot(verification.temporal.validation.qda, main = "ROC PLOT: TEMPORAL VALIDATION QDA MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[1] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.qda$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.qda$n.total,";  Bootstrap samples = ",bootstrap.sample.values[2], sep=""), side=3, col="red", cex=0.8)


  # EXPORT OF PLOT FOR VALIDATION OF QDA MODEL

  pdf(file = "result_QDA_FourfoldPlot_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  fourfold(cross.classification.temporal.validation.qda, std="margin", main="TEMPORAL VALIDATION QDA MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255),  rgb(170,135,210,max=255), rgb(115,70,155,max=255)))
  dev.off()

  #pdf(file = "result_QDA_ROCPlot_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  #roc.plot(verification.temporal.validation.qda, main = "ROC PLOT: TEMPORAL VALIDATION QDA MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  #area.under.roc.curve.temporal.validation.qda<-roc.area(validation.table[,2],predict.result.qda$posterior[,2])
  #dev.off()

  pdf(file = "result_QDA_ROCPlot_bootstrap_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  roc.plot(verification.temporal.validation.qda, main = "ROC PLOT: TEMPORAL VALIDATION QDA MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[2] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.qda$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.qda$n.total,";  Bootstrap samples = ",bootstrap.sample.values[2], sep=""), side=3, col="red", cex=0.8)
  dev.off()

  # Assignation of a matching code between observed and predicted values calculated using the validation dataset
  validation.qda.matching.code<-paste(validation.grouping.variable,as.numeric(levels(predict.result.qda$class))[predict.result.qda$class],sep="")
  validation.qda.matching.code<-gsub("00","1",validation.qda.matching.code)
  validation.qda.matching.code<-gsub("01","2",validation.qda.matching.code)
  validation.qda.matching.code<-gsub("10","3",validation.qda.matching.code)
  validation.qda.matching.code<-gsub("11","4",validation.qda.matching.code)
  validation.qda.matching.code<-as.numeric(validation.qda.matching.code)


  # EXPORT OF QDA MODEL RESULTS
  write.table("RESULTS OF QUADRATIC DISCRIMINANT ANALYSIS",file="result_QDA.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("QDA MODEL OUTPUTS",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Prior Probabilities",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind(names(result.qda$prior),result.qda$prior),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Total number",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind("N",result.qda$N),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Counts",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind(names(result.qda$counts),result.qda$counts),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Means",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(t(rbind(c("",colnames(result.qda$means)),cbind(rownames(result.qda$means),result.qda$means))),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Discriminant function coefficients",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  #Scaling coefficients
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(paste("Coefficients Group",levels(grouping.variable)[1]),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("",colnames(result.qda$scaling[,,1])),cbind(rownames(result.qda$scaling[,,1]),result.qda$scaling[,,1])),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(paste("Coefficients Group",levels(grouping.variable)[2]),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("",colnames(result.qda$scaling[,,2])),cbind(rownames(result.qda$scaling[,,2]),result.qda$scaling[,,2])),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("CONTINGENCY TABLE MODEL RESULT",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.qda$table[,1,],contingency.table.qda$table[,2,],contingency.table.qda$table[,3,])),file="result_QDA.txt", append=TRUE, quote = FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("CONTINGENCY TABLE VALIDATION",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.temporal.validation.qda$table[,1,],contingency.table.temporal.validation.qda$table[,2,],contingency.table.temporal.validation.qda$table[,3,])),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("MATCHING CODE DEFINITION",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind(c("","OBSERVED NO LANDSLIDES: 0","OBSERVED LANDSLIDES: 1"), c("PREDICTED NO LANDSLIDES: 0","00 -> Code 1","10 -> Code 3"), c("PREDICTED LANDSLIDES: 1","01 -> Code 2","11 -> Code 4")),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("FINAL RESULTS",file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("ID","GROUPING VARIABLE","QDA MODEL POSTERIOR PROBABILITY","QDA MODEL CLASSIFICATION","QDA MODEL RESULT MATCHING CODE","QDA VALIDATION GROUPING VARIABLE","QDA VALIDATION MATCHING CODE"),cbind(identification.value,as.numeric(levels(grouping.variable))[grouping.variable],predict.result.qda$posterior[,2],as.numeric(levels(predict.result.qda$class))[predict.result.qda$class],result.qda.matching.code,as.numeric(levels(validation.grouping.variable))[validation.grouping.variable],validation.qda.matching.code)),file="result_QDA.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)

  explanatory.variables<-data.table[,3:dim(data.table)[2]] # Restore to original values of explanatory variables
  validation.explanatory.variables<-validation.table[3:dim(validation.table)[2]] # Restore to original values of validation explanatory variables

  }


#-------------------- LOGISTIC REGRESSION MODEL ---------------------#

if(model.run.matrix[3] == "YES")
  {
  library(Zelig)
  if (class(try(zelig(as.formula(paste(names(data.variables)[1],"~",paste(names(data.variables[,2:dim(data.variables)[2]]),collapse= "+"))), data=data.variables, model="logit")))=="try-error")
    { 
    #zelig(as.formula(paste(names(data.variables)[1],"~",paste(names(data.variables[,2:dim(data.variables)[2]]),collapse= "+"))), data=data.variables, model="logit")
    write.table("Analysis based on Logistic Regression Model was not completed",file="Error_LRM_Analysis.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="Error_LRM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("Error LOG",file="Error_LRM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind("Message",rev(1:length(as.vector(.Traceback)))," ->",as.vector(.Traceback)),file="Error_LRM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    } 

  result.lrm<-NULL
  result.lrm<-zelig(as.formula(paste(names(data.variables)[1],"~",paste(names(data.variables[,2:dim(data.variables)[2]]),collapse= "+"))), data=data.variables, model="logit")
  str(result.lrm)
  #names(result.lrm)
  
  #for predicted value (posterior probablity calculated with model) result.lrm$fitted.values was considered
  
  # Result Predicted  
  #test.explanatory.variables<-setx(result.lrm, fn=NULL, cond=FALSE)
  
  #problem with sim: while zelig() and sex() work well, sim() doesn't work. Changing data.variables (escludind last two variables) sim() works well. Why?    
  #predict.result.lrm<-sim(result.lrm, test.explanatory.variables, num=c(2,2), prev = NULL, bootstrap=FALSE, bootfn=NULL)
  #names(predict.result.lrm)
    
  #predict.result.lrm$qi$ev[]
  #predict.result.lrm$qi$pr[]
 
  #plot(predict.result.lrm)
 
  cross.classification.lrm<-table(as.numeric(result.lrm$y),round(result.lrm$fitted.values),dnn=c("Observed","Predicted"))
  rownames(cross.classification.lrm)<-list("No Landslide","Landslide") # Observed
  colnames(cross.classification.lrm)<-list("No Landslide","Landslide") # Predicted    
  str(cross.classification.lrm)

  # Assignation of a matching code between observed and predicted values
  result.lrm.matching.code<-paste(grouping.variable,round(result.lrm$fitted.values),sep="")
  result.lrm.matching.code<-gsub("00","1",result.lrm.matching.code)
  result.lrm.matching.code<-gsub("01","2",result.lrm.matching.code)
  result.lrm.matching.code<-gsub("10","3",result.lrm.matching.code)
  result.lrm.matching.code<-gsub("11","4",result.lrm.matching.code)
  result.lrm.matching.code<-as.numeric(result.lrm.matching.code)

  #Elaboration of Coefficient of association for contingency table 
  #load package (vcd)  
  library(vcd)
  
  #help(package=vcd)         
  contingency.table.lrm<-table2d_summary(cross.classification.lrm)
  test.table.lrm<-assocstats(cross.classification.lrm)

  #Different plots for contingency table 
  windows()
  fourfold(cross.classification.lrm, std="margin",  main="LOGISTIC REGRESSION MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
  
  #Receiver Operating Characteristic (ROC) plots for one or more models.
  #A ROC curve plots the false alarm rate against the hit rate
  #for a probablistic forecast for a range of thresholds. 
  
  #load package (verification)  
  library(verification)
  
  #verify function
  #Based on the type of inputs, this function calculates a range of verification statistics and skill scores.
  #Additionally, it creates a verify class object that can be further analyzed.

  
  ##### ROC PLOT OBS - POSTERIOR PROBABILITY ASSOCIATED TO 1                                                                                 
  
  # Method using verify function
  verification.results.lrm<-verify(result.lrm$y,result.lrm$fitted.values, frcst.type="prob", obs.type="binary")

  #str(verification.results.lrm)
  #windows()
  #roc.plot(verification.results.lrm, main = "ROC PLOT: LOGISTIC REGRESSION MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  area.under.roc.curve.lrm<-roc.area(result.lrm$y,result.lrm$fitted.values)

  ## showing confidence intervals.  MAY BE SLOW
  
  windows()                                                              
  roc.plot(verification.results.lrm, main = "ROC PLOT: LOGISTIC REGRESSION MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[3] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.lrm$A,2),";  Sample size = ",area.under.roc.curve.lrm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[3], sep=""), side=3, col="red", cex=0.8)
    
  ## Histogram of posterior probability
  windows()                            
  hist(result.lrm$fitted.values, breaks=breaks.histogram.values, freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Logistic Regression Model susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
  pdf(file = "result_LRM_Histogram.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  hist(result.lrm$fitted.values, breaks=breaks.histogram.values, freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Logistic Regression Model susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
  dev.off() 
  
  # EXPORT OF PLOT FOR LRM MODEL
  
  pdf(file = "result_LRM_FourfoldPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  fourfold(cross.classification.lrm, std="margin",  main="LOGISTIC REGRESSION MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
  dev.off()
  
  #pdf(file = "result_LRM_ROCPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  #roc.plot(verification.results.lrm, main = "ROC PLOT: LOGISTIC REGRESSION MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  #dev.off()
  
  pdf(file = "result_LRM_ROCPlot_bootstrap.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  roc.plot(verification.results.lrm, main = "ROC PLOT: LOGISTIC REGRESSION MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[3] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.lrm$A,2),";  Sample size = ",area.under.roc.curve.lrm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[3], sep=""), side=3, col="red", cex=0.8)
  dev.off()

  ## BOOTSTRAP PROCEDURE FOR THE ESTIMATION OF MODEL PREDICTION VARIABILITY
  if(bootstrap.model.variability[3] == "YES")
    {
    bootstrap.sample.model.lrm<-bootstrap.sample.model[3]

    matrix.bootstrap.model.lrm<-matrix(data=NA, nrow=dim(data.table)[1], ncol=(bootstrap.sample.model.lrm*3)+1)
    colnames(matrix.bootstrap.model.lrm)<-rep("na",(bootstrap.sample.model.lrm*3)+1)
    matrix.bootstrap.model.lrm[,1]<-identification.value
    colnames(matrix.bootstrap.model.lrm)[1]<-"ID"
    name.sel.run<-paste(rep("ID_Selection_Run",bootstrap.sample.model.lrm),1:bootstrap.sample.model.lrm,sep="_")
    colnames(matrix.bootstrap.model.lrm)[seq(2,(bootstrap.sample.model.lrm*3)-1,3)]<-name.sel.run
    name.prob.run<-paste(rep("Probability_Run",bootstrap.sample.model.lrm),1:bootstrap.sample.model.lrm,sep="_")
    colnames(matrix.bootstrap.model.lrm)[seq(3,(bootstrap.sample.model.lrm*3),3)]<-name.prob.run
    name.pred.run<-paste(rep("Prediction_Run",bootstrap.sample.model.lrm),1:bootstrap.sample.model.lrm,sep="_")
    colnames(matrix.bootstrap.model.lrm)[seq(4,(bootstrap.sample.model.lrm*3)+1,3)]<-name.pred.run

    selection.index<-NULL
    library(Zelig)
    #Bootstrap procedure
    for (count.boot in 1:bootstrap.sample.model.lrm)
        {
        selection.index<-sample(1:dim(data.table)[1], replace=TRUE, prob=NULL)
        matrix.bootstrap.model.lrm[as.numeric(names(table(selection.index))),(count.boot*3)-1]<-table(selection.index)
        data.variables.bootstrap.model.lrm<-data.table[selection.index,2:dim(data.table)[2]]
        explanatory.variables.bootstrap.model.lrm<-data.table[selection.index,3:dim(data.table)[2]]
        grouping.variable.bootstrap.model.lrm<-as.factor(data.table[selection.index,2])
        result.bootstrap.model.lrm<-zelig(as.formula(paste(names(data.variables.bootstrap.model.lrm)[1],"~",paste(names(data.variables.bootstrap.model.lrm[,2:dim(data.variables.bootstrap.model.lrm)[2]]),collapse= "+"))), data=data.variables.bootstrap.model.lrm, model="logit")
        excluded.variables.bootstrap.model.lrm<-which(match(result.bootstrap.model.lrm$coefficients,NA)==1)
        if (length(excluded.variables.bootstrap.model.lrm) != 0)
          {
          data.variables.bootstrap.model.lrm.selected<-data.variables.bootstrap.model.lrm[,-excluded.variables.bootstrap.model.lrm]
          setx.data.probability<-data.table[as.numeric(names(table(selection.index))),2:dim(data.table)[2]][,-excluded.variables.bootstrap.model.lrm]
          setx.data.prediction<-data.table[,2:dim(data.table)[2]][,-excluded.variables.bootstrap.model.lrm]
          } else
          {
          data.variables.bootstrap.model.lrm.selected<-data.variables.bootstrap.model.lrm
          setx.data.probability<-data.table[as.numeric(names(table(selection.index))),2:dim(data.table)[2]]
          setx.data.prediction<-data.table[,2:dim(data.table)[2]]
          }
        result.bootstrap.model.lrm.selected<-zelig(as.formula(paste(names(data.variables.bootstrap.model.lrm.selected)[1],"~",paste(names(data.variables.bootstrap.model.lrm.selected[,2:dim(data.variables.bootstrap.model.lrm.selected)[2]]),collapse= "+"))), data=data.variables.bootstrap.model.lrm.selected, model="logit")
        x.result.bootstrap.model.lrm.selected.probability<-setx(result.bootstrap.model.lrm.selected,data=setx.data.probability,fn=NULL)
        sim.result.bootstrap.model.lrm.selected.probability<-sim(result.bootstrap.model.lrm.selected,x=x.result.bootstrap.model.lrm.selected.probability,num=c(100,100))
        matrix.bootstrap.model.lrm[as.numeric(names(table(selection.index))),(count.boot*3)]<-colMeans(sim.result.bootstrap.model.lrm.selected.probability$qi$ev)
        x.result.bootstrap.model.lrm.selected.prediction<-setx(result.bootstrap.model.lrm.selected,data=setx.data.prediction,fn=NULL)
        sim.result.bootstrap.model.lrm.selected.prediction<-sim(result.bootstrap.model.lrm.selected,x=x.result.bootstrap.model.lrm.selected.prediction,num=c(100,100))
        matrix.bootstrap.model.lrm[,(count.boot*3)+1]<-colMeans(sim.result.bootstrap.model.lrm.selected.prediction$qi$ev)
        }
        
    # Export of bootstrap sample
    write.table(matrix.bootstrap.model.lrm,file="result_LRM_BootstrapSamples.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=TRUE)

    ID.bootstrap.model.lrm.count<-numeric(length=dim(data.table)[1])
    #Probability (selected values)
    bootstrap.model.lrm.probability.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.probability.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.probability.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.probability.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.probability.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.probability.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    #Prediction (all values)
    bootstrap.model.lrm.prediction.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.prediction.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.prediction.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.prediction.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.prediction.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.lrm.prediction.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    for (count.row.variability in 1:dim(data.table)[1])
        {
        # Statistics on boostrapped probability
        ID.bootstrap.model.lrm.count[count.row.variability]<-length(na.omit(matrix.bootstrap.model.lrm[count.row.variability,seq(2,(bootstrap.sample.model.lrm*3)-1,3)]))
        bootstrap.model.lrm.probability.mean[count.row.variability]<-mean(na.omit(matrix.bootstrap.model.lrm[count.row.variability,seq(3,(bootstrap.sample.model.lrm*3),3)]))
        bootstrap.model.lrm.probability.sd[count.row.variability]<-sd(na.omit(matrix.bootstrap.model.lrm[count.row.variability,seq(3,(bootstrap.sample.model.lrm*3),3)]))
        bootstrap.model.lrm.probability.min[count.row.variability]<-min(na.omit(matrix.bootstrap.model.lrm[count.row.variability,seq(3,(bootstrap.sample.model.lrm*3),3)]))
        bootstrap.model.lrm.probability.max[count.row.variability]<-max(na.omit(matrix.bootstrap.model.lrm[count.row.variability,seq(3,(bootstrap.sample.model.lrm*3),3)]))
        bootstrap.model.lrm.probability.sderror[count.row.variability]<-bootstrap.model.lrm.probability.sd[count.row.variability]/ID.bootstrap.model.lrm.count[count.row.variability]
        bootstrap.model.lrm.probability.quantiles[count.row.variability,]<-quantile(na.omit(matrix.bootstrap.model.lrm[count.row.variability,seq(3,(bootstrap.sample.model.lrm*3),3)]),probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        # Statistics on boostrapped prediction
        bootstrap.model.lrm.prediction.mean[count.row.variability]<-mean(matrix.bootstrap.model.lrm[count.row.variability,seq(4,(bootstrap.sample.model.lrm*3)+1,3)])
        bootstrap.model.lrm.prediction.sd[count.row.variability]<-sd(matrix.bootstrap.model.lrm[count.row.variability,seq(4,(bootstrap.sample.model.lrm*3)+1,3)])
        bootstrap.model.lrm.prediction.min[count.row.variability]<-min(matrix.bootstrap.model.lrm[count.row.variability,seq(4,(bootstrap.sample.model.lrm*3)+1,3)])
        bootstrap.model.lrm.prediction.max[count.row.variability]<-max(matrix.bootstrap.model.lrm[count.row.variability,seq(4,(bootstrap.sample.model.lrm*3)+1,3)])
        bootstrap.model.lrm.prediction.sderror[count.row.variability]<-bootstrap.model.lrm.prediction.sd[count.row.variability]/bootstrap.sample.model.lrm
        bootstrap.model.lrm.prediction.quantiles[count.row.variability,]<-quantile(matrix.bootstrap.model.lrm[count.row.variability,seq(4,(bootstrap.sample.model.lrm*3)+1,3)],probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        }

    # Export of bootstrap sample statistics
    write.table(cbind("ID","LRM_NumberSelectedSamples","LRM_Probability_Mean","LRM_Probability_Sd","LRM_Probability_Min","LRM_Probability_Max","LRM_Probability_Sderror","LRM_Probability_Quantiles_0","LRM_Probability_Quantiles_0.05","LRM_Probability_Quantiles_0.25","LRM_Probability_Quantiles_0.5","LRM_Probability_Quantiles_0.75","LRM_Probability_Quantiles_0.95","LRM_Probability_Quantiles_1","LRM_Prediction_Mean","LRM_Prediction_Sd","LRM_Prediction_Min","LRM_Prediction_Max","LRM_Prediction_Sderror","LRM_Prediction_Quantiles_0","LRM_Prediction_Quantiles_0.05","LRM_Prediction_Quantiles_0.25","LRM_Prediction_Quantiles_0.5","LRM_Prediction_Quantiles_0.75","LRM_Prediction_Quantiles_0.95","LRM_Prediction_Quantiles_1"),file="result_LRM_BootstrapStatistics.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind(identification.value,ID.bootstrap.model.lrm.count,bootstrap.model.lrm.probability.mean,bootstrap.model.lrm.probability.sd,bootstrap.model.lrm.probability.min,bootstrap.model.lrm.probability.max,bootstrap.model.lrm.probability.sderror,bootstrap.model.lrm.probability.quantiles,bootstrap.model.lrm.prediction.mean,bootstrap.model.lrm.prediction.sd,bootstrap.model.lrm.prediction.min,bootstrap.model.lrm.prediction.max,bootstrap.model.lrm.prediction.sderror,bootstrap.model.lrm.prediction.quantiles),file="result_LRM_BootstrapStatistics.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)

    #windows()
    #double.sd.histogram.variability<-hist(bootstrap.model.lrm.probability.sd*2,breaks=seq(0,1,0.05),labels=TRUE)
    #plot(double.sd.histogram.variability$counts, seq(0,0.95,0.05), type="S",ylim=c(0,1), labels=TRUE)

    windows()
    plot(bootstrap.model.lrm.probability.mean,bootstrap.model.lrm.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="LRM BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lrm,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)

    pdf(file = "result_LRM_BootstrapMeansComparison.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(bootstrap.model.lrm.probability.mean,bootstrap.model.lrm.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="LRM BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lrm,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)
    dev.off()
    

    # BOOTSTRAPPED PROBABILITY - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.probability.lrm<-cbind(bootstrap.model.lrm.probability.mean,2*bootstrap.model.lrm.probability.sd)
    parabola.probability.lrm<-na.omit(parabola.probability.lrm[order(parabola.probability.lrm[,1]),])
    colnames(parabola.probability.lrm)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.probability.lrm <- nls(parabola.probability.lrm[,"ordinate"] ~ coeff.a*(parabola.probability.lrm[,"abscissa"]^2) + (-1)*coeff.a*parabola.probability.lrm[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.probability.lrm<-predict(fit.parabola.probability.lrm)
    #coef(fit.parabola.probability.lrm)

    windows()
    plot(parabola.probability.lrm[,"abscissa"],parabola.probability.lrm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="LRM Model Probability Variability (Bootstrap)")
    lines(parabola.probability.lrm[,"abscissa"],value.parabola.probability.lrm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lrm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.lrm),3),coeff.b= -round(coef(fit.parabola.probability.lrm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_LRM_BootstrapProbabilityVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.probability.lrm[,"abscissa"],parabola.probability.lrm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="LRM Model Probability Variability (Bootstrap)")
    lines(parabola.probability.lrm[,"abscissa"],value.parabola.probability.lrm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lrm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.lrm),3),coeff.b= -round(coef(fit.parabola.probability.lrm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()

    # BOOTSTRAPPED PREDICTION - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.prediction.lrm<-cbind(bootstrap.model.lrm.prediction.mean,2*bootstrap.model.lrm.prediction.sd)
    parabola.prediction.lrm<-parabola.prediction.lrm[order(parabola.prediction.lrm[,1]),]
    colnames(parabola.prediction.lrm)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.prediction.lrm <- nls(parabola.prediction.lrm[,"ordinate"] ~ coeff.a*(parabola.prediction.lrm[,"abscissa"]^2) + (-1)*coeff.a*parabola.prediction.lrm[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.prediction.lrm<-predict(fit.parabola.prediction.lrm)
    #coef(fit.parabola.prediction.lrm)

    windows()
    plot(parabola.prediction.lrm[,"abscissa"],parabola.prediction.lrm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="LRM Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.lrm[,"abscissa"],value.parabola.prediction.lrm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lrm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.lrm),3),coeff.b= -round(coef(fit.parabola.prediction.lrm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_LRM_BootstrapPredictionVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.prediction.lrm[,"abscissa"],parabola.prediction.lrm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="LRM Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.lrm[,"abscissa"],value.parabola.prediction.lrm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.lrm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.lrm),3),coeff.b= -round(coef(fit.parabola.prediction.lrm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()
  }


  ## Sensitivity, Specificity, Cohens kappa plot
  roc.plot.lrm.series<-roc.plot(verification.results.lrm,binormal=TRUE)
  #str(roc.plot.lrm.series)
  #roc.plot.lrm.series$plot.data
  #str(roc.plot.lrm.series$plot.data)

  contingency.table.matrix.lrm<-matrix(nrow=dim(roc.plot.lrm.series$plot.data)[1],ncol=8)
  colnames(contingency.table.matrix.lrm)<-c("Threshold","TP","TN","FP","FN","TPR","FPR","COHEN_KAPPA")
  contingency.table.matrix.lrm[,1]<-roc.plot.lrm.series$plot.data[,1,1]
  contingency.table.matrix.lrm[,6]<-roc.plot.lrm.series$plot.data[,2,1]
  contingency.table.matrix.lrm[,7]<-roc.plot.lrm.series$plot.data[,3,1]
  values.odserved<-data.table[,2]
  values.predicted<-result.lrm$fitted.values
  for (threshold.series in 1:dim(roc.plot.lrm.series$plot.data)[1])
      {
      value.threshold<-contingency.table.matrix.lrm[threshold.series,1]
      values.probability.reclassified<-NULL
      values.probability.reclassified<-numeric(length=length(values.odserved))

      for (length.observed.series in 1:length(values.odserved))
        {
        if (values.predicted[length.observed.series] > value.threshold)
           {
           values.probability.reclassified[length.observed.series]<-1
           } else
           {
           values.probability.reclassified[length.observed.series]<-0
           }
        }
        #sum(values.probability.reclassified-round(values.predicted)) # Check sum: It has to be 0 if threshold is equal to 1
        series.pasted<-paste(values.odserved,values.probability.reclassified,sep="")
        series.pasted<-gsub("00","1",series.pasted)
        series.pasted<-gsub("01","2",series.pasted)
        series.pasted<-gsub("10","3",series.pasted)
        series.pasted<-gsub("11","4",series.pasted)
        series.pasted<-as.numeric(series.pasted)

        TP<-length(series.pasted[series.pasted>=4])                   # True Positive
        FN<-length(series.pasted[series.pasted>=3 & series.pasted<4]) # False Negative
        FP<-length(series.pasted[series.pasted>=2 & series.pasted<3]) # False Positive
        TN<-length(series.pasted[series.pasted>=1 & series.pasted<2]) # True Negative

        #TPR<-TP/(TP+FN) # Hit Rate or True Positive Rate or Sensitivity - Assigned before the for cicle using rocplot data
        #FPR<-FP/(FP+TN) # False Alarm Rate or False Positive Rate or 1-Specificity

        # Cohen's Kappa = (agreement-chance)/(1-chance)  where agreement=(TP+TN)/(TP+TN+FP+FN) and chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
        agreement=(TP+TN)/(TP+TN+FP+FN)
        chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
        cohen.kappa.value<-(agreement-chance)/(1-chance)
        #Other
        #library(vcd)
        #cohen.kappa.value<-Kappa(cross.classification.table)
        contingency.table.matrix.lrm[threshold.series,2]<-TP
        contingency.table.matrix.lrm[threshold.series,3]<-TN
        contingency.table.matrix.lrm[threshold.series,4]<-FP
        contingency.table.matrix.lrm[threshold.series,5]<-FN
        contingency.table.matrix.lrm[threshold.series,8]<-cohen.kappa.value

      }

  windows()
  plot(roc.plot.lrm.series$plot.data[,1,1],roc.plot.lrm.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="LRM MODEL EVALUATION PLOT")
  points(roc.plot.lrm.series$plot.data[,1,1],1-roc.plot.lrm.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
  points(roc.plot.lrm.series$plot.data[,1,1], contingency.table.matrix.lrm[,8],col="blue",pch=1,cex=0.6)
  mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
  mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
  mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
  pdf(file = "result_LRM_ModelEvaluationPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(roc.plot.lrm.series$plot.data[,1,1],roc.plot.lrm.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="LRM MODEL EVALUATION PLOT")
  points(roc.plot.lrm.series$plot.data[,1,1],1-roc.plot.lrm.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
  points(roc.plot.lrm.series$plot.data[,1,1], contingency.table.matrix.lrm[,8],col="blue",pch=1,cex=0.6)
  mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
  mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
  mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
  dev.off()

  ## VALIDATION OF LRM MODEL (Matching LRM posterior probability results and validation grouping variable)
  cross.classification.temporal.validation.lrm<-table(validation.grouping.variable,round(result.lrm$fitted.values),dnn=c("Observed","Predicted"))
  rownames(cross.classification.temporal.validation.lrm)<-list("No Landslide","Landslide")  #Observed
  colnames(cross.classification.temporal.validation.lrm)<-list("No Landslide","Landslide")  #Predicted
  #str(cross.classification.temporal.validation.lrm)

  #Elaboration of Coefficient of association for contingency table
  #load package (vcd)
  library(vcd)

  #help(package=vcd)
  contingency.table.temporal.validation.lrm<-table2d_summary(cross.classification.temporal.validation.lrm)
  test.table.temporal.validation.lrm<-assocstats(cross.classification.temporal.validation.lrm)

  #Different plots for contingency table
  windows()
  fourfold(cross.classification.temporal.validation.lrm, std="margin", main="TEMPORAL VALIDATION LRM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(170,135,210,max=255), rgb(115,70,155,max=255)))

  #Receiver Operating Characteristic (ROC) plots for one or more models.
  #load package (verification)
  library(verification)

  # 2nd method using verify function
  verification.temporal.validation.lrm<-verify(validation.table[,2],result.lrm$fitted.values, frcst.type="prob", obs.type="binary")
  #summary(verification.temporal.validation.lrm)

  # showing confidence intervals.  MAY BE SLOW
  area.under.roc.curve.temporal.validation.lrm<-roc.area(validation.table[,2],result.lrm$fitted.values)
  windows()
  roc.plot(verification.temporal.validation.lrm, main = "ROC PLOT: TEMPORAL VALIDATION LRM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[3] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.lrm$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.lrm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[3], sep=""), side=3, col="red", cex=0.8)


  # EXPORT OF PLOT FOR VALIDATION OF LRM MODEL

  pdf(file = "result_LRM_FourfoldPlot_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  fourfold(cross.classification.temporal.validation.lrm, std="margin", main="TEMPORAL VALIDATION LRM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255),  rgb(170,135,210,max=255), rgb(115,70,155,max=255)))
  dev.off()

  #pdf(file = "result_LRM_ROCPlot_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  #roc.plot(verification.temporal.validation.lrm, main = "ROC PLOT: TEMPORAL VALIDATION LRM MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  #area.under.roc.curve.temporal.validation.lrm<-roc.area(verification.table[,2],result.lrm$fitted.values)
  #dev.off()

  pdf(file = "result_LRM_ROCPlot_bootstrap_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  roc.plot(verification.temporal.validation.lrm, main = "ROC PLOT: TEMPORAL VALIDATION LRM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[3] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.lrm$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.lrm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[3], sep=""), side=3, col="red", cex=0.8)
  dev.off()

  # Assignation of a matching code between observed and predicted values calculated using the validation dataset
  validation.lrm.matching.code<-paste(validation.grouping.variable,round(result.lrm$fitted.values),sep="")
  validation.lrm.matching.code<-gsub("00","1",validation.lrm.matching.code)
  validation.lrm.matching.code<-gsub("01","2",validation.lrm.matching.code)
  validation.lrm.matching.code<-gsub("10","3",validation.lrm.matching.code)
  validation.lrm.matching.code<-gsub("11","4",validation.lrm.matching.code)
  validation.lrm.matching.code<-as.numeric(validation.lrm.matching.code)

  
  # EXPORT OF LRM MODEL RESULTS
  write.table("RESULTS OF LOGISTIC REGRESSION MODEL",file="result_LRM.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("LRM MODEL OUTPUTS",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Logistic Regression coefficients",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  #Scaling coefficients
  write.table(cbind(names(result.lrm$coefficients),result.lrm$coefficients),file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("CONTINGENCY TABLE MODEL RESULT",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.lrm$table[,1,],contingency.table.lrm$table[,2,],contingency.table.lrm$table[,3,])),file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("CONTINGENCY TABLE VALIDATION",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.temporal.validation.lrm$table[,1,],contingency.table.temporal.validation.lrm$table[,2,],contingency.table.temporal.validation.lrm$table[,3,])),file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("MATCHING CODE DEFINITION",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind(c("","OBSERVED NO LANDSLIDES: 0","OBSERVED LANDSLIDES: 1"), c("PREDICTED NO LANDSLIDES: 0","00 -> Code 1","10 -> Code 3"), c("PREDICTED LANDSLIDES: 1","01 -> Code 2","11 -> Code 4")),file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("FINAL RESULTS",file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("ID","GROUPING VARIABLE","MODEL POSTERIOR PROBABILITY","MODEL CLASSIFICATION","MODEL RESULT MATCHING CODE","VALIDATION GROUPING VARIABLE","VALIDATION MATCHING CODE"),cbind(identification.value,result.lrm$y,result.lrm$fitted.values,round(result.lrm$fitted.values),result.lrm.matching.code,as.numeric(levels(validation.grouping.variable))[validation.grouping.variable],validation.lrm.matching.code)),file="result_LRM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  }
  


#------------------ NEURAL NETWORK MODEL ANALISYS -------------------#

if(model.run.matrix[4] == "YES")
  {
  library(nnet)

  # This "for" cicle search the best number of Weight Decay and Hidden Layer Nodes basing on the minimization of Sum of Squared Error (SSE) or on the mamixization of ROC area results  

  # INITIATE A NULL TABLE
  nnm.table <- NULL
  total.iteration.number<-3*length(round((length(explanatory.variables)/2)):(length(explanatory.variables)-2))*10
  iteration.number<-0
  
    
  # SEARCH FOR OPTIMAL WEIGHT DECAY WITH RANGE OF WEIGHT DECAYS SUGGESTED BY B. RIPLEY
  for (weight.decay in c(0.0001, 0.001, 0.01))
  {
    # SEARCH FOR OPTIMAL NUMBER OF HIDDEN UNITS
    for (n.nodes in round((length(explanatory.variables)/2)):(length(explanatory.variables)-2))
    {
      # UNITIATE A NULL VECTOR
      sse <- NULL
      # FOR EACH SETTING, RUN NEURAL NET MULTIPLE TIMES
      for (i.counts in 1:10)
      {
       # INITIATE THE RANDOM STATE FOR EACH NET
       set.seed(i.counts)
       # TRAIN NEURAL NETS
       result.nnm <- nnet(explanatory.variables, data.table[,2], size = n.nodes, rang = 0.00001, maxit = 200, MaxNWts=10000, decay = weight.decay, skip = FALSE, trace = TRUE) #original maxit=10000 
       # CALCULATE SSE (Sum of Squared Error) and ROC.area
       test.sse <- sum(((as.numeric(grouping.variable)-1) - round(predict(result.nnm)))^2)
       library(verification)
       test.ROC.area <- (roc.area((as.numeric(grouping.variable)-1),result.nnm$fitted.values))$A
       
       iteration.number<-iteration.number+1
       print(paste("Iteration",iteration.number,"of",total.iteration.number,"-",round((iteration.number/total.iteration.number*100),1),"%","completed"))
         
       # APPEND EACH SSE and ROC.area TO A VECTOR
        if (i.counts == 1) sse <- test.sse else sse <- rbind(sse, test.sse)
        if (i.counts == 1) ROC.area <- test.ROC.area else ROC.area <- rbind(ROC.area, test.ROC.area)
      }
      # APPEND AVERAGED SSE and AVERAGED ROC.area WITH RELATED PARAMETERS TO A TABLE
      nnm.table <- rbind(nnm.table, c(WEIGHT_DECAY = weight.decay, HYDDEN_LAYER_NODES = n.nodes, SUM_SQUARED_ERROR = mean(sse), ROC_AREA = mean(ROC.area)))
    }
  }
  # PRINT OUT THE RESULT
  print(nnm.table)

  # Extracting value of Weight Decay and Number of nodes that minimize the SSE
  pos.sse.min<-which.min(nnm.table[,3])
  weight.decay.sse.min<-nnm.table[pos.sse.min,1]
  n.nodes.sse.min<-nnm.table[pos.sse.min,2]
  sse.min<-min(nnm.table[,3])
  
  
  # Extracting value of Weight Decay and Number of nodes that maximize the ROC Area
  pos.roc.area.max<-which.max(nnm.table[,4])
  weight.decay.roc.area.max<-nnm.table[pos.roc.area.max,1]
  n.nodes.roc.area.max<-nnm.table[pos.roc.area.max,2]
  roc.area.max<-max(nnm.table[,4])

  n.nodes.selected <- n.nodes.sse.min
  weight.decay.selected <- weight.decay.sse.min

  if (class(try(nnet(explanatory.variables, data.table[,2], size = n.nodes.selected, rang = 0.00001, maxit = 100, MaxNWts=10000, decay = weight.decay.selected, skip = FALSE, trace = TRUE)))=="try-error")
    { 
    #nnet(explanatory.variables, data.table[,2], size = n.nodes.selected, rang = 0.00001, maxit = 10000, MaxNWts=100, decay = weight.decay.selected, skip = FALSE, trace = TRUE)
    write.table("Analysis based on Neural Network Model was not completed",file="Error_NNM_Analysis.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="Error_NNM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("Error LOG",file="Error_NNM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind("Message",rev(1:length(as.vector(.Traceback)))," ->",as.vector(.Traceback)),file="Error_NNM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    }
     
  result.nnm<-NULL
  set.seed(seed.value)
  result.nnm<-nnet(explanatory.variables, data.table[,2], size = n.nodes.selected, rang = 0.00001, maxit = 10000, MaxNWts=10000, decay = weight.decay.selected, skip = FALSE, trace = TRUE)
  #names(result.nnm)

    
  # Result Predicted
  predict.result.nnm<-predict(result.nnm)
  str(predict.result.nnm)
  
  # As predicted values also result.nnm$fitted.values can be considered because it corresponds to predict.result.nnm as the sum of the difference of the two vectors two is 0
  # sum(result.nnm$fitted.values-predict.result.nnm)

      
  cross.classification.nnm<-table((as.numeric(grouping.variable)-1),round(predict.result.nnm),dnn=c("Observed","Predicted"))
  rownames(cross.classification.nnm)<-list("No Landslide","Landslide") # Observed
  colnames(cross.classification.nnm)<-list("No Landslide","Landslide") # Predicted    
  str(cross.classification.nnm)
  
  # Assignation of a matching code between observed and predicted values
  result.nnm.matching.code<-paste(grouping.variable,round(predict.result.nnm),sep="")
  result.nnm.matching.code<-gsub("00","1",result.nnm.matching.code)
  result.nnm.matching.code<-gsub("01","2",result.nnm.matching.code)
  result.nnm.matching.code<-gsub("10","3",result.nnm.matching.code)
  result.nnm.matching.code<-gsub("11","4",result.nnm.matching.code)
  result.nnm.matching.code<-as.numeric(result.nnm.matching.code)
  
  #Elaboration of Coefficient of association for contingency table 
  #load package (vcd)  
  library(vcd)
  
  #help(package=vcd)         
  contingency.table.nnm<-table2d_summary(cross.classification.nnm)
  test.table.nnm<-assocstats(cross.classification.nnm)
  
    
  #Different plots for contingency table 
  
  windows()       
  fourfold(cross.classification.nnm,std="margin", main="NEURAL NETWORK MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))

  #Receiver Operating Characteristic (ROC) plots for one or more models.
  #A ROC curve plots the false alarm rate against the hit rate
  #for a probablistic forecast for a range of thresholds. 
  
  #load package (verification)  
  library(verification)
  
  #verify function
  #Based on the type of inputs, this function calculates a range of verification statistics and skill scores.
  #Additionally, it creates a verify class object that can be further analyzed.
  
  ##### ROC PLOT OBS - POSTERIOR PROBABILITY ASSOCIATED TO 1
  
  # Method using verify function
  verification.results.nnm<-verify((as.numeric(grouping.variable)-1),predict.result.nnm, frcst.type="prob", obs.type="binary")
  #summary(verification.results.nnm)

  #str(verification.results.nnm)
  #windows()
  #roc.plot(verification.results.nnm, main = "ROC PLOT: NEURAL NETWORK MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  area.under.roc.curve.nnm<-roc.area((as.numeric(grouping.variable)-1),predict.result.nnm)

  ## showing confidence intervals.  MAY BE SLOW
  
  windows()                                                              
  roc.plot(verification.results.nnm, main = "ROC PLOT: NEURAL NETWORK MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[4] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.nnm$A,2),";  Sample size = ",area.under.roc.curve.nnm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[4], sep=""), side=3, col="red", cex=0.8)
    
  ## Histogram of posterior probability
  windows()                            
  hist(predict.result.nnm, breaks=breaks.histogram.values, freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Neural Network Model susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
  pdf(file = "result_NNM_Histogram.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  hist(predict.result.nnm, breaks=breaks.histogram.values, freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Neural Network Model susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
  dev.off() 
  
  # EXPORT OF PLOT FOR NNM MODEL
  
  pdf(file = "result_NNM_FourfoldPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  fourfold(cross.classification.nnm,std="margin", main="NEURAL NETWORK MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
  dev.off()
  
  #pdf(file = "result_NNM_ROCPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  #roc.plot(verification.results.nnm, main = "ROC PLOT: NEURAL NETWORK MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  #dev.off()
  
  pdf(file = "result_NNM_ROCPlot_bootstrap.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  roc.plot(verification.results.nnm, main = "ROC PLOT: NEURAL NETWORK MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[4] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.nnm$A,2),";  Sample size = ",area.under.roc.curve.nnm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[4], sep=""), side=3, col="red", cex=0.8)
  dev.off()

  ## BOOTSTRAP PROCEDURE FOR THE ESTIMATION OF MODEL PREDICTION VARIABILITY
  if(bootstrap.model.variability[4] == "YES")
    {
    bootstrap.sample.model.nnm<-bootstrap.sample.model[4]

    matrix.bootstrap.model.nnm<-matrix(data=NA, nrow=dim(data.table)[1], ncol=(bootstrap.sample.model.nnm*3)+1)
    colnames(matrix.bootstrap.model.nnm)<-rep("na",(bootstrap.sample.model.nnm*3)+1)
    matrix.bootstrap.model.nnm[,1]<-identification.value
    colnames(matrix.bootstrap.model.nnm)[1]<-"ID"
    name.sel.run<-paste(rep("ID_Selection_Run",bootstrap.sample.model.nnm),1:bootstrap.sample.model.nnm,sep="_")
    colnames(matrix.bootstrap.model.nnm)[seq(2,(bootstrap.sample.model.nnm*3)-1,3)]<-name.sel.run
    name.prob.run<-paste(rep("Probability_Run",bootstrap.sample.model.nnm),1:bootstrap.sample.model.nnm,sep="_")
    colnames(matrix.bootstrap.model.nnm)[seq(3,(bootstrap.sample.model.nnm*3),3)]<-name.prob.run
    name.pred.run<-paste(rep("Prediction_Run",bootstrap.sample.model.nnm),1:bootstrap.sample.model.nnm,sep="_")
    colnames(matrix.bootstrap.model.nnm)[seq(4,(bootstrap.sample.model.nnm*3)+1,3)]<-name.pred.run

    selection.index<-NULL
    library(nnet)
    #Bootstrap procedure
    for (count.boot in 1:bootstrap.sample.model.nnm)
        {
        selection.index<-sample(1:dim(data.table)[1], replace=TRUE, prob=NULL)
        matrix.bootstrap.model.nnm[as.numeric(names(table(selection.index))),(count.boot*3)-1]<-table(selection.index)
        explanatory.variables.bootstrap.model.nnm<-data.table[selection.index,3:dim(data.table)[2]]
        grouping.variable.bootstrap.model.nnm<-data.table[selection.index,2]
        result.bootstrap.model.nnm<-nnet(explanatory.variables.bootstrap.model.nnm, grouping.variable.bootstrap.model.nnm, size = n.nodes.selected, rang = 0.00001, maxit = 10000, MaxNWts=10000, decay = weight.decay.selected, skip = FALSE, trace = TRUE)
        matrix.bootstrap.model.nnm[as.numeric(names(table(selection.index))),(count.boot*3)]<-predict(result.bootstrap.model.nnm,newdata=explanatory.variables[as.numeric(names(table(selection.index))),])
        matrix.bootstrap.model.nnm[,(count.boot*3)+1]<-predict(result.bootstrap.model.nnm,newdata=explanatory.variables)
        }
    # Export of bootstrap sample
    write.table(matrix.bootstrap.model.nnm,file="result_NNM_BootstrapSamples.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=TRUE)

    ID.bootstrap.model.nnm.count<-numeric(length=dim(data.table)[1])
    #Probability (selected values)
    bootstrap.model.nnm.probability.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.probability.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.probability.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.probability.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.probability.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.probability.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    #Prediction (all values)
    bootstrap.model.nnm.prediction.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.prediction.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.prediction.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.prediction.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.prediction.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.nnm.prediction.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    for (count.row.variability in 1:dim(data.table)[1])
        {
        # Statistics on boostrapped probability
        ID.bootstrap.model.nnm.count[count.row.variability]<-length(na.omit(matrix.bootstrap.model.nnm[count.row.variability,seq(2,(bootstrap.sample.model.nnm*3)-1,3)]))
        bootstrap.model.nnm.probability.mean[count.row.variability]<-mean(na.omit(matrix.bootstrap.model.nnm[count.row.variability,seq(3,(bootstrap.sample.model.nnm*3),3)]))
        bootstrap.model.nnm.probability.sd[count.row.variability]<-sd(na.omit(matrix.bootstrap.model.nnm[count.row.variability,seq(3,(bootstrap.sample.model.nnm*3),3)]))
        bootstrap.model.nnm.probability.min[count.row.variability]<-min(na.omit(matrix.bootstrap.model.nnm[count.row.variability,seq(3,(bootstrap.sample.model.nnm*3),3)]))
        bootstrap.model.nnm.probability.max[count.row.variability]<-max(na.omit(matrix.bootstrap.model.nnm[count.row.variability,seq(3,(bootstrap.sample.model.nnm*3),3)]))
        bootstrap.model.nnm.probability.sderror[count.row.variability]<-bootstrap.model.nnm.probability.sd[count.row.variability]/ID.bootstrap.model.nnm.count[count.row.variability]
        bootstrap.model.nnm.probability.quantiles[count.row.variability,]<-quantile(na.omit(matrix.bootstrap.model.nnm[count.row.variability,seq(3,(bootstrap.sample.model.nnm*3),3)]),probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        # Statistics on boostrapped prediction
        bootstrap.model.nnm.prediction.mean[count.row.variability]<-mean(matrix.bootstrap.model.nnm[count.row.variability,seq(4,(bootstrap.sample.model.nnm*3)+1,3)])
        bootstrap.model.nnm.prediction.sd[count.row.variability]<-sd(matrix.bootstrap.model.nnm[count.row.variability,seq(4,(bootstrap.sample.model.nnm*3)+1,3)])
        bootstrap.model.nnm.prediction.min[count.row.variability]<-min(matrix.bootstrap.model.nnm[count.row.variability,seq(4,(bootstrap.sample.model.nnm*3)+1,3)])
        bootstrap.model.nnm.prediction.max[count.row.variability]<-max(matrix.bootstrap.model.nnm[count.row.variability,seq(4,(bootstrap.sample.model.nnm*3)+1,3)])
        bootstrap.model.nnm.prediction.sderror[count.row.variability]<-bootstrap.model.nnm.prediction.sd[count.row.variability]/bootstrap.sample.model.nnm
        bootstrap.model.nnm.prediction.quantiles[count.row.variability,]<-quantile(matrix.bootstrap.model.nnm[count.row.variability,seq(4,(bootstrap.sample.model.nnm*3)+1,3)],probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        }

    # Export of bootstrap sample statistics
    write.table(cbind("ID","NNM_NumberSelectedSamples","NNM_Probability_Mean","NNM_Probability_Sd","NNM_Probability_Min","NNM_Probability_Max","NNM_Probability_Sderror","NNM_Probability_Quantiles_0","NNM_Probability_Quantiles_0.05","NNM_Probability_Quantiles_0.25","NNM_Probability_Quantiles_0.5","NNM_Probability_Quantiles_0.75","NNM_Probability_Quantiles_0.95","NNM_Probability_Quantiles_1","NNM_Prediction_Mean","NNM_Prediction_Sd","NNM_Prediction_Min","NNM_Prediction_Max","NNM_Prediction_Sderror","NNM_Prediction_Quantiles_0","NNM_Prediction_Quantiles_0.05","NNM_Prediction_Quantiles_0.25","NNM_Prediction_Quantiles_0.5","NNM_Prediction_Quantiles_0.75","NNM_Prediction_Quantiles_0.95","NNM_Prediction_Quantiles_1"),file="result_NNM_BootstrapStatistics.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind(identification.value,ID.bootstrap.model.nnm.count,bootstrap.model.nnm.probability.mean,bootstrap.model.nnm.probability.sd,bootstrap.model.nnm.probability.min,bootstrap.model.nnm.probability.max,bootstrap.model.nnm.probability.sderror,bootstrap.model.nnm.probability.quantiles,bootstrap.model.nnm.prediction.mean,bootstrap.model.nnm.prediction.sd,bootstrap.model.nnm.prediction.min,bootstrap.model.nnm.prediction.max,bootstrap.model.nnm.prediction.sderror,bootstrap.model.nnm.prediction.quantiles),file="result_NNM_BootstrapStatistics.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)

    #windows()
    #double.sd.histogram.variability<-hist(bootstrap.model.nnm.probability.sd*2,breaks=seq(0,1,0.05),labels=TRUE)
    #plot(double.sd.histogram.variability$counts, seq(0,0.95,0.05), type="S",ylim=c(0,1), labels=TRUE)

    windows()
    plot(bootstrap.model.nnm.probability.mean,bootstrap.model.nnm.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="NNM BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.nnm,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)

    pdf(file = "result_NNM_BootstrapMeansComparison.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(bootstrap.model.nnm.probability.mean,bootstrap.model.nnm.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="NNM BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.nnm,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)
    dev.off()


    # BOOTSTRAPPED PROBABILITY - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.probability.nnm<-cbind(bootstrap.model.nnm.probability.mean,2*bootstrap.model.nnm.probability.sd)
    parabola.probability.nnm<-na.omit(parabola.probability.nnm[order(parabola.probability.nnm[,1]),])
    colnames(parabola.probability.nnm)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.probability.nnm <- nls(parabola.probability.nnm[,"ordinate"] ~ coeff.a*(parabola.probability.nnm[,"abscissa"]^2) + (-1)*coeff.a*parabola.probability.nnm[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.probability.nnm<-predict(fit.parabola.probability.nnm)
    #coef(fit.parabola.probability.nnm)

    windows()
    plot(parabola.probability.nnm[,"abscissa"],parabola.probability.nnm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="NNM Model Probability Variability (Bootstrap)")
    lines(parabola.probability.nnm[,"abscissa"],value.parabola.probability.nnm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.nnm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.nnm),3),coeff.b= -round(coef(fit.parabola.probability.nnm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_NNM_BootstrapProbabilityVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.probability.nnm[,"abscissa"],parabola.probability.nnm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="NNM Model Probability Variability (Bootstrap)")
    lines(parabola.probability.nnm[,"abscissa"],value.parabola.probability.nnm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.nnm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.nnm),3),coeff.b= -round(coef(fit.parabola.probability.nnm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()

    # BOOTSTRAPPED PREDICTION - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.prediction.nnm<-cbind(bootstrap.model.nnm.prediction.mean,2*bootstrap.model.nnm.prediction.sd)
    parabola.prediction.nnm<-parabola.prediction.nnm[order(parabola.prediction.nnm[,1]),]
    colnames(parabola.prediction.nnm)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.prediction.nnm <- nls(parabola.prediction.nnm[,"ordinate"] ~ coeff.a*(parabola.prediction.nnm[,"abscissa"]^2) + (-1)*coeff.a*parabola.prediction.nnm[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.prediction.nnm<-predict(fit.parabola.prediction.nnm)
    #coef(fit.parabola.prediction.nnm)

    windows()
    plot(parabola.prediction.nnm[,"abscissa"],parabola.prediction.nnm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="NNM Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.nnm[,"abscissa"],value.parabola.prediction.nnm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.nnm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.nnm),3),coeff.b= -round(coef(fit.parabola.prediction.nnm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_NNM_BootstrapPredictionVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.prediction.nnm[,"abscissa"],parabola.prediction.nnm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="NNM Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.nnm[,"abscissa"],value.parabola.prediction.nnm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.nnm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.nnm),3),coeff.b= -round(coef(fit.parabola.prediction.nnm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()
  }
  

  ## Sensitivity, Specificity, Cohens kappa plot
  roc.plot.nnm.series<-roc.plot(verification.results.nnm,binormal=TRUE)
  #str(roc.plot.nnm.series)
  #roc.plot.nnm.series$plot.data
  #str(roc.plot.nnm.series$plot.data)

  contingency.table.matrix.nnm<-matrix(nrow=dim(roc.plot.nnm.series$plot.data)[1],ncol=8)
  colnames(contingency.table.matrix.nnm)<-c("Threshold","TP","TN","FP","FN","TPR","FPR","COHEN_KAPPA")
  contingency.table.matrix.nnm[,1]<-roc.plot.nnm.series$plot.data[,1,1]
  contingency.table.matrix.nnm[,6]<-roc.plot.nnm.series$plot.data[,2,1]
  contingency.table.matrix.nnm[,7]<-roc.plot.nnm.series$plot.data[,3,1]
  values.odserved<-data.table[,2]
  values.predicted<-predict.result.nnm
  for (threshold.series in 1:dim(roc.plot.nnm.series$plot.data)[1])
      {
      value.threshold<-contingency.table.matrix.nnm[threshold.series,1]
      values.probability.reclassified<-NULL
      values.probability.reclassified<-numeric(length=length(values.odserved))

      for (length.observed.series in 1:length(values.odserved))
        {
        if (values.predicted[length.observed.series] > value.threshold)
           {
           values.probability.reclassified[length.observed.series]<-1
           } else
           {
           values.probability.reclassified[length.observed.series]<-0
           }
        }
        #sum(values.probability.reclassified-round(values.predicted)) # Check sum: It has to be 0 if threshold is equal to 1
        series.pasted<-paste(values.odserved,values.probability.reclassified,sep="")
        series.pasted<-gsub("00","1",series.pasted)
        series.pasted<-gsub("01","2",series.pasted)
        series.pasted<-gsub("10","3",series.pasted)
        series.pasted<-gsub("11","4",series.pasted)
        series.pasted<-as.numeric(series.pasted)

        TP<-length(series.pasted[series.pasted>=4])                   # True Positive
        FN<-length(series.pasted[series.pasted>=3 & series.pasted<4]) # False Negative
        FP<-length(series.pasted[series.pasted>=2 & series.pasted<3]) # False Positive
        TN<-length(series.pasted[series.pasted>=1 & series.pasted<2]) # True Negative

        #TPR<-TP/(TP+FN) # Hit Rate or True Positive Rate or Sensitivity - Assigned before the for cicle using rocplot data
        #FPR<-FP/(FP+TN) # False Alarm Rate or False Positive Rate or 1-Specificity

        # Cohen's Kappa = (agreement-chance)/(1-chance)  where agreement=(TP+TN)/(TP+TN+FP+FN) and chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
        agreement=(TP+TN)/(TP+TN+FP+FN)
        chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
        cohen.kappa.value<-(agreement-chance)/(1-chance)
        #Other
        #library(vcd)
        #cohen.kappa.value<-Kappa(cross.classification.table)
        contingency.table.matrix.nnm[threshold.series,2]<-TP
        contingency.table.matrix.nnm[threshold.series,3]<-TN
        contingency.table.matrix.nnm[threshold.series,4]<-FP
        contingency.table.matrix.nnm[threshold.series,5]<-FN
        contingency.table.matrix.nnm[threshold.series,8]<-cohen.kappa.value

      }

  windows()
  plot(roc.plot.nnm.series$plot.data[,1,1],roc.plot.nnm.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="NNM MODEL EVALUATION PLOT")
  points(roc.plot.nnm.series$plot.data[,1,1],1-roc.plot.nnm.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
  points(roc.plot.nnm.series$plot.data[,1,1], contingency.table.matrix.nnm[,8],col="blue",pch=1,cex=0.6)
  mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
  mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
  mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
  pdf(file = "result_NNM_ModelEvaluationPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(roc.plot.nnm.series$plot.data[,1,1],roc.plot.nnm.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="NNM MODEL EVALUATION PLOT")
  points(roc.plot.nnm.series$plot.data[,1,1],1-roc.plot.nnm.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
  points(roc.plot.nnm.series$plot.data[,1,1], contingency.table.matrix.nnm[,8],col="blue",pch=1,cex=0.6)
  mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
  mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
  mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
  dev.off()

  ## VALIDATION OF NNM MODEL (Matching NNM posterior probability results and validation grouping variable)
  cross.classification.temporal.validation.nnm<-table(validation.grouping.variable,round(predict.result.nnm),dnn=c("Observed","Predicted"))
  rownames(cross.classification.temporal.validation.nnm)<-list("No Landslide","Landslide")  #Observed
  colnames(cross.classification.temporal.validation.nnm)<-list("No Landslide","Landslide")  #Predicted
  #str(cross.classification.temporal.validation.nnm)

  #Elaboration of Coefficient of association for contingency table
  #load package (vcd)
  library(vcd)

  #help(package=vcd)
  contingency.table.temporal.validation.nnm<-table2d_summary(cross.classification.temporal.validation.nnm)
  test.table.temporal.validation.nnm<-assocstats(cross.classification.temporal.validation.nnm)

  #Different plots for contingency table
  windows()
  fourfold(cross.classification.temporal.validation.nnm, std="margin", main="TEMPORAL VALIDATION NNM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(170,135,210,max=255), rgb(115,70,155,max=255)))

  #Receiver Operating Characteristic (ROC) plots for one or more models.
  #load package (verification)
  library(verification)

  # 2nd method using verify function
  verification.temporal.validation.nnm<-verify(validation.table[,2],predict.result.nnm, frcst.type="prob", obs.type="binary")
  #summary(verification.temporal.validation.lrm)

  # showing confidence intervals.  MAY BE SLOW
  area.under.roc.curve.temporal.validation.nnm<-roc.area(validation.table[,2],predict.result.nnm)
  windows()
  roc.plot(verification.temporal.validation.nnm, main = "ROC PLOT: TEMPORAL VALIDATION NNM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[4] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.nnm$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.nnm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[4], sep=""), side=3, col="red", cex=0.8)


  # EXPORT OF PLOT FOR VALIDATION OF NNM MODEL

  pdf(file = "result_NNM_FourfoldPlot_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  fourfold(cross.classification.temporal.validation.nnm, std="margin", main="TEMPORAL VALIDATION NNM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255),  rgb(170,135,210,max=255), rgb(115,70,155,max=255)))
  dev.off()

  #pdf(file = "result_NNM_ROCPlot_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  #roc.plot(verification.temporal.validation.nnm, main = "ROC PLOT: TEMPORAL VALIDATION NNM MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
  #area.under.roc.curve.temporal.validation.nnm<-roc.area(verification.table[,2],predict.result.nnm)
  #dev.off()

  pdf(file = "result_NNM_ROCPlot_bootstrap_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  roc.plot(verification.temporal.validation.nnm, main = "ROC PLOT: TEMPORAL VALIDATION NNM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[3] , alpha = 0.05, extra=TRUE, legend=TRUE)
  mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.nnm$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.nnm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[4], sep=""), side=3, col="red", cex=0.8)
  dev.off()

  # Assignation of a matching code between observed and predicted values calculated using the validation dataset
  validation.nnm.matching.code<-paste(validation.grouping.variable,round(predict.result.nnm),sep="")
  validation.nnm.matching.code<-gsub("00","1",validation.nnm.matching.code)
  validation.nnm.matching.code<-gsub("01","2",validation.nnm.matching.code)
  validation.nnm.matching.code<-gsub("10","3",validation.nnm.matching.code)
  validation.nnm.matching.code<-gsub("11","4",validation.nnm.matching.code)
  validation.nnm.matching.code<-as.numeric(validation.nnm.matching.code)

  # EXPORT OF NNM MODEL RESULTS
  
  write.table("RESULTS OF NEURAL NETWORK MODEL",file="result_NNM.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("NNM MODEL OUTPUTS",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Selection of Neural Network Structure",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("WEIGHT DECAY","HIDDEN LAYER NODES","SUM SQUARED ERROR","ROC AREA"),nnm.table),file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Neural Network Structure Selected",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("INPUT NODES","HIDDEN LAYER NODES","OUTPUT NODES"),cbind(result.nnm$n[1],result.nnm$n[2],result.nnm$n[3])),file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Value of Weight Decay Term Selected",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(weight.decay.selected,file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Value of Fitting Criterion Plus Weight Decay Term",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(result.nnm$value,file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("Best Set of Weights Found",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(result.nnm$wts,file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("CONTINGENCY TABLE MODEL RESULT",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.nnm$table[,1,],contingency.table.nnm$table[,2,],contingency.table.nnm$table[,3,])),file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("CONTINGENCY TABLE VALIDATION",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.temporal.validation.nnm$table[,1,],contingency.table.temporal.validation.nnm$table[,2,],contingency.table.temporal.validation.nnm$table[,3,])),file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("MATCHING CODE DEFINITION",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(cbind(c("","OBSERVED NO LANDSLIDES: 0","OBSERVED LANDSLIDES: 1"), c("PREDICTED NO LANDSLIDES: 0","00 -> Code 1","10 -> Code 3"), c("PREDICTED LANDSLIDES: 1","01 -> Code 2","11 -> Code 4")),file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table("FINAL RESULTS",file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  write.table(rbind(c("ID","GROUPING VARIABLE","MODEL POSTERIOR PROBABILITY","MODEL CLASSIFICATION","MODEL RESULT MATCHING CODE","VALIDATION GROUPING VARIABLE","VALIDATION MATCHING CODE"),cbind(identification.value,as.numeric(grouping.variable)-1,predict.result.nnm,round(predict.result.nnm),result.nnm.matching.code,as.numeric(levels(validation.grouping.variable))[validation.grouping.variable],validation.nnm.matching.code)),file="result_NNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
  }
  

#-------------------- FORECAST COMBINATION MODEL --------------------#
#####  FORECAST COMBINATION USING A LOGISTIC REGRESSION MODEL
#####(A constrained Ordinary Least Squared estimation can also be adopted)

if(model.run.matrix[5] == "YES")
  {

  library(Zelig) 
  forecasting.combined.variables<-as.data.frame(cbind(data.variables[,1],predict.result.lda$posterior[,2],predict.result.qda$posterior[,2],result.lrm$fitted.values,predict.result.nnm))
  colnames(forecasting.combined.variables)<-c("FRAX","resultlda","resultqda","resultlrm","resultnnm")   # Names of column mustn't have points

  if (class(try(zelig(as.formula(paste(names(forecasting.combined.variables)[1],"~",paste(names(forecasting.combined.variables)[2:dim(forecasting.combined.variables)[2]],collapse= "+"))), data=forecasting.combined.variables, model="logit")))=="try-error")  
    { 
    #zelig(as.formula(paste(names(forecasting.combined.variables)[1],"~",paste(names(forecasting.combined.variables)[2:dim(forecasting.combined.variables)[2]],collapse= "+"))), data=forecasting.combined.variables, model="logit")
    write.table("The combination of forecast using Logistic Regression Model was not completed",file="Error_CFM_Analysis.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="Error_CFM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("Error LOG",file="Error_CFM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind("Message",rev(1:length(as.vector(.Traceback)))," ->",as.vector(.Traceback)),file="Error_CFM_Analysis.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    } 
        
    result.cfm<-NULL
    result.cfm<-zelig(as.formula(paste(names(forecasting.combined.variables)[1],"~",paste(names(forecasting.combined.variables)[2:dim(forecasting.combined.variables)[2]],collapse= "+"))), data=forecasting.combined.variables, model="logit")
    summary(result.cfm)    
    #names(result.cfm)

    #for predicted value (posterior probablity calculated with model) result.cfm$fitted.values was considered

    cross.classification.cfm<-table(as.numeric(result.cfm$y),round(result.cfm$fitted.values),dnn=c("Observed","Predicted"))
    rownames(cross.classification.cfm)<-list("No Landslide","Landslide") # Observed
    colnames(cross.classification.cfm)<-list("No Landslide","Landslide") # Predicted    
    #str(cross.classification.cfm)
    
    # Assignation of a matching code between observed and predicted values
    result.cfm.matching.code<-paste(grouping.variable,round(result.cfm$fitted.values),sep="")
    result.cfm.matching.code<-gsub("00","1",result.cfm.matching.code)
    result.cfm.matching.code<-gsub("01","2",result.cfm.matching.code)
    result.cfm.matching.code<-gsub("10","3",result.cfm.matching.code)
    result.cfm.matching.code<-gsub("11","4",result.cfm.matching.code)
    result.cfm.matching.code<-as.numeric(result.cfm.matching.code)
    
    #Elaboration of Coefficient of association for contingency table
    #load package (vcd)  
    library(vcd)
    
    #help(package=vcd)         
    contingency.table.cfm<-table2d_summary(cross.classification.cfm)
    test.table.cfm<-assocstats(cross.classification.cfm)
    
      
    #Different plots for contingency table 
    
    windows()       
    fourfold(cross.classification.cfm,std="margin", main="COMBINATION LOGISTIC REGRESSION MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
    
    #Receiver Operating Characteristic (ROC) plots for one or more models.
    #A ROC curve plots the false alarm rate against the hit rate
    #for a probablistic forecast for a range of thresholds. 
    
    #load package (verification)  
    library(verification)
    
    #verify function
    #Based on the type of inputs, this function calculates a range of verification statistics and skill scores.
    #Additionally, it creates a verify class object that can be further analyzed.
    
    
    
    ##### ROC PLOT OBS - POSTERIOR PROBABILITY ASSOCIATED TO 1                                                                                 
    
    # Method using verify function
    verification.results.cfm<-verify(result.cfm$y,result.cfm$fitted.values, frcst.type="prob", obs.type="binary")
    #summary(verification.results.cfm)
    #str(verification.results.qda)
    #windows()
    #roc.plot(verification.results.cfm, main = "ROC PLOT: COMBINATION LOGISTIC REGRESSION", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
    area.under.roc.curve.cfm<-roc.area(result.cfm$y,result.cfm$fitted.values)
    
    
    ## showing confidence intervals.  MAY BE SLOW
    
    windows()                                                              
    roc.plot(verification.results.cfm, main = "ROC PLOT: COMBINATION LOGISTIC REGRESSION", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[5] , alpha = 0.05, extra=TRUE, legend=TRUE)
    mtext(paste("ROC area = ",round(area.under.roc.curve.cfm$A,2),";  Sample size = ",area.under.roc.curve.cfm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[5], sep=""), side=3, col="red", cex=0.8)
      
    ## Histogram of posterior probability
    windows()                            
    hist(result.cfm$fitted.values, breaks=breaks.histogram.values, freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Combination Logistic Regression Model susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
    pdf(file = "result_CFM_Histogram.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    hist(result.cfm$fitted.values, breaks=breaks.histogram.values, freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Combination Logistic Regression Model susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
    dev.off() 
  
  
    # EXPORT OF PLOT FOR CFM MODEL
    
    pdf(file = "result_CFM_FourfoldPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    fourfold(cross.classification.cfm,std="margin", main="COMBINATION LOGISTIC REGRESSION MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
    dev.off()
    
    #pdf(file = "result_CFM_ROCPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    #roc.plot(verification.results.cfm, main = "ROC PLOT: COMBINATION LOGISTIC REGRESSION", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
    #dev.off()
    
    pdf(file = "result_CFM_ROCPlot_bootstrap.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    roc.plot(verification.results.cfm, main = "ROC PLOT: COMBINATION LOGISTIC REGRESSION", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[5] , alpha = 0.05, extra=TRUE, legend=TRUE)
    mtext(paste("ROC area = ",round(area.under.roc.curve.cfm$A,2),";  Sample size = ",area.under.roc.curve.cfm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[5], sep=""), side=3, col="red", cex=0.8)
    dev.off()

  ## BOOTSTRAP PROCEDURE FOR THE ESTIMATION OF MODEL PREDICTION VARIABILITY
  if(bootstrap.model.variability[5] == "YES")
    {
    bootstrap.sample.model.cfm<-bootstrap.sample.model[5]

    matrix.bootstrap.model.cfm<-matrix(data=NA, nrow=dim(data.table)[1], ncol=(bootstrap.sample.model.cfm*3)+1)
    colnames(matrix.bootstrap.model.cfm)<-rep("na",(bootstrap.sample.model.cfm*3)+1)
    matrix.bootstrap.model.cfm[,1]<-identification.value
    colnames(matrix.bootstrap.model.cfm)[1]<-"ID"
    name.sel.run<-paste(rep("ID_Selection_Run",bootstrap.sample.model.cfm),1:bootstrap.sample.model.cfm,sep="_")
    colnames(matrix.bootstrap.model.cfm)[seq(2,(bootstrap.sample.model.cfm*3)-1,3)]<-name.sel.run
    name.prob.run<-paste(rep("Probability_Run",bootstrap.sample.model.cfm),1:bootstrap.sample.model.cfm,sep="_")
    colnames(matrix.bootstrap.model.cfm)[seq(3,(bootstrap.sample.model.cfm*3),3)]<-name.prob.run
    name.pred.run<-paste(rep("Prediction_Run",bootstrap.sample.model.cfm),1:bootstrap.sample.model.cfm,sep="_")
    colnames(matrix.bootstrap.model.cfm)[seq(4,(bootstrap.sample.model.cfm*3)+1,3)]<-name.pred.run

    selection.index<-NULL
    library(Zelig)
    #Bootstrap procedure
    for (count.boot in 1:bootstrap.sample.model.cfm)
        {
        selection.index<-sample(1:dim(data.table)[1], replace=TRUE, prob=NULL)
        matrix.bootstrap.model.cfm[as.numeric(names(table(selection.index))),(count.boot*3)-1]<-table(selection.index)
        data.variables.bootstrap.model.cfm<-forecasting.combined.variables[selection.index,]
        explanatory.variables.bootstrap.model.cfm<-forecasting.combined.variables[selection.index,2:dim(forecasting.combined.variables)[2]]
        grouping.variable.bootstrap.model.cfm<-as.factor(forecasting.combined.variables[selection.index,1])
        result.bootstrap.model.cfm<-zelig(as.formula(paste(names(data.variables.bootstrap.model.cfm)[1],"~",paste(names(data.variables.bootstrap.model.cfm[,2:dim(data.variables.bootstrap.model.cfm)[2]]),collapse= "+"))), data=data.variables.bootstrap.model.cfm, model="logit")
        excluded.variables.bootstrap.model.cfm<-which(match(result.bootstrap.model.cfm$coefficients,NA)==1)
        if (length(excluded.variables.bootstrap.model.cfm) != 0)
          {
          data.variables.bootstrap.model.cfm.selected<-data.variables.bootstrap.model.cfm[,-excluded.variables.bootstrap.model.cfm]
          setx.data.probability<-forecasting.combined.variables[as.numeric(names(table(selection.index))),][,-excluded.variables.bootstrap.model.cfm]
          setx.data.prediction<-forecasting.combined.variables[,-excluded.variables.bootstrap.model.cfm]
          } else
          {
          data.variables.bootstrap.model.cfm.selected<-data.variables.bootstrap.model.cfm
          setx.data.probability<-forecasting.combined.variables[as.numeric(names(table(selection.index))),]
          setx.data.prediction<-forecasting.combined.variables
          }
        result.bootstrap.model.cfm.selected<-zelig(as.formula(paste(names(data.variables.bootstrap.model.cfm.selected)[1],"~",paste(names(data.variables.bootstrap.model.cfm.selected[,2:dim(data.variables.bootstrap.model.cfm.selected)[2]]),collapse= "+"))), data=data.variables.bootstrap.model.cfm.selected, model="logit")
        x.result.bootstrap.model.cfm.selected.probability<-setx(result.bootstrap.model.cfm.selected,data=setx.data.probability,fn=NULL)
        sim.result.bootstrap.model.cfm.selected.probability<-sim(result.bootstrap.model.cfm.selected,x=x.result.bootstrap.model.cfm.selected.probability,num=c(100,100))
        matrix.bootstrap.model.cfm[as.numeric(names(table(selection.index))),(count.boot*3)]<-colMeans(sim.result.bootstrap.model.cfm.selected.probability$qi$ev)
        x.result.bootstrap.model.cfm.selected.prediction<-setx(result.bootstrap.model.cfm.selected,data=setx.data.prediction,fn=NULL)
        sim.result.bootstrap.model.cfm.selected.prediction<-sim(result.bootstrap.model.cfm.selected,x=x.result.bootstrap.model.cfm.selected.prediction,num=c(100,100))
        matrix.bootstrap.model.cfm[,(count.boot*3)+1]<-colMeans(sim.result.bootstrap.model.cfm.selected.prediction$qi$ev)
        }

    # Export of bootstrap sample
    write.table(matrix.bootstrap.model.cfm,file="result_CFM_BootstrapSamples.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=TRUE)

    ID.bootstrap.model.cfm.count<-numeric(length=dim(data.table)[1])
    #Probability (selected values)
    bootstrap.model.cfm.probability.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.probability.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.probability.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.probability.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.probability.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.probability.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    #Prediction (all values)
    bootstrap.model.cfm.prediction.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.prediction.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.prediction.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.prediction.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.prediction.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.prediction.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    for (count.row.variability in 1:dim(data.table)[1])
        {
        # Statistics on boostrapped probability
        ID.bootstrap.model.cfm.count[count.row.variability]<-length(na.omit(matrix.bootstrap.model.cfm[count.row.variability,seq(2,(bootstrap.sample.model.cfm*3)-1,3)]))
        bootstrap.model.cfm.probability.mean[count.row.variability]<-mean(na.omit(matrix.bootstrap.model.cfm[count.row.variability,seq(3,(bootstrap.sample.model.cfm*3),3)]))
        bootstrap.model.cfm.probability.sd[count.row.variability]<-sd(na.omit(matrix.bootstrap.model.cfm[count.row.variability,seq(3,(bootstrap.sample.model.cfm*3),3)]))
        bootstrap.model.cfm.probability.min[count.row.variability]<-min(na.omit(matrix.bootstrap.model.cfm[count.row.variability,seq(3,(bootstrap.sample.model.cfm*3),3)]))
        bootstrap.model.cfm.probability.max[count.row.variability]<-max(na.omit(matrix.bootstrap.model.cfm[count.row.variability,seq(3,(bootstrap.sample.model.cfm*3),3)]))
        bootstrap.model.cfm.probability.sderror[count.row.variability]<-bootstrap.model.cfm.probability.sd[count.row.variability]/ID.bootstrap.model.cfm.count[count.row.variability]
        bootstrap.model.cfm.probability.quantiles[count.row.variability,]<-quantile(na.omit(matrix.bootstrap.model.cfm[count.row.variability,seq(3,(bootstrap.sample.model.cfm*3),3)]),probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        # Statistics on boostrapped prediction
        bootstrap.model.cfm.prediction.mean[count.row.variability]<-mean(matrix.bootstrap.model.cfm[count.row.variability,seq(4,(bootstrap.sample.model.cfm*3)+1,3)])
        bootstrap.model.cfm.prediction.sd[count.row.variability]<-sd(matrix.bootstrap.model.cfm[count.row.variability,seq(4,(bootstrap.sample.model.cfm*3)+1,3)])
        bootstrap.model.cfm.prediction.min[count.row.variability]<-min(matrix.bootstrap.model.cfm[count.row.variability,seq(4,(bootstrap.sample.model.cfm*3)+1,3)])
        bootstrap.model.cfm.prediction.max[count.row.variability]<-max(matrix.bootstrap.model.cfm[count.row.variability,seq(4,(bootstrap.sample.model.cfm*3)+1,3)])
        bootstrap.model.cfm.prediction.sderror[count.row.variability]<-bootstrap.model.cfm.prediction.sd[count.row.variability]/bootstrap.sample.model.cfm
        bootstrap.model.cfm.prediction.quantiles[count.row.variability,]<-quantile(matrix.bootstrap.model.cfm[count.row.variability,seq(4,(bootstrap.sample.model.cfm*3)+1,3)],probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        }

    # Export of bootstrap sample statistics
    write.table(cbind("ID","CFM_NumberSelectedSamples","CFM_Probability_Mean","CFM_Probability_Sd","CFM_Probability_Min","CFM_Probability_Max","CFM_Probability_Sderror","CFM_Probability_Quantiles_0","CFM_Probability_Quantiles_0.05","CFM_Probability_Quantiles_0.25","CFM_Probability_Quantiles_0.5","CFM_Probability_Quantiles_0.75","CFM_Probability_Quantiles_0.95","CFM_Probability_Quantiles_1","CFM_Prediction_Mean","CFM_Prediction_Sd","CFM_Prediction_Min","CFM_Prediction_Max","CFM_Prediction_Sderror","CFM_Prediction_Quantiles_0","CFM_Prediction_Quantiles_0.05","CFM_Prediction_Quantiles_0.25","CFM_Prediction_Quantiles_0.5","CFM_Prediction_Quantiles_0.75","CFM_Prediction_Quantiles_0.95","CFM_Prediction_Quantiles_1"),file="result_CFM_BootstrapStatistics.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind(identification.value,ID.bootstrap.model.cfm.count,bootstrap.model.cfm.probability.mean,bootstrap.model.cfm.probability.sd,bootstrap.model.cfm.probability.min,bootstrap.model.cfm.probability.max,bootstrap.model.cfm.probability.sderror,bootstrap.model.cfm.probability.quantiles,bootstrap.model.cfm.prediction.mean,bootstrap.model.cfm.prediction.sd,bootstrap.model.cfm.prediction.min,bootstrap.model.cfm.prediction.max,bootstrap.model.cfm.prediction.sderror,bootstrap.model.cfm.prediction.quantiles),file="result_CFM_BootstrapStatistics.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)

    #windows()
    #double.sd.histogram.variability<-hist(bootstrap.model.cfm.probability.sd*2,breaks=seq(0,1,0.05),labels=TRUE)
    #plot(double.sd.histogram.variability$counts, seq(0,0.95,0.05), type="S",ylim=c(0,1), labels=TRUE)


    windows()
    plot(bootstrap.model.cfm.probability.mean,bootstrap.model.cfm.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="CFM BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)

    pdf(file = "result_CFM_BootstrapMeansComparison.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(bootstrap.model.cfm.probability.mean,bootstrap.model.cfm.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="CFM BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)
    dev.off()


    # BOOTSTRAPPED PROBABILITY - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.probability.cfm<-cbind(bootstrap.model.cfm.probability.mean,2*bootstrap.model.cfm.probability.sd)
    parabola.probability.cfm<-na.omit(parabola.probability.cfm[order(parabola.probability.cfm[,1]),])
    colnames(parabola.probability.cfm)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.probability.cfm <- nls(parabola.probability.cfm[,"ordinate"] ~ coeff.a*(parabola.probability.cfm[,"abscissa"]^2) + (-1)*coeff.a*parabola.probability.cfm[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.probability.cfm<-predict(fit.parabola.probability.cfm)
    #coef(fit.parabola.probability.cfm)

    windows()
    plot(parabola.probability.cfm[,"abscissa"],parabola.probability.cfm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="CFM Model Probability Variability (Bootstrap)")
    lines(parabola.probability.cfm[,"abscissa"],value.parabola.probability.cfm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.cfm),3),coeff.b= -round(coef(fit.parabola.probability.cfm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_CFM_BootstrapProbabilityVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.probability.cfm[,"abscissa"],parabola.probability.cfm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="CFM Model Probability Variability (Bootstrap)")
    lines(parabola.probability.cfm[,"abscissa"],value.parabola.probability.cfm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.cfm),3),coeff.b= -round(coef(fit.parabola.probability.cfm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()

    # BOOTSTRAPPED PREDICTION - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.prediction.cfm<-cbind(bootstrap.model.cfm.prediction.mean,2*bootstrap.model.cfm.prediction.sd)
    parabola.prediction.cfm<-parabola.prediction.cfm[order(parabola.prediction.cfm[,1]),]
    colnames(parabola.prediction.cfm)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.prediction.cfm <- nls(parabola.prediction.cfm[,"ordinate"] ~ coeff.a*(parabola.prediction.cfm[,"abscissa"]^2) + (-1)*coeff.a*parabola.prediction.cfm[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.prediction.cfm<-predict(fit.parabola.prediction.cfm)
    #coef(fit.parabola.prediction.cfm)

    windows()
    plot(parabola.prediction.cfm[,"abscissa"],parabola.prediction.cfm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="CFM Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.cfm[,"abscissa"],value.parabola.prediction.cfm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.cfm),3),coeff.b= -round(coef(fit.parabola.prediction.cfm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_CFM_BootstrapPredictionVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.prediction.cfm[,"abscissa"],parabola.prediction.cfm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="CFM Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.cfm[,"abscissa"],value.parabola.prediction.cfm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.cfm),3),coeff.b= -round(coef(fit.parabola.prediction.cfm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()
  }

    ## Sensitivity, Specificity, Cohens kappa plot
    roc.plot.cfm.series<-roc.plot(verification.results.cfm,binormal=TRUE)
    #str(roc.plot.cfm.series)
    #roc.plot.cfm.series$plot.data
    #str(roc.plot.cfm.series$plot.data)

    contingency.table.matrix.cfm<-matrix(nrow=dim(roc.plot.cfm.series$plot.data)[1],ncol=8)
    colnames(contingency.table.matrix.cfm)<-c("Threshold","TP","TN","FP","FN","TPR","FPR","COHEN_KAPPA")
    contingency.table.matrix.cfm[,1]<-roc.plot.cfm.series$plot.data[,1,1]
    contingency.table.matrix.cfm[,6]<-roc.plot.cfm.series$plot.data[,2,1]
    contingency.table.matrix.cfm[,7]<-roc.plot.cfm.series$plot.data[,3,1]
    values.odserved<-data.table[,2]
    values.predicted<-result.cfm$fitted.values
    for (threshold.series in 1:dim(roc.plot.cfm.series$plot.data)[1])
        {
        value.threshold<-contingency.table.matrix.cfm[threshold.series,1]
        values.probability.reclassified<-NULL
        values.probability.reclassified<-numeric(length=length(values.odserved))

        for (length.observed.series in 1:length(values.odserved))
          {
          if (values.predicted[length.observed.series] > value.threshold)
             {
             values.probability.reclassified[length.observed.series]<-1
             } else
             {
             values.probability.reclassified[length.observed.series]<-0
             }
          }
          #sum(values.probability.reclassified-round(values.predicted)) # Check sum: It has to be 0 if threshold is equal to 1
          series.pasted<-paste(values.odserved,values.probability.reclassified,sep="")
          series.pasted<-gsub("00","1",series.pasted)
          series.pasted<-gsub("01","2",series.pasted)
          series.pasted<-gsub("10","3",series.pasted)
          series.pasted<-gsub("11","4",series.pasted)
          series.pasted<-as.numeric(series.pasted)

          TP<-length(series.pasted[series.pasted>=4])                   # True Positive
          FN<-length(series.pasted[series.pasted>=3 & series.pasted<4]) # False Negative
          FP<-length(series.pasted[series.pasted>=2 & series.pasted<3]) # False Positive
          TN<-length(series.pasted[series.pasted>=1 & series.pasted<2]) # True Negative

          #TPR<-TP/(TP+FN) # Hit Rate or True Positive Rate or Sensitivity - Assigned before the for cicle using rocplot data
          #FPR<-FP/(FP+TN) # False Alarm Rate or False Positive Rate or 1-Specificity

          # Cohen's Kappa = (agreement-chance)/(1-chance)  where agreement=(TP+TN)/(TP+TN+FP+FN) and chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
          agreement=(TP+TN)/(TP+TN+FP+FN)
          chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
          cohen.kappa.value<-(agreement-chance)/(1-chance)
          #Other
          #library(vcd)
          #cohen.kappa.value<-Kappa(cross.classification.table)
          contingency.table.matrix.cfm[threshold.series,2]<-TP
          contingency.table.matrix.cfm[threshold.series,3]<-TN
          contingency.table.matrix.cfm[threshold.series,4]<-FP
          contingency.table.matrix.cfm[threshold.series,5]<-FN
          contingency.table.matrix.cfm[threshold.series,8]<-cohen.kappa.value

        }

    windows()
    plot(roc.plot.cfm.series$plot.data[,1,1],roc.plot.cfm.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="CFM MODEL EVALUATION PLOT")
    points(roc.plot.cfm.series$plot.data[,1,1],1-roc.plot.cfm.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
    points(roc.plot.cfm.series$plot.data[,1,1], contingency.table.matrix.cfm[,8],col="blue",pch=1,cex=0.6)
    mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
    mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
    mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
    pdf(file = "result_CFM_ModelEvaluationPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(roc.plot.cfm.series$plot.data[,1,1],roc.plot.cfm.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="CFM MODEL EVALUATION PLOT")
    points(roc.plot.cfm.series$plot.data[,1,1],1-roc.plot.cfm.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
    points(roc.plot.cfm.series$plot.data[,1,1], contingency.table.matrix.cfm[,8],col="blue",pch=1,cex=0.6)
    mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
    mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
    mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
    dev.off()

    ## VALIDATION OF CFM MODEL (Matching CFM posterior probability results and validation grouping variable)
    cross.classification.temporal.validation.cfm<-table(validation.grouping.variable,round(result.cfm$fitted.values),dnn=c("Observed","Predicted"))
    rownames(cross.classification.temporal.validation.cfm)<-list("No Landslide","Landslide")  #Observed
    colnames(cross.classification.temporal.validation.cfm)<-list("No Landslide","Landslide")  #Predicted
    #str(cross.classification.temporal.validation.cfm)

    #Elaboration of Coefficient of association for contingency table
    #load package (vcd)
    library(vcd)

    #help(package=vcd)
    contingency.table.temporal.validation.cfm<-table2d_summary(cross.classification.temporal.validation.cfm)
    test.table.temporal.validation.cfm<-assocstats(cross.classification.temporal.validation.cfm)

    #Different plots for contingency table
    windows()
    fourfold(cross.classification.temporal.validation.cfm, std="margin", main="TEMPORAL VALIDATION CFM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(170,135,210,max=255), rgb(115,70,155,max=255)))

    #Receiver Operating Characteristic (ROC) plots for one or more models.
    #load package (verification)
    library(verification)

    # 2nd method using verify function
    verification.temporal.validation.cfm<-verify(validation.table[,2],result.cfm$fitted.values, frcst.type="prob", obs.type="binary")
    #summary(verification.temporal.validation.cfm)

    # showing confidence intervals.  MAY BE SLOW
    area.under.roc.curve.temporal.validation.cfm<-roc.area(validation.table[,2],result.cfm$fitted.values)
    windows()
    roc.plot(verification.temporal.validation.cfm, main = "ROC PLOT: TEMPORAL VALIDATION CFM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[5] , alpha = 0.05, extra=TRUE, legend=TRUE)
    mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.cfm$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.cfm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[5], sep=""), side=3, col="red", cex=0.8)


    # EXPORT OF PLOT FOR VALIDATION OF CFM MODEL

    pdf(file = "result_CFM_FourfoldPlot_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    fourfold(cross.classification.temporal.validation.cfm, std="margin", main="TEMPORAL VALIDATION CFM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255),  rgb(170,135,210,max=255), rgb(115,70,155,max=255)))
    dev.off()

    #pdf(file = "result_CFM_ROCPlot_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    #roc.plot(verification.temporal.validation.cfm, main = "ROC PLOT: TEMPORAL VALIDATION CFM MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
    #area.under.roc.curve.temporal.validation.cfm<-roc.area(verification.table[,2],result.cfm$fitted.values)
    #dev.off()

    pdf(file = "result_CFM_ROCPlot_bootstrap_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    roc.plot(verification.temporal.validation.cfm, main = "ROC PLOT: TEMPORAL VALIDATION CFM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[5] , alpha = 0.05, extra=TRUE, legend=TRUE)
    mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.cfm$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.cfm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[5], sep=""), side=3, col="red", cex=0.8)
    dev.off()

    # Assignation of a matching code between observed and predicted values calculated using the validation dataset
    validation.cfm.matching.code<-paste(validation.grouping.variable,round(result.cfm$fitted.values),sep="")
    validation.cfm.matching.code<-gsub("00","1",validation.cfm.matching.code)
    validation.cfm.matching.code<-gsub("01","2",validation.cfm.matching.code)
    validation.cfm.matching.code<-gsub("10","3",validation.cfm.matching.code)
    validation.cfm.matching.code<-gsub("11","4",validation.cfm.matching.code)
    validation.cfm.matching.code<-as.numeric(validation.cfm.matching.code)


    # EXPORT OF CFM MODEL RESULTS

    write.table("RESULTS OF COMBINATION FORECAST LOGISTIC REGRESSION MODEL",file="result_CFM.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("CFM MODEL OUTPUTS",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("Logistic Regression coefficients",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    #Scaling coefficients
    write.table(cbind(names(result.cfm$coefficients),result.cfm$coefficients),file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("CONTINGENCY TABLE MODEL RESULT",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.cfm$table[,1,],contingency.table.cfm$table[,2,],contingency.table.cfm$table[,3,])),file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("CONTINGENCY TABLE VALIDATION",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.temporal.validation.cfm$table[,1,],contingency.table.temporal.validation.cfm$table[,2,],contingency.table.temporal.validation.cfm$table[,3,])),file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("MATCHING CODE DEFINITION",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind(c("","OBSERVED NO LANDSLIDES: 0","OBSERVED LANDSLIDES: 1"), c("PREDICTED NO LANDSLIDES: 0","00 -> Code 1","10 -> Code 3"), c("PREDICTED LANDSLIDES: 1","01 -> Code 2","11 -> Code 4")),file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("FINAL RESULTS",file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(rbind(c("ID","GROUPING VARIABLE","MODEL POSTERIOR PROBABILITY","MODEL CLASSIFICATION","MODEL RESULT MATCHING CODE","VALIDATION GROUPING VARIABLE","VALIDATION MATCHING CODE"),cbind(identification.value,result.cfm$y,result.cfm$fitted.values,round(result.cfm$fitted.values),result.cfm.matching.code,as.numeric(levels(validation.grouping.variable))[validation.grouping.variable],validation.cfm.matching.code)),file="result_CFM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    }


#-------------- FORECAST COMBINATION MODEL WITHOUT NNM ---------------#
#####  FORECAST COMBINATION USING A LOGISTIC REGRESSION MODEL WITHOUT NNM MODEL
#####  

if(model.run.matrix[6] == "YES")
  {

  library(Zelig) 
  forecasting.combined.variables<-as.data.frame(cbind(data.variables[,1],predict.result.lda$posterior[,2],predict.result.qda$posterior[,2],result.lrm$fitted.values))
  colnames(forecasting.combined.variables)<-c("FRAX","resultlda","resultqda","resultlrm")
  
  if (class(try(zelig(as.formula(paste(names(forecasting.combined.variables)[1],"~",paste(names(forecasting.combined.variables)[2:dim(forecasting.combined.variables)[2]],collapse= "+"))), data=forecasting.combined.variables, model="logit")))=="try-error")
    { 
    #zelig(as.formula(paste(names(forecasting.combined.variables)[1],"~",paste(names(forecasting.combined.variables)[2:dim(forecasting.combined.variables)[2]],collapse= "+"))), data=forecasting.combined.variables, model="logit")
    write.table("The combination of forecast using Logistic Regression Model was not completed",file="Error_CFM_Analysis_NoNNM.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="Error_CFM_Analysis_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("Error LOG",file="Error_CFM_Analysis_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind("Message",rev(1:length(as.vector(.Traceback)))," ->",as.vector(.Traceback)),file="Error_CFM_Analysis_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    } 
        
    result.cfm.nonnm<-NULL
    result.cfm.nonnm<-zelig(as.formula(paste(names(forecasting.combined.variables)[1],"~",paste(names(forecasting.combined.variables)[2:dim(forecasting.combined.variables)[2]],collapse= "+"))), data=forecasting.combined.variables, model="logit")

    #summary(result.cfm.nonnm)
    #names(result.cfm.nonnm)
    
    
    #for predicted value (posterior probablity calculated with model) result.cfm$fitted.values was considered

    cross.classification.cfm.nonnm<-table(as.numeric(result.cfm.nonnm$y),round(result.cfm.nonnm$fitted.values),dnn=c("Observed","Predicted"))
    rownames(cross.classification.cfm.nonnm)<-list("No Landslide","Landslide") # Observed
    colnames(cross.classification.cfm.nonnm)<-list("No Landslide","Landslide") # Predicted    
    #str(cross.classification.cfm.nonnm)
    
    # Assignation of a matching code between observed and predicted values
    result.cfm.nonnm.matching.code<-paste(grouping.variable,round(result.cfm.nonnm$fitted.values),sep="")
    result.cfm.nonnm.matching.code<-gsub("00","1",result.cfm.nonnm.matching.code)
    result.cfm.nonnm.matching.code<-gsub("01","2",result.cfm.nonnm.matching.code)
    result.cfm.nonnm.matching.code<-gsub("10","3",result.cfm.nonnm.matching.code)
    result.cfm.nonnm.matching.code<-gsub("11","4",result.cfm.nonnm.matching.code)
    result.cfm.nonnm.matching.code<-as.numeric(result.cfm.nonnm.matching.code)
    
    #Elaboration of Coefficient of association for contingency table
    #load package (vcd)  
    library(vcd)
    
    #help(package=vcd)         
    contingency.table.cfm.nonnm<-table2d_summary(cross.classification.cfm.nonnm)
    test.table.cfm.nonnm<-assocstats(cross.classification.cfm.nonnm)
    
      
    #Different plots for contingency table 
    
    windows()       
    fourfold(cross.classification.cfm.nonnm, std="margin", main="COMBINATION LOGISTIC NO NNM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
    
    #Receiver Operating Characteristic (ROC) plots for one or more models.
    #A ROC curve plots the false alarm rate against the hit rate
    #for a probablistic forecast for a range of thresholds. 
    
    #load package (verification)  
    library(verification)
    
    #verify function
    #Based on the type of inputs, this function calculates a range of verification statistics and skill scores.
    #Additionally, it creates a verify class object that can be further analyzed.
    
    
    
    ##### ROC PLOT OBS - POSTERIOR PROBABILITY ASSOCIATED TO 1                                                                                 
    
    # Method using verify function
    verification.results.cfm.nonnm<-verify(result.cfm.nonnm$y,result.cfm.nonnm$fitted.values, frcst.type="prob", obs.type="binary")
    #summary(verification.results.cfm.nonnm)
    #str(verification.results.qda)
    #windows()
    #roc.plot(verification.results.cfm.nonnm, main = "ROC PLOT: COMBINATION LOGISTIC NO NNM MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
    area.under.roc.curve.cfm.nonnm<-roc.area(result.cfm.nonnm$y,result.cfm.nonnm$fitted.values)
    
    
    ## showing confidence intervals.  MAY BE SLOW
    
    windows()                                                              
    roc.plot(verification.results.cfm.nonnm, main = "ROC PLOT: COMBINATION LOGISTIC NO NNM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[5] , alpha = 0.05, extra=TRUE, legend=TRUE)
    mtext(paste("ROC area = ",round(area.under.roc.curve.cfm.nonnm$A,2),";  Sample size = ",area.under.roc.curve.cfm.nonnm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[5], sep=""), side=3, col="red", cex=0.8)
      
    ## Histogram of posterior probability
    windows()                            
    hist(result.cfm.nonnm$fitted.values, breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Combination Logistic No NNM Model susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
    pdf(file = "result_CFM_NoNNM_Histogram.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    hist(result.cfm.nonnm$fitted.values, breaks=breaks.histogram.values,freq=TRUE, xlab="Susceptibility Class", ylab="Frequency", main="Histogram of Combination Logistic No NNM Model susceptibility", col=c(rgb(38,115,0,max=255),rgb(233,255,190,max=255),rgb(255,255,0,max=255),rgb(255,128,0,max=255),rgb(255,0,0,max=255)))
    dev.off() 
    
    
    # EXPORT OF PLOT FOR CFM NoNNM MODEL
    
    pdf(file = "result_CFM_NoNNM_FourfoldPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    fourfold(cross.classification.cfm.nonnm, std="margin", main="COMBINATION LOGISTIC NO NNM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(150,220,255,max=255), rgb(0,0,128,max=255)))
    dev.off()
    
    #pdf(file = "result_CFM_NoNNM_ROCPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    #roc.plot(verification.results.cfm.nonnm, main = "ROC PLOT: COMBINATION FORECAST WITHOUT NNM", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
    #dev.off()
    
    pdf(file = "result_CFM_NoNNM_ROCPlot_bootstrap.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    roc.plot(verification.results.cfm.nonnm, main = "ROC PLOT: COMBINATION LOGISTIC NO NNM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[5] , alpha = 0.05, extra=TRUE, legend=TRUE)
    mtext(paste("ROC area = ",round(area.under.roc.curve.cfm.nonnm$A,2),";  Sample size = ",area.under.roc.curve.cfm.nonnm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[5], sep=""), side=3, col="red", cex=0.8)
    dev.off()

  ## BOOTSTRAP PROCEDURE FOR THE ESTIMATION OF MODEL PREDICTION VARIABILITY
  if(bootstrap.model.variability[6] == "YES")
    {
    bootstrap.sample.model.cfm.nonnm<-bootstrap.sample.model[6]

    matrix.bootstrap.model.cfm.nonnm<-matrix(data=NA, nrow=dim(data.table)[1], ncol=(bootstrap.sample.model.cfm.nonnm*3)+1)
    colnames(matrix.bootstrap.model.cfm.nonnm)<-rep("na",(bootstrap.sample.model.cfm.nonnm*3)+1)
    matrix.bootstrap.model.cfm.nonnm[,1]<-identification.value
    colnames(matrix.bootstrap.model.cfm.nonnm)[1]<-"ID"
    name.sel.run<-paste(rep("ID_Selection_Run",bootstrap.sample.model.cfm.nonnm),1:bootstrap.sample.model.cfm.nonnm,sep="_")
    colnames(matrix.bootstrap.model.cfm.nonnm)[seq(2,(bootstrap.sample.model.cfm.nonnm*3)-1,3)]<-name.sel.run
    name.prob.run<-paste(rep("Probability_Run",bootstrap.sample.model.cfm.nonnm),1:bootstrap.sample.model.cfm.nonnm,sep="_")
    colnames(matrix.bootstrap.model.cfm.nonnm)[seq(3,(bootstrap.sample.model.cfm.nonnm*3),3)]<-name.prob.run
    name.pred.run<-paste(rep("Prediction_Run",bootstrap.sample.model.cfm.nonnm),1:bootstrap.sample.model.cfm.nonnm,sep="_")
    colnames(matrix.bootstrap.model.cfm.nonnm)[seq(4,(bootstrap.sample.model.cfm.nonnm*3)+1,3)]<-name.pred.run

    selection.index<-NULL
    library(Zelig)
    #Bootstrap procedure
    for (count.boot in 1:bootstrap.sample.model.cfm.nonnm)
        {
        selection.index<-sample(1:dim(data.table)[1], replace=TRUE, prob=NULL)
        matrix.bootstrap.model.cfm.nonnm[as.numeric(names(table(selection.index))),(count.boot*3)-1]<-table(selection.index)
        data.variables.bootstrap.model.cfm.nonnm<-forecasting.combined.variables[selection.index,]
        explanatory.variables.bootstrap.model.cfm.nonnm<-forecasting.combined.variables[selection.index,2:dim(forecasting.combined.variables)[2]]
        grouping.variable.bootstrap.model.cfm.nonnm<-as.factor(forecasting.combined.variables[selection.index,1])
        result.bootstrap.model.cfm.nonnm<-zelig(as.formula(paste(names(data.variables.bootstrap.model.cfm.nonnm)[1],"~",paste(names(data.variables.bootstrap.model.cfm.nonnm[,2:dim(data.variables.bootstrap.model.cfm.nonnm)[2]]),collapse= "+"))), data=data.variables.bootstrap.model.cfm.nonnm, model="logit")
        excluded.variables.bootstrap.model.cfm.nonnm<-which(match(result.bootstrap.model.cfm.nonnm$coefficients,NA)==1)
        if (length(excluded.variables.bootstrap.model.cfm.nonnm) != 0)
          {
          data.variables.bootstrap.model.cfm.nonnm.selected<-data.variables.bootstrap.model.cfm.nonnm[,-excluded.variables.bootstrap.model.cfm.nonnm]
          setx.data.probability<-forecasting.combined.variables[as.numeric(names(table(selection.index))),][,-excluded.variables.bootstrap.model.cfm.nonnm]
          setx.data.prediction<-forecasting.combined.variables[,-excluded.variables.bootstrap.model.cfm.nonnm]
          } else
          {
          data.variables.bootstrap.model.cfm.nonnm.selected<-data.variables.bootstrap.model.cfm.nonnm
          setx.data.probability<-forecasting.combined.variables[as.numeric(names(table(selection.index))),]
          setx.data.prediction<-forecasting.combined.variables
          }
        result.bootstrap.model.cfm.nonnm.selected<-zelig(as.formula(paste(names(data.variables.bootstrap.model.cfm.nonnm.selected)[1],"~",paste(names(data.variables.bootstrap.model.cfm.nonnm.selected[,2:dim(data.variables.bootstrap.model.cfm.nonnm.selected)[2]]),collapse= "+"))), data=data.variables.bootstrap.model.cfm.nonnm.selected, model="logit")
        x.result.bootstrap.model.cfm.nonnm.selected.probability<-setx(result.bootstrap.model.cfm.nonnm.selected,data=setx.data.probability,fn=NULL)
        sim.result.bootstrap.model.cfm.nonnm.selected.probability<-sim(result.bootstrap.model.cfm.nonnm.selected,x=x.result.bootstrap.model.cfm.nonnm.selected.probability,num=c(100,100))
        matrix.bootstrap.model.cfm.nonnm[as.numeric(names(table(selection.index))),(count.boot*3)]<-colMeans(sim.result.bootstrap.model.cfm.nonnm.selected.probability$qi$ev)
        x.result.bootstrap.model.cfm.nonnm.selected.prediction<-setx(result.bootstrap.model.cfm.nonnm.selected,data=setx.data.prediction,fn=NULL)
        sim.result.bootstrap.model.cfm.nonnm.selected.prediction<-sim(result.bootstrap.model.cfm.nonnm.selected,x=x.result.bootstrap.model.cfm.nonnm.selected.prediction,num=c(100,100))
        matrix.bootstrap.model.cfm.nonnm[,(count.boot*3)+1]<-colMeans(sim.result.bootstrap.model.cfm.nonnm.selected.prediction$qi$ev)
        }

    # Export of bootstrap sample
    write.table(matrix.bootstrap.model.cfm.nonnm,file="result_CFM_NoNNM_BootstrapSamples.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=TRUE)

    ID.bootstrap.model.cfm.nonnm.count<-numeric(length=dim(data.table)[1])
    #Probability (selected values)
    bootstrap.model.cfm.nonnm.probability.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.probability.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.probability.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.probability.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.probability.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.probability.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    #Prediction (all values)
    bootstrap.model.cfm.nonnm.prediction.mean<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.prediction.sd<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.prediction.min<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.prediction.max<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.prediction.sderror<-numeric(length=dim(data.table)[1])
    bootstrap.model.cfm.nonnm.prediction.quantiles<-matrix(nrow=dim(data.table)[1],ncol=7)

    for (count.row.variability in 1:dim(data.table)[1])
        {
        # Statistics on boostrapped probability
        ID.bootstrap.model.cfm.nonnm.count[count.row.variability]<-length(na.omit(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(2,(bootstrap.sample.model.cfm.nonnm*3)-1,3)]))
        bootstrap.model.cfm.nonnm.probability.mean[count.row.variability]<-mean(na.omit(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(3,(bootstrap.sample.model.cfm.nonnm*3),3)]))
        bootstrap.model.cfm.nonnm.probability.sd[count.row.variability]<-sd(na.omit(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(3,(bootstrap.sample.model.cfm.nonnm*3),3)]))
        bootstrap.model.cfm.nonnm.probability.min[count.row.variability]<-min(na.omit(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(3,(bootstrap.sample.model.cfm.nonnm*3),3)]))
        bootstrap.model.cfm.nonnm.probability.max[count.row.variability]<-max(na.omit(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(3,(bootstrap.sample.model.cfm.nonnm*3),3)]))
        bootstrap.model.cfm.nonnm.probability.sderror[count.row.variability]<-bootstrap.model.cfm.nonnm.probability.sd[count.row.variability]/ID.bootstrap.model.cfm.nonnm.count[count.row.variability]
        bootstrap.model.cfm.nonnm.probability.quantiles[count.row.variability,]<-quantile(na.omit(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(3,(bootstrap.sample.model.cfm.nonnm*3),3)]),probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        # Statistics on boostrapped prediction
        bootstrap.model.cfm.nonnm.prediction.mean[count.row.variability]<-mean(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(4,(bootstrap.sample.model.cfm.nonnm*3)+1,3)])
        bootstrap.model.cfm.nonnm.prediction.sd[count.row.variability]<-sd(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(4,(bootstrap.sample.model.cfm.nonnm*3)+1,3)])
        bootstrap.model.cfm.nonnm.prediction.min[count.row.variability]<-min(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(4,(bootstrap.sample.model.cfm.nonnm*3)+1,3)])
        bootstrap.model.cfm.nonnm.prediction.max[count.row.variability]<-max(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(4,(bootstrap.sample.model.cfm.nonnm*3)+1,3)])
        bootstrap.model.cfm.nonnm.prediction.sderror[count.row.variability]<-bootstrap.model.cfm.nonnm.prediction.sd[count.row.variability]/bootstrap.sample.model.cfm.nonnm
        bootstrap.model.cfm.nonnm.prediction.quantiles[count.row.variability,]<-quantile(matrix.bootstrap.model.cfm.nonnm[count.row.variability,seq(4,(bootstrap.sample.model.cfm.nonnm*3)+1,3)],probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
        }

    # Export of bootstrap sample statistics
    write.table(cbind("ID","CFM_NoNNM_NumberSelectedSamples","CFM_NoNNM_Probability_Mean","CFM_NoNNM_Probability_Sd","CFM_NoNNM_Probability_Min","CFM_NoNNM_Probability_Max","CFM_NoNNM_Probability_Sderror","CFM_NoNNM_Probability_Quantiles_0","CFM_NoNNM_Probability_Quantiles_0.05","CFM_NoNNM_Probability_Quantiles_0.25","CFM_NoNNM_Probability_Quantiles_0.5","CFM_NoNNM_Probability_Quantiles_0.75","CFM_NoNNM_Probability_Quantiles_0.95","CFM_NoNNM_Probability_Quantiles_1","CFM_NoNNM_Prediction_Mean","CFM_NoNNM_Prediction_Sd","CFM_NoNNM_Prediction_Min","CFM_NoNNM_Prediction_Max","CFM_NoNNM_Prediction_Sderror","CFM_NoNNM_Prediction_Quantiles_0","CFM_NoNNM_Prediction_Quantiles_0.05","CFM_NoNNM_Prediction_Quantiles_0.25","CFM_NoNNM_Prediction_Quantiles_0.5","CFM_NoNNM_Prediction_Quantiles_0.75","CFM_NoNNM_Prediction_Quantiles_0.95","CFM_NoNNM_Prediction_Quantiles_1"),file="result_CFM_NoNNM_BootstrapStatistics.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind(identification.value,ID.bootstrap.model.cfm.nonnm.count,bootstrap.model.cfm.nonnm.probability.mean,bootstrap.model.cfm.nonnm.probability.sd,bootstrap.model.cfm.nonnm.probability.min,bootstrap.model.cfm.nonnm.probability.max,bootstrap.model.cfm.nonnm.probability.sderror,bootstrap.model.cfm.nonnm.probability.quantiles,bootstrap.model.cfm.nonnm.prediction.mean,bootstrap.model.cfm.nonnm.prediction.sd,bootstrap.model.cfm.nonnm.prediction.min,bootstrap.model.cfm.nonnm.prediction.max,bootstrap.model.cfm.nonnm.prediction.sderror,bootstrap.model.cfm.nonnm.prediction.quantiles),file="result_CFM_NoNNM_BootstrapStatistics.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)

    #windows()
    #double.sd.histogram.variability<-hist(bootstrap.model.cfm.nonnm.probability.sd*2,breaks=seq(0,1,0.05),labels=TRUE)
    #plot(double.sd.histogram.variability$counts, seq(0,0.95,0.05), type="S",ylim=c(0,1), labels=TRUE)

    windows()
    plot(bootstrap.model.cfm.nonnm.probability.mean,bootstrap.model.cfm.nonnm.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="CFM_NoNNM BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm.nonnm,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)

    pdf(file = "result_CFM_NoNNM_BootstrapMeansComparison.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(bootstrap.model.cfm.nonnm.probability.mean,bootstrap.model.cfm.nonnm.prediction.mean,xlab="Probability mean",ylab="Prediction mean", type="p",main="CFM_NoNNM BOOTSTRAP: Mean Probability vs Mean Prediction")
    abline(a=0,b=1,col="red",lty=1,lwd=1)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm.nonnm,sep=""),side=3, padj=-0.5, adj=0.5, col="red",cex=0.8)
    dev.off()


    # BOOTSTRAPPED PROBABILITY - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.probability.cfm.nonnm<-cbind(bootstrap.model.cfm.nonnm.probability.mean,2*bootstrap.model.cfm.nonnm.probability.sd)
    parabola.probability.cfm.nonnm<-na.omit(parabola.probability.cfm.nonnm[order(parabola.probability.cfm.nonnm[,1]),])
    colnames(parabola.probability.cfm.nonnm)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.probability.cfm.nonnm <- nls(parabola.probability.cfm.nonnm[,"ordinate"] ~ coeff.a*(parabola.probability.cfm.nonnm[,"abscissa"]^2) + (-1)*coeff.a*parabola.probability.cfm.nonnm[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.probability.cfm.nonnm<-predict(fit.parabola.probability.cfm.nonnm)
    #coef(fit.parabola.probability.cfm.nonnm)

    windows()
    plot(parabola.probability.cfm.nonnm[,"abscissa"],parabola.probability.cfm.nonnm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="CFM_NoNNM Model Probability Variability (Bootstrap)")
    lines(parabola.probability.cfm.nonnm[,"abscissa"],value.parabola.probability.cfm.nonnm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm.nonnm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.cfm.nonnm),3),coeff.b= -round(coef(fit.parabola.probability.cfm.nonnm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_CFM_NoNNM_BootstrapProbabilityVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.probability.cfm.nonnm[,"abscissa"],parabola.probability.cfm.nonnm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped probability mean",ylab="2 Standard Deviations", type="p",main="CFM_NoNNM Model Probability Variability (Bootstrap)")
    lines(parabola.probability.cfm.nonnm[,"abscissa"],value.parabola.probability.cfm.nonnm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm.nonnm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.probability.cfm.nonnm),3),coeff.b= -round(coef(fit.parabola.probability.cfm.nonnm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()

    # BOOTSTRAPPED PREDICTION - Fit parabola 3 parameter y = ax^2 + bx + c
    parabola.prediction.cfm.nonnm<-cbind(bootstrap.model.cfm.nonnm.prediction.mean,2*bootstrap.model.cfm.nonnm.prediction.sd)
    parabola.prediction.cfm.nonnm<-parabola.prediction.cfm.nonnm[order(parabola.prediction.cfm.nonnm[,1]),]
    colnames(parabola.prediction.cfm.nonnm)<-c("abscissa","ordinate")

    #If y has to be 0 in x=0 and x=1, this means that c=0 and a+b=0, so in our case since a<0, a has to be equal to -b
    fit.parabola.prediction.cfm.nonnm <- nls(parabola.prediction.cfm.nonnm[,"ordinate"] ~ coeff.a*(parabola.prediction.cfm.nonnm[,"abscissa"]^2) + (-1)*coeff.a*parabola.prediction.cfm.nonnm[,"abscissa"], start = c("coeff.a"=-1))
    value.parabola.prediction.cfm.nonnm<-predict(fit.parabola.prediction.cfm.nonnm)
    #coef(fit.parabola.prediction.cfm.nonnm)

    windows()
    plot(parabola.prediction.cfm.nonnm[,"abscissa"],parabola.prediction.cfm.nonnm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="CFM_NoNNM Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.cfm.nonnm[,"abscissa"],value.parabola.prediction.cfm.nonnm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm.nonnm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.cfm.nonnm),3),coeff.b= -round(coef(fit.parabola.prediction.cfm.nonnm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)

    pdf(file = "result_CFM_NoNNM_BootstrapPredictionVariability.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(parabola.prediction.cfm.nonnm[,"abscissa"],parabola.prediction.cfm.nonnm[,"ordinate"],xlim=c(0,1),ylim=c(0,1),xlab="Bootstrapped prediction mean",ylab="2 Standard Deviations", type="p",main="CFM_NoNNM Model Prediction Variability (Bootstrap)")
    lines(parabola.prediction.cfm.nonnm[,"abscissa"],value.parabola.prediction.cfm.nonnm,col="red",lwd=1.5)
    mtext(paste("Number of bootstrap samples: ",bootstrap.sample.model.cfm.nonnm,sep=""),side=3, padj=-0.5, adj=0.5, col="blue",cex=1)
    espr <- expression(Y == coeff.a %*% X ^2 + coeff.b %*% X)
    list.espr.subs <- list(coeff.a = round(coef(fit.parabola.prediction.cfm.nonnm),3),coeff.b= -round(coef(fit.parabola.prediction.cfm.nonnm),3))
    as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]])
    mtext(as.expression(do.call(substitute, list(as.call(espr), list.espr.subs))[[1]]),side=1, padj=-1.5, adj=0.5,col="red",cex=1)
    dev.off()
  }


    ## Sensitivity, Specificity, Cohens kappa plot
    roc.plot.cfm.nonnm.series<-roc.plot(verification.results.cfm.nonnm,binormal=TRUE)
    #str(roc.plot.cfm.series)
    #roc.plot.cfm.series$plot.data
    #str(roc.plot.cfm.series$plot.data)

    contingency.table.matrix.cfm.nonnm<-matrix(nrow=dim(roc.plot.cfm.nonnm.series$plot.data)[1],ncol=8)
    colnames(contingency.table.matrix.cfm.nonnm)<-c("Threshold","TP","TN","FP","FN","TPR","FPR","COHEN_KAPPA")
    contingency.table.matrix.cfm.nonnm[,1]<-roc.plot.cfm.nonnm.series$plot.data[,1,1]
    contingency.table.matrix.cfm.nonnm[,6]<-roc.plot.cfm.nonnm.series$plot.data[,2,1]
    contingency.table.matrix.cfm.nonnm[,7]<-roc.plot.cfm.nonnm.series$plot.data[,3,1]
    values.odserved<-data.table[,2]
    values.predicted<-result.cfm.nonnm$fitted.values
    for (threshold.series in 1:dim(roc.plot.cfm.nonnm.series$plot.data)[1])
        {
        value.threshold<-contingency.table.matrix.cfm.nonnm[threshold.series,1]
        values.probability.reclassified<-NULL
        values.probability.reclassified<-numeric(length=length(values.odserved))

        for (length.observed.series in 1:length(values.odserved))
          {
          if (values.predicted[length.observed.series] > value.threshold)
             {
             values.probability.reclassified[length.observed.series]<-1
             } else
             {
             values.probability.reclassified[length.observed.series]<-0
             }
          }
          #sum(values.probability.reclassified-round(values.predicted)) # Check sum: It has to be 0 if threshold is equal to 1
          series.pasted<-paste(values.odserved,values.probability.reclassified,sep="")
          series.pasted<-gsub("00","1",series.pasted)
          series.pasted<-gsub("01","2",series.pasted)
          series.pasted<-gsub("10","3",series.pasted)
          series.pasted<-gsub("11","4",series.pasted)
          series.pasted<-as.numeric(series.pasted)

          TP<-length(series.pasted[series.pasted>=4])                   # True Positive
          FN<-length(series.pasted[series.pasted>=3 & series.pasted<4]) # False Negative
          FP<-length(series.pasted[series.pasted>=2 & series.pasted<3]) # False Positive
          TN<-length(series.pasted[series.pasted>=1 & series.pasted<2]) # True Negative

          #TPR<-TP/(TP+FN) # Hit Rate or True Positive Rate or Sensitivity - Assigned before the for cicle using rocplot data
          #FPR<-FP/(FP+TN) # False Alarm Rate or False Positive Rate or 1-Specificity

          # Cohen's Kappa = (agreement-chance)/(1-chance)  where agreement=(TP+TN)/(TP+TN+FP+FN) and chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
          agreement=(TP+TN)/(TP+TN+FP+FN)
          chance=((((TN+FN)*(TN+FP))/(TP+TN+FP+FN))+(((TP+FP)*(TP+FN))/(TP+TN+FP+FN)))/(TP+TN+FP+FN)
          cohen.kappa.value<-(agreement-chance)/(1-chance)
          #Other
          #library(vcd)
          #cohen.kappa.value<-Kappa(cross.classification.table)
          contingency.table.matrix.cfm.nonnm[threshold.series,2]<-TP
          contingency.table.matrix.cfm.nonnm[threshold.series,3]<-TN
          contingency.table.matrix.cfm.nonnm[threshold.series,4]<-FP
          contingency.table.matrix.cfm.nonnm[threshold.series,5]<-FN
          contingency.table.matrix.cfm.nonnm[threshold.series,8]<-cohen.kappa.value

        }

    windows()
    plot(roc.plot.cfm.nonnm.series$plot.data[,1,1],roc.plot.cfm.nonnm.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="CFM NoNNM MODEL EVALUATION PLOT")
    points(roc.plot.cfm.nonnm.series$plot.data[,1,1],1-roc.plot.cfm.nonnm.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
    points(roc.plot.cfm.nonnm.series$plot.data[,1,1], contingency.table.matrix.cfm.nonnm[,8],col="blue",pch=1,cex=0.6)
    mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
    mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
    mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
    pdf(file = "result_CFM_NoNNM_ModelEvaluationPlot.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    plot(roc.plot.cfm.nonnm.series$plot.data[,1,1],roc.plot.cfm.nonnm.series$plot.data[,2,1],type="p",pch=1,cex=0.6,col="red",xlim=c(0,1),ylim=c(0,1),xlab="Probability threshold",ylab="Evaluation parameter", main="CFM NoNNM MODEL EVALUATION PLOT")
    points(roc.plot.cfm.nonnm.series$plot.data[,1,1],1-roc.plot.cfm.nonnm.series$plot.data[,3,1],col="dark green",pch=1,cex=0.6)
    points(roc.plot.cfm.nonnm.series$plot.data[,1,1], contingency.table.matrix.cfm.nonnm[,8],col="blue",pch=1,cex=0.6)
    mtext("SENSITIVITY",side=3, padj=-0.5, adj=0.01, col="red",cex=0.8)
    mtext("COHEN'S KAPPA",side=3, padj=-0.5, adj=0.5, col="blue",cex=0.8)
    mtext("SPECIFICITY",side=3, padj=-0.5, adj=0.99, col="dark green",cex=0.8)
    dev.off()

    ## VALIDATION OF CFM NO NNM MODEL (Matching CFM NO NNM posterior probability results and validation grouping variable)
    cross.classification.temporal.validation.cfm.nonnm<-table(validation.grouping.variable,round(result.cfm.nonnm$fitted.values),dnn=c("Observed","Predicted"))
    rownames(cross.classification.temporal.validation.cfm.nonnm)<-list("No Landslide","Landslide")  #Observed
    colnames(cross.classification.temporal.validation.cfm.nonnm)<-list("No Landslide","Landslide")  #Predicted
    #str(cross.classification.temporal.validation.cfm.nonnm)

    #Elaboration of Coefficient of association for contingency table
    #load package (vcd)
    library(vcd)

    #help(package=vcd)
    contingency.table.temporal.validation.cfm.nonnm<-table2d_summary(cross.classification.temporal.validation.cfm.nonnm)
    test.table.temporal.validation.cfm.nonnm<-assocstats(cross.classification.temporal.validation.cfm.nonnm)

    #Different plots for contingency table
    windows()
    fourfold(cross.classification.temporal.validation.cfm.nonnm, std="margin", main="TEMPORAL VALIDATION CFM NO NNM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255), rgb(170,135,210,max=255), rgb(115,70,155,max=255)))

    #Receiver Operating Characteristic (ROC) plots for one or more models.
    #load package (verification)
    library(verification)

    # 2nd method using verify function
    verification.temporal.validation.cfm.nonnm<-verify(validation.table[,2],result.cfm.nonnm$fitted.values, frcst.type="prob", obs.type="binary")
    #summary(verification.temporal.validation.cfm.nonnm)

    # showing confidence intervals.  MAY BE SLOW
    area.under.roc.curve.temporal.validation.cfm.nonnm<-roc.area(validation.table[,2],result.cfm.nonnm$fitted.values)
    windows()
    roc.plot(verification.temporal.validation.cfm.nonnm, main = "ROC PLOT: TEMPORAL VALIDATION CFM NO NNM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[6] , alpha = 0.05, extra=TRUE, legend=TRUE)
    mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.cfm.nonnm$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.cfm.nonnm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[6], sep=""), side=3, col="red", cex=0.8)


    # EXPORT OF PLOT FOR VALIDATION OF CFM NO NNM MODEL

    pdf(file = "result_CFM_NoNNM_FourfoldPlot_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    fourfold(cross.classification.temporal.validation.cfm.nonnm, std="margin", main="TEMPORAL VALIDATION CFM NO NNM MODEL", extended=TRUE, space = 0.2, margin=1, color = c(rgb(255,0,0,max=255), rgb(255,128,0,max=255), rgb(56,168,0,max=255), rgb(170,255,0,max=255),  rgb(170,135,210,max=255), rgb(115,70,155,max=255)))
    dev.off()

    #pdf(file = "result_CFM_NoNNM_ROCPlot_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    #roc.plot(verification.temporal.validation.cfm.nonnm, main = "ROC PLOT: TEMPORAL VALIDATION CFM NO NNM MODEL", binormal = TRUE, plot = "both", extra=TRUE, legend=TRUE)
    #area.under.roc.curve.temporal.validation.cfm.nonnm<-roc.area(verification.table[,2],result.cfm.nonnm$fitted.values)
    #dev.off()

    pdf(file = "result_CFM_NoNNM_ROCPlot_bootstrap_Temporal_Validation.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
    roc.plot(verification.temporal.validation.cfm.nonnm, main = "ROC PLOT: TEMPORAL VALIDATION CFM NO NNM MODEL", binormal=TRUE, plot="both", CI=TRUE, n.boot=bootstrap.sample.values[6] , alpha = 0.05, extra=TRUE, legend=TRUE)
    mtext(paste("ROC area = ",round(area.under.roc.curve.temporal.validation.cfm.nonnm$A,2),";  Sample size = ",area.under.roc.curve.temporal.validation.cfm.nonnm$n.total,";  Bootstrap samples = ",bootstrap.sample.values[6], sep=""), side=3, col="red", cex=0.8)
    dev.off()

    # Assignation of a matching code between observed and predicted values calculated using the validation dataset
    validation.cfm.nonnm.matching.code<-paste(validation.grouping.variable,round(result.cfm.nonnm$fitted.values),sep="")
    validation.cfm.nonnm.matching.code<-gsub("00","1",validation.cfm.nonnm.matching.code)
    validation.cfm.nonnm.matching.code<-gsub("01","2",validation.cfm.nonnm.matching.code)
    validation.cfm.nonnm.matching.code<-gsub("10","3",validation.cfm.nonnm.matching.code)
    validation.cfm.nonnm.matching.code<-gsub("11","4",validation.cfm.nonnm.matching.code)
    validation.cfm.nonnm.matching.code<-as.numeric(validation.cfm.nonnm.matching.code)

    
    
    # EXPORT OF CFM NO NNM MODEL RESULTS

    write.table("RESULTS OF COMBINATION FORECAST LOGISTIC REGRESSION MODEL WITHOUT NNM",file="result_CFM_NoNNM.txt", quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("CFM MODEL OUTPUTS",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("Logistic Regression coefficients",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    #Scaling coefficients
    write.table(cbind(names(result.cfm.nonnm$coefficients),result.cfm.nonnm$coefficients),file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("CONTINGENCY TABLE MODEL RESULT",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.cfm.nonnm$table[,1,],contingency.table.cfm.nonnm$table[,2,],contingency.table.cfm.nonnm$table[,3,])),file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("CONTINGENCY TABLE VALIDATION",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(rbind(c("","No Landslide Predicted","Landslide Predicted","Total"),cbind(c("No Landslide Observed","Landslide Observed","Total"),contingency.table.temporal.validation.cfm.nonnm$table[,1,],contingency.table.temporal.validation.cfm.nonnm$table[,2,],contingency.table.temporal.validation.cfm.nonnm$table[,3,])),file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("MATCHING CODE DEFINITION",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(cbind(c("","OBSERVED NO LANDSLIDES: 0","OBSERVED LANDSLIDES: 1"), c("PREDICTED NO LANDSLIDES: 0","00 -> Code 1","10 -> Code 3"), c("PREDICTED LANDSLIDES: 1","01 -> Code 2","11 -> Code 4")),file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table("FINAL RESULTS",file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    write.table(rbind(c("ID","GROUPING VARIABLE","MODEL POSTERIOR PROBABILITY","MODEL CLASSIFICATION","MODEL RESULT MATCHING CODE","VALIDATION GROUPING VARIABLE","VALIDATION MATCHING CODE"),cbind(identification.value,result.cfm.nonnm$y,result.cfm.nonnm$fitted.values,round(result.cfm.nonnm$fitted.values),result.cfm.nonnm.matching.code,as.numeric(levels(validation.grouping.variable))[validation.grouping.variable],validation.cfm.nonnm.matching.code)),file="result_CFM_NoNNM.txt", append=TRUE, quote = FALSE,sep = "\t", row.names=FALSE, col.names=FALSE)
    }
    
    

#------------------- MODEL PROBABILITY COMPARISON --------------------#

### LDA - Other models
if(model.run.matrix[1] == "YES")
  {
  # LDA - QDA
  #windows()
  #plot(predict.result.lda$posterior[,2],predict.result.qda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LDA Model Probability",ylab="QDA Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_LDA_QDA.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.lda$posterior[,2],predict.result.qda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LDA Model Probability",ylab="QDA Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  # LDA - LRM
  #windows()
  #plot(predict.result.lda$posterior[,2],result.lrm$fitted.values,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LDA Model Probability",ylab="LRM Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_LDA_LRM.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.lda$posterior[,2],result.lrm$fitted.values,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LDA Model Probability",ylab="LRM Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  # LDA - NNM
  #windows()
  #plot(predict.result.lda$posterior[,2],predict.result.nnm,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LDA Model Probability",ylab="NNM Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_LDA_NNM.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.lda$posterior[,2],predict.result.nnm,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LDA Model Probability",ylab="NNM Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  }

if(model.run.matrix[2] == "YES")
  {
  # QDA - LDA
  #windows()
  #plot(predict.result.qda$posterior[,2],predict.result.lda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="QDA Model Probability",ylab="LDA Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_QDA_LDA.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.qda$posterior[,2],predict.result.lda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="QDA Model Probability",ylab="LDA Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  # QDA - LRM
  #windows()
  #plot(predict.result.qda$posterior[,2],result.lrm$fitted.values,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="QDA Model Probability",ylab="LRM Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_QDA_LRM.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.qda$posterior[,2],result.lrm$fitted.values,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="QDA Model Probability",ylab="LRM Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  # QDA - NNM
  #windows()
  #plot(predict.result.qda$posterior[,2],predict.result.nnm,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="QDA Model Probability",ylab="NNM Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_QDA_NNM.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.qda$posterior[,2],predict.result.nnm,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="QDA Model Probability",ylab="NNM Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  }
  
if(model.run.matrix[3] == "YES")
  {
  # LRM - LDA
  #windows()
  #plot(result.lrm$fitted.values,predict.result.lda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LRM Model Probability",ylab="LDA Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_LRM_LDA.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(result.lrm$fitted.values,predict.result.lda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LRM Model Probability",ylab="LDA Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  # LRM - QDA
  #windows()
  #plot(result.lrm$fitted.values,predict.result.qda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LRM Model Probability",ylab="QDA Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_LRM_QDA.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(result.lrm$fitted.values,predict.result.qda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LRM Model Probability",ylab="QDA Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  # LRM - NNM
  #windows()
  #plot(result.lrm$fitted.values,predict.result.nnm,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LRM Model Probability",ylab="NNM Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_LRM_NNM.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(result.lrm$fitted.values,predict.result.nnm,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="LRM Model Probability",ylab="NNM Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  }
  
if(model.run.matrix[4] == "YES")
  {
  # NNM - LDA
  #windows()
  #plot(predict.result.nnm,predict.result.lda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="NNM Model Probability",ylab="LDA Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_NNM_LDA.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.nnm,predict.result.lda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="NNM Model Probability",ylab="LDA Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  # NNM - QDA
  #windows()
  #plot(predict.result.nnm,predict.result.qda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="NNM Model Probability",ylab="QDA Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_NNM_QDA.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.nnm,predict.result.qda$posterior[,2],type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="NNM Model Probability",ylab="QDA Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  # NNM - LRM
  #windows()
  #plot(predict.result.nnm,result.lrm$fitted.values,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="NNM Model Probability",ylab="LRM Model Probability", main="MODEL COMPARISON")
  #abline(a=0,b=1,col="red",lty=1,lwd=1)
  pdf(file = "result_ModelComparison_NNM_LRM.pdf", width = 6, height = 6, onefile = TRUE, family = "Helvetica", fonts = NULL, version = "1.1", paper = "special", pagecentre=TRUE)
  plot(predict.result.nnm,result.lrm$fitted.values,type="p",pch=1,cex=0.85,col="dark blue",xlim=c(0,1),ylim=c(0,1),xlab="NNM Model Probability",ylab="LRM Model Probability", main="MODEL COMPARISON")
  abline(a=0,b=1,col="red",lty=1,lwd=1)
  dev.off()
  }