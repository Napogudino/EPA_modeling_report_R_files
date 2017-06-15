# Run AGNPS.exe from R, process data

# q for napo:
   #  Why are the runs so large?  Why so many PGs?
   #  Reasonable ranges for parameters?  Crit shear stress by soil type (egully.csv)
   # Shouldn't most of SB's soil types be "CbBD" for Disturbed?  celldata doesn't show that.
   #  Can't assume soil properties (Crit shear stress) is same for all soil types.  Currently it's the same for all.
    # Reasonalbe starting value?  Which soil types are the true conglomerate that doesn't form gullies?

#  Manning n sheetflow:  set to 0.03, "rough bare packed soil" and "suburban residential" 
  #  http://www.projectcleanwater.org/images/mannings_n_trwe_summary.pdf


#  Number of model runs: others have used 9x(N parameters) (Melching 2001, p 411)
#install.packages("lhs")
library(lhs)

#  Note: there cannot be any blank values in columns with numerical values, including parameters that will not be altered.
#  Blanks come up as NA values in numerical columns, which appears to override the na="" option in write.table.

indir.base = "C:/Users/Napoleon/Documents/TRW/R/sensitivity_analysis/AnnAGNPS_2016/"
indir.base = "G:/large_datasets/tijuana/los_laureles_canyon/AnnAGNPS_2016/"

indir.model = "san_bernardo"

#indir.reps = "lhsCN"  # Name for folder of given parameter selection trial            ######################
indir.reps = "lhsKd"  # Name for folder of given parameter selection trial

options(scipen=999)

setwd(paste0(indir.base,indir.model))  # Puts you in the directory of the model to run

input.files.wshed  = c("celldata.csv","egully.csv","rchdata.csv","rocurve.csv")        ####################

params.to.test = rbind(c("CN","general/rocurve.csv",0.5,1.5,"NA"),  # x,y are the min and max possible values for the multiplier.
                      c("Hdct_Erodibility_Coef_a","simulation/globfac.csv",0.15,1.75,"n"),
                      c("Saturated_Conductivity","general/soil_layers.csv",0.1,10,"y"),
                      c("Critical_Sheer_Stress","watershed/egully.csv",0.04,4,"y"),
                      c("Sheet_Flow_Mannings_n","watershed/celldata.csv",0.2,20,"y"),
                      c("Operation_Tillage_Depth","general/manoper.csv",0.5,4,"n"))
#  C-factor (internal calculated)?  Erodibility (internally calculated--vary those internal parameters?)
  # Check erodibility against model prediction
  # Width function?  Default is Wells eq8
  # CN--routine translates to Smax values, applies multiplier, transforms back to CNs.

#  
params.df = data.frame(Param=params.to.test[,1],dir.fname=as.character(params.to.test[,2]),min=as.numeric(params.to.test[,3]),max=as.numeric(params.to.test[,4]),logyn=params.to.test[,5])
files.to.load = as.character(unique(params.df$dir.fname))

files.to.copy = c("annagnps_master.csv","AnnAGNPS_SIM_Ephemeral_Gully_Repair_Date.csv","AnnAGNPS.fil","AnnAGNPS.exe")

folders.to.copy = list.dirs(paste0(indir.base,indir.model,"/original/"),full.names=FALSE,recursive=FALSE)
folders.to.copy.fullpath = paste0(indir.base,indir.model,"/original/",folders.to.copy)

#  Generate LHS grid.  For the # of variables in AGNPS
Nbins = 15  # number of bins for the LHS algorithm. Often set to 4/3(M) where M is number of parameters
xlhs = improvedLHS(Nbins,length(params.df$Param))

# Determine multipliers for all parameters from the xlhs matrix
mults.raw = matrix(NA,nrow=length(xlhs[,1]),ncol=length(xlhs[1,]))
for (p in 1:length(params.df[,1])){
  if (params.df[p,"logyn"]=="y"){  # variables with a log range of values (Ksat, Tau crit) need to be sampled in log space
    mult.log = log(params.df[p,"min"])+(log(params.df[p,"max"])-log(params.df[p,"min"]))*xlhs[,p]  # for non-log parameters
    mults.raw[,p] = exp(mult.log)
  } else {
    mults.raw[,p] = params.df[p,"min"]+(params.df[p,"max"]-params.df[p,"min"])*xlhs[,p]
  }
}

mults.raw.df = data.frame(round(mults.raw,3))
names(mults.raw.df) = params.df$Param
#mults.raw[,4]= 0.7

randomize = 1  #  If randomize=0, take the output from the LHS sampling directly. Nmodel runs = Nbins
              #  If randomize=1, resample the LHS output (mult.raw) N times.  Nmodel runs = N
if (randomize==0) {
  N = length(mults.raw[,1])  # Number of models runs (=Nbin for no randomization)
  mults=mults.raw
} else {
  N = 15  # Number of model runs to generate                         ##################
  mults = matrix(NA,nrow=N,ncol=length(params.df[,1]))
  mults.index = matrix(NA,nrow=N,ncol=length(params.df[,1]))
  for (p in 1:length(params.df[,1])){
    mults.index[,p] = sample(1:Nbins,N,replace=TRUE)
    mults[,p] = mults.raw[mults.index[,p],p]
  }
}

library(stringr)
model.num.pad = str_pad(seq(1,N), 3, pad = "0")

newruns.name = paste0(rep("model",times=N),model.num.pad)

#  Create the directory for the run and copy all files needed for the AGNPS run into the new directory
setwd(paste0(indir.base,indir.model))
dir.create(indir.reps)

#  LOOPS START HERE
#  Creates new directories for each model, copies all files over

for (i in 1:N){
setwd(paste0(indir.base,indir.model,"/",indir.reps))
newrun.name = paste0(newruns.name[i])
dir.create(newrun.name)
setwd(newrun.name)

# Copy all base files from original model to the new model
file.copy(from=paste0(indir.base,indir.model,"/original/",files.to.copy),to=paste0(indir.base,indir.model,"/",indir.reps,"/",newrun.name))
#### Copy model folders and files to new folders
setwd(paste0(indir.base,indir.model,"/",indir.reps,"/",newrun.name))
# Copy original model folders and files to the new model folder
for (k in 1:length(folders.to.copy)){
  dir.create(folders.to.copy[k])
  file.copy(from=list.files(folders.to.copy.fullpath[k],full.names=TRUE),to=folders.to.copy[k])
}

#  Loop through all parameters, get new values from the lhs matrix and params.df, assign to model parameter files
for (p in 1:length(params.df$Param)){
  param = as.character(params.df[p,"Param"])
  #file.to.load = as.character(params.df[i,"dir.fname"])
  x = read.csv(paste0(indir.base,indir.model,"/original/",as.character(params.df[p,"dir.fname"])),stringsAsFactors=FALSE)
  #  R reads in "T" as "TRUE" and makes it a binary class.
  # Here, need to convert "TRUE" to "T".
  x[,sapply(x,class) == "logical"] <-
    sapply(x[,sapply(x,class) == "logical"],
           function(m) substr(as.character(m),1,1))
  file.colnames = names(x)
  param.to.test = as.character(params.df[p,"Param"])  # Get list of parameters to test for the file.
  index.cols.param = grep(param.to.test,file.colnames)
  #  Calculate the multiplier for the given parameter and model #:
  
  #  NEXT:  randomly select parameter index value, generates more models
    # mult = mults[rand]
  mult = mults[i,p]  # for non-log parameters
  
  #  Calculate new values and write back to the file
  if (param.to.test=="CN"){  # CN is special, since have to convert CN to Smax, apply the mult factor, and then convert back to CN
    Smax = (1000/x[,index.cols.param])-10
    Smax.new = mult*Smax
    var.new = round(1000/(Smax.new+10),1)  # New CN
  } else {
    var.new = x[,index.cols.param]*mult
  }
  x.new = x
  x.new[,index.cols.param] = var.new
  #  If the new value is <min or >max for that parameter, set it to the min or max.
  #for (j in 1:length(index.cols.param)){
  #  x.new[x.new[,index.cols.param[j]]>params.df[i,"max"],index.cols.param[j]] = params.df[i,"max"]
  #}
  
  write.csv(x.new,file=paste0(indir.base,indir.model,"/",indir.reps,"/",newrun.name,"/",params.df$dir.fname[p]),row.names=FALSE,quote=FALSE,na="")
}  # end loop for parameters
}  # end loop for models

##### RUN THE MODELS
# Get list of model directories
mfl = paste0(indir.base,indir.model,"/",indir.reps,"/",newruns.name)
# i=1
for (i in 1:length(mfl)){
  setwd(mfl[i])
  system("AnnAGNPS.exe")
}

#  Write mult.raw and mult to files so you can read it in later.
mfl.to.write = paste0(indir.base,indir.model,"/",indir.reps,"/")
write.csv(mults.raw.df,file=paste0(mfl.to.write,"mult.raw.csv"))
write.csv(params.df,file=paste0(mfl.to.write,"params.df.csv"))
write.csv(mults,file=paste0(mfl.to.write,"mults.csv"))





