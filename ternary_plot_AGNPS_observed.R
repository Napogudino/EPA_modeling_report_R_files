
library(vcd)

indir = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/models/"
fname = "sed_load_by_texture_AGNPS_observed.csv"

x = read.csv(paste0(indir,fname))

tots = rowSums(x[,2:4])

x.norm = x[,2:4]/tots

N.agnps = length(grep("AGNPS",x$Type))
N.obs = length(grep("Corrected.",x$Type))

pchvec = c(rep(20,times=N.agnps),rep(2,times=N.obs),rep(3,times=1))


ternaryplot(x.norm,pch=pchvec,main="",col="black")
legend("topright",c("AGNPS","Observed.corrected","Observed.uncorrected"),pch=c(20,2,3),col="black")


#  Read in and plot texture from the SSURGO soils (as currently in AGNPS) and from 
#  SDSU-CICESE soil survey in 2015

indir.survey = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/soil_texture/"
fname.survey = "napoleon_texture_LLCW_mapping_2015_in_report.csv"

fname.AGNPS = "AGNPS_soil_textures.csv"

# Load and format survey data
x.surv = read.csv(paste0(indir.survey,fname.survey))
index.hillslopes = grep("\\.H",x.surv$Geologya)
x.surv$Total = rowSums(x.surv[,3:7])
x.surv$Tfines = rowSums(x.surv[,5:7]) 
x.surv$Sand.norm = x.surv$Sand/x.surv$Tfines
x.surv$Silt.norm = x.surv$Silt/x.surv$Tfines
x.surv$Clay.norm = x.surv$Clay/x.surv$Tfines
x.surv.H = x.surv[index.hillslopes,]  # Hillslopes only
x.surv.H.simple = data.frame(Geology=paste0(x.surv.H$Geologya,".surv"),Sand=x.surv.H$Sand.norm,Silt=x.surv.H$Silt.norm,Clay=x.surv.H$Clay.norm)


#  Load and format AGNPS data
x.AGNPS = read.csv(paste0(indir.survey,fname.AGNPS))
agnps.tot = rowSums(x.AGNPS[,4:6])
x.AGNPS.simple = data.frame(Geology=paste0(x.AGNPS$Geology,".agnps"),Sand=x.AGNPS$Sand,Silt=x.AGNPS$Silt,Clay=x.AGNPS$Clay)

x.all = rbind(x.surv.H.simple,x.AGNPS.simple)

#  Plot with just survey data
#  Use different plotting symbol for SC and CG.
SC.index = grep("SC",x.surv.H$Geologya)
CG.index = grep("CG",x.surv.H$Geologya)
pchvec.surv = rep(20,times=length(x.surv.H$Geologya))
pchvec.surv[CG.index] = 22

ternaryplot(data.frame(x.surv.H$Sand.norm,x.surv.H$Silt.norm,x.surv.H$Clay.norm),pch=pchvec.surv,col="black")

## Plot both survey and AGNPS data
SC.surv.index = grep("SC.H.surv",x.all$Geology)
CG.surv.index = grep("CG.H.surv",x.all$Geology)
SC.AGNPS.index = grep("SC.agnps",x.all$Geology)
CG.AGNPS.index = grep("CG.agnps",x.all$Geology)

pchvec.surv = rep(0,times=length(x.surv.H$Geologya))
pchvec.surv[CG.surv.index] = 1
pchvec.surv[SC.AGNPS.index] = 15
pchvec.surv[CG.AGNPS.index] = 16

ternaryplot(data.frame(Sand=x.all$Sand,Silt=x.all$Silt,Clay=x.all$Clay),pch=pchvec.surv,col="black",main="")
grid_legend(0.7,0.7,pch=c(0,1,15,16),col="black",labels=c("SC.survey","CG.survey","SC.AGNPS","CG.AGNPS"))

