#  Ternary plot of soil texture in soils, sediments in traps

library(vcd)

indir.survey = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/soil_texture/"
fname.SSURGO = "SSURGO_texture_for_analagous_soils.csv"
fname.survey = "napoleon_texture_LLCW_mapping_2015_in_report.csv"

indir.AMEC = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/sed_traps/from_TNERR_2010_2015/texture/"
fname.AMEC = "soil_texture_sed_traps_AMEC.csv"


#indir.trap.survey.2017 = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/sed_traps/texture_survey_2017/"
#fname.trap.survey.2017 = "texture_LLCW_sediment_traps_2017_03.csv"

x.surv.in = read.csv(paste0(indir.survey,fname.survey),stringsAsFactors = FALSE)
x.surv = x.surv.in[,c(2:length(x.surv.in[1,]))]
x.AMEC = read.csv(paste0(indir.AMEC,fname.AMEC),stringsAsFactors = FALSE)
#x.2017 = read.csv(paste0(indir.trap.survey.2017,fname.trap.survey.2017),stringsAsFactors = FALSE,row.names=1)
#x.2017.t = data.frame(t(x.2017))
#x.2017.t.totals = rowSums(x.2017.t)
#x.2017.t$SiltClay = round(x.2017.t$X1/x.2017.t.totals,3)
x.SSURGO.in = read.csv(paste0(indir.survey,fname.SSURGO))
x.SSURGO = data.frame(Geology=x.SSURGO.in$Geology,Description=x.SSURGO.in$Soil_ID,Cobble=x.SSURGO.in$Rock_Ratio,Gravel=NA,Sand=x.SSURGO.in$Sand_Ratio,Silt=x.SSURGO.in$Silt_Ratio,Clay=x.SSURGO.in$Clay_Ratio)

x.all = rbind(x.surv,x.AMEC,x.SSURGO)
index.toplot = grep("SURF|SUB|MXSB|USTRAP",x.all$Geology) # includes MX SBasin samples
x.all$Total = rowSums(x.all[,3:7])
x.all$Tfines = rowSums(x.all[,5:7]) 
x.all$Sand.norm = x.all$Sand/x.all$Tfines
x.all$Silt.norm = x.all$Silt/x.all$Tfines
x.all$Clay.norm = x.all$Clay/x.all$Tfines
x.all.H = x.all[index.toplot,]
x.all.H.simple = data.frame(Geology=x.all.H$Geology,Sand=x.all.H$Sand.norm,Silt=x.all.H$Silt.norm,Clay=x.all.H$Clay.norm,Description=x.all.H$Description)

x.all.means = aggregate(x.all.H.simple,by=list(x.all.H.simple$Geology),FUN='mean')

# Set geology label for plotting
SC.SURF.index = grep("SC.SURF",x.all.H.simple$Geology)
SC.SUB.index = grep("SC.SUB",x.all.H.simple$Geology)
CG.SURF.index = grep("CG.SURF",x.all.H.simple$Geology)
CG.SUB.index = grep("CG.SUB",x.all.H.simple$Geology)
MXSB.index = grep("MXSB",x.all.H.simple$Geology)
USTRAP.index = grep("USTRAP",x.all.H.simple$Geology)
SC.SSURGO.SURF.index = grep("SC.SSURGO.SURF",x.all.H.simple$Geology)
SC.SSURGO.SUB.index = grep("SC.SSURGO.SUB",x.all.H.simple$Geology)
SILT.SURF.index = grep("SILTST.SURF",x.all.H.simple$Geology)
SILT.SUB.index = grep("SILTST.SUB",x.all.H.simple$Geology)
CG.SSURGO.SURF.index = grep("CG.SSURGO.SURF",x.all.H.simple$Geology)
CG.SSURGO.SUB.index = grep("CG.SSURGO.SUB",x.all.H.simple$Geology)
CG.MODEL.SURF.index = grep("CG.MODEL.SURF",x.all.H.simple$Geology)
CG.MODEL.SUB.index = grep("CG.MODEL.SUB",x.all.H.simple$Geology)
SC.MODEL.SURF.index = grep("SC.MODEL.SURF",x.all.H.simple$Geology)
SC.MODEL.SUB.index = grep("SC.MODEL.SUB",x.all.H.simple$Geology)


pvec = c(19,19,15,15,0,2,10,10,13,13,8,8,8,8,7,7)
pchvec.all = rep(pvec[1],times=length(x.all.H$Geology))  # set plotting symbol type, first all to pch=1
pchvec.all[SC.SUB.index] = pvec[2]
pchvec.all[CG.SURF.index] = pvec[3]
pchvec.all[CG.SUB.index] = pvec[4]
pchvec.all[MXSB.index] = pvec[5]
pchvec.all[USTRAP.index] = pvec[6]
pchvec.all[SC.SSURGO.SURF.index] = pvec[7]
pchvec.all[SC.SSURGO.SUB.index]=pvec[8]
pchvec.all[CG.SSURGO.SURF.index] = pvec[9]
pchvec.all[CG.SSURGO.SUB.index]=pvec[10]
pchvec.all[SC.MODEL.SURF.index] = pvec[11]
pchvec.all[SC.MODEL.SUB.index]=pvec[12]
pchvec.all[CG.MODEL.SURF.index] = pvec[13]
pchvec.all[CG.MODEL.SUB.index]=pvec[14]
pchvec.all[SILT.SUB.index]=pvec[15]
pchvec.all[SILT.SURF.index]=pvec[16]


col.vec = c("black","grey","black","grey","black","black","black","grey","black","grey","red","magenta","green","orange","black","grey")
colvec.all = rep(col.vec[1],times=length(x.all.H$Geology))  # set plotting symbol type, first all to pch=1
colvec.all[SC.SUB.index] = col.vec[2]
colvec.all[CG.SURF.index] = col.vec[3]
colvec.all[CG.SUB.index] = col.vec[4]
colvec.all[MXSB.index] = col.vec[5]
colvec.all[USTRAP.index] = col.vec[6]
colvec.all[SC.SSURGO.SURF.index] = col.vec[7]
colvec.all[SC.SSURGO.SUB.index] = col.vec[8]
colvec.all[CG.SSURGO.SURF.index] = col.vec[9]
colvec.all[CG.SSURGO.SURF.index] = col.vec[10]
colvec.all[SC.MODEL.SURF.index] = col.vec[11]
colvec.all[SC.MODEL.SUB.index] = col.vec[12]
colvec.all[CG.MODEL.SURF.index] = col.vec[13]
colvec.all[CG.MODEL.SUB.index] = col.vec[14]
colvec.all[SILT.SURF.index] = col.vec[15]
colvec.all[SILT.SUB.index] = col.vec[16]

cbind(x.all.H.simple,pchvec.all,colvec.all)

legtext.vec = c("SC.SURF","SC.SUB","CG.SURF","CG.SUB","MXSB","USTRAP, AMEC","SC.SURF.SURGO","SC.SUB.SURGO","CG.SURF.SURGO","CG.SUB.SURGO","SC.SURF.MODEL","SC.SUB.MODEL","CG.SURF.MODEL","CG.SUB.MODEL","SILT.SURF","SILT.SUB")


#ternaryplot(data.frame(x.surv.H$Sand.norm,x.surv.H$Silt.norm,x.surv.H$Clay.norm),pch=pchvec.surv,col="black",main="",dimnames=c("Sand","Silt","Clay"))
ternaryplot(data.frame(x.all.H.simple$Sand,x.all.H.simple$Silt,x.all.H.simple$Clay),pch=pchvec.all,col=colvec.all,main="",dimnames=c("Sand","Silt","Clay"))
grid_legend(0.75,0.75,pch=pvec,col=col.vec,labels=legtext.vec,gp=gpar(fontsize=8),vgap=unit(0.5,"lines"))

# Plot with SC only
x.all.H.simple.SC = x.all.H.simple[grep("SC|MXSB|USTRAP|SILT",x.all.H.simple$Geology),]
colvec.all.SC = colvec.all[grep("SC|MXSB|USTRAP|SILT",x.all.H.simple$Geology)]
pchvec.all.SC = pchvec.all[grep("SC|MXSB|USTRAP|SILT",x.all.H.simple$Geology)]

pvec.SC = pvec[c(1,2,5,6,7,8,11,12,15,16)]
colvec.SC = col.vec[c(1,2,5,6,7,8,11,12,15,16)]
legtext.vec.SC=legtext.vec[c(1,2,5,6,7,8,11,12,15,16)]

ternaryplot(data.frame(x.all.H.simple.SC$Sand,x.all.H.simple.SC$Silt,x.all.H.simple.SC$Clay),pch=pchvec.all.SC,col=colvec.all.SC,main="",dimnames=c("Sand","Silt","Clay"))
#grid_legend(0.75,0.75,pch=pvec.SC,col=colvec.SC,labels=legtext.vec.SC)
grid_legend(0.75,0.75,pch=pvec.SC,col=colvec.SC,labels=legtext.vec.SC,gp=gpar(fontsize=8),vgap=unit(0.5,"lines"))


# Plot with CG only
x.all.H.simple.CG = x.all.H.simple[grep("CG|MXSB|USTRAP",x.all.H.simple$Geology),]
colvec.all.CG = colvec.all[grep("CG|MXSB|USTRAP",x.all.H.simple$Geology)]
pchvec.all.CG = pchvec.all[grep("CG|MXSB|USTRAP",x.all.H.simple$Geology)]

pvec.CG = pvec[c(3,4,5,6,9,10,13,14)]
colvec.CG = col.vec[c(3,4,5,6,9,10,13,14)]
legtext.vec.CG=legtext.vec[c(3,4,5,6,9,10,13,14)]

ternaryplot(data.frame(x.all.H.simple.CG$Sand,x.all.H.simple.CG$Silt,x.all.H.simple.CG$Clay),pch=pchvec.all.CG,col=colvec.all.CG,main="",dimnames=c("Sand","Silt","Clay"))
grid_legend(0.75,0.75,pch=pvec.CG,col=colvec.CG,labels=legtext.vec.CG)


