#  Author:  Tbiggs
#  Created:  2016-09-18

#  This file 
  #  1 Loads in total annual precipitation and load data from github for:
    #  Sediment traps (US)
    #  Model (AGNPS)
  # 2 Plots in a ternary diagram

# Below is for data on local drive:
#indir = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/models/"
#fname = "sed_load_traps_obs_modelled.csv"
#x = read.csv(paste0(indir,fname))

#  1 Load data to plot from github repository:
url.sed.text.traps.model = "https://raw.githubusercontent.com/tbiggsgithub/EPA_modeling_report_R_files/master/data_files/sed_load_traps_obs_modelled.csv"
x = read.csv(url.sed.text.traps.model)

x[,3:7] = x[,3:7]/1000  # Convert units to thousand tons.  Looks better on plot.

pchvec = c(20,20,2,5,8)
colvec = c("black","grey","black","black","black")
cexvec = c(2,2,1,1,1)

dev.new(height=7,width=4)
par(mfrow=c(2,1),mar=c(1,0,0,0),oma=c(4,4,2,2))

plot(x$Prcp.Lindbergh.cm,x$Obs.raw.tons,ylim=c(0,100),pch=pchvec[1],cex=2,xlab="Annual precip, cm",ylab="Sediment load, thousand tons",xaxt="n",las=1,yaxs="i")
axis(1,labels=FALSE)
points(x$Prcp.Lindbergh.cm,x$Obs.corr.tons,pch=pchvec[2],cex=2,col="grey")
points(x$Prcp.Lindbergh.cm,x$ANGPS.to.CONCEPTS.SSC.unlimited,pch=pchvec[3])
points(x$Prcp.Lindbergh.cm,x$ANGPS.to.CONCEPTS.SSC.limited,pch=pchvec[4])
points(x$Prcp.Lindbergh.cm              ,x$CONCEPTS.SSC.limited,pch=pchvec[5])

plot(x$Prcp.Lindbergh.cm,x$Obs.raw.tons,ylim=c(0,400),pch=pchvec[1],cex=2,xlab="Annual precip, cm",ylab="Sediment load, thousand tons",las=1,yaxs="i")
points(x$Prcp.Lindbergh.cm,x$Obs.corr.tons,pch=pchvec[2],cex=2,col="grey")
points(x$Prcp.Lindbergh.cm,x$ANGPS.to.CONCEPTS.SSC.unlimited,pch=pchvec[3])
points(x$Prcp.Lindbergh.cm,x$ANGPS.to.CONCEPTS.SSC.limited,pch=pchvec[4])
points(x$Prcp.Lindbergh.cm,x$CONCEPTS.SSC.limited,pch=pchvec[5])
points(max(x$Prcp.Lindbergh.cm),375,pch=pchvec[5])
text (max(x$Prcp.Lindbergh.cm)-1,375,labels="4396")

legend("topleft",c("Observed.raw","Observed.corr","AGNPS.no.limit","AGNPS.limit","CONCEPTS.limit"),pch=pchvec,col=colvec,pt.cex=cexvec)
mtext(side=2,paste("Annual Sediment Load, thousand tons"),line=3,outer=TRUE)
mtext(side=1,"Annual rainfall, cm",line=2)

### END OF FIGURE FOR REPORT
############

#  Plot all points, including outliers from CONCEPTS
plot(x$Prcp.Lindbergh.cm,x$Obs.raw.tons,ylim=c(0.01,5000),pch=pchvec[1],cex=2,xlab="Annual precip, cm",ylab="Sediment load, thousand tons",log="y")
points(x$Prcp.Lindbergh.cm,x$Obs.corr.tons,pch=pchvec[2],cex=2,col="grey")
points(x$Prcp.Lindbergh.cm,x$ANGPS.to.CONCEPTS.SSC.unlimited,pch=pchvec[3])
points(x$Prcp.Lindbergh.cm,x$ANGPS.to.CONCEPTS.SSC.limited,pch=pchvec[4])
points(x$Prcp.Lindbergh.cm,x$CONCEPTS.SSC.limited,pch=pchvec[5])

legend("topleft",c("Observed.raw","Observed.corr","AGNPS.no.limit","AGNPS.limit","CONCEPTS.no.limit"),pch=pchvec,col=colvec,pt.cex=cexvec)


