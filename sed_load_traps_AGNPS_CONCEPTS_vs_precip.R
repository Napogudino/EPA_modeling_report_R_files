indir = "G:/mydocuments/SDSU/research/tijuana_watershed/los_laureles_canyon/models/"
fname = "sed_load_traps_obs_modelled.csv"

x = read.csv(paste0(indir,fname))

x[,3:7] = x[,3:7]/1000

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

#legend("topleft",c("Observed.raw","Observed.corr","AGNPS.no.limit","AGNPS.limit","CONCEPTS.no.limit"),pch=pchvec,col=colvec,pt.cex=cexvec)


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

############

plot(x$Prcp.Lindbergh.cm,x$Obs.raw.tons,ylim=c(0.01,5000),pch=pchvec[1],cex=2,xlab="Annual precip, cm",ylab="Sediment load, thousand tons",log="y")
points(x$Prcp.Lindbergh.cm,x$Obs.corr.tons,pch=pchvec[2],cex=2,col="grey")
points(x$Prcp.Lindbergh.cm,x$ANGPS.to.CONCEPTS.SSC.unlimited,pch=pchvec[3])
points(x$Prcp.Lindbergh.cm,x$ANGPS.to.CONCEPTS.SSC.limited,pch=pchvec[4])
points(x$Prcp.Lindbergh.cm,x$CONCEPTS.SSC.limited,pch=pchvec[5])

legend("topleft",c("Observed.raw","Observed.corr","AGNPS.no.limit","AGNPS.limit","CONCEPTS.no.limit"),pch=pchvec,col=colvec,pt.cex=cexvec)


