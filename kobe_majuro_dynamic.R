# History: 2023-08 Rob Scott created
#          2025-09 Arni Magnusson added table

library(FLR4MFCL)

# Read dynamic MSY model results
reppath <- "dynamic_msy"
repnames <- paste0("plot-", seq(4,280,by=4), "_dyn_msy.par.rep")
ffmsy <- NULL
sbsbmsy <- NULL
for(rr in 1:length(repnames))
{
  message(rr)
  rep <- read.MFCLRep(paste(reppath, repnames[rr], sep="/"))
  ffmsy <- c(ffmsy, 1/Fmult(rep))
  sbsbmsy <- seasonMeans(ABBMSY_ts(rep)[,"2021"])
}
npoints <- length(ffmsy)

# Majuro
xmax <- 1.0
ymax <- 1.5
png("yft_majuro_dyn.png", width=600, height=500)
plot(seq(0,xmax,length=10), seq(0,ymax,length=10), axes=FALSE, type="n",
     xlab=expression("SB"/"SB"["F=0"]), ylab=expression("F"/"F"["MSY"]))
axis(1, at=c(0,0.2,0.4,0.6,0.8,1),
     labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), cex.axis=1.25, lwd=2)
axis(2, las=1, cex.axis=1.25, lwd=2)
polygon(c(0,0.2,0.2,0,0), c(0,0,ymax,ymax,0), col="firebrick3", border=NA)
polygon(c(0.2,xmax,xmax,0.2,0.2), c(1,1,ymax,ymax,1), col="darkorange1",
        border=NA)
polygon(x=c(0.2,1,1,0.2,0.2), y=c(0,0,1,1,0), col="darkseagreen2", border=NA)
lines(c(SBSBF0(rep)), rev(ffmsy))
colfunc <- colorRampPalette(c("red","yellow"))
points(c(SBSBF0(rep)), rev(ffmsy), pch=20, cex=2, col=colfunc(npoints))
points(c(SBSBF0(rep))[npoints], rev(ffmsy)[npoints], pch=20, cex=3, col="green")
dev.off()

# Kobe
xmax <- 10.0
ymax <- 1.5
png("yft_kobe_dyn.png", width=600, height=500)
plot(seq(0,xmax,length=10), seq(0,ymax,length=10), axes=FALSE, type="n",
     xlab=expression("SB"/"SB"["MSY"]), ylab=expression("F"/"F"["MSY"]))
axis(1, at=seq(0,xmax,by=2), labels=seq(0,xmax,by=2), cex.axis=1.25, lwd=2)
axis(2, las=1, cex.axis=1.25, lwd=2)
polygon(c(0,1,1,0,0),       c(1,1,ymax,ymax,1), col="firebrick3", border=NA)
polygon(c(1,xmax,xmax,1,1), c(1,1,ymax,ymax,1), col="darkorange1", border=NA)
polygon(c(0,1,1,0,0),       c(0,0,1,1,0),       col="yellow", border=NA)
polygon(c(1,xmax,xmax,1,1), c(0,0,1,1,0),       col="darkseagreen2", border=NA)
lines(c(seasonMeans(ABBMSY_ts(rep))), rev(ffmsy))
colfunc <- colorRampPalette(c("red","yellow"))
points(c(seasonMeans(ABBMSY_ts(rep))), rev(ffmsy), pch=20, cex=2,
       col=colfunc(npoints))
points(c(seasonMeans(ABBMSY_ts(rep)))[npoints], rev(ffmsy)[npoints], pch=20,
       cex=3, col="green")
dev.off()

# Construct table
kobe_majuro <- data.frame(Year=1952:2021,
                          F_Fmsy=rev(ffmsy),
                          SB_SBF0=c(SBSBF0(rep)),
                          SB_SBmsy=c(seasonMeans(ABBMSY_ts(rep))))

# Write table
write.csv(kobe_majuro, "kobe_majuro.csv", quote=FALSE, row.names=FALSE)
