#open the file 
gaia.lightcurve <- read.csv(file= "data/gaia_lightcurve.csv",header=TRUE,check.names=TRUE,na.strings="null",stringsAsFactors=FALSE,sep=",")
# plot
yexp <- 0.1
my.ylimits <- c(max(gaia.lightcurve$averagemag, na.rm = TRUE)+yexp,min(gaia.lightcurve$averagemag,na.rm=TRUE)-yexp)
quartz("Gaia photometry")
plot(x=gaia.lightcurve$JD,y=gaia.lightcurve$averagemag,pch=20,col="red",
	ylim=my.ylimits,
	xlab="Julian Date",
	ylab="Gaia average magnitude",
	main= "Gaia Alert 16bao Lightcurve")
grid(col="black")