# TO DO
# tide number
# what to do with the NAs layind dates

# INFO 
# R Script generates statistical outputs for snowy plover nest initiation cycles

{# SETTINGS & DATA
	{# do you want plots within R (PNG=FALSE) or as PNG (PNG = TRUE)?
		PNG=FALSE
		#PNG = TRUE
	}
	{# define working directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/SnowyPlover_cycles/"
		 outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/SnowyPlover_cycles/Output/" 
	}
	{# load packages, constants and data
		source(paste(wd, 'Constants_Functions.R',sep=""))
	}
}

{# LOAD DATA
	n = read.csv(file=paste(wd, "metadata_nestID_parentsID.csv", sep=''),header=T,sep=";", fill=T, stringsAsFactors=FALSE, col.names=c('year','nest','found','laying','end','fate','lat','lon','male','female'))
		n$fate=tolower(n$fate)
		n$lat=gsub(",", ".", n$lat)
		n$lon=gsub(",", ".",n$lon)
		
	g = read.csv(file=paste(wd, "moonsequ_tidesequ_cond_datetime.csv", sep=''),header=T,sep=";", fill=T, stringsAsFactors=FALSE)
	
	d<-read.csv(file=paste(wd, "CeutaData.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
	colnames(d) = tolower(colnames(d))
	d = d[!is.na(d$ld),]
	d$lat = as.numeric(n$lat[match(d$id,n$nest)])
	d$lon = as.numeric(n$lon[match(d$id,n$nest)])
	# add tide count
	#s = ddply(d[is.n.,(
}
 
 
{# distributions
	str(d)
	ggplot(d, aes(x=ldy, fill=factor(year))) + geom_histogram(alpha  = 0.5, position = 'dodge')
	ggplot(d, aes(x=ldy, fill=factor(year))) + geom_density(alpha  = 0.5)
	ggplot(d, aes(x=ldy)) + geom_density(alpha  = 0.5)
	
	ggplot(d, aes(x=dast, fill=factor(year))) + geom_density(alpha  = 0.5)
	ggplot(d, aes(x=dast, fill=factor(f))) + geom_density(alpha  = 0.5)
	ggplot(d, aes(x=dast)) + geom_density(alpha  = 0.5)
		
}

{# flooding
	d$flooded = ifelse(d$f=='FLOOD',1,0)
	#d$rad_st= 2*d$hour*pi/24
	d$rad_st= 2*d$dast*pi/14.75
	
	
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year), family = 'binomial', d) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year), family = 'binomial', d) 
	
	plot(allEffects(m))
	summary(glht(m))
	
	{# predictions
			m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year), family = 'binomial', d) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(dast = seq(0,15, length.out=300))
					newD$rad_st =2*newD$dast*pi/14.75
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ sin(rad_st)+cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-plogis(X%*%v) *100
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)*100
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)*100
					pp=newD	
	}
	{# raw data
		x = ddply(d,.(year, dast), summarise, mean_ = mean(flooded)*100, sd_ = sd(flooded)*100, n = length(year))
			y = data.frame(year = unique(d$year), col_ =1:length(unique(d$year)), stringsAsFactors = FALSE)
			x$col_ = y$col_[match(x$year, y$year)]
			
		x2 = ddply(d,.(dast), summarise, mean_ = mean(flooded)*100, sd_ = sd(flooded)*100, n = length(year))	
	}
	{# plot 0:40
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_flood.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$dast, 
						xlim=c(0,15), ylim=c(0,30),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Flooded nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(pp$dast, rev(pp$dast)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(pp$dast, pp$pred, col=col_l,lwd=1)
						#symbols( jitter(x$dast),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						symbols( jitter(x2$dast),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 

						
						# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.1, padj=-7,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(17,17,17)+1,c(26,26-4,26-4*2),circles=sqrt(c(1,30,75)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(17,17,17)+4,c(26,26-4,26-4*2),labels=c(1,30,75), xpd=NA, cex=0.5,col='grey30')													
							#text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90, xpd=FALSE) 
							
							#mtext('Predictions &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
		if(PNG == TRUE) {dev.off()}
	}			
	{# plot 0:100
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_flood_means_pre_year.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0.2,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$dast, 
						xlim=c(0,15), ylim=c(0,100),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Flooded nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						symbols( jitter(x$dast),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$dast),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=adjustcolor(col_f, alpha.f = 0.5), fg=col_p,add=TRUE) 
						
						polygon(c(pp$dast, rev(pp$dast)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pp$dast, pp$pred, col=col_m,lwd=1)
							
						if(PNG == TRUE) {dev.off()}
						
						
	}			
		{# model assumptions
			m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year), family = 'binomial', d) 
			#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
			dev.new(width=6,height=9)
			par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
			scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			qqline(resid(m))
									  
				qqnorm(unlist(ranef(m)$year [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$year [1]))
									  
				#qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
				#qqline(unlist(ranef(m)$nest_ID[2]))
			
			plot(fitted(m), jitter(d$flooded, amount=0.05), xlab="Fitted values", ylab="Flooded", las=1, cex.lab=1.2, cex=0.8)
				abline(0,1, lty=3)
				t.breaks <- cut(fitted(m), quantile(fitted(m)))
				means <- tapply(d$flooded, t.breaks, mean)
				semean <- function(x) sd(x)/sqrt(length(x))
				means.se <- tapply(d$flooded, t.breaks, semean)
				points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
				segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
										
				#scatter.smooth(resid(m)~x2$date[x2$sum>720]);abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~sin(d$rad_st));abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~cos(d$rad_st));abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~d$rad_st);abline(h=0, lty=2, col='red')
				#boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
				mtext("glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year), family = 'binomial', d)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
				acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
				spdata=data.frame(resid=resid(m), x=d$lon, y=d$lat)
				spdata=spdata[-which(is.na(spdata$y)),]
						spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
						cex_=c(1,1.5,2,2.5,3)
						spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
						
						plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
						legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
						
						plot(spdata$x[spdata$x<23.930], spdata$y[spdata$x<23.930],col=spdata$col[spdata$x<23.930], cex=as.numeric(spdata$cex[spdata$x<23.930]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
						
						plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
						plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
						dev.off()
		
		}
}
{# 

}
{# moon light
d$rad_nm= 2*d$danm*pi/29.5
}	