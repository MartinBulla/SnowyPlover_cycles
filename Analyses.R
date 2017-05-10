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
	d<-read.csv(file=paste(wd, "CeutaData.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
	colnames(d) = tolower(colnames(d))
	d = d[!is.na(d$ld),]
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
							symbols(c(17,17,17),c(26,26-4,26-4*2),circles=sqrt(c(1,30,75)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
														
							#text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90, xpd=FALSE) 
							
							#mtext('Predictions &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
		if(PNG == TRUE) {dev.off()}
	}			
	{# plot 0:100
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_flood.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$dast, 
						xlim=c(0,15), ylim=c(0,100),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Flooded nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						#symbols( jitter(x$dast),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						symbols( jitter(x2$dast),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=adjustcolor(col_f, alpha.f = 0.5), fg=col_p,add=TRUE) 
						
						polygon(c(pp$dast, rev(pp$dast)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pp$dast, pp$pred, col=col_m,lwd=1)
							
						
						# legend
							mtext(expression(italic('N')*' observations:'),side = 4,line=-0.3, padj=-7,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(8.5,8.5,8.5),c(3.3,2.8,2.3)-0.5,circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(8.5,8.5,8.5)+1,c(3.3,2.8,2.3)-0.5,labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
							
							text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90, xpd=FALSE) 
							
							mtext('Predictions &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
		if(PNG == TRUE) {dev.off()}
	}			
					
}	