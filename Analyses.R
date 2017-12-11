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
		source(paste(wd, 'Prepare_Data.R',sep=""))
	}
}

{# FLOODING
	d = nn[!is.na(nn$fate),]
	summary(factor(d$fate))
	d$fate[d$fate=='tide'] = 'flood'
	d$flooded = ifelse(d$fate=='flood',1,0)
	d$flooded_ = ifelse(d$fate=='flood','yes','no')
	d$year_ = as.factor(d$year)

	#d$rad_st= 2*d$hour*pi/24
	d$rad_st= 2*d$days_after_st*pi/14.765 #or half a synodic month of 29.53 days 
	d$year_tc=factor(paste(d$year,d$st_cycle))
	
	ggplot(d,aes(x = days_after_st)) + geom_histogram()+facet_grid(year ~ .)
	ggplot(d[d$fate=='flood',],aes(x = days_after_st)) + geom_histogram()
	ggplot(d[d$fate=='flood',],aes(x = days_after_st)) + geom_histogram()
	ggplot(d[d$fate=='flood',],aes(x = days_after_st, col=factor(year))) + geom_density()
	ggplot(d[d$fate=='hatch',],aes(x = days_after_st, col=factor(st_cycle))) + geom_density()
	ggplot(d[d$fate%in%c('flood','hatch'),],aes(x = days_after_st, fill=fate)) + geom_histogram()+ylab('# of initiated nests') + xlab('Days after spring tide')+facet_grid(year ~ .)
	ggplot(d[d$fate%in%c('flood','hatch'),],aes(x = days_after_st, fill=fate)) + geom_histogram()+ylab('# of initiated nests') + xlab('Days after spring tide')
		ggsave(file=paste(outdir,'#of_nests_given_day_and_fate.png',sep=''))
		
	ggplot(d,aes(x = days_after_st, fill=flooded_)) + geom_histogram()+ylab('# of initiated nests') + xlab('Days after spring tide')+facet_grid(year ~ .)
	ggplot(d,aes(x = days_after_st, fill=flooded_)) + geom_histogram()+ylab('# of initiated nests') + xlab('Days after spring tide')
	
	# simple
	#m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|pair), family = 'binomial', d) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|female), family = 'binomial', d) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(sin(rad_st) + cos(rad_st)|year_)+(1|st_cycle)+(1|female), family = 'binomial', d)
				# optimize
					# fails
						ss <- getME(m,c("theta","fixef"))
						m2 <- update(m,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
					# optimizers
						# bobyqa - runs
							m3 <- update(m,start=ss,control=glmerControl(optimizer="bobyqa",
                            optCtrl=list(maxfun=2e5)))
								plot(allEffects(m3))
								summary(glht(m3))
								summary(m3)
						# nloptwrap
							m4 <- update(m,start=ss,control=glmerControl(optimizer="nloptwrap",
                            optCtrl=list(maxfun=2e5)))
								plot(allEffects(m3))
								summary(glht(m3))
								summary(m3)
						# try all optimizers
							aa <- allFit(m)
							# bobyqa, nlminbw, optimx.L-BFGS-B ok, others failed
							is.OK <- sapply(aa,is,"merMod")  
							aa.OK <- aa[is.OK]
							lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)
							# how bad was the poor convergence in some optimizers
							(lliks <- sort(sapply(aa.OK,logLik))) # very similar logLik
							aa.fixef <- t(sapply(aa.OK,fixef))
							aa.fixef.m <- melt(aa.fixef)
							models <- levels(aa.fixef.m$Var1)
							ylabs <- substr(models,1,3)
							aa.fixef.m <- transform(aa.fixef.m,Var1=factor(Var1,levels=names(lliks)))
							(gplot1 <- ggplot(aa.fixef.m,aes(x=value,y=Var1,colour=Var1))+geom_point()+
								 facet_wrap(~Var2,scale="free")+
									 scale_colour_brewer(palette="Dark2")+
										 scale_y_discrete(breaks=models,
														  labels=ylabs)+
															  labs(x="",y=""))
							
						#Coefficients of variation of fixed-effect parameter estimates:
						summary(unlist(daply(aa.fixef.m,"Var2",summarise,sd(value)/abs(mean(value)))))
						
						# variances of the random parameters
						aa.stddev <- t(sapply(aa.OK,function(x) sqrt(unlist(VarCorr(x)))))
						print(aa.stddev,digits=3) # NA issues
						
						aa.stddev.m <- melt(aa.stddev) 
						aa.stddev.m <- transform(aa.stddev.m,Var1=factor(Var1,levels=names(lliks)))
						gplot1 %+% aa.stddev.m # does not work, likely because of the NAs
						
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|pair), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|pair), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	
	# year
	d$year_= as.factor(d$year)
	m = glmer(flooded ~ sin(rad_st)*year_ + cos(rad_st)*year_ +(1|year_)+(1|st_cycle), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|st_cycle), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) + scale(st_cycle) + (1|year)+(1|pair), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
		
	plot(allEffects(m))
	summary(glht(m))
	summary(m)
	
	{# predictions
			m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year), family = 'binomial', d) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
				# values to predict for		
					newD=data.frame(days_after_st = seq(0,15, length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
						
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
		x = ddply(d,.(year, days_after_st), summarise, mean_ = mean(flooded)*100, sd_ = sd(flooded)*100, n = length(year))
			y = data.frame(year = unique(d$year), col_ =1:length(unique(d$year)), stringsAsFactors = FALSE)
			x$col_ = y$col_[match(x$year, y$year)]
			
		x2 = ddply(d,.(days_after_st), summarise, mean_ = mean(flooded)*100, sd_ = sd(flooded)*100, n = length(year))	
	}
	{# plot 0:40
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_flood_update.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$days_after_st, 
						xlim=c(0,15), ylim=c(0,30),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Flooded nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(pp$days_after_st, rev(pp$days_after_st)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(pp$days_after_st, pp$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						symbols( jitter(x2$days_after_st),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 

						
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
			png(paste(outdir,"Figure_flood_means_pre_year_update.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0.2,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$days_after_st, 
						xlim=c(0,15), ylim=c(0,100),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Flooded nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_st),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=adjustcolor(col_f, alpha.f = 0.5), fg=col_p,add=TRUE) 
						
						polygon(c(pp$days_after_st, rev(pp$days_after_st)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pp$days_after_st, pp$pred, col=col_m,lwd=1)
							
						if(PNG == TRUE) {dev.off()}
						
						
	}			
		{# model assumptions
			m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|pair), family = 'binomial', d) 
			#m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|pair), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
		
			#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
			dev.new(width=6,height=9)
			par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
			scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			qqline(resid(m))
									  
				qqnorm(unlist(ranef(m)$year [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$year [1]))
				
				qqnorm(unlist(ranef(m)$pair [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$pair [1]))
									  
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

{# NEST INITIATION CYCLE - only days with nests +  best fitted period in days
	d = nn
	d$laid_j = as.numeric(format(as.POSIXct(d$laid),"%j"))
	dd = ddply(d,. (year, moon_cycle, st_cycle, days_after_st, laid_j), summarise, n_nest = length(year))
	#dd = ddply(dd,. (year, moon_cycle, st_cycle), transform, laid_j_yc = laid_j-mean(laid_j))
	dd$rad_lst= 2*dd$laid_j*pi/14.75
	dd$rad_st= 2*dd$days_after_st*pi/14.75
	dd$rad_lm= 2*dd$laid_j*pi/(14.75*2)
	dd$rad_m= 2*dd$days_after_st*pi/(14.75*2)
	#dd$rad_st_yc= 2*dd$laid_j_yc*pi/14.75
	#dd$rad_m_yc= 2*dd$laid_j_yc*pi/(14.75*2)
	dd$year_= as.factor(dd$year)
	# center within year
		dsplit=split(dd,paste(dd$year))
		foo=lapply(dsplit,function(x) {
				#x=incsplit$"s404"
				#x$t_a=c(x$treat[-1], NA) 	
				x$n_nest_c=x$n_nest - mean(x$n_nest)
				return(x)
				}
				)
		dd=do.call(rbind, foo)
	
	# center within year and cycle
		dsplit=split(dd,paste(dd$year, dd$cycle))
		foo=lapply(dsplit,function(x) {
				#x=incsplit$"s404"
				#x$t_a=c(x$treat[-1], NA) 	
				x$n_nest_cc=x$n_nest - mean(x$n_nest)
				return(x)
				}
				)
		dd=do.call(rbind, foo)
	# illumination
	ggplot(d, aes(x = days_after_nm, y = illum_mid, col = as.factor(year))) +   geom_jitter(position = position_jitter(width = 0, height = 0.5)) +stat_smooth()+facet_grid(year ~ .)
	ggsave(file='illumination given days after new moon jitter.png')
	ggplot(d, aes(x = illum_mid, fill = as.factor(year))) + geom_histogram() +facet_grid(year ~ .)
	ggsave(file='days after new moon Year hist.png')
	ggplot(d, aes(x = illum_mid)) + geom_histogram()
	ggplot(d, aes(x = days_after_nm, fill = as.factor(year))) + geom_histogram() +facet_grid(year ~ .)
	ggsave(file='illumination midnight Year hist.png')
	
	# days after nm a st
	ggplot(d, aes(x = days_after_nm)) + geom_histogram()
	ggplot(d, aes(x = days_after_st, fill = as.factor(year))) + geom_histogram() +facet_grid(year ~ .)
	ggsave(file='days after spring tide Year hist.png')
	ggplot(d,aes(x = days_after_st)) + geom_histogram()
	ggsave(file='days after spring tide.png')
	ggplot(d,aes(x = days_after_nm)) + geom_histogram()
	ggsave(file='days after new moon.png')
	
	# laid date
	ggplot(d, aes(x = laid_j, col = as.factor(year))) + geom_density()
	ggsave(file='laid distribution over years.png')
	ggplot(d, aes(x = laid_j, col = as.factor(year))) + geom_density()+facet_grid(year ~ .)
	ggsave(file='laid distribution over years 2.png')
	
	densityplot(~dd$n_nest)
	ggplot(dd, aes(x = laid_j, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)
	ggsave(file='lay date vs # of nests.png')
	ggplot(dd, aes(x = laid_j, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .) + coord_cartesian(ylim = c(0,5))
	ggsave(file='lay date vs # of nests_zoom_.png')
	#d$rad_st= 2*d$hour*pi/24
	
	# tide hight
	ggplot(d, aes(x = days_after_st, y = max_t_h, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)
	
	# n_nest~st
	ggplot(dd, aes(x = days_after_st, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)+ylab('# of initiated nests') + xlab ('Days after last spring tide')+ coord_cartesian(ylim = c(0,5))
	ggsave(paste(outdir,'#of_nests_given_spring_tide_cycle_zoomed.png',sep=''))
	
	# laying date
	m = lmer(n_nest ~ sin(rad_lst) + cos(rad_lst) + sin(rad_lm) + cos(rad_lm) + (1|year) + (1|moon_cycle) +(1|st_cycle), dd) 
		#m = lmer(n_nest ~ sin(rad_st_yc) + cos(rad_st_yc) + sin(rad_m_yc) + cos(rad_m_yc) + (1|year) + (1|moon_cycle) +(1|st_cycle), dd) 
	m = glmer(n_nest ~ sin(rad_lst) + cos(rad_lst) + sin(rad_lm) + cos(radl_m) + (1|year) + (1|moon_cycle) +(1|st_cycle),family = 'poisson', dd) 
	
	# laying within tide cycle
 	m = lmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year) +(1|st_cycle), dd) 
	m = lmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle), dd) 
	dispersion_glmer(m) # if over 1.4 then overdispersion is serious 
		

	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year) +(1|st_cycle), family = 'poisson',dd)	
	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle),family = 'poisson', dd) 
	
		# centred within year
			m = lmer(n_nest_c ~ sin(rad_st) + cos(rad_st) + (1|st_cycle), dd) 
			m = lmer(n_nest_c ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|st_cycle), dd)
		# centred within cycle
			m = lmer(n_nest_cc ~ sin(rad_st) + cos(rad_st) + (1|year), dd) 
			m = lmer(n_nest_cc ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year), dd)	
			
	plot(allEffects(m))
	summary(glht(m))
	summary(m)

	# best fitting period in days for all and per year
		period=c(seq(7,90,by=0.5)) #period=c(0.5,0.75,1.5,seq(1,21, by=1), seq(22,26, by=0.25), seq(27,48,by=1))
		period=period[order(period)]	
		
		# run periodic regressions
			l = list()
			for(j in 1:length(unique(dd$year))){		
				 o = list()
				 for(jj in 1:length(period)){
							v=dd[dd$year==unique(dd$year)[j],] # first row removes as it contains lag 0 autocorrelation 
							v$period=period[jj]
							v$rad= (2*pi*v$laid_j) / (period[jj])
							v$sin_=sin(v$rad)
							v$cos_=cos(v$rad)
							o[[as.character(period[jj])]] = lm(n_nest ~ sin_ + cos_ ,v)
							#print(jj)
							}
			# extract AIC estimates from model summaries and create deltaAIC
				aa = data.frame(period, aic = sapply(o, AIC))
				aa$delta=aa$aic-min(aa$aic)
				aa=aa[order(aa$delta),]
			
			# add metadata
				aa$year=unique(dd$year)[j]
				aa$n_days=nrow(v)
			
			# save to a list
				l[[j]]=aa
				
			# plot and save the data
				 png(file=paste(outdir,"period_AIC/",unique(dd$year)[j],".png", sep =""), width=3, height=2.5, units = "in", res = 600)
				 	par(mar=c(3.2,3.5,2.5,0.7), ps=12, mgp=c(2,0.5,0), las=1, cex.lab=0.8, cex.axis=0.65, tcl=-0.2, cex.main=0.8)
					plot(delta~period,type="n", aa, main=paste(aa$year[1],"; # day: ", aa$n_days[1]), xlim=c(7,90), xaxt='n')#, ylim=c(0,100))
					axis(1,seq(7,90))
					#legend("topright", legend=c("simple", "interaction"), pch=c(1,16), col=c("black","grey"), pt.cex=0.7, cex=0.5)
					abline(v=c(14.75,14.75*2),lty=3)
					abline(h=3, lty=3, col="red")
					points(delta~period,aa, cex=0.7)
					dev.off()
				 print(paste(j,aa$year[1],aa$n_days[1]))
			}
			

}

{# NEST INITIATION CYCLE - all days within the season
	# prepare data
	dd$datetime_j = as.numeric(format(as.POSIXct(dd$datetime_),"%j"))
	dd$rad_lst= 2*dd$datetime_j*pi/14.75
	dd$rad_st= 2*dd$days_after_st*pi/14.75
	dd$rad_st= 2*dd$days_after_st*pi/14.75
	dd$rad_lm= 2*dd$datetime_j*pi/(14.75*2)
	dd$rad_m= 2*dd$days_after_st*pi/(14.75*2)
	#dd$rad_st_yc= 2*dd$laid_j_yc*pi/14.75
	#dd$rad_m_yc= 2*dd$laid_j_yc*pi/(14.75*2)
	dd$year_= as.factor(dd$year)
	densityplot(~dd$n_nest)
		
	# n_nest~st
	ggplot(dd, aes(x = days_after_st, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)+ylab('# of initiated nests') + xlab ('Days after last spring tide')
	ggsave(paste(outdir,'#of_nests_given_spring_tide_cycle_ALL-DAYS.png',sep=''))
	ggplot(dd, aes(x = days_after_st, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)+ylab('# of initiated nests') + xlab ('Days after last spring tide')+ coord_cartesian(ylim = c(0,5))
	ggsave(paste(outdir,'#of_nests_given_spring_tide_cycle_ALL-DAYS_zoomed.png',sep=''))
	
	# laying within tide cycle
 	m = lmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year) +(1|st_cycle), dd) 
	m = lmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle), dd) 
	dispersion_glmer(m) # if over 1.4 then overdispersion is serious 
		
	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year) +(1|st_cycle), family = 'poisson',dd)	
	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle),family = 'poisson', dd) 
				
				# optimize based on https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
				tt <- getME(m,"theta")
				ll <- getME(m,"lower")
				min(tt[ll==0]) # issue - it is close to one
				
				derivs1 <- m@optinfo$derivs
				sc_grad1 <- with(derivs1,solve(Hessian,gradient))
				max(abs(sc_grad1))
				max(pmin(abs(sc_grad1),abs(derivs1$gradient))) # it is below the tolerance (0.001)
				
				ss <- getME(m,c("theta","fixef"))
				m <- update(m,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4))) # worked
				
	plot(allEffects(m))
	summary(glht(m))
	summary(m)
	
	# best fitting period in days for all and per year
		period=c(seq(7,90,by=0.5)) #period=c(0.5,0.75,1.5,seq(1,21, by=1), seq(22,26, by=0.25), seq(27,48,by=1))
		period=period[order(period)]	
		
		# run periodic regressions
			l = list()
			for(j in 1:length(unique(dd$year))){		
				 o = list()
				 for(jj in 1:length(period)){
							v=dd[dd$year==unique(dd$year)[j],] # first row removes as it contains lag 0 autocorrelation 
							v$period=period[jj]
							v$rad= (2*pi*v$datetime_j) / (period[jj])
							v$sin_=sin(v$rad)
							v$cos_=cos(v$rad)
							o[[as.character(period[jj])]] = lm(n_nest ~ sin_ + cos_ ,v)
							#print(jj)
							}
			# extract AIC estimates from model summaries and create deltaAIC
				aa = data.frame(period, aic = sapply(o, AIC))
				aa$delta=aa$aic-min(aa$aic)
				aa=aa[order(aa$delta),]
			
			# add metadata
				aa$year=unique(dd$year)[j]
				aa$n_days=nrow(v)
			
			# save to a list
				l[[j]]=aa
				
			# plot and save the data
				 png(file=paste(outdir,"period_AIC/",unique(dd$year)[j],"all-days.png", sep =""), width=3, height=2.5, units = "in", res = 600)
				 	par(mar=c(3.2,3.5,2.5,0.7), ps=12, mgp=c(2,0.5,0), las=1, cex.lab=0.8, cex.axis=0.65, tcl=-0.2, cex.main=0.8)
					plot(delta~period,type="n", aa, main=paste(aa$year[1],"; # day: ", aa$n_days[1]), xlim=c(7,90), xaxt='n')#, ylim=c(0,100))
					axis(1,seq(7,90))
					#legend("topright", legend=c("simple", "interaction"), pch=c(1,16), col=c("black","grey"), pt.cex=0.7, cex=0.5)
					abline(v=c(14.75,14.75*2),lty=3)
					abline(h=3, lty=3, col="red")
					points(delta~period,aa, cex=0.7)
					dev.off()
				 print(paste(j,aa$year[1],aa$n_days[1]))
			}
			
	
	
}	

		{# model assumptions
			#m = lmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle), dd) 
			m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle),family = 'poisson', dd) 
			
			#png(paste(outdir,"model_ass/n_nest_all-data.png", sep=""), width=6,height=9,units="in",res=600)
			png(paste(outdir,"model_ass/n_nest_all-data_poisson.png", sep=""), width=6,height=9,units="in",res=600)
			#dev.new(width=6,height=9)
			par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
			scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			qqline(resid(m))
									  
				qqnorm(unlist(ranef(m)$year [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$year [1]))
				
				qqnorm(unlist(ranef(m)$year [2]), main = " slope",col='red')
				qqline(unlist(ranef(m)$year [2]))
				
				qqnorm(unlist(ranef(m)$year [3]), main = " slope",col='red')
				qqline(unlist(ranef(m)$year [3]))
				
				qqnorm(unlist(ranef(m)$st_cycle [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$st_cycle [1]))
									  
				#qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
				#qqline(unlist(ranef(m)$nest_ID[2]))
			length(dd$n_nest)
			length(dd$n_nest[!is.na(dd$st_cycle)])
			length(fitted(m))
			plot(dd$n_nest[!is.na(dd$st_cycle)]~fitted(m), xlab="Fitted values", ylab="n_nest", las=1, cex.lab=1.2, cex=0.8)
				abline(0,1, lty=3, col = 'red')
										
				#scatter.smooth(resid(m)~x2$date[x2$sum>720]);abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~sin(dd$rad_st[!is.na(dd$st_cycle)]));abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~cos(dd$rad_st[!is.na(dd$st_cycle)]));abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~dd$rad_st[!is.na(dd$st_cycle)]);abline(h=0, lty=2, col='red')
				#boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
				#mtext("lmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle), dd) ", side = 3, line = 0.5, cex=0.8,outer = TRUE)
				mtext("glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle),family = 'poisson', dd)  ", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
				acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
				dev.off()
		
		}

			
{# HATCHING
	d = nn[!is.na(nn$fate),]
	summary(factor(d$fate))
	d$fate[d$fate=='tide'] = 'flood'
	d$success = ifelse(d$fate=='hatch','yes','no')
	d$success_ = ifelse(d$fate=='hatch',1,0)
	d$year_ = as.factor(d$year)

	#d$rad_st= 2*d$hour*pi/24
	d$rad_st= 2*d$days_after_st*pi/14.765 #or half a synodic month of 29.53 days 
	d$year_tc=factor(paste(d$year,d$st_cycle))
	
	ggplot(d,aes(x = days_after_st, fill = success)) + geom_histogram()+facet_grid(year ~ .)
	ggplot(d,aes(x = days_after_st, fill = success)) + geom_density(alpha = 0.5)+facet_grid(year ~ .)
	ggplot(d[d$fate=='hatch',],aes(x = days_after_st, col=factor(st_cycle))) + geom_density()
	
	# simple
	#m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|pair), family = 'binomial', d) 
	m = glmer(success_ ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|female), family = 'binomial', d) 
	m = glmer(success_ ~ sin(rad_st) + cos(rad_st) +(sin(rad_st) + cos(rad_st)|year_)+(1|st_cycle)+(1|female), family = 'binomial', d)
				# optimize
					# fails
						ss <- getME(m,c("theta","fixef"))
						m2 <- update(m,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
					# optimizers
						# bobyqa - runs
							m3 <- update(m,start=ss,control=glmerControl(optimizer="bobyqa",
                            optCtrl=list(maxfun=2e5)))
								plot(allEffects(m3))
								summary(glht(m3))
								summary(m3)
						# nloptwrap
							m4 <- update(m,start=ss,control=glmerControl(optimizer="nloptwrap",
                            optCtrl=list(maxfun=2e5)))
								plot(allEffects(m3))
								summary(glht(m3))
								summary(m3)
						# try all optimizers
							aa <- allFit(m)
							# bobyqa, nlminbw, optimx.L-BFGS-B ok, others failed
							is.OK <- sapply(aa,is,"merMod")  
							aa.OK <- aa[is.OK]
							lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)
							# how bad was the poor convergence in some optimizers
							(lliks <- sort(sapply(aa.OK,logLik))) # very similar logLik
							aa.fixef <- t(sapply(aa.OK,fixef))
							aa.fixef.m <- melt(aa.fixef)
							models <- levels(aa.fixef.m$Var1)
							ylabs <- substr(models,1,3)
							aa.fixef.m <- transform(aa.fixef.m,Var1=factor(Var1,levels=names(lliks)))
							(gplot1 <- ggplot(aa.fixef.m,aes(x=value,y=Var1,colour=Var1))+geom_point()+
								 facet_wrap(~Var2,scale="free")+
									 scale_colour_brewer(palette="Dark2")+
										 scale_y_discrete(breaks=models,
														  labels=ylabs)+
															  labs(x="",y=""))
							
						#Coefficients of variation of fixed-effect parameter estimates:
						summary(unlist(daply(aa.fixef.m,"Var2",summarise,sd(value)/abs(mean(value)))))
						
						# variances of the random parameters
						aa.stddev <- t(sapply(aa.OK,function(x) sqrt(unlist(VarCorr(x)))))
						print(aa.stddev,digits=3) # NA issues
						
						aa.stddev.m <- melt(aa.stddev) 
						aa.stddev.m <- transform(aa.stddev.m,Var1=factor(Var1,levels=names(lliks)))
						gplot1 %+% aa.stddev.m # does not work, likely because of the NAs
						
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|pair), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|pair), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	
	# year
	d$year_= as.factor(d$year)
	m = glmer(flooded ~ sin(rad_st)*year_ + cos(rad_st)*year_ +(1|year_)+(1|st_cycle), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|st_cycle), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) + scale(st_cycle) + (1|year)+(1|pair), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
		
	plot(allEffects(m))
	summary(glht(m))
	summary(m)
	
	{# predictions
			m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year), family = 'binomial', d) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
				# values to predict for		
					newD=data.frame(days_after_st = seq(0,15, length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
						
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
		x = ddply(d,.(year, days_after_st), summarise, mean_ = mean(flooded)*100, sd_ = sd(flooded)*100, n = length(year))
			y = data.frame(year = unique(d$year), col_ =1:length(unique(d$year)), stringsAsFactors = FALSE)
			x$col_ = y$col_[match(x$year, y$year)]
			
		x2 = ddply(d,.(days_after_st), summarise, mean_ = mean(flooded)*100, sd_ = sd(flooded)*100, n = length(year))	
	}
	{# plot 0:40
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_flood_update.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$days_after_st, 
						xlim=c(0,15), ylim=c(0,30),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Flooded nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(pp$days_after_st, rev(pp$days_after_st)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(pp$days_after_st, pp$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						symbols( jitter(x2$days_after_st),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 

						
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
			png(paste(outdir,"Figure_flood_means_pre_year_update.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0.2,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$days_after_st, 
						xlim=c(0,15), ylim=c(0,100),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Flooded nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_st),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=adjustcolor(col_f, alpha.f = 0.5), fg=col_p,add=TRUE) 
						
						polygon(c(pp$days_after_st, rev(pp$days_after_st)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pp$days_after_st, pp$pred, col=col_m,lwd=1)
							
						if(PNG == TRUE) {dev.off()}
						
						
	}			
		{# model assumptions
			m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|pair), family = 'binomial', d) 
			#m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|pair), family = 'binomial', d[d$fate%in%c('flood','hatch'),]) 
		
			#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
			dev.new(width=6,height=9)
			par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
			scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			qqline(resid(m))
									  
				qqnorm(unlist(ranef(m)$year [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$year [1]))
				
				qqnorm(unlist(ranef(m)$pair [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$pair [1]))
									  
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


{# random sampling of days within the year
  for (w in unique(nn$year)){
	d = nn[nn$year == w,]
	d$laid_j = as.numeric(format(as.POSIXct(d$laid),"%j"))
	ii_=ii[ii$year == w,]
	ii_$day_j = as.numeric(format(as.POSIXct(ii_$noon),"%j"))
	
	l = list()
	for( i in 1:10){
		s = data.frame(day_j = sample(seq(min(d$laid_j), max(d$laid_j)), nrow(d), replace = T))
		
		s$ill_midnight = ii_$illumination_noon[match(s$day_j, ii_$day_j)]
		s$sample = i
		l[[i]] = s
	}
	s = do.call(rbind,l)
	ggplot(s, aes(x = ill_midnight, fill = as.factor(sample))) + geom_histogram() + facet_grid(sample ~ .) 
	ggsave(file=paste(outdir, 'illumination random sample from ', w, ' days.png', sep=""))
	}
}

{# OLD

{#
dd = ddply(d,. (year, laid , moon_cycle, st_cycle, days_after_st), summarise, n_nest = length(year))
	#dd = ddply(dd,. (year, moon_cycle, st_cycle), transform, laid_j_yc = laid_j-mean(laid_j))
	
	dsplit=split(dd,paste(dd$year))
		foo=lapply(dsplit,function(x) {
				#x=dsplit$"2006"
				y = data.frame(laid = seq(min(x$laid), max(x$laid), by = 'day'), n_nest = 0, year = x$year[1])
				y = y[!y$laid%in%x$laid,]
				y = 
				#x$t_a=c(x$treat[-1], NA) 	
				x = merge(x,y, all=TRUE)
				x$days_b = c(NA,x$days_after_st[-length(x$days_after_st)])	
				x$days_after_st[is.na(x$days_after_st)]=x$days_b[is.na(x$days_after_st)]+1
				# repeat for those with two days after each other with no data
				x$days_b = c(NA,x$days_after_st[-length(x$days_after_st)])	
				x$days_after_st[is.na(x$days_after_st)]=x$days_b[is.na(x$days_after_st)]+1
				
				x$days_b = c(NA,x$st_cycle[-length(x$st_cycle)])	
				x$st_cycle[is.na(x$st_cycle)]=x$days_b[is.na(x$st_cycle)]
				# repeat for those with two days after each other with no data
				x$days_b = c(NA,x$st_cycle[-length(x$st_cycle)])	
				x$st_cycle[is.na(x$st_cycle)]=x$days_b[is.na(x$st_cycle)]
				return(x)
				}
				)
		dd=do.call(rbind, foo) 
}
{# LOAD DATA
	
	d<-read.csv(file=paste(wd, "metadata_illu_tide_temp.csv", sep=''),header=T,sep=";", fill=T, stringsAsFactors=FALSE)
	#, col.names=c('year','nest','found','laid','end','found_julian','laid_julian','end_julian','fate',)
	colnames(d)[colnames(d)=='nest_ID'] = 'nest'
	#nrow(d[d$laid=='NA',])
	d$lat = as.numeric(n$lat[match(d$nest,n$nest)])
	d$lon = as.numeric(n$lon[match(d$nest,n$nest)])
	d$laid=as.POSIXct(d$laid)
	d$pk=1:nrow(d)
	d$laid[d$laid=='2016-09-04'] = '2016-04-09'
	d$laid[d$laid=='2016-12-04'] = '2016-04-12'
	d$laid[d$laid=='2016-08-05'] = '2016-05-08'
	d$laid[d$laid=='2016-10-05'] = '2016-05-10'
	d$laid[d$laid=='2016-11-05'] = '2016-05-11'
	d$laid[d$laid=='2016-12-05'] = '2016-05-12'
	d$laid[d$laid=='2006-01-05'] = '2006-05-01'
	d$laid[d$laid=='2006-01-06'] = '2006-06-01'
	d=d[!is.na(d$laid),]
	
	j =  sqldf("select*from d join gs_", stringsAsFactors = FALSE)
	d = sqldf("select*from j WHERE laid BETWEEN  datetime_ and int OR laid = datetime_")
		#d[!d$pk%in%dd$pk,]
	
				
	# add tide count
	#s = ddply(d[is.n.,(
}
{# distributions
	ggplot(d, aes(x= d_after_springtide, y = tide_height, col=as.factor(st_cycle))) + geom_point()+geom_line()+facet_grid(year ~ .)
	ggplot(d, aes(x= d_after_springtide, y = tide_height, col=as.factor(st_cycle))) + geom_point()+geom_line()
	

	str(d)
	ggplot(d, aes(x=ldy, fill=factor(year))) + geom_histogram(alpha  = 0.5, position = 'dodge')
	ggplot(d, aes(x=ldy, fill=factor(year))) + geom_density(alpha  = 0.5)
	ggplot(d, aes(x=ldy)) + geom_density(alpha  = 0.5)
	
	ggplot(d, aes(x=dast, fill=factor(year))) + geom_density(alpha  = 0.5)
	ggplot(d, aes(x=dast, fill=factor(f))) + geom_density(alpha  = 0.5)
	ggplot(d, aes(x=dast)) + geom_density(alpha  = 0.5)
	
}
{# moon light
d$rad_nm= 2*d$danm*pi/29.5
}
}

	