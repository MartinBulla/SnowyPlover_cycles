# MB - finalize figure with flood, # nest, and st_cycle, and days, including distributions or histograms or both
# R Script generates statistical outputs for snowy plover nest initiation cycles
{# SETTINGS & DATA
	{# do you want plots within R (PNG=FALSE) or as PNG (PNG = TRUE)?
		PNG=FALSE
		#PNG = TRUE
	}
	{# define working and output directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/SnowyPlover_cycles/"
		 outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/SnowyPlover_cycles/Output/" 
	}
	{# load packages, constants and data
		source(paste(wd, 'Constants_Functions.R',sep=""))
		source(paste(wd, 'Prepare_Data.R',sep=""))
	}
}

# INTRO
{# Figure Xa
{# prepare
	tt$rad_st= 2*tt$days_after_st*pi/tt$dur # using specific duration of the given spring tide cycle; alternative with fixed duration: tt$rad_st= 2*tt$days_after_st*pi/14.765
	
	# define breeding season
		s = ddply(dd,.(year), summarise, start_ = min(day_j), end_ = max(day_j))
		tt$start_ = s$start_[match(tt$year,s$year)]
		tt$end_ = s$end_[match(tt$year,s$year)]
		tt$breeding_season = ifelse(tt$day_j>=tt$start_ & tt$day_j<=tt$end_,'yes','no')
	
	# make cycle 1 the first cycle of the given year
		tt = ddply(tt,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)
	}
{# plot
   geom.text.size = 2
   theme.size = 7 ##theme.size = (14/5) * geom.text.size
   dev.new(width = 3.5, height = 2)
	
   ggplot(tt,aes(x = st_cycle_c, y = max_tide_height, col = breeding_season))+geom_point(size = 0.5)+facet_wrap(~year, nrow  =2)+
    expand_limits(x = 0, y = 0)+
	scale_x_continuous(limits = c(0,25),expand = c(0,0),breaks= seq(5,25,by = 10), labels=seq(5,25,by = 10))+  
    scale_y_continuous(limits = c(0,180),expand = c(0,0),breaks= seq(0,180,by = 60), labels=seq(0,180,by = 60))+
	labs(x = "Season [spring-tide cycle #]",color = 'Breeding\nseason', y ="Maximum daily tide [cm]")+  
    scale_colour_manual(values=c("grey80", "firebrick3"))+
	guides(color = guide_legend(override.aes = list(size = 2)))+
  
    theme_light()+
    theme(#text = element_text(size = theme.size),
		axis.line=element_line(colour="grey70", size=0.25),
		panel.border=element_rect(colour="grey70",fill = NA, size=0.25),
		panel.grid = element_blank(),
		panel.margin = unit(0, "mm"),
		strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
		strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
		#strip.background = element_blank(), 
		#strip.text = element_blank(),
								
		#panel.margin.y = unit(-1.2, "lines"),
		axis.title=element_text(size=7, colour="grey30"),
		axis.title.y = element_text(vjust=1),
		axis.title.x = element_text(vjust=0.2),
		axis.text.x=element_text(size=7, margin = margin(t=0.1)),
		axis.text.y=element_text(size=6),
		#axis.title.y.right= element_text(vjust=0, angle = 90),
		axis.ticks.length=unit(0.5,"mm"),
		legend.background=element_rect(colour=NA),
		legend.key=element_rect(fill="grey99", colour="white"),
		legend.key.size = unit(2.5,"mm"),
		legend.text=element_text(size=6, colour="grey30"),
		legend.title=element_text(size=7, colour="grey30")
			#axis.ticks.margin,
			#strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
		#strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
								#strip.background = element_blank(), 
								#strip.text = element_blank(),
								
								#legend.position="none"
								#legend.background=element_rect(colour="white"),
								#legend.key=element_rect(fill="grey99", colour="white"),
								#legend.text=element_text(size=7, colour="grey30"),
								#legend.title=element_text(size=8, colour="grey30")
		)
		
ggsave(file=paste(outdir,'Figure_X.png',sep=''),width = 3.5, height = 2, units = "in",dpi = 600)
  }
}
  {# Figure X - old later delete
	geom.text.size = 2
	theme.size = 7 ##theme.size = (14/5) * geom.text.size
	dev.new(width = 3.5, height = 2.5)
	
	ggplot(tt[tt$year!=2014,],aes(x = st_cycle_c, y = max_tide_height, col = breeding_season))+geom_point()+facet_wrap(~year, nrow  =2)+
		geom_text(aes(x, y,label=lab),
            data=data.frame(x=20, y=200, flooded_ = c("yes","no"), lab=c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2015", "2016"),
                            year=c(2006,2007,2008,2009,2010,2011,2012,2013,2015,2016)), size=geom.text.size, colour = "grey30")+
  ylim(c(0,200))+
  scale_x_continuous(limits = c(0,25),breaks= seq(0,25,by = 5), labels=c("","5", "", "15", "","25"))+  # FOR SIME REASON March does not appear
  labs(x = "Season [spring-tide cycle #]",color = 'Breeding\nseason', y ="Maximum daily tide height [cm]")+  
  scale_fill_manual(values=c("grey60", "firebrick3"))+
  #coord_cartesian( xlim = c(90,195))+
  theme_light()+
  theme(#text = element_text(size = theme.size),
		axis.line=element_line(colour="grey70", size=0.25),
		panel.border=element_rect(colour="grey70",fill = NA, size=0.25),
		panel.grid = element_blank(),
		panel.margin = unit(1, "mm"),
		panel.margin.y = unit(-1.2, "lines"),
		strip.background = element_blank(), strip.text.y = element_blank(),
		axis.title=element_text(size=7, colour="grey30"),
		axis.title.y = element_text(vjust=1),
		axis.title.x = element_text(vjust=0.2),
		axis.text.x=element_text(size=7, margin = margin(t=0.1)),
		axis.text.y=element_text(size=6),
		#axis.title.y.right= element_text(vjust=0, angle = 90),
		axis.ticks.length=unit(0.5,"mm"),
		legend.background=element_rect(colour=NA),
		legend.key=element_rect(fill=NA, colour=NA),
		legend.key.size = unit(2.5,"mm"),
		legend.text=element_text(size=6, colour="grey30"),
		legend.title=element_text(size=7, colour="grey30")
			#axis.ticks.margin,
			#strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
		#strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
								#strip.background = element_blank(), 
								#strip.text = element_blank(),
								
								#legend.position="none"
								#legend.background=element_rect(colour="white"),
								#legend.key=element_rect(fill="grey99", colour="white"),
								#legend.text=element_text(size=7, colour="grey30"),
								#legend.title=element_text(size=8, colour="grey30")
		)
ggsave(file=paste(outdir,'Figure_D.png',sep=''),width = 3.5, height = 3.5*1.5, units = "in",dpi = 600)
  }
{# Supplementary Figure SX
  	geom.text.size = 2
	theme.size = 7 ##theme.size = (14/5) * geom.text.size
	dev.new(width = 3.5, height = 2)
	
	tt_ =tt[which(tt$breeding_season=='yes'),]
    
	ggplot( tt_,aes(x = days_after_st, y = max_tide_height, col=st_cycle_c))+geom_point(size = 0.5)+facet_wrap(~year, nrow  =2)+
    expand_limits(x = 0, y = 0)+
	scale_x_continuous(limits = c(0,15),expand = c(0,0),breaks= seq(5,15,by = 5), labels=seq(5,15,by = 5))+  
    scale_y_continuous(limits = c(0,180),expand = c(0,0),breaks= seq(0,180,by = 60), labels=seq(0,180,by = 60))+
	labs(x = "Days after srping tide",color = 'Spring-tide\ncycle #', y ="Maximum daily tide [cm]")+  
	scale_colour_gradient(low = "deepskyblue", high = "orange", guide = "colourbar")+
    #scale_colour_manual(values=c("grey80", "firebrick3"))+
	#guides(color = guide_legend(override.aes = list(size = 2)))+
    theme_light()+
    theme(#text = element_text(size = theme.size),
		axis.line=element_line(colour="grey70", size=0.25),
		panel.border=element_rect(colour="grey70",fill = NA, size=0.25),
		panel.grid = element_blank(),
		panel.margin = unit(0, "mm"),
		strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
		strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
		#strip.background = element_blank(), 
		#strip.text = element_blank(),
								
		#panel.margin.y = unit(-1.2, "lines"),
		axis.title=element_text(size=7, colour="grey30"),
		axis.title.y = element_text(vjust=1),
		axis.title.x = element_text(vjust=0.2),
		axis.text.x=element_text(size=7, margin = margin(t=0.1)),
		axis.text.y=element_text(size=6),
		#axis.title.y.right= element_text(vjust=0, angle = 90),
		axis.ticks.length=unit(0.5,"mm"),
		legend.background=element_rect(colour=NA),
		legend.key=element_rect(fill="grey99", colour="white"),
		legend.key.size = unit(2.5,"mm"),
		legend.text=element_text(size=6, colour="grey30"),
		legend.title=element_text(size=7, colour="grey30")
			#axis.ticks.margin,
			#strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
		#strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
								#strip.background = element_blank(), 
								#strip.text = element_blank(),
								
								#legend.position="none"
								#legend.background=element_rect(colour="white"),
								#legend.key=element_rect(fill="grey99", colour="white"),
								#legend.text=element_text(size=7, colour="grey30"),
								#legend.title=element_text(size=8, colour="grey30")
		)
		
ggsave(file=paste(outdir,'Figure_SX_.png',sep=''),width = 3.5, height = 2, units = "in",dpi = 600)
  }
		{# not in the manuscript
		{# Supplementar Table T - test of the heigh tide patterns
		m = lmer(max_tide_height ~ cos(rad_st) + sin(rad_st) +
					((cos(rad_st) + sin(rad_st))|st_cycle_c)+(1|year),data=tt, REML= FALSE)
		pred=c('Intercept','cos','sin')
	
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='Max tide',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='Max tide',type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
		
		# export
			o=rbind(oii,ri)
			sname = tempfile(fileext='.xls')
			wb = loadWorkbook(sname,create = TRUE)	
			createSheet(wb, name = "output")
			writeWorksheet(wb, o, sheet = "output")
			saveWorkbook(wb)
			shell(sname)
	}
		{# model assumptions
			m = lmer(max_tide_height ~ cos(rad_st) + sin(rad_st) +((cos(rad_st) + sin(rad_st))|st_cycle)+(1|year),data=tt, REML= FALSE)
			
			# check whether random structure ok
				m = lmer(max_tide_height ~ cos(rad_st) + sin(rad_st) +
					(1|year)+ (cos(rad_st) + sin(rad_st)|st_cycle_c),data=tt, REML= FALSE)
				mz = lmer(max_tide_height ~ cos(rad_st) + sin(rad_st) +
					(1|year)+ (cos(rad_st) + sin(rad_st)||st_cycle_c),data=tt, REML= FALSE)
				anova(mz,m)
				summary(rePCA(m))
			
			dev.new(width=6,height=9)
			par(mfrow=c(4,3))
						
			scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
						
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$st_cycle[1]), main = " st_cycle")
			qqline(unlist(ranef(m)$st_cycle[1]))			
						
			qqnorm(unlist(ranef(m)$st_cycle[2]), main = " st_cycle")
			qqline(unlist(ranef(m)$st_cycle[2]))	
			
			tt$res = resid(m)
			plot(resid(m)~cos(tt$rad_st))
			plot(resid(m)~sin(tt$rad_st))				
			plot(resid(m)~tt$rad_st)
			plot(unlist(ranef(m)$st_cycle[1])~c(1:nrow(ranef(m)$st_cycle[1])))
			plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
	
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
			}
		{# explore 
			ggplot(tt, aes(x = max_tide_height, fill = factor(year))) + geom_histogram()+ facet_grid(year ~ .)
			ggplot(tt, aes(x = days_after_st, y = max_tide_height, col = factor(st_cycle))) + stat_smooth(se=FALSE)	+ facet_grid(year ~ .)
			ggplot(tt, aes(x = date, y = max_tide_height, col = factor(st_cycle)))+ geom_point()+ facet_wrap(~year, scales = "free_x", ncol = 1)	#facet_grid(year ~ .,scales = 'free')
			ggplot(tt, aes(x = date, y = max_tide_height, col = factor(year))) + stat_smooth(se=FALSE)	+ facet_grid(year ~ .)
			ggplot(tt, aes(x = date, y = max_tide_height, col = factor(year))) + geom_point()	+ facet_grid(year ~ .)
			ggplot(tt, aes(x = date, y = max_tide_height, col = factor(year))) + geom_point()+geom_line()
		}
	}

# METHODS
{# lenght of incubation period for hatched nests found during laying
	x = nn[nn$fate=='hatch' & !is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & nn$clutch_size_found!=nn$clutch_size,]
	x$lay_inc = as.numeric(difftime(x$end_,x$laid,units = 'hours'))/24
	x$inc = ifelse(x$clutch_size == 1, x$lay_inc, ifelse(x$clutch_size == 2, x$lay_inc-3, x$lay_inc-5))
	nrow(x) # 51 nests
	summary(x$inc)
	summary(as.numeric(difftime(x$end_,x$laid,'hours')))
	#x[order(x$inc),]
	#x[x$lay_inc<28,c('nest','clutch_size_found','clutch_size','laid','end_','lay_inc')]
}
{# nest stage found
	# 93 during laying
		nrow(nn[!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & nn$clutch_size_found!=nn$clutch_size,]) # 93
	# 43 age unknown or older than 10 days and hatchded (note that 13 clutches found >10 days failed and are not within this dataset)
	  length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' &  (nn$float_min == 11| is.na(nn$floated)))]) # 43 unknownd age or older than 10 days and hatched
		length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' &  nn$float_min == 11 )]) # 25 older than 10 days and hatched
				length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch'& nn$float_min == 11 & nn$found == nn$end_  )]) # of which 1 found during hatching
		length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' & is.na(nn$floated))]) # 18 unknown age (not floated) but hatched
				length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' & is.na(nn$floated) & nn$found == nn$end_)]) # of which 7 found at hatching 
	
	# 616 with complete clutch and eggs younger than 11 days
		nrow(nn) - nrow(nn[!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & nn$clutch_size_found!=nn$clutch_size,]) -  length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' &  (nn$float_min == 11| is.na(nn$floated)))]) # 616
}
{# sample size
	nrow(nn) # 752 nests
}
{# clutch sizes
	summary(factor(nn$clutch_size))
	100*summary(factor(nn$clutch_size))/nrow(nn)
}

{# Figure MAP with nests
   # prepare data
	 m <- n[!is.na(n$fate),]
	 m$fate[m$fate=='tide'] <- 'flood'
	 m$flooded <- ifelse(m$fate=='flood',1,0)

     register_google(key = "AIzaSyDOx8S3GS9Jry23VpJjaXglP4p4vBBxxZg")

	 map <- get_map(location = c(lon = -106.9580, lat = 23.90835), maptype = "satellite", source = "google", zoom = 14, color = "bw") 
		#ggmap(map)
	# plot
		geom.text.size = 2
		theme.size = 7 
		dev.new(width = 3.5, height = 2)
		cols<- c('green','red')
		
		mapPoints <- ggmap(map) + geom_point(size=0.2, aes(x = lon, y = lat, colour = as.factor(flooded)), data = m) + facet_wrap(~ year, ncol=5)
		
		mapPoints + scale_colour_manual(values=(cols), labels =c("No", "Yes")) +
		  scale_x_continuous(name = 'Longitude [°]', breaks= -106.9580, labels=c("-106.958"))+
		  labs(color = 'Flooded', y ='Latitude [°]')+  	
		  guides(color = guide_legend(override.aes = list(size = 2)))+
		  theme_light() +
		  theme( #text = element_text(size = theme.size),
			axis.line=element_line(colour="grey70", size=0.25),
			panel.border=element_rect(colour="grey70",fill = NA, size=0.25),
			panel.grid = element_blank(),
			panel.spacing = unit(0, "mm"),
			strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
			strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
			axis.title=element_text(size=7, colour="grey30"),
			axis.title.y = element_text(vjust=1),
			axis.title.x = element_text(vjust=0.2),
			#axis.text.x=element_text(size=7, margin = margin(t=0.1)),
			axis.text.x=element_text(size=5, margin = margin(t=0.1)),
			axis.text.y=element_text(size=6), 
			axis.ticks.length=unit(0.5,"mm"),
			legend.background=element_rect(colour=NA),
			legend.key=element_rect(fill=NA, colour=NA),
			legend.key.size = unit(2.5,"mm"),
			legend.text=element_text(size=6, colour="grey30"),
			legend.title=element_text(size=7, colour="grey30")

			#axis.ticks.margin,
			#strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
			#strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
			#strip.background = element_blank(), 
			#strip.text = element_blank(),
			#legend.position="none"
			#legend.background=element_rect(colour="white"),
			#legend.key=element_rect(fill="grey99", colour="white"),
			#legend.text=element_text(size=7, colour="grey30"),
			#legend.title=element_text(size=8, colour="grey30")
		  )
	ggsave(file=paste(outdir,'Figure_Map_larger.png',sep=''),width = 3.5*1.5, height = 2*1.5, units = "in",dpi = 600)	  
}

# RESULTS
{# number and proportion of flooded nests
	summary(factor(nn$fate))
	summary(factor(nn$fate))/nrow(nn)
	nrow(nn[nn$fate=='tide']
	nn$flooded_ = ifelse(nn$fate=='tide','yes','no')
	summary(factor(nn$flooded_))
	summary(factor(nn$flooded_))/nrow(nn)
	
	nh = nn[nn$fate%in%c('tide','hatch','unhatch'),]
	summary(factor(nh$fate))/nrow(nh)
	
	# number of initiated nests per year
	nn$n=1
	nt = ddply(nn,.(year), summarise, n = sum(n))
	summary(nt) 
	
	# number of flooded nests per year
	nn$n=1
	nt = ddply(nn,.(year), summarise, n = sum(n[fate%in%c('tide')]))
	summary(nt)
	table(nt$n)

}
{# FIGURE D - nest frequencies per year with tideheights (uses all nests)
  {# prepare	
	d = nn
	summary(factor(d$fate))
	
	d$flooded = ifelse(d$fate=='tide',1,0)
	d$flooded_ = ifelse(d$fate=='tide','yes','no')
	d$year_ = as.factor(d$year)
	d$laid_j=as.POSIXlt(d$laid, format = "%d%b%y")$yday
	
	d$rad_st = 2*d$days_after_st*pi/d$dur
	d$y_cl=factor(paste(d$year,d$st_cycle))
	dd$y_cl_fs = paste(dd$year, dd$st_cycle,dd$first_second)
	#standardize within cycle
	d = ddply(d,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)
	d = ddply(d,.(year), transform, max_year_z = scale(max_tide_height))
	d = ddply(d,.(year, st_cycle), transform, max_yr_cl_z = scale(max_tide_height))
	d$st_cycle_c[d$st_cycle_c==8]=7
 
	NP <- c((seq(as.Date("2006-03-01"), as.Date("2006-07-31"), by="days")), 
        seq(as.Date("2007-03-01"), as.Date("2007-07-31"), by="days"), 
        seq(as.Date("2008-03-01"), as.Date("2008-07-31"), by="days"), 
        seq(as.Date("2009-03-01"), as.Date("2009-07-31"), by="days"), 
        seq(as.Date("2010-03-01"), as.Date("2010-07-31"), by="days"), 
        seq(as.Date("2011-03-01"), as.Date("2011-07-31"), by="days"), 
        seq(as.Date("2012-03-01"), as.Date("2012-07-31"), by="days"),
        seq(as.Date("2013-03-01"), as.Date("2013-07-31"), by="days"),
        seq(as.Date("2015-03-01"), as.Date("2015-07-31"), by="days"),
        seq(as.Date("2016-03-01"), as.Date("2016-07-31"), by="days"))
	np <- as.data.frame(NP) # nesting period
	colnames(np) <- c("date")
	np["nests_laid"] <- rep(0, length(np$date)) # new col were the number of layed nests are supposed to go
	np$date <- as.POSIXct(np$date)
	np$n_nest <- (dd$n_nest[match(np$date,dd$datetime_)])
	np[is.na(np)] <- 0
	np$flooded_ = NA
	# get illumination
	np$illum <- ii$illumination_noon[match(as.character(np$date), substring(ii$noon,1,10))]
	np$illum <- round(np$illum,2)
	np$yday <- yday(np$date)
	np$year <- as.numeric(format(as.Date(np$date, format="%d/%m/%Y"),"%Y"))
	# max tide height
	np$max_tide_height = tt$max_tide_height[match(np$date, tt$date)]
	d$yday = yday(d$laid)
	d = d[order(d$laid),]
	# get the median for the laying date
	median_l <- tapply(d$yday, as.factor(d$year),median) 
	median_l <- as.data.frame.table(median_l)
	colnames(median_l) <- c("year","median") 
	median_l$year <- as.integer(as.character(median_l$year)) # needs to be the same class as base data
}
  {# plot
		dev.new(width = 3.5, height = 3.5*1.5)
		geom.text.size = 2
		#theme.size = (14/5) * geom.text.size
		theme.size = 7
		p = ggplot(d, aes(x= yday , fill=as.factor(flooded_))) + 
		  geom_vline(aes(xintercept = median), median_l, size = 0.5, col = 'orange')+ # median of laying dates	
		  geom_line(data = np[!is.na(np$max_tide_height),], aes(x = yday, y = max_tide_height / 15), size =0.5,color='steelblue') +
		  geom_histogram(bins = 105) + 
		  facet_grid(year ~.) +
		  geom_text(aes(x, y,label=lab),
		            data=data.frame(x=187, y=14, flooded_ = c("yes","no"), lab=c("2006 (N=158)", "2007 (N=129)", "2008 (N=75)", "2009 (N=79)", "2010 (N=94)", "2011 (N=67)", "2012 (N=33)", "2013 (N=29)", "2015 (N=43)", "2016 (N=45)"),
		                            year=c(2006,2007,2008,2009,2010,2011,2012,2013,2015,2016)), size=geom.text.size, hjust = 0.55,colour = "grey30")+
		  scale_x_continuous(limits = c(74,196),breaks= c(74,105,135,166,196), labels=c("March","April", "May", "June", "July"))+  
		  labs(y = "# of initiated nests",fill = 'Flooded', x =NULL)+  
		  scale_fill_manual(values=c("grey60", "firebrick3"))+
		  coord_cartesian( xlim = c(74,196))+
		  theme_light()+
		  theme(#text = element_text(size = theme.size),
		    axis.line=element_line(colour="grey70", size=0.25),
		    panel.border=element_rect(colour="grey70",fill = NA, size=0.25),
		    panel.grid = element_blank(),
		    #panel.margin = unit(1, "mm"), # it throw's me a warning when I use this (has been deprecated), needed to use panel.spacing instead
		    panel.spacing = unit(1, "mm"),
		    strip.background = element_blank(), strip.text.y = element_blank(),
		    axis.title=element_text(size=7, colour="grey30"),
		    axis.title.y = element_text(vjust=1),
		    axis.title.x = element_text(vjust=0.2),
		    axis.text.x=element_text(size=7, margin = margin(t=0.1)),
		    axis.text.y=element_text(size=6),
		    axis.title.y.right= element_text(vjust=0, angle = 90, color = 'steelblue'), # still works fine for me
		    axis.text.y.right= element_text(color = 'steelblue'),
		    axis.ticks.length=unit(0.5,"mm"),
		    legend.background=element_rect(colour=NA),
		    legend.key=element_rect(fill="grey99", colour="white"),
		    legend.key.size = unit(2.5,"mm"),
		    legend.text=element_text(size=6, colour="grey30"),
		    legend.title=element_text(size=7, colour="grey30")
		    #axis.ticks.margin,
		    #strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
		    #strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
		    #strip.background = element_blank(), 
		    #strip.text = element_blank(),
		    
		    #legend.position="none"
		    #legend.background=element_rect(colour="white"),
		    #legend.key=element_rect(fill="grey99", colour="white"),
		    #legend.text=element_text(size=7, colour="grey30"),
		    #legend.title=element_text(size=8, colour="grey30")
		  )  
		  
		p + scale_y_continuous(breaks=c(0,5,10,15),labels=c("0","5","10","15"), name= "Number of initiated nests", limits = c(0, 15),
		                       sec.axis = sec_axis(~ . * 10, name = "Tide height [cm]", breaks = c(0,50,100,150)))
		
		if(PNG == TRUE) {ggsave(file=paste(outdir,'Figure_D.png',sep=''),width = 3.5, height = 3.5*1.5, units = "in",dpi = 600)}
		}
}

{# FLOODING
  {# prepare	
	d = nn[nn$fate%in%c("hatch","unhatch","tide"),] # only flooded, hatched or unhatched nests
	summary(factor(d$fate))
	d$flooded = ifelse(d$fate=='tide',1,0)
	d$flooded_ = ifelse(d$fate=='tide','yes','no')
	d$year_ = as.factor(d$year)
	d$laid_j=as.POSIXlt(d$laid, format = "%d%b%y")$yday
	d$days_after_r = ceiling(d$days_after_st)
	d$rad_st = 2*d$days_after_st*pi/d$dur
		###d$y_cl=factor(paste(d$year,d$st_cycle))
		###dd$y_cl_fs = paste(dd$year, dd$st_cycle,dd$first_second)
	
	d = ddply(d,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1) # center within year
		summary(factor(d$st_cycle_c))
	d$st_cycle_c[d$st_cycle_c==7]=6 # only 1 datapoints for cycle 7, so make it 6 
	###d = ddply(d,.(year), transform, max_year_z = scale(max_tide_height)) # standardize tide within year
	###d = ddply(d,.(year, st_cycle), transform, max_yr_cl_z = scale(max_tide_height)) # standardize tide within year and cycle
   }	
	  {# explore (not in paper)
		table(d$year,d$st_cycle_c)
		table(d$year, d$flooded_)
		table(d$flooded_)
		ggplot(d,aes(x = laid_j, fill=as.factor(flooded_))) + geom_histogram(position="identity", alpha = 0.3)+ facet_grid(year ~ .) 
		ggplot(d,aes(x = as.factor(flooded_), y = max_tide_height, fill=as.factor(flooded_))) + geom_boxplot()+ facet_grid(year ~ .)
		ggplot(d,aes(x = days_after_st, y = max_tide_height, col=as.factor(flooded_))) + geom_point()+ facet_grid(year ~ .)
		###ggplot(d,aes(x = days_after_st)) + geom_density()#+ facet_grid(year ~ .)
		###ggplot(d,aes(x = days_after_st)) + geom_histogram()
		ggplot(d,aes(x = days_after_st, col = flooded_)) + geom_density()#+ facet_grid(year ~ .)
		ggplot(d,aes(x = days_after_st, fill = flooded_)) + geom_histogram(position='identity', bins = 16)#+ facet_grid(year ~ .)
	#+ facet_grid(year ~ .)
		ggplot(d,aes(x = days_after_st, fill=as.factor(flooded_))) + geom_histogram(position="identity")+ylab('# of initiated nests') + xlab('Days after spring tide')		
		ggplot(d,aes(x = days_after_st, col = flooded_)) + geom_density()+ facet_grid(year ~ .,scales = "free_y" )
		ggplot(d,aes(x = days_after_st, fill = flooded_)) + geom_histogram(position='identity', bins = 16, alpha = 0.4)+ facet_grid(year ~ .,scales = "free_y" )
		ggplot(d,aes(x = days_after_st, col = flooded_, fill = year_)) + geom_density(alpha = 0.2)
	 
		ggplot(d,aes(x = days_after_st, y = max_tide_height, col=as.factor(year))) + geom_point() + stat_smooth(se=FALSE)
				
		ggplot(d,aes(x = st_cycle_c, fill=as.factor(flooded_))) + geom_histogram(position="identity", alpha = 0.3)
		ggplot(d,aes(x = st_cycle_c, fill=as.factor(flooded_))) + geom_histogram(position="identity", alpha = 0.3)+ facet_grid(year ~ .) 
		ggplot(d,aes(x = st_cycle_c, fill=as.factor(flooded_))) + geom_histogram(position="identity", alpha = 0.3)+ facet_grid(year ~ .) 
		
		ggplot(d,aes(x = lat)) + geom_density()+facet_wrap(~year)
		ggplot(d,aes(x = lat, y = lon))+ stat_smooth()+ geom_point()+facet_wrap(~year)
		ggplot(d,aes(x = st_cycle_c, y=lat, col = flooded_)) + geom_point()+facet_wrap(~year) 
		
		ggplot(d,aes(x = st_cycle_c, y=lat)) +stat_smooth(method='lm', col ='black')+geom_point(aes(col=flooded_))+facet_wrap(~year)
		ggplot(d,aes(x = st_cycle_c, y=lat)) +stat_smooth(method='lm', col ='black')+geom_point(aes(col=flooded_))
		cor(d$lat,d$st_cycle_c)
		ggplot(d,aes(x = st_cycle_c, y=lon)) +stat_smooth(method='lm', col ='black')+geom_point(aes(col=flooded_))+facet_wrap(~year)
		
		ggplot(d,aes(x = days_after_st, y=lat)) +stat_smooth(col ='black')+geom_point(aes(col=flooded_))+facet_wrap(~st_cycle_c)+ylab('lon')
		#ggsave(file=paste(outdir,'#of_nests_given_day_and_fate.png',sep=''))
	  }
  {# Supplementary Table S1
	{# gaussian model
		m = lmer(flooded ~ scale(lat)+scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female),d)
			# taking the correlated variables of cycle and lat (m = lmer(st_cycle_c ~ scale(lat)+(1|year)+(1|female),  d) )out one at the time, does not change the results
			#m = lmer(flooded ~ scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female), d) 
			#m = lmer(flooded ~ scale(lat)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female), d) 
		
		pred=c('Intercept','lat','cycle','cos','sin')
	
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		og=rbind(oii,ri)
		}
	{# binomial model - does not really converge, so used without female ID
		#m = glmer(flooded ~scale(lat)+scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female), family = 'binomial', d) 
		m = glmer(flooded ~scale(lat)+scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c), family = 'binomial', d) 
	
		pred=c('Intercept','lat','cycle','cos','sin')
	
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='binomial',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,1)
			oi$lwr_r=round(oi$lwr,1)
			oi$upr_r=round(oi$upr,1)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='binomial',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
			ob=rbind(oii,ri)
		}
	{# export to excel
			
			sname = tempfile(fileext='.xls')
			wb = loadWorkbook(sname,create = TRUE)	
			createSheet(wb, name = "output")
			writeWorksheet(wb, rbind(og,ob), sheet = "output")
			saveWorkbook(wb)
			shell(sname)
		}
  }
	{# later delete	
		AIC(m2,m1)
		anova(m2,m1)
	summary(m)
	summary(glht(ms))
	plot(allEffects(msl))
	plot(allEffects(m))
	plot(allEffects(m2))
	plot(allEffects(m1, x.var = 'rad_st'))
	
		anova(ms2,ms)
		#summary(rePCA(ms))
		summary(ms)
		summary(glht(ms))
		ggplot(ranef(ms)$year, aes(x=ranef(ms)$year))+geom_histogram()
	}
		{# model assumptions gaussian
			m = lmer(flooded ~ scale(lat)+scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female),d)
			mean(as.numeric(unlist(ranef(m)$year[1])))
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_flood_gaus_N.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
			par(mfrow=c(5,3))
			
			plot(fitted(m), jitter(d$flooded, amount=0.05), xlab="Fitted values", ylab="Flooded", las=1, cex.lab=1.2, cex=0.8)
				abline(0,1, lty=3)
				t.breaks <- cut(fitted(m), quantile(fitted(m)))
				means <- tapply(d$flooded, t.breaks, mean)
				semean <- function(x) sd(x)/sqrt(length(x))
				means.se <- tapply(d$flooded, t.breaks, semean)
				points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
				segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
			
			mtext("lmer(flooded ~ scale(lat)+scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female),d)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$female[1]), main = " female")
			qqline(unlist(ranef(m)$female[1]))			
			
			qqnorm(unlist(ranef(m)$st_cycle[1]), main = " st_cycle")
			qqline(unlist(ranef(m)$st_cycle[1]))
			
			scatter.smooth(resid(m)~sin(d$rad_st));abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(d$rad_st));abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~d$rad_st);abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~scale(d$lat));abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~scale(d$st_cycle_c));abline(h=0, lty=2, col='red')
			
			#plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
						
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			spdata=data.frame(resid=resid(m), x=d$lon, y=d$lat)
				spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))				
				cex_=c(1,1.5,2,2.5,3)#cex_=c(1,2,3,3.5,4)
				spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
						
			plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
			
			spp = spdata[spdata$resid>0,]
			plot(spp$x, spp$y,col=spp$col, cex=as.numeric(spp$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
						
			spp = spdata[spdata$resid>=0,]
			plot(spp$x, spp$y,col=spp$col, cex=as.numeric(spp$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
						
			if(PNG == TRUE) {dev.off()}
			
			d$res =resid(m)
			ggplot(d, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
			if(PNG == TRUE) {ggsave(paste(outdir,'mod_ass_flood_gaus_res_year.png',sep=''))}
			#ggplot(d, aes(x = days_after_st, y = res, col = factor(st_cycle))) + geom_point()	+ facet_grid(year ~ .)	
	}
		{# model assumptions binomial model
			m = glmer(flooded ~scale(lat)+scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female), family = 'binomial', d) 
			
			If(PNG == TRUE) {png(paste(outdir,"mod_ass_flood_binom.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
				
			par(mfrow=c(5,3))
						
			#scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			#scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
						
			plot(fitted(m), jitter(d$flooded, amount=0.05), xlab="Fitted values", ylab="Flooded", las=1, cex.lab=1.2, cex=0.8)
				abline(0,1, lty=3)
				t.breaks <- cut(fitted(m), quantile(fitted(m)))
				means <- tapply(d$flooded, t.breaks, mean)
				semean <- function(x) sd(x)/sqrt(length(x))
				means.se <- tapply(d$flooded, t.breaks, semean)
				points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
				segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
			mtext("glmer(flooded ~scale(lat)+scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female), family = 'binomial', d)", side = 3, line = -2, cex=0.7,outer = TRUE)	
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$female[1]), main = " female")
			qqline(unlist(ranef(m)$female[1]))			
			
			qqnorm(unlist(ranef(m)$st_cycle[1]), main = " st_cycle")
			qqline(unlist(ranef(m)$st_cycle[1]))
			
			scatter.smooth(resid(m)~sin(d$rad_st));abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(d$rad_st));abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~d$rad_st);abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~scale(d$lat));abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~scale(d$st_cycle_c));abline(h=0, lty=2, col='red')
			
			#plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
						
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			spdata=data.frame(resid=resid(m), x=d$lon, y=d$lat)
				spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))				
				cex_=c(1,1.5,2,2.5,3)#cex_=c(1,2,3,3.5,4)
				spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
						
			plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
			
			spp = spdata[spdata$resid>0,]
			plot(spp$x, spp$y,col=spp$col, cex=as.numeric(spp$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
						
			spp = spdata[spdata$resid>=0,]
			plot(spp$x, spp$y,col=spp$col, cex=as.numeric(spp$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
						
			if(PNG == TRUE) {dev.off()}
			
			d$res =resid(m)
			ggplot(d, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
			if(PNG == TRUE) {ggsave(paste(outdir,'mod_ass_flood_binom_res_year.png',sep=''))}
	}
}
{# NEST INITIATION
  {# prepare data
	dd$days_after_r = ceiling(dd$days_after_st)
	dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$l_n = 1:nrow(dd)
	dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
	###dd$rad_st_yc= 2*dd$laid_j_yc*pi/14.75
	###dd$rad_m_yc= 2*dd$laid_j_yc*pi/(14.75*2)
	dd$year_= as.factor(dd$year)
	###dd$y_cl_fs = paste(dd$year, dd$st_cycle,dd$first_second)
	###dd$y_cl = paste(dd$year, dd$st_cycle)
	
	dd = ddply(dd,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)#standardize within year
		###dd$st_cycle_cf = as.factor(dd$st_cycle_c)
		
	##dd$y_cl_fs = paste(dd$year, dd$st_cycle_c,dd$first_second)
	##dd$y_cl = paste(dd$year, dd$st_cycle_c)	
	}	
	{# explore - not in paper
		require(lomb)
		lsp(dd$n_nest, from = 1, to = 17, type = 'period')
		
		densityplot(~dd$n_nest)
		ggplot(nn,aes(x = st_cycle_c))+geom_density()
		ggplot(nn,aes(x = st_cycle_c))+geom_density()+facet_wrap(~year)
		ggplot(nn,aes(x = st_cycle_c))+geom_histogram(bins=8)
		ggplot(nn,aes(x = st_cycle_c))+geom_histogram(bins=8)+facet_wrap(~year)

		ggplot(nn,aes(x = days_after_st_r))+geom_density()
		ggplot(nn,aes(x = days_after_st_r))+geom_density()+facet_wrap(~year)
		ggplot(nn,aes(x = days_after_st_r))+geom_histogram(bins=16)
		ggplot(nn,aes(x = days_after_st_r))+geom_histogram(bins=16)+facet_wrap(~year)
		
		ggplot(dd,aes(x = n_nest))+geom_density()+facet_wrap(~year)
		ggplot(dd, aes(x = st_cycle_c, y = n_nest)) + geom_point() +stat_smooth()
		ggplot(dd, aes(x = factor(st_cycle_c), y = n_nest)) + geom_boxplot()
		ggplot(dd, aes(x = st_cycle_c, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()
		#ggplot(dd, aes(x = st_cycle_c, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)
		ggplot(dd, aes(x = st_cycle_c, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ ., scales = 'free_y')
	   # n_nest~st
		ggplot(dd, aes(x = days_after_st, y = n_nest, col = st_cycle_c)) + geom_point() +stat_smooth()+facet_grid(year ~ ., scales = 'free_y')+ylab('# of initiated nests') + xlab ('Days after last spring tide')
		ggplot(dd, aes(x = days_after_st, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)+ylab('# of initiated nests') + xlab ('Days after last spring tide') 
		
		ggplot(dd, aes(x = days_after_st, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)+ylab('# of initiated nests') + xlab ('Days after last spring tide')+ coord_cartesian(ylim = c(0,5))

		ggplot(dd, aes(x = days_after_st, y = n_nest, col = factor(st_cycle_c))) + geom_point() +stat_smooth(se = FALSE)+facet_grid(year ~ ., scales = 'free_y')+ylab('# of initiated nests') + xlab ('Days after last spring tide')
	
		ggplot(dd[!(dd$year=='2008' & dd$st_cycle_c==6),], aes(x = days_after_st, y = n_nest, col = as.integer(st_cycle_c), group=st_cycle_c)) + scale_colour_gradient(name = "Spring tide cycle", low = "deepskyblue", high = "orange")+stat_smooth(se = FALSE)+facet_grid(year ~ ., scales = 'free_y')+ylab('# of initiated nests') + xlab ('Days after last spring tide')
		
		dd8 = dd[dd$year=='2008',]
		table(dd8$st_cycle_c)
		ggplot(dd8, aes(x = days_after_st, y = n_nest)) + geom_point()+ scale_colour_gradient(name = "category", low = "deepskyblue", high = "orange")+xlab ('Days after last spring tide')+facet_grid(st_cycle_c ~ ., scales = 'free_y')+stat_smooth(se = FALSE)+ylab('# of initiated nests')
	
	
	#ggsave(paste(outdir,'#of_nests_given_spring_tide_cycle_ALL-DAYS.png',sep=''))
	#ggsave(paste(outdir,'#of_nests_given_spring_tide_cycle_ALL-DAYS_zoomed.png',sep=''))
		
		ggplot(dd,aes(x = as.factor(year), y = max_tide_height))+geom_boxplot()
		ggplot(dd,aes(col = as.factor(year), x = max_tide_height))+geom_density()
		ggplot(dd,aes(col = as.factor(year), x = max_year_z))+geom_density()
		ggplot(dd,aes(x = max_tide_height, fill = year_))+geom_density(alpha = 0.2)
	}
  {# Supplementary Table S2
	 {# gaussian model complex
		m = lmer(n_nest ~ scale(st_cycle_c)*cos(rad_st)+ scale(st_cycle_c)*sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
		#m2 = lmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + ( sin(rad_st)+cos(rad_st)|year/st_cycle_c), dd) 
		
		pred=c('Intercept','cycle','cos','sin','cycle:cos','cycle:sin')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogc=rbind(oii,ri)
			
	}
	 {# gaussian model simple
		m = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
		#m2 = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) +( sin(rad_st)+cos(rad_st)|year/st_cycle_c), dd) 
		
		pred=c('Intercept','cycle','cos','sin')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogs=rbind(oii,ri)
			
	}
	 {# poisson model complex
		m = glmer(n_nest ~  scale(st_cycle_c)*cos(rad_st)+ scale(st_cycle_c)*sin(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		#m2 = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + ( sin(rad_st)+cos(rad_st)|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','cos','sin','cycle:cos','cycle:sin')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		opc=rbind(oii,ri)
			
	}
	 {# poisson model simple
		m = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		#m2 = glmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) +( sin(rad_st)+cos(rad_st)|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','cos','sin')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ops=rbind(oii,ri)
			
	}
	 {# export to excel
			
			sname = tempfile(fileext='.xls')
			wb = loadWorkbook(sname,create = TRUE)	
			createSheet(wb, name = "output")
			writeWorksheet(wb, rbind(ogc,ogs,opc,ops), sheet = "output")
			saveWorkbook(wb)
			shell(sname)
		}
  }
		{# model assumptions gaussian complex
			m = lmer(n_nest ~ st_cycle_c *cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_N-nest_gausComplex.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
				
			par(mfrow=c(5,3))
			scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col = 'red')
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
			plot(fitted(m),dd$n_nest,xlab="Fitted values", ylab="# initiated nest", las=1, cex.lab=1.2, cex=0.8, col ='grey')
					abline(0,1, lty=3,col='red')
				
			
			mtext("lmer(n_nest ~ st_cycle_c *cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second), dd)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$`st_cycle_c:year`[1]), main = "`st_cycle_c:year`")
			qqline(unlist(ranef(m)$`st_cycle_c:year`[1]))			
			
			qqnorm(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]), main = "`first_second:(st_cycle_c:year)`")
			qqline(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]))
			
			scatter.smooth(resid(m)~sin(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$rad_st, col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$st_cycle_c, col = 'grey');abline(h=0, lty=2, col='red')
			
			dd$res = resid(m)
			#plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
						
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
		if(PNG == TRUE) {dev.off()}
			
			ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
				ggsave(paste(outdir,'mod_ass_N-nest_gaus_res_year.png',sep=''))
			#ggplot(d, aes(x = days_after_st, y = res, col = factor(st_cycle))) + geom_point()	+ facet_grid(year ~ .)	
	
	}
		{# model assumptions gaussian simple
			m = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_N-nest_gausSimple.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
				
			par(mfrow=c(5,3))
			scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col = 'red')
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
			plot(fitted(m),dd$n_nest,xlab="Fitted values", ylab="# initiated nest", las=1, cex.lab=1.2, cex=0.8, col ='grey')
					abline(0,1, lty=3,col='red')
				
			
			mtext("lm = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year",col='grey')
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$`st_cycle_c:year`[1]), main = "`st_cycle_c:year`",col='grey')
			qqline(unlist(ranef(m)$`st_cycle_c:year`[1]))			
			
			qqnorm(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]), main = "`first_second:(st_cycle_c:year)`",col='grey')
			qqline(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]))
			
			scatter.smooth(resid(m)~sin(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$rad_st, col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$st_cycle_c, col = 'grey');abline(h=0, lty=2, col='red')
			
			dd$res = resid(m)
			#plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
						
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
		if(PNG == TRUE) {dev.off()}
			
			ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
				ggsave(paste(outdir,'mod_ass_N-nest_gausSimple_res_year.png',sep=''))
			#ggplot(d, aes(x = days_after_st, y = res, col = factor(st_cycle))) + geom_point()	+ facet_grid(year ~ .)	
	
	}
		{# model assumptions poisson complex
			m = glmer(n_nest ~  st_cycle_c*cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
			
			
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_N-nest_poisComplex.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
			par(mfrow=c(5,3))
			scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col = 'red')
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
			plot(fitted(m),dd$n_nest,xlab="Fitted values", ylab="# initiated nest", las=1, cex.lab=1.2, cex=0.8, col ='grey')
					abline(0,1, lty=3,col='red')
				
			
			mtext("glmer(n_nest ~  st_cycle_c*cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$`st_cycle_c:year`[1]), main = "`st_cycle_c:year`")
			qqline(unlist(ranef(m)$`st_cycle_c:year`[1]))			
			
			qqnorm(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]), main = "`first_second:(st_cycle_c:year)`")
			qqline(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]))
			
			scatter.smooth(resid(m)~sin(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$rad_st, col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$st_cycle_c, col = 'grey');abline(h=0, lty=2, col='red')
			
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
		if(PNG == TRUE) {dev.off()}
			
			ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
				ggsave(paste(outdir,'mod_ass_N-nest_poisComplex_res_year.png',sep=''))
	
	}
		{# model assumptions poisson simple
			m = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
			
			
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_N-nest_poisSimple.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
			par(mfrow=c(5,3))
			scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col = 'red')
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
			plot(fitted(m),dd$n_nest,xlab="Fitted values", ylab="# initiated nest", las=1, cex.lab=1.2, cex=0.8, col ='grey')
					abline(0,1, lty=3,col='red')
				
			
			mtext("glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$`st_cycle_c:year`[1]), main = "`st_cycle_c:year`")
			qqline(unlist(ranef(m)$`st_cycle_c:year`[1]))			
			
			qqnorm(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]), main = "`first_second:(st_cycle_c:year)`")
			qqline(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]))
			
			scatter.smooth(resid(m)~sin(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$rad_st, col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$st_cycle_c, col = 'grey');abline(h=0, lty=2, col='red')
			
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
		if(PNG == TRUE) {dev.off()}
			
			ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
				ggsave(paste(outdir,'mod_ass_N-nest_poisSimple_res_year.png',sep=''))
	
	}
	{# later DELETE
	{# model USE
				
		m1 = lmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year/st_cycle_c/first_second), dd) 
		m1s = lmer(n_nest ~ sin(rad_st) + cos(rad_st)+st_cycle_c + (1|year/st_cycle_c/first_second), dd) 
		anova(m1,m1s)
		
		ms = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
		m = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year)+ (1|l_n), family = 'poisson',dd) 
		anova(ms,m)
			summary(m)
	summary(glht(m1))
	plot(allEffects(ms))
	plot(allEffects(m1s))
	plot(allEffects(m1, x.var = 'rad_st'))
	
		
		# simple
		m = glmer(n_nest ~ st_cycle_c+sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
		summary(rePCA(m))
		#mx = glmer(n_nest ~ st_cycle_c+sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd) 
		
		plot(allEffects(m))
		summary(m)
		# complex
			mc = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year)+ (1|l_n), family = 'poisson',dd) 
			#mc = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
			plot(allEffects(mc, x.var = 'rad_st'))
		
	acf(resid(m1), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		summary(m)
		summary(glht(m))
		plot(allEffects(m))	
		plot(Effect(m, focal.predictor = 'rad_st:st_cycle'))	
		#m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle),family = 'poisson', dd) 
	dd$res = resid(m)
	plot(unlist(ranef(m)$y_cl_fs[1])~c(1:nrow(ranef(m)$y_cl_fs[1])), col = dd$first_second)
	plot(unlist(ranef(m)$st_cycle[1])~c(1:nrow(ranef(m)$st_cycle[1])))
	plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
	plot(resid(m)~cos(dd$rad_st))
	plot(resid(m)~sin(dd$rad_st))				
	plot(resid(m)~dd$rad_st)
	ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
	ggplot(dd, aes(x = days_after_st, y = res, col = factor(st_cycle))) + stat_smooth(se=FALSE, alpha =0.3)	+ geom_point()+  facet_grid(year ~ .)								
	#### USE
	}
	{# per cycle (last 8th cycle not used - only 10 datapoints)
		l = list()
		for(i in 1:7){
			print(i)
			if(i==3){m =glmer(n_nest ~ sin(rad_st) + cos(rad_st)+ (1|year) + (1|l_n), family = 'poisson',dd[dd$st_cycle_c%in%c(i),])}
			if(i==5){m =glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|l_n), family = 'poisson',dd[dd$st_cycle_c%in%c(i),])}
			if(i%in%c(1,2,4,6,7)){
			m =glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year/first_second)+ (1|l_n), family = 'poisson',dd[dd$st_cycle_c%in%c(i),])
					}					
			nsim <- 5000
			bsim <- sim(m, n.sim=nsim) 
			v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for		
					newD=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(j in 1:nsim) predmatrix[,j] <- exp(X%*%bsim@fixef[j,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD
					pp$st_cycle = i
					l[[i]] = pp
			}
		s = do.call(rbind,l)
		s$n_nest = s$pred
		s$st_cycle_c = as.factor(s$st_cycle)
		ggplot(s,aes(y = n_nest, x = days_after_st))+geom_line()+  facet_wrap(~st_cycle_c)+
		geom_ribbon(data=s,aes(ymin=lwr,ymax=upr),alpha=0.3)
		
	}
	}
  
  {# Figure ISa
	{# raw data
			x2 = ddply(dd,.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
			x1 = ddply(dd[dd$st_cycle_c == 1,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x3 = ddply(dd[dd$st_cycle_c == 3,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x5 = ddply(dd[dd$st_cycle_c == 5,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			

		# dummy line to set legend to max 150 nests
			xx = data.frame(days_after_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			x2 = rbind(xx,x2)
			x1 = rbind(xx,x1)
			x3 = rbind(xx,x3)
			x5 = rbind(xx,x5)
			
	}
	{# predictions
	   {# gauss - interaction
			m = lmer(n_nest ~ st_cycle_c *cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second), dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	days_after_st	
					newD1=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 1)
					newD2=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 3)
					newD3=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gi=newD	
	}
       {# poiss - interaction
			m = glmer(n_nest ~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for		days_after_st
					newD1=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 1)
					newD2=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 3)
					newD3=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st),data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pci=newD	
	}
     }
	{# gaussina interaction
	   if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_gausInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
		par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
		plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
		axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
		#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
		mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
		mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
		mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
		mtext("a",side=3,line=-0.4, adj=-0.26,cex=0.7, las=1, col='grey30',font=2)
						
		gii = gi[gi$st_cycle_c ==1,]
		polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
			rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
		lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
		symbols( (x1$days_after_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
		axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
		#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
		mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
		#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
		mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
		gii = gi[gi$st_cycle_c ==3,]
		polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
				rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
		lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
		symbols( (x3$days_after_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
		axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
		#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
		mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
		#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
		mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
		gii = gi[gi$st_cycle_c ==5,]
		polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
			rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
		lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
		symbols((x5$days_after_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	{# poisson interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_poissInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==1,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x1$days_after_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==3,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x3$days_after_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = pci[pci$st_cycle_c ==5,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols((x5$days_after_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	
	}
  {# Figure ISb
	 {# raw data
			dd$max_tide_height_r = round(dd$max_tide_height,-1)
		
			x2 = ddply(dd,.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
			x1 = ddply(dd[dd$st_cycle_c == 1,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x3 = ddply(dd[dd$st_cycle_c == 3,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x5 = ddply(dd[dd$st_cycle_c == 5,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			

		# dummy line to set legend to max 150 nests
			xx = data.frame(max_tide_height_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			x2 = rbind(xx,x2)
			x1 = rbind(xx,x1)
			x3 = rbind(xx,x3)
			x5 = rbind(xx,x5)
	
	}
	 {# predictions
	   {# gauss - interaction
			m = lmer(n_nest ~ st_cycle_c *max_tide_height + (1|year/st_cycle_c), dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	days_after_st	
					newD1=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 1)
					
					newD2=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 3)
					newD3=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c *max_tide_height,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gi=newD	
	}
       {# poiss - interaction
			dd$st_cycle_z=scale(dd$st_cycle_c)
			m = glmer(n_nest ~  st_cycle_z*max_tide_z + (1|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for	days_after_st	
					newD1=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 1, st_cycle_z = -1.595432)
					newD2=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 3, st_cycle_z = -0.4602477)
					newD3=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 5, st_cycle_z = 0.6749365)
					newD = rbind(newD1,newD2,newD3)
					newD$max_tide_z = scale(newD$max_tide_height)
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_z *max_tide_z,data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pci=newD	
	}
     }
	 
	 {# gaussina interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_gausInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						mtext("b",side=3,line=-0.4, adj=-0.26,cex=0.7, las=1, col='grey30',font=2)

						gii = gi[gi$st_cycle_c ==1,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x1$max_tide_height_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = gi[gi$st_cycle_c ==3,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x3$max_tide_height_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = gi[gi$st_cycle_c ==5,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols((x5$max_tide_height_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	 {# poisson interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_poissInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==1,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x1$max_tide_height_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==3,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x3$max_tide_height_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = pci[pci$st_cycle_c ==5,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols((x5$max_tide_height_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
}

  {# Supplementary Table S3
	 {# gaussian model complex
		m = lmer(n_nest ~ scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c/first_second), dd)
		pred=c('Intercept','cycle','tide','cycle:tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogc=rbind(oii,ri)
			
	}
	 {# gaussian model simple
		m = lmer(n_nest ~ scale(st_cycle_c) +scale(max_tide_height) + (1|year/st_cycle_c/first_second), dd) 
		
		pred=c('Intercept','cycle','tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogs=rbind(oii,ri)
			
	}
	 {# poisson model complex
		m = glmer(n_nest ~  scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','tide','cycle:tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		opc=rbind(oii,ri)
			
	}
	 {# poisson model simple
		m = glmer(n_nest ~ scale(st_cycle_c) +scale(max_tide_height) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		#m2 = glmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) +( sin(rad_st)+cos(rad_st)|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ops=rbind(oii,ri)
			
	}
	 {# export to excel
			
			sname = tempfile(fileext='.xls')
			wb = loadWorkbook(sname,create = TRUE)	
			createSheet(wb, name = "output")
			writeWorksheet(wb, rbind(ogc,ogs,opc,ops), sheet = "output")
			saveWorkbook(wb)
			shell(sname)
		}
  }
		{# add model assumptions
			
		}
		{# models later delete
	m = glmer(n_nest ~ scale(st_cycle_c)+scale(max_year_z) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	m = glmer(n_nest ~ scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	mz = glmer(n_nest ~ scale(st_cycle_c)*scale(max_year_z) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	
	
	anova(m,mz)
	summary(m)
	m = lmer(n_nest ~ scale(st_cycle_c)+scale(max_year_z) + (1|year/st_cycle_c) , dd[!is.na(dd$max_tide_height),])
	m = lmer(n_nest ~ scale(st_cycle_c)*scale(max_year_z) + (1|year/st_cycle_c) , dd[!is.na(dd$max_tide_height),])
	
	m = glmer(n_nest ~ max_yr_cl_z + (1|year) +(1|st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_yr_cl_z + (1|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_yr_cl_z + (max_yr_cl_z|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_tide_height + (max_tide_height|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ poly(max_tide_height,2) + (1|year) +(1|st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ poly(max_yr_cl_z,2) + (1|year/st_cycle/first_second) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_yr_cl_z),])
	m = glmer(n_nest ~ poly(max_yr_cl_z,2)*first_second + (1|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_yr_cl_z),])
	summary(m)
	summary(glht(m))
	plot(allEffects(m))
	plot(allEffects(m, x.var = 'max_year_z'))
	}

}

{# Figure F
  {# run first	
   {# flooding
	{# prepare	
	d = nn[nn$fate%in%c("hatch","unhatch","tide"),] # only flooded, hatched or unhatched nests
	summary(factor(d$fate))
	d$flooded = ifelse(d$fate=='tide',1,0)
	d$flooded_ = ifelse(d$fate=='tide','yes','no')
	d$year_ = as.factor(d$year)
	d$laid_j=as.POSIXlt(d$laid, format = "%d%b%y")$yday
	d$days_after_r = ceiling(d$days_after_st)
	d$rad_st = 2*d$days_after_st*pi/d$dur
		###d$y_cl=factor(paste(d$year,d$st_cycle))
		###dd$y_cl_fs = paste(dd$year, dd$st_cycle,dd$first_second)
	
	d = ddply(d,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1) # center within year
		
	d$st_cycle_c[d$st_cycle_c==7]=6 # only 2 datapoints for cycle 7, so make it 6 summary(factor(d$st_cycle_c))
	###d = ddply(d,.(year), transform, max_year_z = scale(max_tide_height)) # standardize tide within year
	###d = ddply(d,.(year, st_cycle), transform, max_yr_cl_z = scale(max_tide_height)) # standardize tide within year and cycle
   }	
	{# predictions gaus
		m = lmer(flooded ~ lat+st_cycle_c+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female),d)
		#m = lmer(flooded ~ st_cycle_c+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female),d)
			nsim <- 5000
			bsim <- sim(m, n.sim=nsim)  
				
		# coefficients
			#v = fixef(m)
			v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
			
		# values to predict for	cycle after spring tide	
			newD=data.frame(days_after_st = mean(d$days_after_st),lat = mean(d$lat),st_cycle_c = seq(1,6,length.out=300)) #,
			newD$rad_st =2*newD$days_after_st*pi/14.77 # mean duration or spring-tide in our data #0.1#
		
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ lat+st_cycle_c+sin(rad_st)+cos(rad_st),data=newD)	
						
		# calculate predicted values and creditability intervals
			newD$pred <-(X%*%v) 
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
				predmatrix[predmatrix < 0] <- 0
				newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
				newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
				newD$pred <- apply(predmatrix, 1, quantile, prob=0.5)
		pfc=newD	
		
		# values to predict for	days after spring tide	
			newD=data.frame(days_after_st = seq(1, 16,length.out=300),lat = mean(d$lat), st_cycle_c = mean(d$st_cycle_c))  #
			newD$rad_st =2*newD$days_after_st*pi/14.77
						
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ lat+st_cycle_c+sin(rad_st)+cos(rad_st),data=newD)	
						
		# calculate predicted values and creditability intervals
			newD$pred <-(X%*%v) 
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
				predmatrix[predmatrix < 0] <- 0
				newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
				newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
				newD$pred <- apply(predmatrix, 1, quantile, prob=0.5)
		pfs=newD

	}
	{# raw data
		#x = ddply(d,.(year, days_after_st), summarise, mean_ = mean(flooded), sd_ = sd(flooded), n = length(flooded))
		#	y = data.frame(year = unique(d$year), col_ =1:length(unique(d$year)), stringsAsFactors = FALSE)
		#	x$col_ = y$col_[match(x$year, y$year)]
		
		xfs = ddply(d,.(days_after_r), summarise, mean_ = mean(flooded),  n = length(flooded))
		# add dummy variable to ensure that the circle size is same in all plots 
			xfs = rbind(data.frame(days_after_r = -100, mean_=-10 , n = 150, stringsAsFactors=FALSE),xfs)
		xfc = ddply(d,.(st_cycle_c), summarise, mean_ = mean(flooded),n = length(flooded))
		# add dummy variable to ensure that the circle size is same in all plots 
			xfc = rbind(data.frame(st_cycle_c = -10, mean_=-10, n = 150, stringsAsFactors=FALSE),xfc)	
	}
    }
   {# nest initiation
    {# prepare
	dd$days_after_r = ceiling(dd$days_after_st)
	dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$l_n = 1:nrow(dd)
	dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
	###dd$rad_st_yc= 2*dd$laid_j_yc*pi/14.75
	###dd$rad_m_yc= 2*dd$laid_j_yc*pi/(14.75*2)
	dd$year_= as.factor(dd$year)
	###dd$y_cl_fs = paste(dd$year, dd$st_cycle,dd$first_second)
	###dd$y_cl = paste(dd$year, dd$st_cycle)
	
	dd = ddply(dd,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)#standardize within year
		###dd$st_cycle_cf = as.factor(dd$st_cycle_c)
		
	##dd$y_cl_fs = paste(dd$year, dd$st_cycle_c,dd$first_second)
	##dd$y_cl = paste(dd$year, dd$st_cycle_c)	
	}	
	{# predictions gaus
		m = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
			nsim <- 5000
			bsim <- sim(m, n.sim=nsim)  
				
	# coefficients
		v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
		ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
	
	# values to predict for	st_cylce_c Effect	
		newD=data.frame(days_after_st = mean(dd$days_after_st),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
		newD$rad_st =2*newD$days_after_st*pi/14.77
	
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <-(X%*%v)
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
				predmatrix[predmatrix < 0] <- 0
				newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
				newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
		pnc=newD	
					
	# values to predict for	days_after_st	
		newD=data.frame(days_after_st = seq(1,16, length.out=300),st_cycle_c = mean(dd$st_cycle_c))
		newD$rad_st =2*newD$days_after_st*pi/14.77
	
		# exactly the model which was used has to be specified here 
			X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
		# calculate predicted values and creditability intervals
			newD$pred <-(X%*%v)
			predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
			for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
				predmatrix[predmatrix < 0] <- 0
				newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
				newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
		pns=newD	
	}
	{# raw data
		xns = ddply(dd,.(days_after_r), summarise, mean_ = mean(n_nest),  n = length(year))
			# add dummy variable to ensure that the circle size is same in all plots 
			xns = rbind(data.frame(days_after_r = -100, mean_=-10 , n = 150, stringsAsFactors=FALSE),xns)
		
		xnc = ddply(dd,.(st_cycle_c), summarise, mean_ = mean(n_nest),  n = length(year))
			# add dummy variable to ensure that the circle size is same in all plots 
			xnc = rbind(data.frame(st_cycle_c = -10, mean_=-10, n = 150, stringsAsFactors=FALSE),xnc)	
 }
	}
  }
  {# plot
	if(PNG == TRUE) {
		png(paste(outdir,"Figure_FgauDens.png", sep=""), width=3.5,height=3.8,units="in",res=600) 
		}else{
		dev.new(width=3.5,height=1.5*2+0.5)
					}	
	
	par(mar=c(0.25,0.5,0,0.1),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
	
	layout(mat = matrix(c(1,2,3,4,5,6,7,8,9,10,0,0),4), widths = c(1.5,1.5,.5), heights = c(0.3,1.45,0.3,1.45))
	
	#A)
	# density flooded season
		par(mar=c(0.25,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=FALSE)
		plot(density(d$st_cycle_c[d$flooded_=='no']), col = 'grey', xaxt='n',yaxt='n', xlim = c(0.5,8), ylim =c(0,0.376), main =NA,zero.line=FALSE)
		lines(density(d$st_cycle_c[d$flooded_=='yes']), col = 'red')
		mtext("a",side=3,line=-.4, cex=0.7, adj = 1,las=1, col='grey30', font = 2)
	# prediction flooded season
		par(mar=c(2.2,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
		plot(pfc$pred~pfc$st_cycle_c, 
						xlim=c(0.5,8), ylim=c(-0.07,1),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = '',type='n')
						
			polygon(c(pfc$st_cycle_c, rev(pfc$st_cycle_c)), c(pfc$lwr, 
					rev(pfc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
			lines(pfc$st_cycle_c, pfc$pred, col=col_l,lwd=1)
			symbols( (xfc$st_cycle_c),(xfc$mean_), circles=sqrt(xfc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
			
			axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
			axis(2, at=seq(0,1, by = 0.1), label=c('0%','','20%','','40%','','60%','','80%','','100%'), mgp=c(0,0.1,0))
			mtext("Spring-tide cycle #\nnest initiated",side=1,line=0.9, cex=0.6, las=1, col='grey30', xpd = TRUE)
			mtext("Flooded nests",side=2,line=1, cex=0.6, las=3, col='grey30')
			
	# C		
	# density # of nests season
		par(mar=c(0.25,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=FALSE)
		plot(density(nn$st_cycle_c), col = 'grey', xaxt='n',yaxt='n', xlim = c(1,8), ylim =c(0,0.3136), main =NA,zero.line=FALSE)
		mtext("c",side=3,line=-.4, cex=0.7, adj = 1,las=1, col='grey30', font = 2)
	
	# prediction # of nests season
		par(mar=c(2.2,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
		plot(pnc$pred~pnc$st_cycle_c, 
				xlim=c(0.5,8), ylim=c(-0.025,1.6),
				xaxt='n',
				yaxt='n',
				xaxs = 'i', yaxs = 'i',
				ylab = "",xlab = '',type='n')
			
			polygon(c(pnc$st_cycle_c, rev(pnc$st_cycle_c)), c(pnc$lwr, 
					rev(pnc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
			lines(pnc$st_cycle_c, pnc$pred, col=col_l,lwd=1)
			symbols( (xnc$st_cycle_c),(xnc$mean_), circles=sqrt(xnc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
			
			axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
			axis(2, at=seq(0,1.6, by = 0.2), label=c('0.0','','0.4','','0.8','','1.2','','1.6'), mgp=c(0,0.2,0))
			mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
			mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
			
	# B
	# density flooded within cycle
		par(mar=c(0.25,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=FALSE)
		plot(density(d$days_after_st[d$flooded_=='no']), col = 'grey', xaxt='n',yaxt='n', xlim = c(0.5,16), ylim =c(0.008,0.1026781), main =NA,zero.line=FALSE)
		lines(density(d$days_after_st[d$flooded_=='yes']), col = 'red') #summary(density(d$days_after_st[d$flooded_=='yes'])$y)
		mtext("b",side=3,line=-.4, cex=0.7, adj = 1,las=1, col='grey30', font = 2)		
	
	# predictions flooded within cycle
		par(mar=c(2.2,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
		plot(pfs$pred~pfs$days_after_st, 
			xlim=c(0.5,16), ylim=c(-0.07,1),
			xaxt='n',
			yaxt='n',
			xaxs = 'i', yaxs = 'i',
			ylab = "",xlab = '',type='n')
						
			axis(1, at=seq(1,16, by = 3), label=TRUE, mgp=c(0,-0.20,0))
			#axis(2, at=seq(0,.4, by = 0.05), label=c('0%','','10%','','20%','','30%','','40%'), mgp=c(0,0.1,0))
			mtext("Days from spring tide\nnest initiated",side=1,line=0.9, cex=0.6, las=1, col='grey30', xpd = TRUE)
			#mtext("Flooded nests",side=2,line=1, cex=0.6, las=3, col='grey30')
			
			polygon(c(pfs$days_after_st, rev(pfs$days_after_st)), c(pfs$lwr, 
				rev(pfs$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
			lines(pfs$days_after_st, pfs$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xfs$days_after_r),(xfs$mean_), circles=sqrt(xfs$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
			#abline(h=0.09033613)#mean(d$flooded))
	# D
	# density # of nests season
		par(mar=c(0.25,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=FALSE)
		plot(density(nn$days_after_st), col = 'grey', xaxt='n',yaxt='n', xlim = c(0.5,16), ylim =c(0,0.07528), main =NA,zero.line=FALSE)
		mtext("d",side=3,line=-.4, cex=0.7, adj = 1,las=1, col='grey30', font = 2)
		
	# predictions # nests within cycle
		par(mar=c(2.2,0.5,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
		plot(pns$pred~pns$days_after_st, 
						xlim=c(0.5,16), ylim=c(-0.025,1.6),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = '',type='n')
															
			axis(1, at=seq(1,16, by = 3), label=TRUE, mgp=c(0,-0.20,0))
			#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
			mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
			#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
			polygon(c(pns$days_after_st, rev(pns$days_after_st)), c(pns$lwr, 
				rev(pns$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
			lines(pns$days_after_st, pns$pred, col=col_l,lwd=1)
			symbols((xns$days_after_r),(xns$mean_), circles=sqrt(xns$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					
	# legend 1
		par(mar=c(0.25,0.7,0,0.1),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
		plot(x=c(0,1), y=c(0,1), type='n',xaxt='n',yaxt='n',ylab = "",xlab = '')
			mtext('Non-flooded',side = 4,line=-1.4, padj=0,cex=0.5,las=1,col='grey', xpd=TRUE)
			mtext('Flooded',side = 4,line=-1.4, padj=1.6,cex=0.5,las=1,col='red', xpd=TRUE)
	
	# legend 2	
		#par(mar=c(2.2,0.5,0,0),ps=12, cex=1, font.main = 1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
		plot(x=c(0,1), y=c(0,1), type='n',xaxt='n',yaxt='n',ylab = "",xlab = '')
		
		mtext(expression(italic('N')*' nest:'),side = 4,line=-1.4, padj=-9,cex=0.5,las=1,col='grey30', xpd=TRUE) 
		symbols(c(0.2,0.2,0.2),c(0.85,0.85-0.15,0.85-0.15*2.1),circles=sqrt(c(50,100,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE)
		text(c(0.2,0.2,0.2)+0.7,c(0.85,0.85-0.15,0.85-0.15*2.1),labels=c(50,100,150), xpd=NA, cex=0.5,col='grey30')																	
	if(PNG == TRUE) {dev.off()}
  }
}  

{# Figure SZa - initiated nests
	 {# raw data
			x2 = ddply(dd,.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
			x1 = ddply(dd[dd$st_cycle_c == 1,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x3 = ddply(dd[dd$st_cycle_c == 3,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x5 = ddply(dd[dd$st_cycle_c == 5,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
		xc = ddply(dd,.(st_cycle_c), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
		# dummy line to set legend to max 150 nests
			xx = data.frame(days_after_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			x2 = rbind(xx,x2)
			x1 = rbind(xx,x1)
			x3 = rbind(xx,x3)
			x5 = rbind(xx,x5)
			
			xx = data.frame(st_cycle_c = -10, mean_=-10,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			xc = rbind(xx,xc)
	}
	 {# predictions
	  {# gauss - interaction
			m = lmer(n_nest ~ st_cycle_c *cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second), dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	days_after_st	
					newD1=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 1)
					newD2=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 3)
					newD3=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gi=newD	
	}
 }
	 {# plot
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_gausIntnew.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						mtext("a",side=3,line=-0.4, adj=-0.26,cex=0.7, las=1, col='grey30',font=2)
						
						gii = gi[gi$st_cycle_c ==1,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x1$days_after_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = gi[gi$st_cycle_c ==3,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x3$days_after_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = gi[gi$st_cycle_c ==5,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols((x5$days_after_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	}
{# Figure SZb  
	{# prepare data
	dd$rad_lst= 2*dd$day_j*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$rad_lm= 2*dd$day_j*pi/(14.75*2)
	dd$rad_m= 2*dd$days_after_st*pi/(14.75*2)
	dd$l_n = 1:nrow(dd)
	dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
	#dd$rad_st_yc= 2*dd$laid_j_yc*pi/14.75
	#dd$rad_m_yc= 2*dd$laid_j_yc*pi/(14.75*2)
	dd$year_= as.factor(dd$year)
	
	dd = ddply(dd,.(year), transform, max_year_z = scale(max_tide_height))
	dd = ddply(dd,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)
	dd$max_tide_z =scale(dd$max_tide_height)
	}	
	{# raw data
			dd$max_tide_height_r = round(dd$max_tide_height,-1)
		
			x2 = ddply(dd,.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
			x1 = ddply(dd[dd$st_cycle_c == 1,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x3 = ddply(dd[dd$st_cycle_c == 3,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x5 = ddply(dd[dd$st_cycle_c == 5,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
		xc = ddply(dd,.(st_cycle_c), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
		# dummy line to set legend to max 150 nests
			xx = data.frame(max_tide_height_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			x2 = rbind(xx,x2)
			x1 = rbind(xx,x1)
			x3 = rbind(xx,x3)
			x5 = rbind(xx,x5)
			
			xx = data.frame(st_cycle_c = -100, mean_=-10,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			xc = rbind(xx,xc)			
	}
	 {# predictions
	   {# gauss - interaction
			m = lmer(n_nest ~ st_cycle_c *max_tide_height + (1|year/st_cycle_c), dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	days_after_st	
					newD1=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 1)
					
					newD2=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 3)
					newD3=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c *max_tide_height,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gi=newD	
	}
       } 
	{# gaussina interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_gausIntnew.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						mtext("b",side=3,line=-0.4, adj=-0.26,cex=0.7, las=1, col='grey30',font=2)

						gii = gi[gi$st_cycle_c ==1,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x1$max_tide_height_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = gi[gi$st_cycle_c ==3,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x3$max_tide_height_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = gi[gi$st_cycle_c ==5,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols((x5$max_tide_height_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
} 


##### OLD #####
{# check
	y = nn[!is.na(nn$end_) & nn$fate == 'hatch' & !(!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & nn$clutch_size_found!=nn$clutch_size),]
	y$lay_inc = as.numeric(difftime(y$end_,y$laid,units = 'hours'))/24
	summary(as.numeric(difftime(y$end_,y$laid,units = 'days')))
	y = y[order(y$lay_inc),]
	y$lay_inc[order(y$lay_inc)]
	nrow(y[y$lay_inc<27|y$lay_inc>33,c('nest','clutch_size_found','clutch_size','laid','end_','lay_inc')])
	nrow(y[y$lay_inc>33,c('nest','clutch_size_found','clutch_size','laid','end_','lay_inc')])
}	


	{# plot gaussian
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_Fgaus.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$days_after_st, 
						xlim=c(-1,15), ylim=c(-0.035,0.4),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,.4, by = 0.05), label=c('0%','','10%','','20%','','30%','','40%'), mgp=c(0,0.1,0))
						mtext("Nest initiation\n[days from spring tide]",side=1,line=0.9, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("Flooded nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(pp$days_after_st, rev(pp$days_after_st)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(pp$days_after_st, pp$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
						# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.1, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(17,17,17)+0.01,c(0.26,0.26-0.04,0.26-0.04*2.25),circles=sqrt(c(20,40,60)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(17,17,17)+3,c(0.26,0.26-0.04,0.26-0.04*2.25),labels=c(20,40,60), xpd=NA, cex=0.5,col='grey30')													
							#text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90, xpd=FALSE) 
							
							#mtext('Predictions &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
		if(PNG == TRUE) {dev.off()}
	}			
	{# binomial plot
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_Fbin.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0.2,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
								
				plot(pb$pred~pb$days_after_st, 
						xlim=c(-1,15), ylim=c(-0.035,0.8),
						xaxt='n',yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flooding probability]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,.8, by = 0.2), label=c('0%','20%','40%','60%','80%'), mgp=c(0,0.1,0))
						mtext("Nest initiation\n[days from spring tide]",side=1,line=0.9, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("Flooded nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						polygon(c(pb$days_after_st, rev(pb$days_after_st)), c(pb$lwr, 
								rev(pb$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pb$days_after_st, pb$pred, col=col_m,lwd=1)
						
						symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_st),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=adjustcolor(col_f, alpha.f = 0.5), fg=col_p,add=TRUE) 
						
						
							
						if(PNG == TRUE) {dev.off()}
						
						
	}			
   }

   
  {# Figure I - initiated nests
	 {# raw data
			x2 = ddply(dd,.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
			x1 = ddply(dd[dd$st_cycle_c == 1,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x3 = ddply(dd[dd$st_cycle_c == 3,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x5 = ddply(dd[dd$st_cycle_c == 5,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
		xc = ddply(dd,.(st_cycle_c), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
		# dummy line to set legend to max 150 nests
			xx = data.frame(days_after_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			x2 = rbind(xx,x2)
			x1 = rbind(xx,x1)
			x3 = rbind(xx,x3)
			x5 = rbind(xx,x5)
			
			xx = data.frame(st_cycle_c = -10, mean_=-10,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			xc = rbind(xx,xc)
	}
	 {# predictions
	   {# gauss
			m = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	st_cylce_c Effect	
					newD=data.frame(days_after_st = mean(dd$days_after_st),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gsc=newD	
					
				# values to predict for	days_after_st	
					newD=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = mean(dd$st_cycle_c))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gs=newD	
	}
       {# poiss
			m = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for	st_cylce_c Effect	
					newD=data.frame(days_after_st = mean(dd$days_after_st),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					psc=newD				
				
				# values to predict for		days_after_st
					newD=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = mean(dd$st_cycle_c))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					ps=newD	
	}
       {# gauss - interaction
			m = lmer(n_nest ~ st_cycle_c *cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second), dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	days_after_st	
					newD1=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 1)
					newD2=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 3)
					newD3=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gi=newD	
	}
       {# poiss - interaction
			m = glmer(n_nest ~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for		days_after_st
					newD1=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 1)
					newD2=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 3)
					newD3=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st),data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pci=newD	
	}
     }
	 {# gaussina simple
	   {# plot ctcke
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_gausS_cycle.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gsc$pred~gsc$st_cycle_c, 
						xlim=c(0.5,8.5), ylim=c(0,1.5),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						polygon(c(gsc$st_cycle_c, rev(gsc$st_cycle_c)), c(gsc$lwr, 
								rev(gsc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gsc$st_cycle_c, gsc$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xc$st_cycle_c),(xc$mean_), circles=sqrt(xc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=FALSE) 
						
						axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,1.4, by = 0.2), label=TRUE, mgp=c(0,0.2,0))
						mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
												
									
		if(PNG == TRUE) {dev.off()}
	}			
	   {# plot day
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_gausS.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gs$pred~gs$days_after_st, 
						xlim=c(-1,15), ylim=c(0,1.5),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(gs$days_after_st, rev(gs$days_after_st)), c(gs$lwr, 
								rev(gs$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gs$days_after_st, gs$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					
						# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.3, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(15,15,15)+15*0.2,c(1.26,1.26-0.14,1.26-0.1*3.25),circles=sqrt(c(10,75,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(15,15,15)+15*0.4,c(1.26,1.26-0.14,1.26-0.1*3.25),labels=c(10,75,150), xpd=NA, cex=0.5,col='grey30')																				
						
		if(PNG == TRUE) {dev.off()}
	}			
	}
	 {# poisson simple	
	  {# plot ctcke
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_poisS_cycle.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(psc$pred~psc$st_cycle_c, 
						xlim=c(0.5,8.5), ylim=c(0,1.5),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						polygon(c(psc$st_cycle_c, rev(psc$st_cycle_c)), c(psc$lwr, 
								rev(psc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(psc$st_cycle_c, psc$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xc$st_cycle_c),(xc$mean_), circles=sqrt(xc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=FALSE) 
						
						axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,1.4, by = 0.2), label=TRUE, mgp=c(0,0.2,0))
						mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
																									
		if(PNG == TRUE) {dev.off()}
	}			
	  {# plot day
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_poisS.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(ps$pred~ps$days_after_st, 
						xlim=c(-1,15), ylim=c(0,1.5),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
					axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
					#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
					mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
					mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
					polygon(c(ps$days_after_st, rev(ps$days_after_st)), c(ps$lwr, 
								rev(ps$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(ps$days_after_st, ps$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.3, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(15,15,15)+15*0.2,c(1.26,1.26-0.14,1.26-0.1*3.25),circles=sqrt(c(10,75,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(15,15,15)+15*0.4,c(1.26,1.26-0.14,1.26-0.1*3.25),labels=c(10,75,150), xpd=NA, cex=0.5,col='grey30')										
						
		if(PNG == TRUE) {dev.off()}
	}			
	  			
	}
	 {# gaussina interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_gausInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						mtext("a",side=3,line=-0.4, adj=-0.26,cex=0.7, las=1, col='grey30',font=2)
						
						gii = gi[gi$st_cycle_c ==1,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x1$days_after_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = gi[gi$st_cycle_c ==3,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x3$days_after_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = gi[gi$st_cycle_c ==5,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols((x5$days_after_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	 {# poisson interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_poissInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==1,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x1$days_after_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==3,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x3$days_after_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = pci[pci$st_cycle_c ==5,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols((x5$days_after_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	
	}
 


{# NEST INITIATION
  {# prepare data
	dd$days_after_r = ceiling(dd$days_after_st)
	dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$l_n = 1:nrow(dd)
	dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
	###dd$rad_st_yc= 2*dd$laid_j_yc*pi/14.75
	###dd$rad_m_yc= 2*dd$laid_j_yc*pi/(14.75*2)
	dd$year_= as.factor(dd$year)
	###dd$y_cl_fs = paste(dd$year, dd$st_cycle,dd$first_second)
	###dd$y_cl = paste(dd$year, dd$st_cycle)
	
	dd = ddply(dd,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)#standardize within year
		###dd$st_cycle_cf = as.factor(dd$st_cycle_c)
		
	##dd$y_cl_fs = paste(dd$year, dd$st_cycle_c,dd$first_second)
	##dd$y_cl = paste(dd$year, dd$st_cycle_c)	
	}	
	{# explore - not in paper
		densityplot(~dd$n_nest)
		ggplot(nn,aes(x = st_cycle_c))+geom_density()
		ggplot(nn,aes(x = st_cycle_c))+geom_density()+facet_wrap(~year)
		ggplot(nn,aes(x = st_cycle_c))+geom_histogram(bins=8)
		ggplot(nn,aes(x = st_cycle_c))+geom_histogram(bins=8)+facet_wrap(~year)

		ggplot(nn,aes(x = days_after_st_r))+geom_density()
		ggplot(nn,aes(x = days_after_st_r))+geom_density()+facet_wrap(~year)
		ggplot(nn,aes(x = days_after_st_r))+geom_histogram(bins=16)
		ggplot(nn,aes(x = days_after_st_r))+geom_histogram(bins=16)+facet_wrap(~year)
		
		ggplot(dd,aes(x = n_nest))+geom_density()+facet_wrap(~year)
		ggplot(dd, aes(x = st_cycle_c, y = n_nest)) + geom_point() +stat_smooth()
		ggplot(dd, aes(x = factor(st_cycle_c), y = n_nest)) + geom_boxplot()
		ggplot(dd, aes(x = st_cycle_c, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()
		#ggplot(dd, aes(x = st_cycle_c, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)
		ggplot(dd, aes(x = st_cycle_c, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ ., scales = 'free_y')
	   # n_nest~st
		ggplot(dd, aes(x = days_after_st, y = n_nest, col = st_cycle_c)) + geom_point() +stat_smooth()+facet_grid(year ~ ., scales = 'free_y')+ylab('# of initiated nests') + xlab ('Days after last spring tide')
		ggplot(dd, aes(x = days_after_st, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)+ylab('# of initiated nests') + xlab ('Days after last spring tide') 
		
		ggplot(dd, aes(x = days_after_st, y = n_nest, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)+ylab('# of initiated nests') + xlab ('Days after last spring tide')+ coord_cartesian(ylim = c(0,5))

		ggplot(dd, aes(x = days_after_st, y = n_nest, col = factor(st_cycle_c))) + geom_point() +stat_smooth(se = FALSE)+facet_grid(year ~ ., scales = 'free_y')+ylab('# of initiated nests') + xlab ('Days after last spring tide')
	
		ggplot(dd[!(dd$year=='2008' & dd$st_cycle_c==6),], aes(x = days_after_st, y = n_nest, col = as.integer(st_cycle_c), group=st_cycle_c)) + scale_colour_gradient(name = "Spring tide cycle", low = "deepskyblue", high = "orange")+stat_smooth(se = FALSE)+facet_grid(year ~ ., scales = 'free_y')+ylab('# of initiated nests') + xlab ('Days after last spring tide')
		
		dd8 = dd[dd$year=='2008',]
		table(dd8$st_cycle_c)
		ggplot(dd8, aes(x = days_after_st, y = n_nest)) + geom_point()+ scale_colour_gradient(name = "category", low = "deepskyblue", high = "orange")+xlab ('Days after last spring tide')+facet_grid(st_cycle_c ~ ., scales = 'free_y')+stat_smooth(se = FALSE)+ylab('# of initiated nests')
	
	
	#ggsave(paste(outdir,'#of_nests_given_spring_tide_cycle_ALL-DAYS.png',sep=''))
	#ggsave(paste(outdir,'#of_nests_given_spring_tide_cycle_ALL-DAYS_zoomed.png',sep=''))
		
		ggplot(dd,aes(x = as.factor(year), y = max_tide_height))+geom_boxplot()
		ggplot(dd,aes(col = as.factor(year), x = max_tide_height))+geom_density()
		ggplot(dd,aes(col = as.factor(year), x = max_year_z))+geom_density()
		ggplot(dd,aes(x = max_tide_height, fill = year_))+geom_density(alpha = 0.2)
	}
	  
  {# Supplementary Table S2
	 {# gaussian model complex
		m = lmer(n_nest ~ scale(st_cycle_c)*cos(rad_st)+ scale(st_cycle_c)*sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
		#m2 = lmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + ( sin(rad_st)+cos(rad_st)|year/st_cycle_c), dd) 
		
		pred=c('Intercept','cycle','cos','sin','cycle:cos','cycle:sin')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogc=rbind(oii,ri)
			
	}
	 {# gaussian model simple
		m = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
		#m2 = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) +( sin(rad_st)+cos(rad_st)|year/st_cycle_c), dd) 
		
		pred=c('Intercept','cycle','cos','sin')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogs=rbind(oii,ri)
			
	}
	 {# poisson model complex
		m = glmer(n_nest ~  scale(st_cycle_c)*cos(rad_st)+ scale(st_cycle_c)*sin(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		#m2 = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + ( sin(rad_st)+cos(rad_st)|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','cos','sin','cycle:cos','cycle:sin')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		opc=rbind(oii,ri)
			
	}
	 {# poisson model simple
		m = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		#m2 = glmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) +( sin(rad_st)+cos(rad_st)|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','cos','sin')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ops=rbind(oii,ri)
			
	}
	 {# export to excel
			
			sname = tempfile(fileext='.xls')
			wb = loadWorkbook(sname,create = TRUE)	
			createSheet(wb, name = "output")
			writeWorksheet(wb, rbind(ogc,ogs,opc,ops), sheet = "output")
			saveWorkbook(wb)
			shell(sname)
		}
  }
		{# model assumptions gaussian complex
			m = lmer(n_nest ~ st_cycle_c *cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_N-nest_gausComplex.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
				
			par(mfrow=c(5,3))
			scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col = 'red')
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
			plot(fitted(m),dd$n_nest,xlab="Fitted values", ylab="# initiated nest", las=1, cex.lab=1.2, cex=0.8, col ='grey')
					abline(0,1, lty=3,col='red')
				
			
			mtext("lmer(n_nest ~ st_cycle_c *cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second), dd)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$`st_cycle_c:year`[1]), main = "`st_cycle_c:year`")
			qqline(unlist(ranef(m)$`st_cycle_c:year`[1]))			
			
			qqnorm(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]), main = "`first_second:(st_cycle_c:year)`")
			qqline(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]))
			
			scatter.smooth(resid(m)~sin(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$rad_st, col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$st_cycle_c, col = 'grey');abline(h=0, lty=2, col='red')
			
			dd$res = resid(m)
			#plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
						
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
		if(PNG == TRUE) {dev.off()}
			
			ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
				ggsave(paste(outdir,'mod_ass_N-nest_gaus_res_year.png',sep=''))
			#ggplot(d, aes(x = days_after_st, y = res, col = factor(st_cycle))) + geom_point()	+ facet_grid(year ~ .)	
	
	}
		{# model assumptions gaussian simple
			m = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_N-nest_gausSimple.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
				
			par(mfrow=c(5,3))
			scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col = 'red')
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
			plot(fitted(m),dd$n_nest,xlab="Fitted values", ylab="# initiated nest", las=1, cex.lab=1.2, cex=0.8, col ='grey')
					abline(0,1, lty=3,col='red')
				
			
			mtext("lm = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year",col='grey')
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$`st_cycle_c:year`[1]), main = "`st_cycle_c:year`",col='grey')
			qqline(unlist(ranef(m)$`st_cycle_c:year`[1]))			
			
			qqnorm(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]), main = "`first_second:(st_cycle_c:year)`",col='grey')
			qqline(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]))
			
			scatter.smooth(resid(m)~sin(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$rad_st, col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$st_cycle_c, col = 'grey');abline(h=0, lty=2, col='red')
			
			dd$res = resid(m)
			#plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
						
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
		if(PNG == TRUE) {dev.off()}
			
			ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
				ggsave(paste(outdir,'mod_ass_N-nest_gausSimple_res_year.png',sep=''))
			#ggplot(d, aes(x = days_after_st, y = res, col = factor(st_cycle))) + geom_point()	+ facet_grid(year ~ .)	
	
	}
		{# model assumptions poisson complex
			m = glmer(n_nest ~  st_cycle_c*cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
			
			
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_N-nest_poisComplex.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
			par(mfrow=c(5,3))
			scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col = 'red')
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
			plot(fitted(m),dd$n_nest,xlab="Fitted values", ylab="# initiated nest", las=1, cex.lab=1.2, cex=0.8, col ='grey')
					abline(0,1, lty=3,col='red')
				
			
			mtext("glmer(n_nest ~  st_cycle_c*cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$`st_cycle_c:year`[1]), main = "`st_cycle_c:year`")
			qqline(unlist(ranef(m)$`st_cycle_c:year`[1]))			
			
			qqnorm(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]), main = "`first_second:(st_cycle_c:year)`")
			qqline(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]))
			
			scatter.smooth(resid(m)~sin(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$rad_st, col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$st_cycle_c, col = 'grey');abline(h=0, lty=2, col='red')
			
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
		if(PNG == TRUE) {dev.off()}
			
			ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
				ggsave(paste(outdir,'mod_ass_N-nest_poisComplex_res_year.png',sep=''))
	
	}
		{# model assumptions poisson simple
			m = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
			
			
			if(PNG == TRUE) {png(paste(outdir,"mod_ass_N-nest_poisSimple.png", sep=""), width=6,height=9,units="in",res=600) 
					}else{
					dev.new(width=6,height=9)
					}	
			par(mfrow=c(5,3))
			scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col = 'red')
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
			plot(fitted(m),dd$n_nest,xlab="Fitted values", ylab="# initiated nest", las=1, cex.lab=1.2, cex=0.8, col ='grey')
					abline(0,1, lty=3,col='red')
				
			
			mtext("glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)", side = 3, line = -2, cex=0.7,outer = TRUE)		
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
			qqline(resid(m))
			 
			qqnorm(unlist(ranef(m)$year[1]), main = " year")
			qqline(unlist(ranef(m)$year[1]))
			
			qqnorm(unlist(ranef(m)$`st_cycle_c:year`[1]), main = "`st_cycle_c:year`")
			qqline(unlist(ranef(m)$`st_cycle_c:year`[1]))			
			
			qqnorm(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]), main = "`first_second:(st_cycle_c:year)`")
			qqline(unlist(ranef(m)$`first_second:(st_cycle_c:year)`[1]))
			
			scatter.smooth(resid(m)~sin(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~cos(dd$rad_st), col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$rad_st, col = 'grey');abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~dd$st_cycle_c, col = 'grey');abline(h=0, lty=2, col='red')
			
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
	
			
		if(PNG == TRUE) {dev.off()}
			
			ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
				ggsave(paste(outdir,'mod_ass_N-nest_poisSimple_res_year.png',sep=''))
	
	}
	{# later DELETE
	{# model USE
				
		m1 = lmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year/st_cycle_c/first_second), dd) 
		m1s = lmer(n_nest ~ sin(rad_st) + cos(rad_st)+st_cycle_c + (1|year/st_cycle_c/first_second), dd) 
		anova(m1,m1s)
		
		ms = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
		m = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year)+ (1|l_n), family = 'poisson',dd) 
		anova(ms,m)
			summary(m)
	summary(glht(m1))
	plot(allEffects(ms))
	plot(allEffects(m1s))
	plot(allEffects(m1, x.var = 'rad_st'))
	
		
		# simple
		m = glmer(n_nest ~ st_cycle_c+sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
		summary(rePCA(m))
		#mx = glmer(n_nest ~ st_cycle_c+sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd) 
		
		plot(allEffects(m))
		summary(m)
		# complex
			mc = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year)+ (1|l_n), family = 'poisson',dd) 
			#mc = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
			plot(allEffects(mc, x.var = 'rad_st'))
		
	acf(resid(m1), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		summary(m)
		summary(glht(m))
		plot(allEffects(m))	
		plot(Effect(m, focal.predictor = 'rad_st:st_cycle'))	
		#m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle),family = 'poisson', dd) 
	dd$res = resid(m)
	plot(unlist(ranef(m)$y_cl_fs[1])~c(1:nrow(ranef(m)$y_cl_fs[1])), col = dd$first_second)
	plot(unlist(ranef(m)$st_cycle[1])~c(1:nrow(ranef(m)$st_cycle[1])))
	plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
	plot(resid(m)~cos(dd$rad_st))
	plot(resid(m)~sin(dd$rad_st))				
	plot(resid(m)~dd$rad_st)
	ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
	ggplot(dd, aes(x = days_after_st, y = res, col = factor(st_cycle))) + stat_smooth(se=FALSE, alpha =0.3)	+ geom_point()+  facet_grid(year ~ .)								
	#### USE
	}
	{# per cycle (last 8th cycle not used - only 10 datapoints)
		l = list()
		for(i in 1:7){
			print(i)
			if(i==3){m =glmer(n_nest ~ sin(rad_st) + cos(rad_st)+ (1|year) + (1|l_n), family = 'poisson',dd[dd$st_cycle_c%in%c(i),])}
			if(i==5){m =glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|l_n), family = 'poisson',dd[dd$st_cycle_c%in%c(i),])}
			if(i%in%c(1,2,4,6,7)){
			m =glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year/first_second)+ (1|l_n), family = 'poisson',dd[dd$st_cycle_c%in%c(i),])
					}					
			nsim <- 5000
			bsim <- sim(m, n.sim=nsim) 
			v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for		
					newD=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(j in 1:nsim) predmatrix[,j] <- exp(X%*%bsim@fixef[j,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD
					pp$st_cycle = i
					l[[i]] = pp
			}
		s = do.call(rbind,l)
		s$n_nest = s$pred
		s$st_cycle_c = as.factor(s$st_cycle)
		ggplot(s,aes(y = n_nest, x = days_after_st))+geom_line()+  facet_wrap(~st_cycle_c)+
		geom_ribbon(data=s,aes(ymin=lwr,ymax=upr),alpha=0.3)
		
	}
	}
  {# Figure ISa
	 {# raw data
			x2 = ddply(dd,.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
			x1 = ddply(dd[dd$st_cycle_c == 1,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x3 = ddply(dd[dd$st_cycle_c == 3,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x5 = ddply(dd[dd$st_cycle_c == 5,],.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
		xc = ddply(dd,.(st_cycle_c), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
		# dummy line to set legend to max 150 nests
			xx = data.frame(days_after_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			x2 = rbind(xx,x2)
			x1 = rbind(xx,x1)
			x3 = rbind(xx,x3)
			x5 = rbind(xx,x5)
			
			xx = data.frame(st_cycle_c = -10, mean_=-10,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			xc = rbind(xx,xc)
	}
	 {# predictions
	   {# gauss
			m = lmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) + (1|year/st_cycle_c/first_second), dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	st_cylce_c Effect	
					newD=data.frame(days_after_st = mean(dd$days_after_st),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gsc=newD	
					
				# values to predict for	days_after_st	
					newD=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = mean(dd$st_cycle_c))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gs=newD	
	}
       {# poiss
			m = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for	st_cylce_c Effect	
					newD=data.frame(days_after_st = mean(dd$days_after_st),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					psc=newD				
				
				# values to predict for		days_after_st
					newD=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = mean(dd$st_cycle_c))
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+ sin(rad_st) + cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					ps=newD	
	}
       {# gauss - interaction
			m = lmer(n_nest ~ st_cycle_c *cos(rad_st)+ st_cycle_c*sin(rad_st) + (1|year/st_cycle_c/first_second), dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	days_after_st	
					newD1=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 1)
					newD2=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 3)
					newD3=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st),data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gi=newD	
	}
       {# poiss - interaction
			m = glmer(n_nest ~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for		days_after_st
					newD1=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 1)
					newD2=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 3)
					newD3=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					newD$rad_st =2*newD$days_after_st*pi/14.75
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st),data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pci=newD	
	}
     }
	 {# gaussina simple
	   {# plot ctcke
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_gausS_cycle.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gsc$pred~gsc$st_cycle_c, 
						xlim=c(0.5,8.5), ylim=c(0,1.5),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						polygon(c(gsc$st_cycle_c, rev(gsc$st_cycle_c)), c(gsc$lwr, 
								rev(gsc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gsc$st_cycle_c, gsc$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xc$st_cycle_c),(xc$mean_), circles=sqrt(xc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=FALSE) 
						
						axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,1.4, by = 0.2), label=TRUE, mgp=c(0,0.2,0))
						mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
												
									
		if(PNG == TRUE) {dev.off()}
	}			
	   {# plot day
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_gausS.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gs$pred~gs$days_after_st, 
						xlim=c(-1,15), ylim=c(0,1.5),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(gs$days_after_st, rev(gs$days_after_st)), c(gs$lwr, 
								rev(gs$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gs$days_after_st, gs$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					
						# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.3, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(15,15,15)+15*0.2,c(1.26,1.26-0.14,1.26-0.1*3.25),circles=sqrt(c(10,75,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(15,15,15)+15*0.4,c(1.26,1.26-0.14,1.26-0.1*3.25),labels=c(10,75,150), xpd=NA, cex=0.5,col='grey30')																				
						
		if(PNG == TRUE) {dev.off()}
	}			
	}
	 {# poisson simple	
	  {# plot ctcke
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_poisS_cycle.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(psc$pred~psc$st_cycle_c, 
						xlim=c(0.5,8.5), ylim=c(0,1.5),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						polygon(c(psc$st_cycle_c, rev(psc$st_cycle_c)), c(psc$lwr, 
								rev(psc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(psc$st_cycle_c, psc$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xc$st_cycle_c),(xc$mean_), circles=sqrt(xc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=FALSE) 
						
						axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,1.4, by = 0.2), label=TRUE, mgp=c(0,0.2,0))
						mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
																									
		if(PNG == TRUE) {dev.off()}
	}			
	  {# plot day
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_poisS.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(ps$pred~ps$days_after_st, 
						xlim=c(-1,15), ylim=c(0,1.5),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
					axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
					#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
					mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
					mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
					polygon(c(ps$days_after_st, rev(ps$days_after_st)), c(ps$lwr, 
								rev(ps$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(ps$days_after_st, ps$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.3, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(15,15,15)+15*0.2,c(1.26,1.26-0.14,1.26-0.1*3.25),circles=sqrt(c(10,75,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(15,15,15)+15*0.4,c(1.26,1.26-0.14,1.26-0.1*3.25),labels=c(10,75,150), xpd=NA, cex=0.5,col='grey30')										
						
		if(PNG == TRUE) {dev.off()}
	}			
	  			
	}
	 {# gaussina interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_gausInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						mtext("a",side=3,line=-0.4, adj=-0.26,cex=0.7, las=1, col='grey30',font=2)
						
						gii = gi[gi$st_cycle_c ==1,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x1$days_after_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = gi[gi$st_cycle_c ==3,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x3$days_after_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = gi[gi$st_cycle_c ==5,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols((x5$days_after_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	 {# poisson interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_I_poissInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==1,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x1$days_after_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==3,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols( (x3$days_after_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$days_after_st, 
						xlim=c(-1,15), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = pci[pci$st_cycle_c ==5,]
						polygon(c(gii$days_after_st, rev(gii$days_after_st)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$days_after_st, gii$pred, col=col_l,lwd=1)
						symbols((x5$days_after_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	
	}

  {# Supplementary Table S3
	 {# gaussian model complex
		m = lmer(n_nest ~ scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c/first_second), dd)
		pred=c('Intercept','cycle','tide','cycle:tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogc=rbind(oii,ri)
			
	}
	 {# gaussian model simple
		m = lmer(n_nest ~ scale(st_cycle_c) +scale(max_tide_height) + (1|year/st_cycle_c/first_second), dd) 
		
		pred=c('Intercept','cycle','tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogs=rbind(oii,ri)
			
	}
	 {# poisson model complex
		m = glmer(n_nest ~  scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','tide','cycle:tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		opc=rbind(oii,ri)
			
	}
	 {# poisson model simple
		m = glmer(n_nest ~ scale(st_cycle_c) +scale(max_tide_height) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		#m2 = glmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) +( sin(rad_st)+cos(rad_st)|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ops=rbind(oii,ri)
			
	}
	 {# export to excel
			
			sname = tempfile(fileext='.xls')
			wb = loadWorkbook(sname,create = TRUE)	
			createSheet(wb, name = "output")
			writeWorksheet(wb, rbind(ogc,ogs,opc,ops), sheet = "output")
			saveWorkbook(wb)
			shell(sname)
		}
  }
		{# add model assumptions
			
		}
		{# models later delete
	m = glmer(n_nest ~ scale(st_cycle_c)+scale(max_year_z) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	m = glmer(n_nest ~ scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	mz = glmer(n_nest ~ scale(st_cycle_c)*scale(max_year_z) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	
	
	anova(m,mz)
	summary(m)
	m = lmer(n_nest ~ scale(st_cycle_c)+scale(max_year_z) + (1|year/st_cycle_c) , dd[!is.na(dd$max_tide_height),])
	m = lmer(n_nest ~ scale(st_cycle_c)*scale(max_year_z) + (1|year/st_cycle_c) , dd[!is.na(dd$max_tide_height),])
	
	m = glmer(n_nest ~ max_yr_cl_z + (1|year) +(1|st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_yr_cl_z + (1|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_yr_cl_z + (max_yr_cl_z|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_tide_height + (max_tide_height|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ poly(max_tide_height,2) + (1|year) +(1|st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ poly(max_yr_cl_z,2) + (1|year/st_cycle/first_second) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_yr_cl_z),])
	m = glmer(n_nest ~ poly(max_yr_cl_z,2)*first_second + (1|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_yr_cl_z),])
	summary(m)
	summary(glht(m))
	plot(allEffects(m))
	plot(allEffects(m, x.var = 'max_year_z'))
	}
  {# Figure ISb
	 {# raw data
			dd$max_tide_height_r = round(dd$max_tide_height,-1)
		
			x2 = ddply(dd,.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
			x1 = ddply(dd[dd$st_cycle_c == 1,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x3 = ddply(dd[dd$st_cycle_c == 3,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x5 = ddply(dd[dd$st_cycle_c == 5,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
		xc = ddply(dd,.(st_cycle_c), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
		# dummy line to set legend to max 150 nests
			xx = data.frame(max_tide_height_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			x2 = rbind(xx,x2)
			x1 = rbind(xx,x1)
			x3 = rbind(xx,x3)
			x5 = rbind(xx,x5)
			
			xx = data.frame(st_cycle_c = -100, mean_=-10,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			xc = rbind(xx,xc)			
	}
	 {# predictions
	   {# gauss
			m = lmer(n_nest ~ st_cycle_c +max_tide_height + (1|year/st_cycle_c), dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	st_cylce_c Effect	
					newD=data.frame(max_tide_height = mean(dd$max_tide_height),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+max_tide_height,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gsc=newD	
					
				# values to predict for	days_after_st	
					newD=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = mean(dd$st_cycle_c))
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+max_tide_height,data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gs=newD	
	}
       {# poiss
			
			m = glmer(n_nest ~ st_cycle_c +  max_tide_z + (1|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for	st_cylce_c Effect	
					newD=data.frame(max_tide_z = mean(dd$max_tide_z),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+max_tide_z,data=newD)
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					psc=newD				
				
				# values to predict for	days_after_st	
					newD=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = mean(dd$st_cycle_c))
					newD$max_tide_z = scale(newD$max_tide_height)
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+max_tide_z,data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					ps=newD	
	}
       
	   {# gauss - interaction
			m = lmer(n_nest ~ st_cycle_c *max_tide_height + (1|year/st_cycle_c), dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	days_after_st	
					newD1=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 1)
					
					newD2=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 3)
					newD3=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c *max_tide_height,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gi=newD	
	}
       {# poiss - interaction
			dd$st_cycle_z=scale(dd$st_cycle_c)
			m = glmer(n_nest ~  st_cycle_z*max_tide_z + (1|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for	days_after_st	
					newD1=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 1, st_cycle_z = -1.595432)
					newD2=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 3, st_cycle_z = -0.4602477)
					newD3=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 5, st_cycle_z = 0.6749365)
					newD = rbind(newD1,newD2,newD3)
					newD$max_tide_z = scale(newD$max_tide_height)
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_z *max_tide_z,data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pci=newD	
	}
     }
	 
	 {# gaussina simple
	   {# plot ctcke
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_gausS_cycle.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gsc$pred~gsc$st_cycle_c, 
						xlim=c(0.5,8.5), ylim=c(0,1.5),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						polygon(c(gsc$st_cycle_c, rev(gsc$st_cycle_c)), c(gsc$lwr, 
								rev(gsc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gsc$st_cycle_c, gsc$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xc$st_cycle_c),(xc$mean_), circles=sqrt(xc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=FALSE) 
						
						axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,1.4, by = 0.2), label=TRUE, mgp=c(0,0.2,0))
						mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
												
									
		if(PNG == TRUE) {dev.off()}
	}			
	   {# plot day
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_gausS.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gs$pred~gs$max_tide_height, 
						xlim=c(48,152), ylim=c(0,1.5),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Maximum daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(gs$max_tide_height, rev(gs$max_tide_height)), c(gs$lwr, 
								rev(gs$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gs$max_tide_height, gs$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (x2$max_tide_height),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					
						# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.3, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(15,15,15)+15*0.2,c(1.26,1.26-0.14,1.26-0.1*3.25),circles=sqrt(c(10,75,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(15,15,15)+15*0.4,c(1.26,1.26-0.14,1.26-0.1*3.25),labels=c(10,75,150), xpd=NA, cex=0.5,col='grey30')																				
						
		if(PNG == TRUE) {dev.off()}
	}			
	}
	 {# poisson simple
	   {# plot ctcke
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_poicS_cycle.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(psc$pred~psc$st_cycle_c, 
						xlim=c(0.5,8.5), ylim=c(0,1.5),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						polygon(c(psc$st_cycle_c, rev(psc$st_cycle_c)), c(psc$lwr, 
								rev(psc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(psc$st_cycle_c, psc$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xc$st_cycle_c),(xc$mean_), circles=sqrt(xc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=FALSE) 
						
						axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,1.4, by = 0.2), label=TRUE, mgp=c(0,0.2,0))
						mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
												
									
		if(PNG == TRUE) {dev.off()}
	}			
	   {# plot day
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_poisS.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(ps$pred~ps$max_tide_height, 
						xlim=c(48,152), ylim=c(0,1.5),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Maximum daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(ps$max_tide_height, rev(ps$max_tide_height)), c(ps$lwr, 
								rev(ps$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(ps$max_tide_height, ps$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (x2$max_tide_height),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					
						# legend
							#legend('topright', symbols(
							#mtext(expression(italic('N')*' nests:'),side = 4,line=0.3, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							#symbols(c(15,15,15)+15*0.2,c(1.26,1.26-0.14,1.26-0.1*3.25),circles=sqrt(c(10,75,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							#text(c(15,15,15)+15*0.4,c(1.26,1.26-0.14,1.26-0.1*3.25),labels=c(10,75,150), xpd=NA, cex=0.5,col='grey30')																				
						
		if(PNG == TRUE) {dev.off()}
	}			
	}	
	 {# gaussina interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_gausInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						mtext("b",side=3,line=-0.4, adj=-0.26,cex=0.7, las=1, col='grey30',font=2)

						gii = gi[gi$st_cycle_c ==1,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x1$max_tide_height_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = gi[gi$st_cycle_c ==3,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x3$max_tide_height_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = gi[gi$st_cycle_c ==5,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols((x5$max_tide_height_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	 {# poisson interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_poissInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==1,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x1$max_tide_height_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==3,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x3$max_tide_height_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = pci[pci$st_cycle_c ==5,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols((x5$max_tide_height_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
}

}


{# MAX TIDE
	{# prepare data
	dd$rad_lst= 2*dd$day_j*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$rad_lm= 2*dd$day_j*pi/(14.75*2)
	dd$rad_m= 2*dd$days_after_st*pi/(14.75*2)
	dd$l_n = 1:nrow(dd)
	dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
	#dd$rad_st_yc= 2*dd$laid_j_yc*pi/14.75
	#dd$rad_m_yc= 2*dd$laid_j_yc*pi/(14.75*2)
	dd$year_= as.factor(dd$year)
	
	dd = ddply(dd,.(year), transform, max_year_z = scale(max_tide_height))
	dd = ddply(dd,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)
	dd$max_tide_z =scale(dd$max_tide_height)
	}	
		{# explore - not in paper
		ggplot(dd,aes(x = as.factor(year), y = max_tide_height))+geom_boxplot()
		ggplot(dd,aes(col = as.factor(year), x = max_tide_height))+geom_density()
		ggplot(dd,aes(col = as.factor(year), x = max_year_z))+geom_density()
		ggplot(dd,aes(x = max_tide_height, fill = year_))+geom_density(alpha = 0.2)
	}
	{# Supplementary Table S3
	 {# gaussian model complex
		m = lmer(n_nest ~ scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c/first_second), dd)
		pred=c('Intercept','cycle','tide','cycle:tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogc=rbind(oii,ri)
			
	}
	 {# gaussian model simple
		m = lmer(n_nest ~ scale(st_cycle_c) +scale(max_tide_height) + (1|year/st_cycle_c/first_second), dd) 
		
		pred=c('Intercept','cycle','tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='gaussian_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='gaussian_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ogs=rbind(oii,ri)
			
	}
	 {# poisson model complex
		m = glmer(n_nest ~  scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','tide','cycle:tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_com',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_com',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		opc=rbind(oii,ri)
			
	}
	 {# poisson model simple
		m = glmer(n_nest ~ scale(st_cycle_c) +scale(max_tide_height) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd)
		#m2 = glmer(n_nest ~ st_cycle_c +cos(rad_st)+ sin(rad_st) +( sin(rad_st)+cos(rad_st)|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
		
		pred=c('Intercept','cycle','tide')
		bsim <- sim(m, n.sim=nsim)  
		# Fixed effects
			v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
			ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
			oi=data.frame(model='pois_s',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
			rownames(oi) = NULL
			oi$estimate_r=round(oi$estimate,3)
			oi$lwr_r=round(oi$lwr,3)
			oi$upr_r=round(oi$upr,3)
			#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
			oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
		# Random effects
			l=data.frame(summary(m)$varcor)
			l = l[is.na(l$var2),]
			ri=data.frame(model='pois_s',type='random (var)',effect=l$var1, estimate_r=paste(round(100*l$vcov/sum(l$vcov)),'%'), lwr_r=NA, upr_r=NA)
		
		ops=rbind(oii,ri)
			
	}
	 {# export to excel
			
			sname = tempfile(fileext='.xls')
			wb = loadWorkbook(sname,create = TRUE)	
			createSheet(wb, name = "output")
			writeWorksheet(wb, rbind(ogc,ogs,opc,ops), sheet = "output")
			saveWorkbook(wb)
			shell(sname)
		}
  }
		{# add model assumptions
			
		}
	{# models later delete
	m = glmer(n_nest ~ scale(st_cycle_c)+scale(max_year_z) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	m = glmer(n_nest ~ scale(st_cycle_c)*scale(max_tide_height) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	mz = glmer(n_nest ~ scale(st_cycle_c)*scale(max_year_z) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd)
	
	
	anova(m,mz)
	summary(m)
	m = lmer(n_nest ~ scale(st_cycle_c)+scale(max_year_z) + (1|year/st_cycle_c) , dd[!is.na(dd$max_tide_height),])
	m = lmer(n_nest ~ scale(st_cycle_c)*scale(max_year_z) + (1|year/st_cycle_c) , dd[!is.na(dd$max_tide_height),])
	
	m = glmer(n_nest ~ max_yr_cl_z + (1|year) +(1|st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_yr_cl_z + (1|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_yr_cl_z + (max_yr_cl_z|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_tide_height + (max_tide_height|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ poly(max_tide_height,2) + (1|year) +(1|st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ poly(max_yr_cl_z,2) + (1|year/st_cycle/first_second) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_yr_cl_z),])
	m = glmer(n_nest ~ poly(max_yr_cl_z,2)*first_second + (1|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_yr_cl_z),])
	summary(m)
	summary(glht(m))
	plot(allEffects(m))
	plot(allEffects(m, x.var = 'max_year_z'))
	}
	{# Figure M
	 {# raw data
			dd$max_tide_height_r = round(dd$max_tide_height,-1)
		
			x2 = ddply(dd,.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
			x1 = ddply(dd[dd$st_cycle_c == 1,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x3 = ddply(dd[dd$st_cycle_c == 3,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
			x5 = ddply(dd[dd$st_cycle_c == 5,],.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))			
		xc = ddply(dd,.(st_cycle_c), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))
		# dummy line to set legend to max 150 nests
			xx = data.frame(max_tide_height_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			x2 = rbind(xx,x2)
			x1 = rbind(xx,x1)
			x3 = rbind(xx,x3)
			x5 = rbind(xx,x5)
			
			xx = data.frame(st_cycle_c = -100, mean_=-10,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			xc = rbind(xx,xc)			
	}
	 {# predictions
	   {# gauss
			m = lmer(n_nest ~ st_cycle_c +max_tide_height + (1|year/st_cycle_c), dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	st_cylce_c Effect	
					newD=data.frame(max_tide_height = mean(dd$max_tide_height),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+max_tide_height,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gsc=newD	
					
				# values to predict for	days_after_st	
					newD=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = mean(dd$st_cycle_c))
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+max_tide_height,data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gs=newD	
	}
       {# poiss
			
			m = glmer(n_nest ~ st_cycle_c +  max_tide_z + (1|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for	st_cylce_c Effect	
					newD=data.frame(max_tide_z = mean(dd$max_tide_z),st_cycle_c = seq(1,max(dd$st_cycle_c), length.out=300))
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+max_tide_z,data=newD)
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					psc=newD				
				
				# values to predict for	days_after_st	
					newD=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = mean(dd$st_cycle_c))
					newD$max_tide_z = scale(newD$max_tide_height)
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c+max_tide_z,data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					ps=newD	
	}
       
	   {# gauss - interaction
			m = lmer(n_nest ~ st_cycle_c *max_tide_height + (1|year/st_cycle_c), dd) 
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				# values to predict for	days_after_st	
					newD1=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 1)
					
					newD2=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 3)
					newD3=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 5)
					newD = rbind(newD1,newD2,newD3)
					
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_c *max_tide_height,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					gi=newD	
	}
       {# poiss - interaction
			dd$st_cycle_z=scale(dd$st_cycle_c)
			m = glmer(n_nest ~  st_cycle_z*max_tide_z + (1|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
					#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
				
				# values to predict for	days_after_st	
					newD1=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 1, st_cycle_z = -1.595432)
					newD2=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 3, st_cycle_z = -0.4602477)
					newD3=data.frame(max_tide_height = seq(min(dd$max_tide_height),max(dd$max_tide_height), length.out=300),st_cycle_c = 5, st_cycle_z = 0.6749365)
					newD = rbind(newD1,newD2,newD3)
					newD$max_tide_z = scale(newD$max_tide_height)
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ st_cycle_z *max_tide_z,data=newD)		
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pci=newD	
	}
     }
	 
	 {# gaussina simple
	   {# plot ctcke
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_gausS_cycle.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gsc$pred~gsc$st_cycle_c, 
						xlim=c(0.5,8.5), ylim=c(0,1.5),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						polygon(c(gsc$st_cycle_c, rev(gsc$st_cycle_c)), c(gsc$lwr, 
								rev(gsc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gsc$st_cycle_c, gsc$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xc$st_cycle_c),(xc$mean_), circles=sqrt(xc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=FALSE) 
						
						axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,1.4, by = 0.2), label=TRUE, mgp=c(0,0.2,0))
						mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
												
									
		if(PNG == TRUE) {dev.off()}
	}			
	   {# plot day
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_gausS.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gs$pred~gs$max_tide_height, 
						xlim=c(48,152), ylim=c(0,1.5),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Maximum daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(gs$max_tide_height, rev(gs$max_tide_height)), c(gs$lwr, 
								rev(gs$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gs$max_tide_height, gs$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (x2$max_tide_height),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					
						# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.3, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(15,15,15)+15*0.2,c(1.26,1.26-0.14,1.26-0.1*3.25),circles=sqrt(c(10,75,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(15,15,15)+15*0.4,c(1.26,1.26-0.14,1.26-0.1*3.25),labels=c(10,75,150), xpd=NA, cex=0.5,col='grey30')																				
						
		if(PNG == TRUE) {dev.off()}
	}			
	}
	 {# poisson simple
	   {# plot ctcke
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_poicS_cycle.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(psc$pred~psc$st_cycle_c, 
						xlim=c(0.5,8.5), ylim=c(0,1.5),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						polygon(c(psc$st_cycle_c, rev(psc$st_cycle_c)), c(psc$lwr, 
								rev(psc$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(psc$st_cycle_c, psc$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (xc$st_cycle_c),(xc$mean_), circles=sqrt(xc$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=FALSE) 
						
						axis(1, at=seq(1,8, by = 1), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,1.4, by = 0.2), label=TRUE, mgp=c(0,0.2,0))
						mtext("Spring-tide cycle #",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
												
									
		if(PNG == TRUE) {dev.off()}
	}			
	   {# plot day
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_poisS.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(ps$pred~ps$max_tide_height, 
						xlim=c(48,152), ylim=c(0,1.5),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Maximum daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(ps$max_tide_height, rev(ps$max_tide_height)), c(ps$lwr, 
								rev(ps$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(ps$max_tide_height, ps$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (x2$max_tide_height),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
					
						# legend
							#legend('topright', symbols(
							#mtext(expression(italic('N')*' nests:'),side = 4,line=0.3, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							#symbols(c(15,15,15)+15*0.2,c(1.26,1.26-0.14,1.26-0.1*3.25),circles=sqrt(c(10,75,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							#text(c(15,15,15)+15*0.4,c(1.26,1.26-0.14,1.26-0.1*3.25),labels=c(10,75,150), xpd=NA, cex=0.5,col='grey30')																				
						
		if(PNG == TRUE) {dev.off()}
	}			
	}	
	 {# gaussina interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_gausInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						mtext("b",side=3,line=-0.4, adj=-0.26,cex=0.7, las=1, col='grey30',font=2)

						gii = gi[gi$st_cycle_c ==1,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x1$max_tide_height_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = gi[gi$st_cycle_c ==3,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x3$max_tide_height_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(gi$pred~gi$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = gi[gi$st_cycle_c ==5,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols((x5$max_tide_height_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	 {# poisson interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_M_poissInt.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.5*3,height=1.5)
					}	
				
				par(mfrow = c(1,3), mar=c(0.25,0.5,0,0.25),oma = c(1.4, 1.5, 0.3, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						#yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==1,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x1$max_tide_height_r),(x1$mean_), circles=sqrt(x1$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						gii = pci[pci$st_cycle_c ==3,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols( (x3$max_tide_height_r),(x3$mean_), circles=sqrt(x3$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
				plot(pci$pred~pci$max_tide_height, 
						xlim=c(48,152), ylim=c(0,2.2),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
												
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						#axis(2, at=seq(0,.3, by = 0.05), label=c('0%','5%','10%','15%','20%','25%','30%'), mgp=c(0,0.1,0))
						mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = pci[pci$st_cycle_c ==5,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols((x5$max_tide_height_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
}
}	 	
# OLD not use	
	{# interaction
		m = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year/st_cycle_c/first_second)+ (1|l_n), family = 'poisson',dd) 
		m = glmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year)+ (1|l_n), family = 'poisson',dd) 
		ms = glmer(n_nest ~ st_cycle_c +sin(rad_st) + cos(rad_st) + (1|year)+ (1|l_n), family = 'poisson',dd) 
		anova(m,ms)
		# do all three and hypothesis testing to get a feeling for it and check raneffs
		m3 = lmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (sin(rad_st)+ cos(rad_st)|st_cycle_c)+(1|year), dd) 
		m3s = lmer(n_nest ~ sin(rad_st) + cos(rad_st)+st_cycle_c + (sin(rad_st)+ cos(rad_st)|st_cycle_c)+(1|year), dd) 
		anova(m3,m3s)
		
		m2 = lmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (sin(rad_st)+ cos(rad_st)|year), dd) 
		m2s = lmer(n_nest ~ sin(rad_st)+ cos(rad_st)+st_cycle_c + (sin(rad_st)+ cos(rad_st)|year), dd) 
		anova(m2,m2s)
		
		m1 = lmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year/st_cycle_c/first_second), dd) 
		m1s = lmer(n_nest ~ sin(rad_st) + cos(rad_st)+st_cycle_c + (1|year/st_cycle_c/first_second), dd) 
		anova(m1,m1s)
		
		m0 = lmer(n_nest ~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c + (1|year), dd) 
		m0s = lmer(n_nest ~ sin(rad_st) + cos(rad_st)+st_cycle_c + (1|year), dd) 
		anova(m0,m0s)
		
		AIC(m0,m1,m2,m3)[order(AIC(m0,m1,m2,m3)$AIC),]
		AIC(m0,m1,m2,m3,m0s,m1s,m2s,m3s)[order(AIC(m0,m1,m2,m3,m0s,m1s,m2s,m3s)$AIC),]
		
		m = glmer(n_nest ~ sin(rad_st)*poly(st_cycle_c,2)+ cos(rad_st)*poly(st_cycle_c,2) + (1|year)+ (1|l_n), family = 'poisson',dd) 
		m = glmer(n_nest ~ sin(rad_st)+ cos(rad_st)+poly(st_cycle_c,2) + (1|year)+ (1|l_n), family = 'poisson',dd) 
		
		m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|l_n), family = 'poisson',dd[dd$year>2012,]) 
		
		
		dd$early_late = ifelse(dd$st_cycle_c<5, 'early','late')
		m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year)+(1|st_cycle_c)+ (1|l_n), family = 'poisson',dd[dd$st_cycle_c>1 & dd$st_cycle_c<5,]) 
		m = glmer(n_nest ~ sin(rad_st)*early_late + cos(rad_st)*early_late + (1|year)+(1|st_cycle_c)+ (1|l_n), family = 'poisson',dd) 
		
		dd$st_cycle_c_f = as.factor(dd$st_cycle_c)
		m = glmer(n_nest ~ sin(rad_st)*st_cycle_c_f + cos(rad_st)*st_cycle_c_f + (1|year)+ (1|l_n), family = 'poisson',dd) 
		m = glmer(n_nest ~ sin(rad_st)*first_second + cos(rad_st)*first_second + (1|year)+ (1|l_n), family = 'poisson',dd) 
		
	summary(m)
	summary(glht(m1s))
	plot(allEffects(m))
	plot(allEffects(m, x.var = 'rad_st'))
	}
		{# predictions
			m = m
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5, 0.975))
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
				# values to predict for		
					l = list()
					for(i in 1:8){
					newD=data.frame(days_after_st = seq(0,max(dd$days_after_st), length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
					newD$st_cycle_c = i
					l[[i]] = newD
					}
					newD = do.call(rbind,l)
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ sin(rad_st)*st_cycle_c + cos(rad_st)*st_cycle_c,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
	}
	{# raw data
		dd$days_after_r = round(dd$days_after_st)
		x2 = ddply(dd,.(days_after_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))	
	}
	{# plot 
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_n-nest_time_season_dot.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$days_after_st, 
						xlim=c(0,16), ylim=c(0,1.4),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Days after spring tide",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Number of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						coll = brewer.pal(9,"Blues")[2:9]
						
						for(i in 1:8){
						ppi = pp[pp$st_cycle_c == i,]
						polygon(c(ppi$days_after_st, rev(ppi$days_after_st)), c(ppi$lwr, 
								rev(ppi$upr)), border=NA, col=transpcol(coll[i],newalpha = 50)) #0,0,0 black 0.5 is transparents RED
							lines(ppi$days_after_st, ppi$lwr, col=coll[i],lwd=0.5,lty=3)
							lines(ppi$days_after_st, ppi$upr, col=coll[i],lwd=0.5,lty=3)
							lines(ppi$days_after_st, ppi$pred, col=coll[i],lwd=1)
						
						}	
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$med_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						#symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
	{### does not run
	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (scale(sin(rad_st)) + scale(cos(rad_st))|year/st_cycle_c)+ (1|l_n), family = 'poisson',dd) 
		
	m0 = glmer(n_nest ~ 1 + (1|y_cl_fs)+ (1|l_n), family = 'poisson',dd) 
	
	m = glmer(n_nest ~ st_cycle_c*sin(rad_st) + st_cycle_c*cos(rad_st) + (1|year) + (1|l_n), family = 'poisson',dd) 
	
	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year) +(1|st_cycle)+ (1|l_n), family = 'poisson',dd)
	
	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year/st_cycle)+ (1|l_n), family = 'poisson',dd) 
	
	
	
	table(dd$st_cycle_c,dd$year)
	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (1|year/st_cycle/first_second)+ (1|l_n), family = 'poisson',dd) #quasipoisson
	
	m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|y_cl)+ (1|l_n), family = 'poisson',dd) #quasipoisson
	
acf(resid(m1), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		summary(m)
		summary(glht(m))
		plot(allEffects(m))	
		plot(Effect(m, focal.predictor = 'rad_st:st_cycle'))	
		#m = glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle),family = 'poisson', dd) 
	dd$res = resid(m)
	plot(unlist(ranef(m)$y_cl_fs[1])~c(1:nrow(ranef(m)$y_cl_fs[1])), col = dd$first_second)
	plot(unlist(ranef(m)$st_cycle[1])~c(1:nrow(ranef(m)$st_cycle[1])))
	plot(unlist(ranef(m)$year[1])~c(1:nrow(ranef(m)$year[1])))
	plot(resid(m)~cos(dd$rad_st))
	plot(resid(m)~sin(dd$rad_st))				
	plot(resid(m)~dd$rad_st)
	ggplot(dd, aes(x = days_after_st, y = res, col = factor(year))) + stat_smooth(se=FALSE)+ geom_point()	+ facet_grid(year ~ .)	
	ggplot(dd, aes(x = days_after_st, y = res, col = factor(st_cycle))) + stat_smooth(se=FALSE, alpha =0.3)	+ geom_point()+  facet_grid(year ~ .)	
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
	}
	{# best fitting period in days for all and per year
		period=c(seq(7,90,by=0.5)) #period=c(0.5,0.75,1.5,seq(1,21, by=1), seq(22,26, by=0.25), seq(27,48,by=1))
		period=period[order(period)]	
		
		# run periodic regressions
			l = list()
			for(j in 1:length(unique(dd$year))){		
				 o = list()
				 for(jj in 1:length(period)){
							v=dd[dd$year==unique(dd$year)[j],] # first row removes as it contains lag 0 autocorrelation 
							v$period=period[jj]
							v$rad= (2*pi*v$day_j) / (period[jj])
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
			m=m1s 
			
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
				
				qqnorm(unlist(ranef(m)$st_cycle_c [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$st_cycle_c [1]))
				
				qqnorm(unlist(ranef(m)$first_second [1]), main = " intercept",col='red')
				qqline(unlist(ranef(m)$first_second [1]))				
				#qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
				#qqline(unlist(ranef(m)$nest_ID[2]))
			length(dd$n_nest)
			length(dd$n_nest[!is.na(dd$st_cycle)])
			length(fitted(m))
			plot(dd$n_nest~fitted(m), xlab="Fitted values", ylab="n_nest", las=1, cex.lab=1.2, cex=0.8)
				abline(0,1, lty=3, col = 'red')
										
				#scatter.smooth(resid(m)~x2$date[x2$sum>720]);abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~sin(dd$rad_st));abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~cos(dd$rad_st));abline(h=0, lty=2, col='red')
				scatter.smooth(resid(m)~dd$rad_st);abline(h=0, lty=2, col='red')
				#boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
				#mtext("lmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle), dd) ", side = 3, line = 0.5, cex=0.8,outer = TRUE)
				mtext("glmer(n_nest ~ sin(rad_st) + cos(rad_st) + (sin(rad_st) + cos(rad_st)|year) +(1|st_cycle),family = 'poisson', dd)  ", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
				acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
				
				
				dev.off()
		
		}
{ # SILVIA # plot with laying dates per year, median and illumination
NP <- c((seq(as.Date("2006-04-01"), as.Date("2006-07-31"), by="days")), 
        seq(as.Date("2007-04-01"), as.Date("2007-07-31"), by="days"), 
        seq(as.Date("2008-04-01"), as.Date("2008-07-31"), by="days"), 
        seq(as.Date("2009-04-01"), as.Date("2009-07-31"), by="days"), 
        seq(as.Date("2010-04-01"), as.Date("2010-07-31"), by="days"), 
        seq(as.Date("2011-04-01"), as.Date("2011-07-31"), by="days"), 
        seq(as.Date("2012-04-01"), as.Date("2012-07-31"), by="days"),
        seq(as.Date("2013-04-01"), as.Date("2013-07-31"), by="days"),
        seq(as.Date("2015-04-01"), as.Date("2015-07-31"), by="days"),
        seq(as.Date("2016-04-01"), as.Date("2016-07-31"), by="days"))
np <- as.data.frame(NP) # nesting period
colnames(np) <- c("date")
np["nests_laid"] <- rep(0, length(np$date)) # new col were the number of layed nests are supposed to go
np$date <- as.POSIXct(np$date)
np$n_nest <- (dd$n_nest[match(np$date,dd$datetime_)])
np[is.na(np)] <- 0
# get illumination
np$illum <- ii$illumination_noon[match(as.character(np$date), substring(ii$noon,1,10))]
np$illum <- round(np$illum,2)
np$yday <- yday(np$date)
np$year <- format(as.Date(np$date, format="%d/%m/%Y"),"%Y")
np$year <- as.factor(np$year)
# max tide height
np$max_tide_height = tt$max_tide_height[match(np$date, tt$date)]
#require(RColorBrewer)
#cols <- brewer.pal(10, "Paired") # ['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a']
# years seperately, barplot 
p <- ggplot(np, aes(x= yday, y = n_nest, fill=as.factor(year))) + 
  geom_bar(stat = "identity") + 
  facet_grid(year ~ .) + theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_x_continuous(breaks= c(91,121,152,182), labels=c("April", "May", "June", "July")) + 
  labs(x = "Laying Date") + guides(fill=guide_legend(title="Year"))
# with illumination
p <- p + geom_line(data = np, aes(x = yday, y = illum * 10), colour= "darkgrey") + 
  scale_y_continuous(breaks=c(0,2,5,7,10),labels=c("0","2","5","7","10"), name= "Clutches completed", 
                     sec.axis = sec_axis(~ . * 10, name = "Illumination [%]", breaks = c(0,25,50,75,100)))
# get the median for the laying date
median_l <- tapply(dd$yday, as.factor(dd$year),median) # zeros included
median_l <- as.data.frame.table(median_l)
colnames(median_l) <- c("year","median") 
# add to plot
p <- p + geom_vline(aes(xintercept = median), data= median_l, colour = "black") 
p + coord_cartesian( xlim = c(90,195))
# with max_tide_height
p = ggplot(np, aes(x= yday, y = n_nest, fill=as.factor(year))) + 
  geom_bar(stat = "identity") + 
  facet_grid(year ~ .) + theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_x_continuous(breaks= c(91,121,152,182), labels=c("April", "May", "June", "July")) + 
  labs(x = "Laying Date") + guides(fill=guide_legend(title="Year"))
 p = p + geom_line(data = np, aes(x = yday, y = max_tide_height / 15), colour= "darkgrey") 
 p+ scale_y_continuous(breaks=c(0,5,10),labels=c("0","5","10"), name= "Clutches completed")
 p+scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Tide height [m]", breaks = c(0,75,150)))
}
		}
{# MAX TIDE
	{# prepare data
	dd$datetime_j = as.numeric(format(as.POSIXct(dd$datetime_),"%j"))
	dd$rad_lst= 2*dd$datetime_j*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$rad_lm= 2*dd$datetime_j*pi/(14.75*2)
	dd$rad_m= 2*dd$days_after_st*pi/(14.75*2)
	dd$l_n = 1:nrow(dd)
	dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
	#dd$rad_st_yc= 2*dd$laid_j_yc*pi/14.75
	#dd$rad_m_yc= 2*dd$laid_j_yc*pi/(14.75*2)
	dd$year_= as.factor(dd$year)
	dd$max_tide_height = tt$max_tide_height[match(dd$datetime_, tt$date)]
	dd = ddply(dd,.(year), transform, max_year_z = scale(max_tide_height))
	dd = ddply(dd,.(year, st_cycle), transform, max_yr_cl_z = scale(max_tide_height))
	dd = ddply(dd,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)
	ggplot(dd,aes(x = max_tide_height, fill = year_))+geom_density(alpha = 0.2)
	}	
	{# models
	m = glmer(n_nest ~ scale(st_cycle_c)+scale(max_year_z) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ scale(st_cycle_c)*scale(max_year_z) + (1|year/st_cycle_c) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	summary(m)
	m = lmer(n_nest ~ scale(st_cycle_c)+scale(max_year_z) + (1|year/st_cycle_c) , dd[!is.na(dd$max_tide_height),])
	m = lmer(n_nest ~ scale(st_cycle_c)*scale(max_year_z) + (1|year/st_cycle_c) , dd[!is.na(dd$max_tide_height),])
	
	m = glmer(n_nest ~ max_yr_cl_z + (1|year) +(1|st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_yr_cl_z + (1|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_yr_cl_z + (max_yr_cl_z|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ max_tide_height + (max_tide_height|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ poly(max_tide_height,2) + (1|year) +(1|st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
	m = glmer(n_nest ~ poly(max_yr_cl_z,2) + (1|year/st_cycle/first_second) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_yr_cl_z),])
	m = glmer(n_nest ~ poly(max_yr_cl_z,2)*first_second + (1|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_yr_cl_z),])
	summary(m)
	summary(glht(m))
	plot(allEffects(m))
	plot(allEffects(m, x.var = 'max_year_z'))
	}
	{# predictions
			m = glmer(n_nest ~ max_yr_cl_z + (max_yr_cl_z|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
			m = glmer(n_nest ~ scale(max_tide_height) + (scale(max_tide_height)|year/st_cycle) + (1|l_n), family = 'poisson',dd[!is.na(dd$max_tide_height),])
						plot(allEffects(m))
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
				# values to predict for		
					newD=data.frame(max_tide_height = seq(53,150, length.out=300))
					newD$max_yr_cl_z =(newD$max_tide_height-mean(newD$max_tide_height))/sd(newD$max_tide_height)
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ max_yr_cl_z,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
	}
	{# raw data
		dd$max_tide_height_r = round(dd$max_tide_height,-1)
		
		x2 = ddply(dd,.(max_tide_height_r), summarise, mean_ = mean(n_nest),med_ = median(n_nest), sd_ = sd(n_nest), n = length(year))	
	}
	{# plot 0:40
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_max-tide_n-nest.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.5, 1.5, 0.2, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$max_tide_height, 
						xlim=c(50,150), ylim=c(0,1.4),
						xaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Tide height [m]",xlab = NULL,type='n')
						
						axis(1, at=seq(50,150, by = 25), label=TRUE, mgp=c(0,-0.20,0))
						mtext("Tide height [m]",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						mtext("Number of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(pp$max_tide_height, rev(pp$max_tide_height)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(pp$max_tide_height, pp$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						symbols( jitter(x2$max_tide_height_r),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
						# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' days:'),side = 4,line=0.1, padj=-7,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(150,150,150)*1.1,c(1.2,1.2-0.15,1.2-0.15*2),circles=sqrt(c(1,30,75)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(150,150,150)*1.2,c(1.2,1.2-0.15,1.2-0.15*2),labels=c(1,30,75), xpd=NA, cex=0.5,col='grey30')													
							#text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90, xpd=FALSE) 
							
							#mtext('Predictions &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
		if(PNG == TRUE) {dev.off()}
	}			
	
}
#### NOT RELEVANT			
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
	ggplot(d, aes(x = days_after_st, y = max_tide_height, col = as.factor(year))) + geom_point() +stat_smooth()+facet_grid(year ~ .)
	
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
{# {# Figure F	- rest
  
   {# flooding
	{# prepare	
	d = nn[!is.na(nn$fate),]
	d = d[d$fate%in%c("hatch","unhatch","flood"),] # only flooded, hatched or unhatched nests
	summary(factor(d$fate))
	d$flooded = ifelse(d$fate=='flood',1,0)
	d$flooded_ = ifelse(d$fate=='flood','yes','no')
	d$year_ = as.factor(d$year)
	d$laid_j=as.POSIXlt(d$laid, format = "%d%b%y")$yday
	d$days_after_r = ceiling(d$days_after_st)
	d$rad_st = 2*d$days_after_st*pi/d$dur
		###d$y_cl=factor(paste(d$year,d$st_cycle))
		###dd$y_cl_fs = paste(dd$year, dd$st_cycle,dd$first_second)
	
	d = ddply(d,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1) # center within year
		
	d$st_cycle_c[d$st_cycle_c==7]=6 # only 2 datapoints for cycle 7, so make it 6 summary(factor(d$st_cycle_c))
	###d = ddply(d,.(year), transform, max_year_z = scale(max_tide_height)) # standardize tide within year
	###d = ddply(d,.(year, st_cycle), transform, max_yr_cl_z = scale(max_tide_height)) # standardize tide within year and cycle
   }	
	{# predictions gaus
			#m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year)+(1|st_cycle)+(1|female), family = 'binomial', d) 
			m = lmer(flooded ~ lat+st_cycle_c+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female),d)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
				# values to predict for	days after spring tide	
					newD=data.frame(days_after_st = seq(0,max(d$days_after_st), length.out=300),lat = mean(d$lat),st_cycle_c = mean(d$st_cycle_c))
					newD$rad_st =2*newD$days_after_st*pi/14.75
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ lat+st_cycle_c+sin(rad_st)+cos(rad_st),data=newD)	
						
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD
				
				# values to predict for	cycle after spring tide	
					newD=data.frame(days_after_st = mean(d$days_after_st),lat = mean(d$lat),st_cycle_c = seq(0,max(d$st_cycle_c), length.out=300))
					newD$rad_st =2*newD$days_after_st*pi/14.75
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ lat+st_cycle_c+sin(rad_st)+cos(rad_st),data=newD)	
						
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pc=newD	
	}
	{# predictions bin
			d$lat_z = scale(d$lat)
			d$st_cycle_cz = (d$st_cycle_c-mean(d$st_cycle_c))/sd(d$st_cycle_c)
			#m = glmer(flooded ~lat_z+st_cycle_cz+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female), family = 'binomial', d)
			m = glmer(flooded ~sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c)+(1|female), family = 'binomial', d)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					#names(v)=c("(Intercept)","lat","st_cycle_c","sin(rad_st)","cos(rad_st)")
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
				# values to predict for		
					newD=data.frame(days_after_st = seq(0,max(d$days_after_st), length.out=300))#,lat_z = 0,st_cycle_cz = 0)
					newD$rad_st =2*newD$days_after_st*pi/14.75
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ sin(rad_st)+cos(rad_st),data=newD)	
						
				# calculate predicted values and creditability intervals
					newD$pred <-plogis(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pb=newD	

				# values to predict for	cycle after spring tide	
				m = glmer(flooded ~scale(st_cycle_c)+(1|year)+(1|st_cycle_c)+(1|female), family = 'binomial', d)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					#names(v)=c("(Intercept)","lat","st_cycle_c","sin(rad_st)","cos(rad_st)")
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.975))
				
				newD=data.frame(st_cycle_c = seq(0,max(d$st_cycle_c), length.out=300))
				
				# exactly the model which was used has to be specified here
					X <- model.matrix(~ scale(st_cycle_c),data=newD)	
						
				# calculate predicted values and creditability intervals
					newD$pred <-plogis(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pbc=newD	
	}
	{# raw data
		x = ddply(d,.(year, days_after_st), summarise, mean_ = mean(flooded), sd_ = sd(flooded), n = length(flooded))
			y = data.frame(year = unique(d$year), col_ =1:length(unique(d$year)), stringsAsFactors = FALSE)
			x$col_ = y$col_[match(x$year, y$year)]
		
		x2 = ddply(d,.(days_after_r), summarise, mean_ = mean(flooded), sd_ = sd(flooded), n = length(flooded))
		
		xc = ddply(d,.(st_cycle_c), summarise, mean_ = mean(flooded),n = length(flooded))
		# dummy line to set legend to max 150 nests
			#xx = data.frame(days_after_r = -100, mean_=-10 ,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			#x2 = rbind(xx,x2)
			#x1 = rbind(xx,x1)
			#x3 = rbind(xx,x3)
			#x5 = rbind(xx,x5)
			
			#xx = data.frame(st_cycle_c = -10, mean_=-10,med_=-10, sd_ = 0, n = 150, stringsAsFactors=FALSE)
			#xc = rbind(xx,xc)
	}
    }
	
	
	
	{# plot gaussian
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_Fgaus.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				plot(pp$pred~pp$days_after_st, 
						xlim=c(-1,15), ylim=c(-0.035,0.4),
						xaxt='n',
						yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flodded nests [%]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,.4, by = 0.05), label=c('0%','','10%','','20%','','30%','','40%'), mgp=c(0,0.1,0))
						mtext("Nest initiation\n[days from spring tide]",side=1,line=0.9, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("Flooded nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						
						polygon(c(pp$days_after_st, rev(pp$days_after_st)), c(pp$lwr, 
								rev(pp$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
							lines(pp$days_after_st, pp$pred, col=col_l,lwd=1)
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_r),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
						# legend
							#legend('topright', symbols(
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.1, padj=-8,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(17,17,17)+0.01,c(0.26,0.26-0.04,0.26-0.04*2.25),circles=sqrt(c(20,40,60)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(17,17,17)+3,c(0.26,0.26-0.04,0.26-0.04*2.25),labels=c(20,40,60), xpd=NA, cex=0.5,col='grey30')													
							#text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90, xpd=FALSE) 
							
							#mtext('Predictions &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
		if(PNG == TRUE) {dev.off()}
	}			
	{# binomial plot
		if(PNG == TRUE) {
			png(paste(outdir,"Figure_Fbin.png", sep=""), width=1.85,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85,height=1.5)
					}	
				
				par(mar=c(0.25,0.5,0.2,1.2),oma = c(1.55, 1.5, 0.15, 1.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
								
				plot(pb$pred~pb$days_after_st, 
						xlim=c(-1,15), ylim=c(-0.035,0.8),
						xaxt='n',yaxt='n',
						xaxs = 'i', yaxs = 'i',
						ylab = "Flooding probability]",xlab = NULL,type='n')
						
						axis(1, at=seq(0,15, by = 3), label=TRUE, mgp=c(0,-0.20,0))
						axis(2, at=seq(0,.8, by = 0.2), label=c('0%','20%','40%','60%','80%'), mgp=c(0,0.1,0))
						mtext("Nest initiation\n[days from spring tide]",side=1,line=0.9, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("Flooded nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						polygon(c(pb$days_after_st, rev(pb$days_after_st)), c(pb$lwr, 
								rev(pb$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pb$days_after_st, pb$pred, col=col_m,lwd=1)
						
						symbols( (x2$days_after_r),(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						#symbols( jitter(x$days_after_st),jitter(x$mean_), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) 
						#symbols( jitter(x2$days_after_st),jitter(x2$mean_), circles=sqrt(x2$n/pi),inches=0.14/1.75,bg=adjustcolor(col_f, alpha.f = 0.5), fg=col_p,add=TRUE) 
						
						
							
						if(PNG == TRUE) {dev.off()}
						
						
	}			
   }

# transformation
# for rad_st take the rad_st from the dataset that is closest to the mean, as this way you can use precise 'dur' for tranformation
			index = which(abs(d$rad_st-mean(d$rad_st))==min(abs(d$rad_st-mean(d$rad_st))))
		
			newD=data.frame(days_after_st =d$days_after_st[index], dur = d$dur[index], rad_st = d$rad_st[index],st_cycle_c = seq(1,6,length.out=300)) #lat = mean(d$lat),
					
			#newD$days_after_st =newD$rad_st*newD$dur/(2*pi)#d$dur[order(d$days_after_st)][224]
			#newD=data.frame(days_after_st = mean(d$days_after_st),lat = mean(d$lat),st_cycle_c = seq(1,6,length.out=300))
			#newD$rad_st =2*newD$days_after_st*pi/17#d$dur[order(d$days_after_st)][224]
			#newD = newD[order(newD$st_cycle_c),]


   