# R Script generates statistical outputs for 
#"Nest initiation and flooding in response to season and semi-lunar spring tides in a ground-nesting shorebird"
#by Plaschke et al 2019

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
{# Figure 3
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
		panel.spacing = unit(0, "mm"),
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
		
ggsave(file=paste(outdir,'Figure_3.png',sep=''),width = 3.5, height = 2, units = "in",dpi = 600)
  }
}
{# Figure S1
  	geom.text.size = 2
	theme.size = 7 ##theme.size = (14/5) * geom.text.size
	dev.new(width = 3.5, height = 2)
	
	s = ddply(dd,.(year), summarise, start_ = min(day_j), end_ = max(day_j))
		tt$start_ = s$start_[match(tt$year,s$year)]
		tt$end_ = s$end_[match(tt$year,s$year)]
		tt$breeding_season = ifelse(tt$day_j>=tt$start_ & tt$day_j<=tt$end_,'yes','no')
		tt = ddply(tt,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)
	tt_ =tt[which(tt$breeding_season=='yes'),]
    
	ggplot( tt_,aes(x = days_after_st, y = max_tide_height, col=st_cycle_c))+geom_point(size = 0.5)+facet_wrap(~year, nrow  =2)+
    expand_limits(x = 0, y = 0)+
	scale_x_continuous(limits = c(0,15),expand = c(0,0),breaks= seq(5,15,by = 5), labels=seq(5,15,by = 5))+  
    scale_y_continuous(limits = c(0,180),expand = c(0,0),breaks= seq(0,180,by = 60), labels=seq(0,180,by = 60))+
	labs(x = "Days after spring tide",color = 'Spring-tide\ncycle #', y ="Highest daily tide [cm]")+  
	scale_colour_gradient(low = "deepskyblue", high = "orange", guide = "colourbar")+
    #scale_colour_manual(values=c("grey80", "firebrick3"))+
	#guides(color = guide_legend(override.aes = list(size = 2)))+
    theme_light()+
    theme(#text = element_text(size = theme.size),
		axis.line=element_line(colour="grey70", size=0.25),
		panel.border=element_rect(colour="grey70",fill = NA, size=0.25),
		panel.grid = element_blank(),
		#panel.margin = unit(0, "mm"),
		panel.spacing = unit(0, "mm"),
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
		
ggsave(file=paste(outdir,'Figure_S1.png',sep=''),width = 3.5, height = 2, units = "in",dpi = 600)
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

{# Figure S2
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
		  scale_x_continuous(name = 'Longitude [??]', breaks= -106.9580, labels=c("-106.958"))+
		  labs(color = 'Flooded', y ='Latitude [??]')+  	
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

		  )
	ggsave(file=paste(outdir,'Figure_S2.png',sep=''),width = 3.5*1.5, height = 2*1.5, units = "in",dpi = 600)	  
}

# RESULTS
{# number and proportion of flooded nests
	summary(factor(nn$fate))
	summary(factor(nn$fate))/nrow(nn)
	nrow(nn[nn$fate=='tide'])
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
	nt = ddply(nn,.(year), summarise, n_all = sum(n), n = sum(n[fate%in%c('tide')]))
	summary(nt)
	table(nt$n)
	nt$prop_flooded = nt$n/nt$n_all
	summary(nt$prop_flooded*100)
	
}
{# FIGURE 4 - nest frequencies per year with tideheights (uses all nests)
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
		  geom_point(data=d[d$year==2012,], aes(x=196, y=2), inherit.aes=FALSE)+
		  geom_point(data=d[d$year==2015,], aes(x=196, y=2), inherit.aes=FALSE)+
		  #annotate("point", x = 190, y = 10, colour = "blue") +
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
		  
		 p <- p + scale_y_continuous(breaks=c(0,5,10,15),labels=c("0","5","10","15"), name= "Number of initiated nests", limits = c(0, 15),
		                       sec.axis = sec_axis(~ . * 10, name = "Tide height [cm]", breaks = c(0,50,100,150)))
		
		if(PNG == TRUE) {ggsave(file=paste(outdir,'Figure_4_rev.png',sep=''),width = 3.5, height = 3.5*1.5, units = "in",dpi = 600)}else{p}
		}
}

# FLOODING
  {# prepare	
	d = nn[nn$fate%in%c("hatch","unhatch","tide"),] # only flooded, hatched or unhatched nests
	summary(factor(d$fate))
	d$flooded = ifelse(d$fate=='tide',1,0)
	d$flooded_ = ifelse(d$fate=='tide','yes','no')
	d$year_ = as.factor(d$year)
	d$laid_j=as.POSIXlt(d$laid, format = "%d%b%y")$yday
	d$days_after_r = ceiling(d$days_after_st)
	d$rad_st = 2*d$days_after_st*pi/d$dur

	
	d = ddply(d,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1) # center within year
		summary(factor(d$st_cycle_c))
	d$st_cycle_c[d$st_cycle_c==7]=6 # only 1 datapoints for cycle 7, so make it 6 

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
  {# Table 1
	{# gaussian model
		m = lmer(flooded ~ scale(lat)+scale(st_cycle_c)+sin(rad_st) +cos(rad_st)+(1|year)+(1|st_cycle_c),d)
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
			??
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
			??
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

# NEST INITIATION
  {# prepare data
	dd$days_after_r = ceiling(dd$days_after_st)
	dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
	dd$l_n = 1:nrow(dd)
	dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
	dd$year_= as.factor(dd$year)
	
	dd = ddply(dd,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)#standardize within year

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
  {# Table 2
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
			??
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
			??
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
			??
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
			??
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

  {# Table S1
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
		{# models without imputed tide heights
		dd = dd[!dd$datetime_%in%dx$date,]
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
 

{# Figure 5
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
	#d=d[d$st_cycle_c<6,]
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
		png(paste(outdir,"Figure_5.png", sep=""), width=3.5,height=3.8,units="in",res=600) 
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

{# Figure S3 a - initiated nests 	 
	 {# prepare raw data
			dd$days_after_r = ceiling(dd$days_after_st)
			dd$rad_st= 2*dd$days_after_st*pi/dd$dur # instead of everage semil-lunar cycle 14.75, use actual one
			dd$first_second = as.factor(ifelse(0.5>dd$days_after_st/dd$dur,'second','first'))
			dd$year_= as.factor(dd$year)	
			dd = ddply(dd,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)#standardize within year
			dd$days_after_r = ceiling(dd$days_after_st)
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
	 {# prepare predictions
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
			png(paste(outdir,"Figure_S3a.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
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
						#mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
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
						#mtext("Days from spring tide",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
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
{# Figure S3 b  
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
	{# prepare raw data
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
	{# prepare predictions
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
	{# plot gaussina interaction
	 	if(PNG == TRUE) {
			png(paste(outdir,"Figure_S3b.png", sep=""), width=1.5*3,height=1.5,units="in",res=600) 
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
						#mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						#mtext("1st spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
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
						mtext("Highest daily tide [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						#mtext("3rd spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
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
						#mtext("Max daily tide height [m]",side=1,line=0.4, cex=0.6, las=1, col='grey30', xpd = TRUE)
						#mtext("# of initiated nests",side=2,line=1, cex=0.6, las=3, col='grey30')
						#mtext("5th spring-tide cycle",side=3,line=-0.4, cex=0.6, las=1, col='grey30')
						
						
						gii = gi[gi$st_cycle_c ==5,]
						polygon(c(gii$max_tide_height, rev(gii$max_tide_height)), c(gii$lwr, 
								rev(gii$upr)), border=NA, col=col_lb) #0,0,0 black 0.5 is transparents RED
						lines(gii$max_tide_height, gii$pred, col=col_l,lwd=1)
						symbols((x5$max_tide_height_r),(x5$mean_), circles=sqrt(x5$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) 
						
		if(PNG == TRUE) {dev.off()}
	}			
} 
  