# PREPARE DATA
{# nest data
   nn = read.csv(file=paste(wd, "SNPL_nest_bird_data.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE) 
		
   nn$floated = as.Date(nn$floated, "%Y-%m-%d")
   nn$found = as.Date(nn$found, "%Y-%m-%d")
   nn$end_ = as.Date(nn$end, "%Y-%m-%d")
   nn$clutch_size = as.numeric(nn$clutch_size)
		
   # prepare laying dates
	# floating stage of youngest egg to later calculate nest initiation
	nn$float_1 = as.numeric(nn$float_1)
	nn$float_2 = as.numeric(nn$float_2)
	nn$float_3 = as.numeric(nn$float_3)
	nn$float_min = pmin(nn$float_1,nn$float_2,nn$float_3, na.rm=TRUE) # smallest value equals the youngest egg

	# exclude 13 failed nests found when older >10 
	nrow(nn[which(nn$float_min == 11 & nn$fate !='hatch'),]) # 13
	nn <- nn[-which(nn$float_min == 11 & nn$fate !='hatch'),]

	# calculate start of incubation date based on floated date and floating stages (found date- floating stage min in days)
	nn$init = nn$floated - days(nn$float_min) 

	# adjust start of incubation for 42 nests that hatched and according to floating found older >10 days (28 of which 5 during hatching) or  we miss the floating info 
	  # check first
		length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' & (nn$float_min == 11 | is.na(nn$floated) | nn$floated == nn$end_ ) )]) # 42
		length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' & nn$float_min == 11 )]) # of which 25 found old and hatched
			length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch'& nn$float_min == 11 & nn$found == nn$end_  )]) # of which 1 found during hatching
		length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' & is.na(nn$floated))]) # 17 not floated, but hatched
			length(nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' & is.na(nn$floated) & nn$found == nn$end_)]) # of which 7 found at hatching 

	   # calculate	
	   nn$init[which(!is.na(nn$end_ ) & nn$fate=='hatch' & (nn$float_min == 11 | is.na(nn$floated) | nn$floated == nn$end_ ) )] = nn$end_[which(!is.na(nn$end_ ) & nn$fate=='hatch' & (nn$float_min == 11 | is.na(nn$floated) | nn$floated == nn$end_ ) )]-days(25)
	
	# nest initiation (first egg laid) based on incubation onset and clutch size - later adjusted for those found during laying	
	  nn$laid <- ifelse(!is.na(nn$clutch_size) & nn$clutch_size == 3, nn$init-days(5), nn$init)
	  nn$laid <- ifelse(!is.na(nn$clutch_size) & nn$clutch_size == 2, nn$init-days(3), nn$laid)
	
	# adjust for nests found during egg laying
	nrow(nn[!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & nn$clutch_size_found!=nn$clutch_size,])
	 # adjust for 1 nest found with 1 egg (later three), but we lack foating of that egg - we assume that it was laid previous day
	   nn$laid[!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & is.na(nn$float_min) & nn$clutch_size_found!=nn$clutch_size] = nn$found[!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & is.na(nn$float_min) & nn$clutch_size_found!=nn$clutch_size]-days(1)

	 # adjust laying data for 92 nests found during laying based on found date and clutch size and floating state of the youngest egg which idicates number of days before youngest egg was laid before found
	  nrow(nn[!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & !is.na(nn$float_min) & nn$clutch_size_found!=nn$clutch_size,])
		#nrow(nn[!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & !is.na(nn$float_min) & nn$clutch_size_found < nn$clutch_size,]) # 3 unclear cases left
			#nrow(nn[is.na(nn$clutch_size_found) | is.na(nn$clutch_size) |is.na(nn$float_min) | nn$clutch_size_found==nn$clutch_size  ,]) #747
	  summary(factor(nn$clutch_size_found))
	  nn$laid <- ifelse(!(!is.na(nn$clutch_size_found) & !is.na(nn$clutch_size) & !is.na(nn$float_min) & nn$clutch_size_found!=nn$clutch_size), nn$laid,    # if we do NOT have clutch size found and clutch size and floating stage and nest was complete take LAID, else
					ifelse(!is.na(nn$clutch_size_found) & nn$clutch_size_found == 1, nn$found-days(nn$float_min),                                                                                  # if nest was incomplete...
						ifelse(!is.na(nn$clutch_size_found) & nn$clutch_size_found == 2, nn$found-days(nn$float_min)-3, # assuming 3 day period for 2 egg clutch
							   nn$laid))) # I had this here ' nn$found-days(nn$float_min)-days(5)' but that is taken care of in the initial laid script 54 alread, right?) 
								# Yes, after correcting the code in line 53, everything is fine.
	   
							
	nn$laid <- as.Date(nn$laid, origin ="1970-01-01") # to transform it back to date from 	
	nn$laid=as.POSIXct(nn$laid)
    nn$pk=1:nrow(nn)
	
	n = nn
   # impute locations 
			l = list()
			x = unique(n$nest[is.na(n$lat)])
			for( i in x){
				xi = n[n$nest==i,]
				ni = n[n$laid>xi$laid-5*24*60*60 & n$laid<xi$laid+5*24*60*60,]
				xi$lat = median(ni$lat,na.rm=TRUE)
				xi$lon = median(ni$lon,na.rm=TRUE)
				l[[i]] = xi
				}
			xx = do.call(rbind,l)	
			n = rbind(n[!n$nest%in%x,],xx)	
		
	# moon tide cycle data	
		  g = read.csv(file=paste(wd, "moonphases.csv", sep=''), header = TRUE,sep=";", fill=T, stringsAsFactors=FALSE) 
		  
		  # create spring-tide cycle # within each year
			gs=g[g$event%in%c('nm','fm'),]
			gs$int=c(gs$datetime_[-1],gs$datetime_[nrow(gs)])
			gs$st_start=as.POSIXct(gs$datetime_)
			gs$st_end=as.POSIXct(gs$int)
			gs=gs[order(gs$year, gs$st_start),]
			gs = ddply(gs,.(year),transform, st_cycle=1:length(year))
			gs_=gs[,c('st_start','st_end','st_cycle')] 
			gs_$dur = as.numeric(difftime(gs_$st_end, gs_$st_start, units = 'days'))
				#nrow(gs_[gs_$dur>14.765,])
				#gs_[gs_$dur>14.765,]
		  
		  # add tide cycles to nests
			j =  sqldf("select*from n join gs_", stringsAsFactors = FALSE)
			nn = sqldf("select*from j WHERE laid BETWEEN  st_start and st_end OR laid = st_start")
			#n[!n$pk%in%nn$pk,]
		  # standardized cycles withih year
			nn = ddply(nn,. (year), transform, st_cycle_c = st_cycle - min(st_cycle)+1)

		  # calculate days after last spring tide
			nn$days_after_st = as.numeric(difftime(nn$laid, nn$st_start, units ='days'))	
			nn$days_after_st_r = ceiling(nn$days_after_st)
			#summary(nn$days_after_st)
		
		  # create moon cycle # within each year
			gs=g[g$event%in%c('nm'),]
			gs$int=c(gs$datetime_[-1],gs$datetime_[nrow(gs)])
			gs$m_start=as.POSIXct(gs$datetime_)
			gs$m_end=as.POSIXct(gs$int)
			gs=gs[order(gs$year, gs$m_start),]
			gs = ddply(gs,.(year),transform, moon_cycle=1:length(year))
			gss_=gs[,c('m_start','m_end','moon_cycle')] 
		
		  # add moon cycles to nests
			j =  sqldf("select*from nn join gss_", stringsAsFactors = FALSE)
			nn = sqldf("select*from j WHERE laid BETWEEN  m_start and m_end OR laid = m_start")
			#n[!n$pk%in%nn$pk,]	
		
		  # calculate days after last new moon
			nn$days_after_nm = as.numeric(difftime(nn$laid, nn$m_start, units ='days'))	
			
		  # add tide hight at the given day
			tt = read.csv(file=paste(wd, "tides_all.csv", sep=''), header = TRUE,sep=",", fill=T, stringsAsFactors=FALSE) 
			tt = tt[,c('pk','year','date','max_tide_height')]
			tt$date = as.POSIXct(tt$date)
			
			# substitute NAs with mean height of previous and next day
				dx = tt[is.na(tt$max_tide_height),c('date','max_tide_height')]
				dx2 = tt[!is.na(tt$max_tide_height) & as.character(tt$date)%in%as.character(dx$date+24*60*60),c('date','max_tide_height')]
				dx2$date=dx2$date-24*60*60
				dx3 = tt[!is.na(tt$max_tide_height) & as.character(tt$date)%in%as.character(dx$date-24*60*60),c('date','max_tide_height')]
				dx3$date=dx3$date+24*60*60
				dxx = rbind(dx2,dx3)
				dxx = ddply(dxx,.(date),summarise, max_tide_height=mean(max_tide_height))
				tt$max_tide_height[is.na(tt$max_tide_height)] = dxx$max_tide_height[match(tt$date[is.na(tt$max_tide_height)], dxx$date)]
				
			nn$max_tide_height =tt$max_tide_height[match(nn$laid, tt$date)] 
		
		# add moon illumination at the given day	
			ii = read.csv(file=paste(wd, "illumination_all.csv", sep=''), header = TRUE,sep=",", fill=T, stringsAsFactors=FALSE) 
			nn$illum_mid = ii$illumination_noon[match(as.character(nn$laid), substring(ii$noon,1,10))]

	}		
{# aggregated dataset with number of nests per day
		# run 'nest data' part first
		dd = ddply(nn[!is.na(nn$fate),],. (year, laid ), summarise, n_nest = length(year)) # exclude fates with NA
		#dd = ddply(nn,. (year, laid ), summarise, n_nest = length(year))	
		names(dd)[names(dd)=='laid'] = 'datetime_'
		
		# include days with no nests laid
		dsplit=split(dd,paste(dd$year))
		foo=lapply(dsplit,function(x) {
				#x=dsplit$"2006"
				y = data.frame(datetime_ = seq(min(x$datetime_), max(x$datetime_), by = 'day'), n_nest = 0, year = x$year[1])
				y = y[!y$datetime_%in%x$datetime_,]
				y = 
				#x$t_a=c(x$treat[-1], NA) 	
				x = merge(x,y, all=TRUE)
				return(x)
				})
				
		dd=do.call(rbind, foo)		
		
		# julian date
			dd$day_j = as.numeric(format(as.POSIXct(dd$datetime_),"%j"))	
		
		# add tide cycles to nests
		j =  sqldf("select*from dd join gs_", stringsAsFactors = FALSE)
		dd = sqldf("select*from j WHERE datetime_ BETWEEN  st_start and st_end OR datetime_ = st_start")
		dd$days_after_st = as.numeric(difftime(dd$datetime_, dd$st_start, units ='days'))		
		
		# add moon cycles to nests
		j =  sqldf("select*from dd join gss_", stringsAsFactors = FALSE)
		dd = sqldf("select*from j WHERE datetime_ BETWEEN  m_start and m_end OR datetime_ = m_start")
		dd$days_after_nm = as.numeric(difftime(dd$datetime_, dd$m_start, units ='days'))	
		# add max tide height
		dd$max_tide_height =tt$max_tide_height[match(dd$datetime_, tt$date)]
	}	
{# finalize tide dataset	
		# add moon cycles to tide
			jj =  sqldf("select*from tt join gss_", stringsAsFactors = FALSE)
			tt_ = sqldf("select*from jj WHERE date BETWEEN  m_start and m_end OR date = m_start")
		# add tide cycles to tide
			jj =  sqldf("select*from tt_ join gs_", stringsAsFactors = FALSE)
			tt = sqldf("select*from jj WHERE date BETWEEN  st_start and st_end OR date = st_start")
		# calculate days after last new moon
			tt$days_after_nm = as.numeric(difftime((tt$date), tt$m_start, units ='days'))	
		# calculate days after last spring tide
			tt$days_after_st = as.numeric(difftime((tt$date), tt$st_start, units ='days'))	
		# use only complete data and years for which we have nesting data
			tt = tt[complete.cases(tt),]
			tt = tt[!tt$year == 2014,]
		# julian date
			tt$day_j = as.numeric(format(tt$date,"%j"))
}