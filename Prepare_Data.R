{# LOAD & PREPARE DATA
	# nest data
	  n = read.csv(file=paste(wd, "metadata_nests_birds.csv", sep=''),header=T,sep=";", fill=T, stringsAsFactors=FALSE, col.names=c('year','nest','found','laid','end','fate','lat','lon','male','female'))
		n = n[!is.na(n$year),]
		n = n[!is.na(n$laid),]
		n$laid=as.POSIXct(n$laid)
		n$fate=tolower(n$fate)
		n$pair=paste(n$male,n$female)
		n$pk=1:nrow(n)
		#n$lat=gsub(",", ".", n$lat)
		#n$lon=gsub(",", ".",n$lon)
		
	# moon tide data	
	  g = read.csv(file=paste(wd, "moonsequ_tidesequ.csv", sep=''),header=T,sep=";", fill=T, stringsAsFactors=FALSE, col.names=c('year','datetime_','event','moon_cycle','start_moon_cycle','tide_cycle','start_tide_cycle'))
	
	# create tide cycle # within each year
		gs=g[g$event%in%c('nm','fm'),]
		gs$int=c(gs$datetime_[-1],gs$datetime_[nrow(gs)])
		gs$datetime_=as.POSIXct(gs$datetime_)
		gs$int=as.POSIXct(gs$int)
		gs=gs[order(gs$year, gs$datetime_),]
		gs = ddply(gs,.(year),transform, tide_cycle=1:length(year))
		gs_=gs[,c('datetime_','int','tide_cycle')] 
	
	# add tide cycles to nests
		j =  sqldf("select*from n join gs_", stringsAsFactors = FALSE)
		nn = sqldf("select*from j WHERE laid BETWEEN  datetime_ and int OR laid = datetime_")
		#n[!n$pk%in%nn$pk,]
	
	# calculate days after last spring tide
		nn$days_after_st = as.numeric(difftime(nn$laid, nn$datetime_, units ='days'))
}