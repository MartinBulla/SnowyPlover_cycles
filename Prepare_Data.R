{# LOAD & PREPARE DATA
	# nest data
	  n = read.csv(file=paste(wd, "metadata_nests_birds.csv", sep=''),header=T,sep=";", fill=T, stringsAsFactors=FALSE, col.names=c('year','nest','found','laid','end','fate','lat','lon','male','female'))
		
	  nn = read.csv(file=paste(wd, "metadata_nests_birds_CK.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE, col.names=c('year','nest','found','laid','end','fate1','fate','lat','lon','male','female'))
	  	 n$fate = nn$fate[match(tolower(paste(n$year,n$nest)), tolower(paste(nn$year,nn$nest)))]
		#n$lat=gsub(",", ".", n$lat)
		#n$lon=gsub(",", ".",n$lon)
		n = n[!is.na(n$year),]
		n = n[!is.na(n$laid),]
		n$laid=as.POSIXct(n$laid)
		n$fate=tolower(n$fate)
		n$pair=paste(n$male,n$female)
		n$pk=1:nrow(n)
		
	# moon tide cycle data	
	  g = read.csv(file=paste(wd, "moonsequ_tidesequ.csv", sep=''), header = TRUE,sep=";", fill=T, stringsAsFactors=FALSE, col.names=c('year','datetime_','event','moon_cycle','start_moon_cycle','tide_cycle','start_tide_cycle')) 
	  ### later delete
	  #g = read.csv(file=paste(wd, "moonsequ_tidesequ_mtide_illu.csv", sep=''), header = TRUE,sep=",", fill=T, stringsAsFactors=FALSE, col.names=c('pk','year','datetime_','event','moon_cycle','start_moon_cycle','tide_cycle','start_tide_cycle', 'max_tide_height','i_noon')) #moonsequ_tidesequ
	  #ggplot(g, aes(x=event, y=i_noon, col = as.factor(year)))+geom_point()
	  ### later delter
	  
	# create spring-tide cycle # within each year
		gs=g[g$event%in%c('nm','fm'),]
		gs$int=c(gs$datetime_[-1],gs$datetime_[nrow(gs)])
		gs$st_start=as.POSIXct(gs$datetime_)
		gs$st_end=as.POSIXct(gs$int)
		gs=gs[order(gs$year, gs$st_start),]
		gs = ddply(gs,.(year),transform, st_cycle=1:length(year))
		gs_=gs[,c('st_start','st_end','st_cycle')] 
	
	# add tide cycles to nests
		j =  sqldf("select*from n join gs_", stringsAsFactors = FALSE)
		nn = sqldf("select*from j WHERE laid BETWEEN  st_start and st_end OR laid = st_start")
		#n[!n$pk%in%nn$pk,]
	
	 # calculate days after last spring tide
		nn$days_after_st = as.numeric(difftime(nn$laid, nn$st_start, units ='days'))	
		
	# create moon cycle # within each year
		gs=g[g$event%in%c('nm'),]
		gs$int=c(gs$datetime_[-1],gs$datetime_[nrow(gs)])
		gs$m_start=as.POSIXct(gs$datetime_)
		gs$m_end=as.POSIXct(gs$int)
		gs=gs[order(gs$year, gs$m_start),]
		gs = ddply(gs,.(year),transform, moon_cycle=1:length(year))
		gs_=gs[,c('m_start','m_end','moon_cycle')] 
	
	# add tide cycles to nests
		j =  sqldf("select*from nn join gs_", stringsAsFactors = FALSE)
		nn = sqldf("select*from j WHERE laid BETWEEN  m_start and m_end OR laid = m_start")
		#n[!n$pk%in%nn$pk,]	
	
	# calculate days after last new moon
		nn$days_after_nm = as.numeric(difftime(nn$laid, nn$m_start, units ='days'))	
		
	# add tide hight and illumination at the given day
		tt = read.csv(file=paste(wd, "tides_all.csv", sep=''), header = TRUE,sep=",", fill=T, stringsAsFactors=FALSE) 
		str(nn)
		nn$max_t_h =tt$max_tide_height[match(as.character(nn$laid), tt$date)] # some days have all high tides NA and hence there are no data, we shall use the next hight tide data
		
		ii = read.csv(file=paste(wd, "illumination_all.csv", sep=''), header = TRUE,sep=",", fill=T, stringsAsFactors=FALSE) 
		#nn$illum_mp = ii$illumination_mp[match(as.character(nn$laid), substring(ii$meridian_passing,1,10))]
		nn$illum_noon = ii$illumination_noon[match(as.character(nn$laid), substring(ii$noon,1,10))]
			#plot(nn$illum_mp~nn$illum_noon)
}