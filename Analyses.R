# TO DO
# tide number

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
	st = unique(
	d$flooded = ifelse(d$f=='FLOOD',1,0)
	#d$rad_st= 2*d$hour*pi/24
	d$rad_st= 2*d$dast*pi/14.75
	
	m = glmer(flooded ~ sin(rad_st) + cos(rad_st) +(1|year), family = 'binomial', d) 
	plot(allEffects(m))
	summary(glht(m))