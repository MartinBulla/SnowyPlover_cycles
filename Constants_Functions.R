{# TOOLS
	{# define datetime
		Sys.setenv(TZ="UTC")	
	}
	
	{# load packages
		sapply(c('arm','effects', 'data.table','ggplot2','grid', 'lattice','magrittr','multcomp','plyr','raster','RColorBrewer','sqldf','stringr','XLConnect'),
    function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ))

	}
	
	{# define constants
		varnames = c("tag", "datetime_", "x", "y","z", "temp", "batt")
		
		min_ = -0.1
		max_ = 6
		
		#yr_col = mypalette<-brewer.pal(12,"Greens")
		wr_col="grey50"
		ln_col="grey80"
		disturb='#5eab2b'
		cv_x = "#99c978"
		cv_y = "#f0b2b2"
		cv_z = "#ADD8E6"
		tem = 'dodgerblue'
		col_f = '#FCB42C'
		col_m = '#535F7C'
		
		col_p="gray53"  # color of point's outline
		col_pb="gray98"  # color of point's background
		col_l="gray73"  # line color of prediction '#FCB42C'
		col_lb="gray92"  # line color of prediction '#FCB42C'
				
		# writing color
				wr_col="grey50"
				ln_col="grey80"

	}
	
}	

