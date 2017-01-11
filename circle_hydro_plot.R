library(circlize)
load("/Users/kylechezik/Documents/Simon_Fraser_University/PhD_Research/Projects/River-Network-Flow-Trends/02a_Data_Clean.RData")
Wtshds = dplyr::filter(Data, Station.ID == "08MF005" | Station.ID == "08LF027" | Station.ID == "08LB024")
# Station 08LB024; Years = 1970, 2003, 2004
# Station 08LF027; Years = 2001, 2002
#Wtshds %>% dplyr::filter(Station.ID=="08MF005") %>% group_by(Year) %>% summarize(days = n()) %>% print.data.frame()
Date = c(seq(as.Date("1970-01-01"), as.Date("1970-12-31"), by = "day"),seq(as.Date("2001-01-01"), as.Date("2004-12-31"), by = "day"))
fill = data.frame(Station.ID = c(rep("08LB024",365), rep("08LF027",730), rep("08LB024",731)), Date = Date, Flow.Data = rep(NA, length(Date)), Year = format(Date, "%Y"), nMonth = as.numeric(format(Date, "%m")), Month = format(Date, "%b"), Day = as.numeric(format(Date, "%d")), DOY = as.numeric(format(Date, "%j")))
Wtshds = bind_rows(Wtshds, fill) %>% tbl_df()
Wtshds$Station.ID = as.factor(Wtshds$Station.ID); Wtshds$Month = as.factor(Wtshds$Month)
Wtshds = data.frame(Wtshds, Period = rep("annual",nrow(Wtshds))) %>% arrange(Station.ID,Date,DOY)
Wtshds = Wtshds %>% group_by(Station.ID) %>% mutate(window = sort(rep(seq(from=1, to=ceiling(n()/7), by = 1), 7))[1:n()]) %>% tbl_df()


#S_Wtrshd_Earlier = dplyr::filter(Data, Station.ID == "08MH029")
#S_Wtrshd_Later = dplyr::filter(Data, Station.ID == "08LB024")


#vector scaled 0-1 for function 
col = ((Wtshds$Year-min(Wtshds$Year))/(diff(range(Wtshds$Year))))

#color ramp function
library(RColorBrewer)
ramp = rev(brewer.pal(11, "RdYlBu"))
FUN = colorRamp(ramp, bias=1)

#apply function
cols = FUN(col)
cols = rgb(cols, maxColorValue=256)
cols = paste(cols, "99", sep = "")
Wtshds=data.frame(Wtshds, cols, stringsAsFactors = F) %>% tbl_df()

setwd("~/Documents/Simon_Fraser_University/PhD_Research/Projects/River-Network-Flow-Trends/GIF")
plyr::d_ply(Wtshds, "window", function(t){
	# creating a name for each plot file with leading zeros
	Year.n = as.character(unique(t$Year))
	DOY.n = as.character(max(t$DOY))
	name = paste(Year.n,DOY.n,'plot.png',sep='_')
	
	#saves the plot as a .png file in the working directory
	png(name)
	
	par(mar = c(1, 1, 1, 1), lwd = 0.9, cex = 0.7, bg = NA)
	circos.par("start.degree" = 90)
	circos.initialize(factors = "annual", xlim = c(1,366))
	plyr::d_ply(t, "Station.ID", function(i){
		#browser()
		circos.trackPlotRegion(factors = i$Period, x = i$DOY, y = i$Flow.Data, "track.height"=0.2, panel.fun = function(DOY, Flow.Data) {
			
			plyr::d_ply(i, "Year", function(o){
				circos.trackLines(o$Period, o$DOY, o$Flow.Data, col = o$cols, lwd = 1.3)
			})
			
			circos.axis(h = "top", track.index = 1)
		})
	})
	circos.clear()
	dev.off()
})

