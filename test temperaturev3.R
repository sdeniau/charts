library(rjson)
library(tidyverse)
library(lubridate)
library(stringr)
library(ggtext)
library(systemfonts)
library(this.path)
library(gifski)

#set working directory = source file directory
sourcedir <- this.dir()
setwd(sourcedir)

#set fonts for charts
register_variant(
        name = "subtitle",
        family = "EB Garamond",
        weight = "normal"
)

register_variant(
        name = "title",
        family = "Fira Sans Condensed",
        weight = "ultrabold"
)

register_variant(
        name = "text",
        family = "Fira Sans",
        weight = "normal"
)

register_variant(
        name = "label",
        family = "Fira Sans",
        weight = "medium"
)

register_variant(
        name = "axis",
        family = "Fira Mono",
        weight = "medium"
)

# Set theme for ggplot
theme_set(theme_minimal())
theme_update(
        text = element_text(family = "text"),
        plot.title = element_markdown(family = "title",
                                      size = 24,
                                      color = "gray10"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(
                family= "subtitle",
                face = "italic",
                size = 18,
                margin = margin(0, 0, 15, 0),
                color = "gray40"
        ),
        axis.text = element_text(family="axis", size = 12),
        axis.title = element_text(family="label", size = 12),
        axis.line = element_line(color = "gray60"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        plot.margin = margin(15, 5, 11, 5),
        plot.background = element_rect(
                fill = "#E3E4FF", 
                color = "#E3E4FF"
        ),
        plot.caption = element_textbox_simple(
                family="text",
                lineheight = 1,
                size = 12,
                color = "grey40",
                margin = margin(10, 0, 0, 0),
                hjust = 0
        ),
        plot.caption.position = "plot",
        legend.position = "none"
)


# download data from JSON file and build dataframe

result <- fromJSON(file = "oisst2.1_natlan1_sst_day_v3.json")

is.not.null <- function(x) !is.null(x)

df <- data.frame(year = integer(),    
                    day = numeric(),
                    temp = numeric())

for (i in 1:43){
        list <- result[[i]]  
        year <- as.integer(list[[1]])

        for(j in 1:366){
        temp <- list[[2]][[j]]
        if (is.not.null(temp)) df <- rbind(df,c(year,j,temp))

        }
}

names(df) <- c("year","day","temp")

moy <- data.frame(day = numeric(),
                 temp = numeric())
list<-result[[44]]
for(j in 1:366){
        temp <- list[[2]][[j]]
        if (is.not.null(temp)) moy <- rbind(moy,c(j,temp))
        
}
names(moy)<-c("day","tempmoy")

data <- df |> 
        left_join(moy,by=c("day"="day")) |> 
        mutate(anom=temp-tempmoy,
               date=parse_date_time(x = paste(year, day), 
                                    orders = "yj"),
               month=month(date),
               nday=day(date)
)

# labels for ggplot chart

title = "Sea Surface Temperature anomaly (North Atl.)"
caption = "Source: Sea Surface Temperature (SST) from NOAA Optimum Interpolation SST (OISST) version 2.1.
                Anomaly calculate from 1982-2011 SST mean"

# create png file for each year x month

for (theyear in 1981:2023){
        monthmin <- min(data |> filter(year==theyear)
                        |> select(month))
        monthmax <- max(data |> filter(year==theyear)
                        |> select(month))
for(nmonth in monthmin:monthmax)   {     
        subyear <- paste("<span style = 'color:#ff0000;'>",theyear,"</span>",sep="")
        subtitle <- paste ("from 1981 to",subyear,sep=" ")
png<-ggplot(data |> filter(year <theyear),aes(x=day,y=anom,group=year)) +
        annotate("text",x=182,y=0.5,label="+0.5°C") +
        annotate("text",x=182,y=1,label="+1.0°C") +
        annotate("text",x=182,y=0,label="+0°C",color="blue") +
        geom_path(linewidth=.3, color="darkgrey",) +
        geom_path(data=data |> filter(year==theyear,month<=nmonth),color="red")+
        geom_hline(yintercept=0,color="blue", 
                   linetype="dashed",
                   linewidth=0.5)+
        labs(title=title,
             subtitle=subtitle,
             caption=caption) +
        coord_polar(start=pi) +
        scale_y_continuous(limits = c(-0.7,1.3),
                           breaks=c(-0.5,0,0.5,1)) +
        scale_x_continuous(limits = c(1,365),
                           breaks=seq(1, 331, by= 30),
                           labels=c("Jan.","Feb.","Mars",
                                    "Apr.","May","June",
                                    "Jul.","Aug.","Sep.",
                                    "Oct.","Nov.","Dec.")) +
        theme(plot.title.position = "plot",
              plot.caption.position = "plot",
              legend.position = "none",
              axis.line = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())

nm<-nmonth
if (nm<10) nm<-paste("0",nm,sep="")

ggsave(paste("images/plot",theyear,nm,".png",sep=""), width = 7, height = 8)

} #end months loop
} #end years loop

# create animated gif files from all png files

png_files <- list.files("images/", pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = "animation.gif", width = 700, height = 800, delay = 0.03, loop=FALSE)

