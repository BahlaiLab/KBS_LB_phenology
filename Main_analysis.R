# Main analysis of KBS Ladybeetle 2024 data, all species niche partitioning and phenology

#bring data into R

LB<-read.csv(file="data/LTER2024_working_alldata1.csv", header=T,
             na.strings=c(NA))


##### let's take a look at this data
summary(LB)

#clean data
#first, we fix dates, make them ISO'ed
library(lubridate)
 #previous format had capitalized dates and changed the name of the various columns- let's harmonize that

colnames(LB)[which(names(LB) == "Date")] <- "DATE"
colnames(LB)[which(names(LB) == "ADULTS")] <- "SumOfADULTS"
colnames(LB)[which(names(LB) == "TREAT_DESC")] <- "TREAT"
colnames(LB)[which(names(LB) == "Replicate")] <- "REPLICATE"

# 
# #not run
# LB$newdate<-mdy(LB$DATE)#parses the date format used for the forest plots
# LB$newdate<-mdy_hm(LB$DATE)#parses the date format used in the main plots

#well, crap, neither command gets all of the dates to parse correctly
#we have an issue because data from the main site exported with time stamps but 
#the forest site did not. Ugh.


#Christie's solution: brute force removal of timestamps
#access appended timestamps in A NEW FORMAT OMG
LB$DATE<-gsub(" 0:00:00", "", LB$DATE)#remove time stamp strings

LB$newdate<-mdy(LB$DATE)#parses the date format now used by all observations

LB$DOY<-yday(LB$newdate)
LB$week<-isoweek(LB$newdate)
LB$Year<-year(LB$newdate)

summary(LB)#bingo! looks like it worked!


#let's reorder our habitats right here at the top
LB$HABITAT<-factor(LB$HABITAT, 
                   levels=c("maize", "soybeans","wheat", "alfalfa", "poplar", "ES", "CF", "DF", "SF"))
#and relabel them so they're in the same case
levels(LB$HABITAT)<-c("maize", "soybean","wheat", "alfalfa", "poplar", "ES", "coniferous", "deciduous", "succesional")


#let's take a look at ladybeetles by treatment
#we need to aggregate the data by rep first, because subsamples are zero-biased
library(dplyr)

#filter the data for only LB species we want to focus on- the native species for this study!

lb_list<-c("ABIPN", "BURSI", "CMAC", "CSTIG","CTRIF", "CYCSP", "H13", "HCONV","HGLAC", 
           "HPARN")

LB<-LB[which(LB$SPID %in% lb_list),] 

lb_rep<-aggregate(data=LB, SumOfADULTS~ Year+week+TREAT+HABITAT+REPLICATE+SPID, FUN=sum)
lb_rep_N<-aggregate(data=LB, SumOfADULTS~ Year+week+TREAT+HABITAT+REPLICATE+SPID, FUN=length)
#change variable name to reflect that it's number of traps
lb_rep_N<-rename(lb_rep_N, TRAPS=SumOfADULTS)
#merge trap data into lb_rep data frame

lb_weekly<-merge(lb_rep, lb_rep_N)

#in previous analysis we culled early data but we want to keep it in for this analysis
##cull data prior to Harmonia's arrival in 1994
#lb_weekly1994<-lb_weekly[which(lb_weekly$Year>=1994),]

#let's figure out how to re-categorize our treatments. Remember T1-4 are annual, 5-7 are perennial, and the rest are forest
annuallist<-c("T1","T2", "T3", "T4" )
perlist<-c("T5", "T6", "T7")
lb_weekly$TREAT_CAT<-ifelse(lb_weekly$TREAT %in% annuallist, "Annual",
                                (ifelse(lb_weekly$TREAT %in% perlist, "Perennial", "Forest")))

#remember to cull the data at a standard time point 
#(we use DOY 222 in other studies which corresponds to week 32, 
# but this cuts out a major harmonia activity peak, so let's use first week of sept
# =week 35)

lb_weekly_culled<-lb_weekly[which(lb_weekly$week<=35),]



library(ggplot2)

lb_boxplot<-ggplot(lb_weekly_culled, aes(x=TREAT_CAT, y=SumOfADULTS, fill=SPID))+
  geom_boxplot()
lb_boxplot

#let's try re-aggregating our data at a yearly resolution
lb_yearly_captures<-aggregate(data=lb_weekly_culled, SumOfADULTS~ Year+TREAT+HABITAT+REPLICATE+SPID, FUN=sum)
lb_yearly_N<-aggregate(data=lb_weekly_culled, TRAPS~ Year+TREAT+HABITAT+REPLICATE+SPID, FUN=sum)


#also, just so we know what we're comparing here, how many of each species did we catch?
lb_tots<-aggregate(data=lb_weekly_culled, SumOfADULTS~ SPID, FUN=sum)
lb_tots

#merge yearly captures with sampling intensity data
lb_yearly<-merge(lb_yearly_captures, lb_yearly_N)
#compute a new variable- average number of beetles per trap
lb_yearly$pertrap<-lb_yearly$SumOfADULTS/lb_yearly$TRAPS



#let's repeat the boxplot but with yearly data
lb_yearly_boxplot<-ggplot(lb_yearly, aes(x=HABITAT, y=pertrap, fill=SPID))+
  geom_boxplot()+
  #comment out colour scale because it's only for 2 species- do throughout
  #scale_fill_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"))+
  labs(x="Plant community", y="Captures per trap", fill="Species")+
  theme_classic()+ theme(legend.position = "none", axis.text.x=element_text(angle=90))
lb_yearly_boxplot

# #import ladybug icons
# library(jpeg)
# library(magick)
# library(cowplot)
# library(grid)
# 
# 
# harm<-magick::image_read("data/ha_square.jpeg")
# csept<-magick::image_read("data/c7_square.jpeg")
# lbset<-c(csept, harm)
# 
# lbicon<-magick::image_append(image_scale(lbset, "100"), stack=TRUE)

#let's look at the populations over time instead
lb_yearly_plot<-ggplot(lb_yearly, aes(x=Year, y=SumOfADULTS, fill=SPID, shape=SPID, linetype=SPID, color=SPID))+
  geom_point(size=0.5, position="jitter", alpha=0.5)+
  geom_smooth()+
  facet_wrap(~SPID, ncol=2)+
  #scale_fill_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  #scale_color_manual(values=c("darkred", "darkorange"), labels=c("C7", "HA"), name="Species")+
  #scale_shape_manual(values=c(4, 1), labels=c("C7", "HA"), name="Species")+
  #scale_linetype_manual(values=c(1, 1), labels=c("C7", "HA"), name="Species")+
  labs(x="Year", y="Captures per trap")+
  theme_classic()+ theme(legend.background = element_rect(fill='transparent'))#+
  
  #annotation_custom(rasterGrob(lbicon), 2016.05, 2017.95, 153, 183)

lb_yearly_plot



# rawtrends<-plot_grid(lb_yearly_plot, lb_yearly_boxplot,  ncol=1, rel_widths=c(1), labels=c('A', 'B'), 
#                      align="v", axis="l")
# 
# rawtrends
# 
# pdf("plots/figurerawtrends.pdf", height=8, width=6, bg="white")
# rawtrends
# dev.off()
# 
# svg("plots/figurerawtrends.svg", height=8, width=6, bg="white")
# rawtrends
# dev.off()




###################################
#Begin weather data processing



#download weather data from KBS weather station
#download a local copy into the data folder and then pull it from there to load

# # how file was downloaded- commented out so it's not run and re-downloaded each time
# # Specify URL where file is stored
# url <- "http://lter.kbs.msu.edu/datatables/7.csv"
# # Specify destination where file should be saved
# destfile <- "data/kbsweather.csv"
# # Apply download.file function in R
# download.file(url, destfile)


weather<-read.csv(file="data/kbsweather2025.csv",
                    header=T, sep=",", na.strings="", comment.char = '#')
#extract day of year, so we have a continuous variable running for each year.
#since we're in a temperate northern climate, this is convenient- not too 
#much insect action happening at the december-january transition, so we 
#can use the yearly break as a blocking variable for rowing season.
#it's convenient living where we do! 

weather$DOY<-yday(weather$date)
weather$week<-isoweek(weather$date)
#do a few simple plots to make sure the data makes sense -this is
#a good way to check that the importation was successful

plot(weather$DOY, weather$air_temp_mean)
plot(weather$DOY, weather$precipitation)

#let's cut out the data from before 1989 so we can process the weather data more quickly. Al so we'll cut off the weather
#data that's causing us problems- we don't need it anyway
weather<-subset(weather, weather$year>=1989& weather$year<=2024)


#lets also get rid of the variables we don't need:
weather$flag_precip<-NULL
weather$flag_air_temp_mean<-NULL
weather$flag_air_temp_max<-NULL
weather$flag_air_temp_min<-NULL

#also, these data are sorted in descending order. It's easier to think of this 
#stuff in ascending order, so let's sort the data by year and DOY

weather<-weather[order(weather$year, weather$DOY),]

#Let's examine the data to see how complete it is
summary(weather)

#let's pre-process these weather data so we get rid of missing values
# we can write a function to do this for us.
#if missing data is rare, it is probably safe to assume that missing
#temperatures are similar to the weather on the day before or after.
#for the sake of simplicity, let's replace a missing value with the 
#value for that variable for the day before

#first, define the function

replace.missing<-function(vec){
  #create a vector to put our new values into
  New = c()
  for (i in 1:(length(vec))){
    if (is.na(vec[i])){
      vec[i]<-mean(c(vec[i-1], vec[i+1]), na.rm=TRUE)
      #if the data is missing, sub in the value from the measurement before
      
    } else{
      #if the value is not missing, just pass it through to the result vector
      vec[i]<-vec[i]
    }
    New=c(New, vec[i])
  }
  if (any(is.na(New))){
    replace.missing(New)
  }
  return(New)
}
#now let's use our replace missing function to gap fill our weather data
weather$temp_mean_cleaned<-replace.missing(weather$air_temp_mean)
weather$temp_min_cleaned<-replace.missing(weather$air_temp_min)
weather$temp_max_cleaned<-replace.missing(weather$air_temp_max)

# calculate the degree day accumulation for the first half of the day dd1,
#assuming a sine wave structure of temperature over the day
#use a development threshold of 10C, well, because it's a nice number
#to work with
#we'll use the model presented in Allen 1976 which uses daily max and min temperatures
#and assumes temperature follows a sine wave

allen<-function(maxi, mini, thresh){
  #if threshold is not given, assume it's 10 Celcius
  if(missing(thresh)) {
    thresh<-10
  } else {
    thresh<-thresh
  }
  dd1<-c()
  dd2<-c()
  for (i in 1:length(maxi)){
    if (maxi[i]>= thresh & mini[i]<thresh) {
      #first half of day
      #amplitude of temperature difference
      alpha1<-(maxi[i]-mini[i])/2
      #average temperature
      avg1<-(maxi[i]+mini[i])/2
      #theta is time point when temperature crosses the threshold
      #assuming temperature is roughly following the sine curve
      theta1<-asin((thresh-avg1)/alpha1)
      #use these to calculate degree day accumulation over first half of day
      dd1.T<-(1/(2*pi))*((avg1-thresh)*(pi/2 - theta1)+alpha1*cos(theta1))
      dd1<-c(dd1, dd1.T)
      #second half of day
      #two possible cases, min temperature on day i+1 could be below thereshold or above
      #for below threshold:
      if (mini[i+1]<thresh){
        #amplitude of temperature difference
        alpha2<-(maxi[i]-mini[i+1])/2
        #average temperature
        avg2<-(maxi[i]+mini[i+1])/2
        #theta is time point when temperature crosses the threshold
        #assuming temperature is roughly following the sine curve
        theta2<-asin((thresh-avg2)/alpha2)
        #use these to calculate degree day accumulation over first half of day
        dd2.T<-(1/(2*pi))*((avg2-thresh)*(pi/2 - theta2)+alpha2*cos(theta2))
        dd2<-c(dd2, dd2.T)
      } else { #for above threshold
        #second half of day
        avg2<-(maxi[i]+mini[i+1])/2
        dd2.T<-(avg2-thresh)/2
        dd2<-c(dd2, dd2.T)
      }
      
    } else if (mini[i]>=thresh){
      #first half of day
      avg1<-(maxi[i]+mini[i])/2
      dd1.T<-(avg1-thresh)/2
      dd1<-c(dd1, dd1.T)
      #second half of day, as above, two possible cases
      if (mini[i+1]>=thresh){
        avg2<-(maxi[i]+mini[i+1])/2
        dd2.T<-(avg2-thresh)/2
        dd2<-c(dd2, dd2.T)
      } else{
        #amplitude of temperature difference
        alpha2<-(maxi[i]-mini[i+1])/2
        #average temperature
        avg2<-(maxi[i]+mini[i+1])/2
        #theta is time point when temperatur crosses the threshold
        #assuming temperature is roughly following the sine curve
        theta2<-asin((thresh-avg2)/alpha2)
        #use these to calculate degree day accumulation over first half of day
        dd2.T<-(1/(2*pi))*((avg2-thresh)*(pi/2 - theta2)+alpha2*cos(theta2))
        dd2<-c(dd2, dd2.T)
      }
      
    }
    else  {
      #if temperature doesn't get over threshold, no degree days accumulated
      #first half of day
      dd1<-c(dd1, 0)
      #second half of day
      dd2<-c(dd2, 0)
    }
    #total accumulation over the day is just first half of day plus second
    
  }
  
  return(dd1+dd2)
  
}


#do some checks to make sure the function is working properly

weather$dd<-allen(weather$temp_max_cleaned, weather$temp_min_cleaned, 10)



#plot to make sure nothing weird is happening- look for more degree days midyear,
#and NO negative values. Looks like we're WINNING!
plot(weather$DOY, weather$dd)

#now write a new function to calculate accumulated degree days


accum.allen<-function(maxi, mini, thresh, DOY, startday){
  #if startday is not given, assume it's day 1
  if(missing(startday)) {
    startday<-1
  } else {
    startday<-startday
  }
  dd<-allen(maxi, mini, thresh)
  dd.accum<-c()
  for (i in 1:length(dd)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (DOY[i]==1){
      dd.accum.day=0
    }
    #the accumulation on day i is the degree day accumulation before
    #plus the dd accumulated on that day
    dd.accum.day<-dd.accum.day+dd[i]
    
    #but if the degdays are accumulating before the startday, we want to forget them
    if (DOY[i]<startday){
      dd.accum.day=0
    }
    #add that day's accumulation to the vector
    dd.accum<-c(dd.accum, dd.accum.day)
  }
  return (dd.accum)
}

#same sort of checks. Run the function for our data
start<-1
weather$dd.accum<-accum.allen(weather$temp_max_cleaned, weather$temp_min_cleaned, 10, weather$DOY, start)
#and plot that thing to look for problems:
plot(weather$DOY, weather$dd.accum)
#looks good! victory!!!

#we have good reason to think precipitation may also be important for ladybeetles
#let's use the functions developed for the lampyrid analysis to aggregate some precipitation metrics

accum.precip<-function (precip, week){
  precip.acc<-c()
  counter<-week[1]
  accumulation<-0
  for (i in 1:length(precip)){
    if(week[i]==counter){
      accumulation<-accumulation + precip[i]
    }else{
      counter<-week[i]
      accumulation<-precip[i]
    }
    precip.acc<-c(precip.acc, accumulation)
  }
  return(precip.acc)
}

#run the precipitation accumulation function
weather$prec.accum<-accum.precip(weather$precipitation, weather$week)


#looks good! now let's count rainy days
#this is a simple thing, doesn't really need a function to encode for it, but what the heck
#might as well be consistent with how we've handled processing other weather data
#encoding rain days as 0/1 will allow us to simply sum up the number of rainy days for whatever time 
#period we like

rainy.days<-function (precip, week){
  rainy.days<-c()
  for (i in 1:length(precip)){
    if(precip[i]>0){
      raindays<-1
    }else{
      raindays<-0
    }
    rainy.days<-c(rainy.days, raindays)
  }
  return(rainy.days)
}

#and now the rain day counter
weather$rain.days<-rainy.days(weather$precipitation, weather$week)

#finally, we need to be able to compute the accumulated precipitation over the season from a given timepoint
#another function? I think SO! base this one on the degree day accumulation function 


accum.precip.time<-function(precip, DOY, startday){
  #if startday is not given, assume it's day 1
  if(missing(startday)) {
    startday<-1
  } else {
    startday<-startday
  }
  prec.accum<-c()
  for (i in 1:length(DOY)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (DOY[i]==1){
      prec.accum.day=0
    }
    #the accumulation on day i is the precip accumulation before
    #plus the precip accumulated on that day
    prec.accum.day<-prec.accum.day+precip[i]
    
    #but if the precip is accumulating before the startday, we want to forget them
    if (DOY[i]<startday){
      prec.accum.day=0
    }
    #add that day's accumulation to the vector
    prec.accum<-c(prec.accum, prec.accum.day)
  }
  return (prec.accum)
}

weather$prec.accum.0<-accum.precip.time(weather$precipitation, weather$DOY, start)
#and plot that thing to look for problems:
plot(weather$DOY, weather$prec.accum.0)

#now let's put together a weekly 'weather report'

weather1<-group_by(weather, year, week)

weather_weekly<-dplyr::summarize(weather1,
                          mean.prec=mean(precipitation),
                          rain.days=sum(rain.days),
                          weekly.precip=max(prec.accum),
                          yearly.precip.accum=max(prec.accum.0),
                          max.rainfall=max(precipitation),
                          mean.temp=mean(temp_mean_cleaned),
                          min.temp=min(temp_min_cleaned),
                          max.temp=max(temp_max_cleaned),
                          weekly.dd=max(dd),
                          yearly.dd.accum=max(dd.accum),
                          )

#let's merge in the weather data to the ladybeetle data
#first rename the year column in one of the datasets
lb_weekly_culled<-rename(lb_weekly_culled, year=Year)
lb_all<-merge(lb_weekly_culled, weather_weekly)

#while we're at this, let's make some yearly summary data that will allow us to
#characterize weather by year. Since it looks like seasonality plays a role in within-year 
#partitioning (spoilers!) let's get some accumulations at key points in the season- let's do
#week 25, 30, and 35 and get dd accum, precip accum for each year

keypoints<-c(20, 25, 30, 35)

weather_keypoints<-weather_weekly[which(weather_weekly$week  %in% keypoints),]

#cull out the non-accumulated data

weather_keypoints1<-weather_keypoints[,c(1:2, 6, 12)]

#now we need to recast each of the response columns as their own unique responses by week
#dd accum
library(reshape2)
dd.year<-dcast(weather_keypoints1, year~week,
                      value.var ="yearly.dd.accum",  sum)
colnames(dd.year)<-c("year", "dd20", "dd25", "dd30", "dd35")
#create metrics for DIFFERENCE from last time point too
dd.year$dd25.dif<-dd.year$dd25-dd.year$dd20
dd.year$dd30.dif<-dd.year$dd30-dd.year$dd25
dd.year$dd35.dif<-dd.year$dd35-dd.year$dd30

#precip
precip.year<-dcast(weather_keypoints1, year~week,
               value.var ="yearly.precip.accum",  sum)

colnames(precip.year)<-c("year", "precip20", "precip25", "precip30", "precip35")

#create metrics for DIFFERENCE from last time point too
precip.year$precip25.dif<-precip.year$precip25-precip.year$precip20
precip.year$precip30.dif<-precip.year$precip30-precip.year$precip25
precip.year$precip35.dif<-precip.year$precip35-precip.year$precip30


#ok, now we can merge this into a yearly weather summary matrix

weather_yearly<-merge(dd.year, precip.year)

#let's do some quick plots to look at ladybeetles by various environmental parameters
lb_all$pertrap<-lb_all$SumOfADULTS/lb_all$TRAPS

#let's look at these data by week
lb_summary_week<-ggplot(lb_all, aes(x=week, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_week

#ok, same thing but for degree day accumulation
lb_summary_dd<-ggplot(lb_all, aes(x=yearly.dd.accum, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_dd

#let's look at trapping frequency and DD
lb_summary_traps<-ggplot(lb_all, aes(x=yearly.dd.accum, y=TRAPS, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_traps

#yikes! What are all those 10 trap observations? Christie to investigate!
#looks like there are a few rare occasions that the LB were sampled twice in one week (Monday, then Friday?)
#offset in models should account for the worst of that.

#let's look at rain days
lb_summary_raindays<-ggplot(lb_all, aes(x=rain.days, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_raindays

#let's look at mean temp
lb_summary_meantemp<-ggplot(lb_all, aes(x=mean.temp, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_meantemp

#let's look at min temp
lb_summary_mintemp<-ggplot(lb_all, aes(x=min.temp, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_mintemp

#let's look at max rainfall
lb_summary_maxrainfall<-ggplot(lb_all, aes(x=max.rainfall, y=pertrap, fill=year))+
  #geom_point(pch=21)+
  scale_fill_binned()+
  geom_smooth(aes(color=as.factor(year)))+
  facet_wrap(vars(SPID), nrow=2)

lb_summary_maxrainfall

#let's look at some ordination- we'll visualize and conduct analyses to describe how 
# the two species are using space, over time.
library(reshape2)
library(vegan)

#let's visualize this!
nativepal<-c("orange", "red", "pink", "yellow4","yellow", "blue", "purple", "coral",  "palegreen", "violet")

#create a matrix of observations by community
#create parallel yearly and weekly analyses
landscape.year<-dcast(lb_all, year+REPLICATE+SPID~HABITAT,
                      value.var ="SumOfADULTS",  sum)

landscape.week<-dcast(lb_all, year+week+SPID~HABITAT,
                      value.var ="SumOfADULTS",  sum)
#because we have some rep by week/year combinations with zero observations, we must remove them prior to analysis
landscape.year.1<-landscape.year[rowSums(landscape.year[4:12])>2,]
landscape.week.1<-landscape.week[rowSums(landscape.week[4:12])>2,]

#strip out the context- yes I know! this seems counter-intuitive and awful
#but vegan (and most community analysis packages) want your response variable as its own object

com.matrix.year<-landscape.year.1[,4:12]
com.matrix.week<-landscape.week.1[,4:12]

#set up ordination with year data

ord.year<-metaMDS(com.matrix.year, autotransform=TRUE)
ord.year

#we will only plot the 7 most common species (captures >1000)

plot(ord.year, disp='sites', type='n')
#maybe display the most common as ellipses and less common as points?
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[1], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="ABIPN")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[2], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="BURSI")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[3], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="CMAC")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[4], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="CSTIG")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[5], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="CTRIF")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[6], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="CYCSP")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[7], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="H13")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[8], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="HCONV")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[9], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="HGLAC")
ordiellipse(ord.year, landscape.year.1$SPID, draw="polygon", col=nativepal[10], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="HPARN")

#points(ord.year, display="sites", select=which(landscape.year.1$SPID=="ABIPN"), pch=15, col="orange")
#points(ord.year, display="sites", select=which(landscape.year.1$SPID=="C7"), pch=19, col="red")
#points(ord.year, display="sites", select=which(landscape.year.1$SPID=="CMAC"), pch=18, col="pink")
#points(ord.year, display="sites", select=which(landscape.year.1$SPID=="PQUA"), pch=16, col="yellow4")
ordilabel(ord.year, display="species", cex=0.75, col="black")

#bring the relevant environmental data back into our enviromental frame
yearly.context<-merge(landscape.year.1, weather_yearly, all.x = T)


#is the spatiotemporal distribution of ladybeetle species differnet over years?
#we will do a permanova to check
specmod.y<-adonis2(com.matrix.year~SPID, data=landscape.year.1, method="bray")
summary(specmod.y)
specmod.y

fit.year<-envfit(ord.year~year+dd35.dif+
                   precip35.dif, data=yearly.context, perm=999)
summary(fit.year)
fit.year


plot(fit.year)

# #save to pdf
# pdf("plots/NMDS_yearly.pdf", height=6, width=6)
# plot(ord.year, disp='sites', type='n')
# points(ord.year, display="sites", select=which(landscape.year$SPID=="HAXY"), pch=19, cex=0.5,col="orange")
# points(ord.year, display="sites", select=which(landscape.year$SPID=="C7"), pch=15, cex=0.5, col="red")
# plot(fit.year)
# ordilabel(ord.year, display="species", cex=0.75, col="black")
# dev.off()



#and now for week

ord.week<-metaMDS(com.matrix.week, autotransform=TRUE)
ord.week

plot(ord.week, disp='sites', type='n')
#match up formatting to above
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[1], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="ABIPN")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[2], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="BURSI")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[3], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="CMAC")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[4], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="CSTIG")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[5], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="CTRIF")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[6], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="CYCSP")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[7], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="H13")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[8], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="HCONV")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[9], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="HGLAC")
ordiellipse(ord.week, landscape.week.1$SPID, draw="polygon", col=nativepal[10], kind="ehull", conf=0.95, label=FALSE, cex=0.75, show.groups="HPARN")

ordilabel(ord.week, display="species", cex=0.75, col="black")

#bring the relevant environmental data back into our environmental frame
weekly.context<-merge(landscape.week.1, weather_weekly, all.x = T)

#is the spatiotemporal distribution of harmonia different from that of C7?
#we will do a permanova to check
specmod<-adonis2(com.matrix.week~SPID, data=landscape.week.1, method="bray")
specmod

#we're performing a model selection, using backwards selection from all environmental variables
#we're using the P value and R square, and paying attention to which variables seem too colinear to include

fit.week<-envfit(ord.week~year+
                   weekly.precip+max.temp+min.temp+yearly.dd.accum,
                 data=weekly.context, perm=999)
summary(fit.week)
fit.week

plot(fit.week)

# #save to pdf
# pdf("plots/NMDS_weekly.pdf", height=6, width=6)
# plot(ord.week, disp='sites', type='n')
# points(ord.week, display="sites", select=which(landscape.week.1$SPID=="HAXY"), pch=19, cex=0.5,col="orange")
# points(ord.week, display="sites", select=which(landscape.week.1$SPID=="C7"), pch=15, cex=0.5, col="red")
# ordilabel(ord.week, display="species", cex=0.75, col="black")
# plot(fit.week)
# dev.off()


##### not run- section where we're trying to bring these plots into ggplot- it's just too messy to do this with 14 
# species and base r is easier to customize for this

# #built a two-panel PDF
# #guh, looks like with the base vegan plots it's still easiest to do the base R
# #can we turn these plots into grobs? extract the data, remember we've transposed it so plant 
# #community was across the top of the matrix
# 
# #yearly
# year.scores.species<-as.data.frame(scores(ord.year, "site"))
# year.scores.plant<-as.data.frame(scores(ord.year, "species"))
# year.scores.plant$Community <- rownames(year.scores.plant)
# 
# 
# arrow_factor<-ordiArrowMul(fit.year)
# year.data.fit<-as.data.frame(scores(fit.year, display="vectors"))*arrow_factor
# year.data.fit$vari<-rownames(year.data.fit)
# 
# #make the names on the vectors nicer
# year.data.fit$vari<-gsub("precip35.dif","precip35", year.data.fit$vari)
# year.data.fit$vari<-gsub("dd35.dif","dd35", year.data.fit$vari)
# arrow_factor<-ordiArrowMul(fit.year)
# fudgexy<-c(0.1, 0.14, -0.2)#jitter the vector labels a bit
# fudgeyy<-c(-0.08, -0.06, 0.12)
# 
# 
# library(dplyr)
# year.ellipse.labels <- year.scores.species %>%
#   group_by(landscape.year.1$SPID) %>%
#   summarise(NMDS1 = mean(NMDS1),
#             NMDS2 = mean(NMDS2))
# 
# names(year.ellipse.labels)[1] <- 'SPID'
# 
# year.hulls <- year.scores.species %>%
#   group_by(landscape.year.1$SPID) %>%
#   slice(chull(NMDS1, NMDS2))
# names(year.hulls)[3] <- 'SPID'
# 
# 
# yearnmds<-ggplot()+
#   geom_point(data=year.scores.species,
#              aes(x=NMDS1,y=NMDS2,shape=landscape.year.1$SPID,colour=landscape.year.1$SPID), size=1)+# add the point markers
#   #scale_colour_manual(values=c("C7" = "darkred", "HAXY" = "darkorange"), labels=c("C7", "HA")) +
#   #scale_shape_manual(values=c("C7" = 4, "HAXY" = 1), labels=c("C7", "HA"))+
#   geom_polygon(data = year.hulls,
#                aes(x = NMDS1, y = NMDS2,  group = SPID, fill=SPID),
#                alpha = 0.3, color = "black", linewidth = 0.3)+
#   geom_text(data = year.ellipse.labels,
#             aes(x = NMDS1, y = NMDS2, label = SPID),
#              size = 5, fontface = "bold", color = "black")+
#   geom_segment(data=year.data.fit, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
#                arrow=arrow(length = unit(0.03, "npc")), size=0.8, color="blue")+
#   geom_label(data=year.data.fit, aes(x=NMDS1+fudgexy, y=NMDS2+fudgeyy, label=vari),size= 5, color="blue", fill="white", alpha=0.7, label.size=NA)+
#   geom_label(data=year.scores.plant,aes(x=NMDS1,y=NMDS2,label=Community),size=4,vjust=0, fill="white", alpha=0.9) +  # add the site labels
#   coord_fixed()+
#   theme_classic()+
#   theme(legend.position = "none")
# 
# yearnmds
# 
# #weekly
# 
# 
# week.scores.species<-as.data.frame(scores(ord.week, "site"))
# week.scores.plant<-as.data.frame(scores(ord.week, "species"))
# week.scores.plant$Community <- rownames(week.scores.plant)
# 
# 
# arrow_factorw<-ordiArrowMul(fit.week)
# week.data.fit<-as.data.frame(scores(fit.week, display="vectors"))*arrow_factorw
# week.data.fit$vari<-rownames(week.data.fit)
# 
# #make the names on the vectors nicer
# week.data.fit$vari<-gsub("yearly.precip.accum","precip", week.data.fit$vari)
# week.data.fit$vari<-gsub("yearly.dd.accum","dd", week.data.fit$vari)
# fudgex<-c(0.15, -0.285, 0)#jitter the vector labels a bit
# fudgey<-c(0.15, -0.08, 0.17)#jitter the vector labels a bit
# 
# weeknmds<-ggplot()+
#   geom_point(data=week.scores.species,
#              aes(x=NMDS1,y=NMDS2,shape=landscape.week.1$SPID,colour=landscape.week.1$SPID), size=1)+# add the point markers
#   scale_colour_manual(values=c("C7" = "darkred", "HAXY" = "darkorange"), labels=c("C7", "HA")) +
#   scale_shape_manual(values=c("C7" = 4, "HAXY" = 1), labels=c("C7", "HA"))+
#   geom_segment(data=week.data.fit, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
#                arrow=arrow(length = unit(0.03, "npc")), size=0.8, color="blue")+
#   geom_label(data=week.data.fit, aes(x=NMDS1+fudgex, y=NMDS2+fudgey, label=vari),size= 5, color="blue", fill="white", alpha=0.7, label.size=NA)+
#   geom_label(data=week.scores.plant,aes(x=NMDS1,y=NMDS2,label=Community),size=4,vjust=0, fill="white", alpha=0.9) +  # add the site labels
#   coord_fixed()+
#   theme_classic()+
#   theme(legend.position = "none")
# 
# weeknmds
# 
# #ok, finally. Put it together
# ggnmds<-plot_grid(yearnmds, weeknmds,  ncol=1, rel_widths=c(1), labels=c('A', 'B'), 
#                      align="h", axis="l")
# 
# ggnmds
# 
# 
# 
# pdf("plots/figureNMSDs1.pdf", height=10, width=6)
# ggnmds
# dev.off()
# 
# 
# 


# let's rough in our gam models. Just like with the multivariate analysis, we'll look at
#two different scales- within year dynamics and between year dynamics
library(mgcv)
library(visreg)
library(ggpubr)
library(Hmisc)
library(cowplot)

#pearson correlation of environmental parameters

round(cor(lb_all[10:19], method="pearson"), digits=2)
#start withe the drivers of within-year variation




#warning- this model takes  while to run but I *want* to do it with the interactions dangit
# I have been broken. This cannot run as an all-species model so we'll do it one species at a time


##################### abipn 

lb_all.abipn<-lb_all[which(lb_all$SPID=="ABIPN"),]

gam_lb.abipn<-gam(SumOfADULTS~s(yearly.dd.accum, sp=1)+
              s(weekly.precip, sp=1)+
              s(max.temp, sp=1)+
              s(min.temp, sp=1)+ 
              HABITAT+
              s(year, sp=1)+
              offset(log(TRAPS)), method="REML", data=lb_all.abipn, family="quasipoisson")
summary(gam_lb.abipn)
anova(gam_lb.abipn) #significance of parametric terms

#not run- causes hangups in casual runs!
# #check concurvity
# concurvity(gam_lb)
# #looks fine, sweet!
# gam.check(gam_lb)


withinyear.dd.abipn<-visreg(gam_lb.abipn, "yearly.dd.accum", partial=F, rug=FALSE, 
                      overlay=T, scale="response", gg=TRUE,
                      line=list(lty=1, col=nativepal[1]), fill=list(fill=nativepal[1], alpha=0.4))+
  labs(x="Degree day accumulation", y="")+
  theme_classic()+ theme(legend.position = "none")

withinyear.dd.abipn

withinyear.rain.abipn<-visreg(gam_lb.abipn, "weekly.precip",  partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE,
                              line=list(lty=1, col=nativepal[1]), fill=list(fill=nativepal[1], alpha=0.4))+
  labs(x="Total precip within week (mm)", y="")+
  theme_classic()+ theme(legend.position = "none")

withinyear.rain.abipn

withinyear.temp.abipn<-visreg(gam_lb.abipn, "max.temp",  partial=F, rug=FALSE, 
                        overlay=T, scale="response", gg=TRUE,
                        line=list(lty=1, col=nativepal[1]), fill=list(fill=nativepal[1], alpha=0.4))+
  labs(x="Maximum temperature within week (C)", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim=c(0, 40))

withinyear.temp.abipn

withinyear.mintemp.abipn<-visreg(gam_lb.abipn, "min.temp",  partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE,
                              line=list(lty=1, col=nativepal[1]), fill=list(fill=nativepal[1], alpha=0.4))+
  labs(x="Minimum temperature within week (C)", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim=c(0, 40))

withinyear.mintemp.abipn

withinyear.habitat.abipn<-visreg(gam_lb.abipn, "HABITAT",  partial=F, rug=FALSE, 
                           overlay=T, scale="response", gg=TRUE,
                           line=list(lty=1, col=nativepal[1]), fill=list(fill=nativepal[1], alpha=0.4))+
  labs(x="Habitat", y="")+
  theme_classic()+ theme(legend.position = "none", axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

withinyear.habitat.abipn

withinyear.yearly.abipn<-visreg(gam_lb.abipn, "year",   partial=F, rug=FALSE, 
                          overlay=T, scale="response", gg=TRUE,
                          line=list(lty=1, col=nativepal[1]), fill=list(fill=nativepal[1], alpha=0.4))+
  labs(x="Year", y="")+
  theme_classic()+ theme(legend.position = c(0.92, 0.85),legend.background = element_rect(fill='transparent'))+
  coord_cartesian(xlim=c(1989, 2024))


withinyear.yearly.abipn

#plot the withinyear model all together:

withinyear.modelplot.abipn<-plot_grid(withinyear.yearly.abipn,withinyear.dd.abipn,  withinyear.mintemp.abipn, withinyear.temp.abipn, withinyear.rain.abipn, withinyear.habitat.abipn,  
                              ncol=1, rel_heights = c(1, 1, 1, 1, 1,  2), labels=c('A', 'B', 'C', 'D', 'E', 'F'), align="v")
withinyear.modelplot.abipn

#create overall y axis label
partresid<-text_grob(paste("        Partial residual captures"), color="black", size=12, rot=90)


#now replot with grob label
withinyear.plot.abipn<-plot_grid(partresid, withinyear.modelplot.abipn, ncol=2, rel_widths = c(1,11))

withinyear.plot.abipn

pdf("plots/figurewithinyeargamabipn.pdf", height=10, width=5)
withinyear.plot.abipn
dev.off()


#we'll want to extract the data associated with activity peaks

#ok, I think we found the method we should use! here's the tutorial:
# https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/

#first we create a new dataframe that we can use our model to predict the values for optima
#we use good guesses at values for other optima to create conditions where species is reasonably abundant for modelled parameter 

#create data for abipn, holding everything constant but degree days
newData.abipn.dd <- with(lb_all.abipn,
                  data.frame(yearly.dd.accum = seq(0, 1500, length = 300),#use natural range of data
                             TRAPS=5, 
                             year=1990, #select year when this species is most abundant- 1990
                             weekly.precip=0, # species likes it dry
                             max.temp=26, #species maxes near 26
                             min.temp=12, #species maxes near 12
                             SPID="ABIPN", 
                             HABITAT="coniferous")) #species likes conifers best

#make the same frame but for 1 more degday
newData.abipin.1.dd<- with(lb_all.abipn,
                     data.frame(yearly.dd.accum = seq(1, 1501, length = 300), #use natural range of data
                                TRAPS=5, 
                                year=1990, #select year when this species is most abundant- 1990
                                weekly.precip=0, # species likes it dry
                                max.temp=26, #species maxes near 26
                                min.temp=12, #species maxes near 12
                                SPID="ABIPN", 
                                HABITAT="coniferous")) #species likes conifers best

#make predictions
predict.dd.abipn<-predict(gam_lb.abipn, newData.abipn.dd, type="link")
predict.dd.abipn.1<-predict(gam_lb.abipn, newData.abipin.1.dd, type="link")

dd.abipn.der<-as.data.frame(cbind(newData.abipn.dd$yearly.dd.accum, predict.dd.abipn, predict.dd.abipn.1))
dd.abipn.der$slope<-(dd.abipn.der$predict.dd.abipn.1-dd.abipn.der$predict.dd.abipn)/1

#slope approaches zero at 356 and 1118 degree days (we look for places where the slope changes from negative to positive or vice versa)- 
#note dd is not significant in the model but data suggests two adult activity peaks- 2 generations per year

#create data for abipn, holding everything constant but minimum temperature
newData.abipn.mint <- with(lb_all.abipn,
                         data.frame(yearly.dd.accum = 1118,
                                    TRAPS=5, 
                                    year=1990, #select year when this species is most abundant- 1990
                                    weekly.precip=0, # species likes it dry
                                    max.temp=26, #species maxes near 26
                                    min.temp= seq(-5, 18, length = 300), #use natural range of data
                                    SPID="ABIPN", 
                                    HABITAT="coniferous")) #species likes conifers best

#make the same frame but for 0.2 more degrees celcius
newData.abipin.1.mint<- with(lb_all.abipn,
                           data.frame(yearly.dd.accum = 1118,
                                      TRAPS=5, 
                                      year=1990, #select year when this species is most abundant- 1990
                                      weekly.precip=0, # species likes it dry
                                      max.temp=26, #species maxes near 26
                                      min.temp=seq(-4.8, 18.2, length = 300), #use natural range of data
                                      SPID="ABIPN", 
                                      HABITAT="coniferous")) #species likes conifers best

#make predictions
predict.mint.abipn<-predict(gam_lb.abipn, newData.abipn.mint, type="link")
predict.mint.abipn.1<-predict(gam_lb.abipn, newData.abipin.1.mint, type="link")

mint.abipn.der<-as.data.frame(cbind(newData.abipn.mint$min.temp, predict.mint.abipn, predict.mint.abipn.1))
mint.abipn.der$slope<-(mint.abipn.der$predict.mint.abipn.1-mint.abipn.der$predict.mint.abipn)/1


#slope approaches zero at minimum temperature of 10.5 C
#significant factor in the model


#create data for abipn, holding everything constant but maximum temperature
newData.abipn.maxt <- with(lb_all.abipn,
                           data.frame(yearly.dd.accum = 1118,
                                      TRAPS=5, 
                                      year=1990, #select year when this species is most abundant- 1990
                                      weekly.precip=0, # species likes it dry
                                      max.temp=seq(18, 40, length = 300), #use natural range of data
                                      min.temp= 12, #species maxes near 12
                                      SPID="ABIPN", 
                                      HABITAT="coniferous")) #species likes conifers best

#make the same frame but for 0.2 more degrees celcius
newData.abipin.1.maxt<- with(lb_all.abipn,
                             data.frame(yearly.dd.accum = 1118,
                                        TRAPS=5, 
                                        year=1990, #select year when this species is most abundant- 1990
                                        weekly.precip=0, # species likes it dry
                                        max.temp=seq(18.2, 40.2, length = 300), #use natural range of data
                                        min.temp= 12, #species maxes near 12
                                        SPID="ABIPN", 
                                        HABITAT="coniferous")) #species likes conifers best

#make predictions
predict.maxt.abipn<-predict(gam_lb.abipn, newData.abipn.maxt, type="link")
predict.maxt.abipn.1<-predict(gam_lb.abipn, newData.abipin.1.maxt, type="link")

maxt.abipn.der<-as.data.frame(cbind(newData.abipn.maxt$max.temp, predict.maxt.abipn, predict.maxt.abipn.1))
maxt.abipn.der$slope<-(maxt.abipn.der$predict.maxt.abipn.1-maxt.abipn.der$predict.maxt.abipn)/1


#slope approaches zero at minimum temperature of 26.2 C
#significant factor in the model


#create data for abipn, holding everything constant but precipitation
newData.abipn.precip <- with(lb_all.abipn,
                           data.frame(yearly.dd.accum = 1118,
                                      TRAPS=5, 
                                      year=1990, #select year when this species is most abundant- 1990
                                      weekly.precip=seq(0, 150, length = 300), #use natural range of data
                                      max.temp=26, #species maxes near 26
                                      min.temp= 12, #species maxes near 12
                                      SPID="ABIPN", 
                                      HABITAT="coniferous")) #species likes conifers best

#make the same frame but for 0.2 more degrees celcius
newData.abipin.1.precip<- with(lb_all.abipn,
                             data.frame(yearly.dd.accum = 1118,
                                        TRAPS=5, 
                                        year=1990, #select year when this species is most abundant- 1990
                                        weekly.precip=seq(1, 151, length = 300), #use natural range of data
                                        max.temp=26, #species maxes near 26
                                        min.temp= 12, #species maxes near 12
                                        SPID="ABIPN", 
                                        HABITAT="coniferous")) #species likes conifers best

#make predictions
predict.precip.abipn<-predict(gam_lb.abipn, newData.abipn.precip, type="link")
predict.precip.abipn.1<-predict(gam_lb.abipn, newData.abipin.1.precip, type="link")

precip.abipn.der<-as.data.frame(cbind(newData.abipn.precip$max.temp, predict.precip.abipn, predict.precip.abipn.1))
precip.abipn.der$slope<-(precip.abipn.der$predict.precip.abipn.1-precip.abipn.der$predict.precip.abipn)/1

#this species peaks at zero- no rain
#significant factor in model


#ok, now let's predict the mean captures for each habitat, given peak abundance in other parameters 

newData.abipn.habitat<- with(lb_all.abipn,
                               data.frame(yearly.dd.accum = 1118,
                                          TRAPS=5, 
                                          year=1990, #select year when this species is most abundant- 1990
                                          weekly.precip=0, # species likes it dry
                                          max.temp=26, #species maxes near 26
                                          min.temp= 12, #species maxes near 12
                                          SPID="ABIPN", 
                                          HABITAT=c("poplar", "coniferous")))#just literally list each habitat of interest, probably the peak ones
predict(gam_lb.abipn, newData.abipn.habitat, type="link")


#poplar 0.57, coniferous 0.67

##################### bursi 

lb_all.bursi<-lb_all[which(lb_all$SPID=="BURSI"),]

gam_lb.bursi<-gam(SumOfADULTS~s(yearly.dd.accum, sp=1)+
                    s(weekly.precip, sp=1)+
                    s(max.temp, sp=1)+
                    s(min.temp, sp=1)+ 
                    HABITAT+
                    s(year, sp=1)+
                    offset(log(TRAPS)), method="REML", data=lb_all.bursi, family="quasipoisson")
summary(gam_lb.bursi)
anova(gam_lb.bursi) #significance of parametric terms

#everything but min temp significant here

#not run- causes hangups in casual runs!
# #check concurvity
# concurvity(gam_lb)
# #looks fine, sweet!
# gam.check(gam_lb)



withinyear.dd.bursi<-visreg(gam_lb.bursi, "yearly.dd.accum", partial=F, rug=FALSE, 
                            overlay=T, scale="response", gg=TRUE,
                            line=list(lty=1, col=nativepal[2]), fill=list(fill=nativepal[2], alpha=0.4))+
  labs(x="Degree day accumulation", y="")+
  theme_classic()+ theme(legend.position = "none")

withinyear.dd.bursi

withinyear.rain.bursi<-visreg(gam_lb.bursi, "weekly.precip",  partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE,
                              line=list(lty=1, col=nativepal[2]), fill=list(fill=nativepal[2], alpha=0.4))+
  labs(x="Total precip within week (mm)", y="")+
  theme_classic()+ theme(legend.position = "none")

withinyear.rain.bursi

withinyear.temp.bursi<-visreg(gam_lb.bursi, "max.temp",  partial=F, rug=FALSE, 
                              overlay=T, scale="response", gg=TRUE,
                              line=list(lty=1, col=nativepal[2]), fill=list(fill=nativepal[2], alpha=0.4))+
  labs(x="Maximum temperature within week (C)", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim=c(0, 40))

withinyear.temp.bursi

withinyear.mintemp.bursi<-visreg(gam_lb.bursi, "min.temp",  partial=F, rug=FALSE, 
                                 overlay=T, scale="response", gg=TRUE,
                                 line=list(lty=1, col=nativepal[2]), fill=list(fill=nativepal[2], alpha=0.4))+
  labs(x="Minimum temperature within week (C)", y="")+
  theme_classic()+ theme(legend.position = "none")+
  coord_cartesian(xlim=c(0, 40))

withinyear.mintemp.bursi

withinyear.habitat.bursi<-visreg(gam_lb.bursi, "HABITAT",  partial=F, rug=FALSE, 
                                 overlay=T, scale="response", gg=TRUE,
                                 line=list(lty=1, col=nativepal[2]), fill=list(fill=nativepal[2], alpha=0.4))+
  labs(x="Habitat", y="")+
  theme_classic()+ theme(legend.position = "none", axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

withinyear.habitat.bursi

withinyear.yearly.bursi<-visreg(gam_lb.bursi, "year",   partial=F, rug=FALSE, 
                                overlay=T, scale="response", gg=TRUE,
                                line=list(lty=1, col=nativepal[2]), fill=list(fill=nativepal[2], alpha=0.4))+
  labs(x="Year", y="")+
  theme_classic()+ theme(legend.position = c(0.92, 0.85),legend.background = element_rect(fill='transparent'))+
  coord_cartesian(xlim=c(1989, 2024))


withinyear.yearly.bursi

#plot the withinyear model all together:

withinyear.modelplot.bursi<-plot_grid(withinyear.yearly.bursi,withinyear.dd.bursi,  withinyear.mintemp.bursi, withinyear.temp.bursi, withinyear.rain.bursi, withinyear.habitat.bursi,  
                                      ncol=1, rel_heights = c(1, 1, 1, 1, 1,  2), labels=c('A', 'B', 'C', 'D', 'E', 'F'), align="v")
withinyear.modelplot.bursi

#create overall y axis label
partresid<-text_grob(paste("        Partial residual captures"), color="black", size=12, rot=90)


#now replot with grob label
withinyear.plot.bursi<-plot_grid(partresid, withinyear.modelplot.bursi, ncol=2, rel_widths = c(1,11))

withinyear.plot.bursi

pdf("plots/figurewithinyeargambursi.pdf", height=10, width=5)
withinyear.plot.bursi
dev.off()


#we'll want to extract the data associated with activity peaks

#ok, I think we found the method we should use! here's the tutorial:
# https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/

#first we create a new dataframe that we can use our model to predict the values for optima
#we use good guesses at values for other optima to create conditions where species is reasonably abundant for modelled parameter 

#create data for bursi, holding everything constant but degree days
newData.bursi.dd <- with(lb_all.bursi,
                         data.frame(yearly.dd.accum = seq(0, 1500, length = 300),#use natural range of data
                                    TRAPS=5, 
                                    year=2006, #select year when this species is most abundant- 2006
                                    weekly.precip=0, # species likes it dry (or very wet!)
                                    max.temp=32, #species maxes near 32
                                    min.temp=18, #species maxes near 18
                                    SPID="BURSI", 
                                    HABITAT="ES")) #species likes ES best

#make the same frame but for 1 more degday
newData.abipin.1.dd<- with(lb_all.bursi,
                           data.frame(yearly.dd.accum = seq(1, 1501, length = 300), #use natural range of data
                                      TRAPS=5, 
                                      year=2006, #select year when this species is most abundant- 2006
                                      weekly.precip=0, # species likes it dry (or very wet!)
                                      max.temp=32, #species maxes near 32
                                      min.temp=18, #species maxes near 18
                                      SPID="BURSI", 
                                      HABITAT="ES")) #species likes ES best

#make predictions
predict.dd.bursi<-predict(gam_lb.bursi, newData.bursi.dd, type="link")
predict.dd.bursi.1<-predict(gam_lb.bursi, newData.abipin.1.dd, type="link")

dd.bursi.der<-as.data.frame(cbind(newData.bursi.dd$yearly.dd.accum, predict.dd.bursi, predict.dd.bursi.1))
dd.bursi.der$slope<-(dd.bursi.der$predict.dd.bursi.1-dd.bursi.der$predict.dd.bursi)/1

#slope approaches zero at 642 degree days 
#dd is significant in the model but data suggests one adult activity peak- one generation per year

#create data for bursi, holding everything constant but minimum temperature
newData.bursi.mint <- with(lb_all.bursi,
                           data.frame(yearly.dd.accum = 700,
                                      TRAPS=5, 
                                      year=2006, #select year when this species is most abundant- 2006
                                      weekly.precip=0, # species likes it dry (or very wet!)
                                      max.temp=32, #species maxes near 32
                                      min.temp= seq(-5, 18, length = 300), #use natural range of data
                                      SPID="BURSI", 
                                      HABITAT="ES")) #species likes ES best

#make the same frame but for 0.2 more degrees celcius
newData.abipin.1.mint<- with(lb_all.bursi,
                             data.frame(yearly.dd.accum = 700,
                                        TRAPS=5, 
                                        year=2006, #select year when this species is most abundant- 2006
                                        weekly.precip=0, # species likes it dry (or very wet!)
                                        max.temp=32, #species maxes near 32
                                        min.temp=seq(-4.8, 18.2, length = 300), #use natural range of data
                                        SPID="BURSI", 
                                        HABITAT="ES")) #species likes ES best

#make predictions
predict.mint.bursi<-predict(gam_lb.bursi, newData.bursi.mint, type="link")
predict.mint.bursi.1<-predict(gam_lb.bursi, newData.abipin.1.mint, type="link")

mint.bursi.der<-as.data.frame(cbind(newData.bursi.mint$min.temp, predict.mint.bursi, predict.mint.bursi.1))
mint.bursi.der$slope<-(mint.bursi.der$predict.mint.bursi.1-mint.bursi.der$predict.mint.bursi)/1


#not a significant factor in the model, slope changes very little in scale of data


#create data for bursi, holding everything constant but maximum temperature
newData.bursi.maxt <- with(lb_all.bursi,
                           data.frame(yearly.dd.accum = 700,
                                      TRAPS=5, 
                                      year=2006, #select year when this species is most abundant- 2006
                                      weekly.precip=0, # species likes it dry (or very wet!)
                                      max.temp=seq(18, 40, length = 300), #use natural range of data
                                      min.temp=18, #species maxes near 18
                                      SPID="BURSI", 
                                      HABITAT="ES")) #species likes ES best

#make the same frame but for 0.2 more degrees celcius
newData.abipin.1.maxt<- with(lb_all.bursi,
                             data.frame(yearly.dd.accum = 700,
                                        TRAPS=5, 
                                        year=2006, #select year when this species is most abundant- 2006
                                        weekly.precip=0, # species likes it dry (or very wet!)
                                        max.temp=seq(18.2, 40.2, length = 300), #use natural range of data
                                        min.temp=18, #species maxes near 18
                                        SPID="BURSI", 
                                        HABITAT="ES")) #species likes ES best

#make predictions
predict.maxt.bursi<-predict(gam_lb.bursi, newData.bursi.maxt, type="link")
predict.maxt.bursi.1<-predict(gam_lb.bursi, newData.abipin.1.maxt, type="link")

maxt.bursi.der<-as.data.frame(cbind(newData.bursi.maxt$max.temp, predict.maxt.bursi, predict.maxt.bursi.1))
maxt.bursi.der$slope<-(maxt.bursi.der$predict.maxt.bursi.1-maxt.bursi.der$predict.maxt.bursi)/1


#slope approaches zero at minimum temperature of 31.8C
#significant factor in the model


#create data for bursi, holding everything constant but precipitation
newData.bursi.precip <- with(lb_all.bursi,
                             data.frame(yearly.dd.accum = 700,
                                        TRAPS=5, 
                                        year=2006, #select year when this species is most abundant- 2006
                                        weekly.precip=seq(0, 150, length = 300), #use natural range of data
                                        max.temp=32, #species maxes near 32
                                        min.temp=18, #species maxes near 18
                                        SPID="BURSI", 
                                        HABITAT="ES")) #species likes ES best

#make the same frame but for 0.2 more degrees celcius
newData.abipin.1.precip<- with(lb_all.bursi,
                               data.frame(yearly.dd.accum = 700,
                                          TRAPS=5, 
                                          year=2006, #select year when this species is most abundant- 2006
                                          weekly.precip=seq(1, 151, length = 300), #use natural range of data
                                          max.temp=32, #species maxes near 32
                                          min.temp=18, #species maxes near 18
                                          SPID="BURSI", 
                                          HABITAT="ES")) #species likes ES best

#make predictions
predict.precip.bursi<-predict(gam_lb.bursi, newData.bursi.precip, type="link")
predict.precip.bursi.1<-predict(gam_lb.bursi, newData.abipin.1.precip, type="link")

precip.bursi.der<-as.data.frame(cbind(newData.bursi.precip$max.temp, predict.precip.bursi, predict.precip.bursi.1))
precip.bursi.der$slope<-(precip.bursi.der$predict.precip.bursi.1-precip.bursi.der$predict.precip.bursi)/1

#significant factor but no peak in range of data- slight increase at low and high values
#significant factor in model


#ok, now let's predict the mean captures for each habitat, given peak abundance in other parameters 

newData.bursi.habitat<- with(lb_all.bursi,
                             data.frame(yearly.dd.accum = 700,
                                        TRAPS=5, 
                                        year=2006, #select year when this species is most abundant- 2006
                                        weekly.precip=0, # species likes it dry
                                        max.temp=32, #species maxes near 32
                                        min.temp=18, #species maxes near 18
                                        SPID="BURSI", 
                                        HABITAT="ES")) #species likes ES best #just literally list each habitat of interest, probably the peak ones
predict(gam_lb.bursi, newData.bursi.habitat, type="link")


#ES 0.84


