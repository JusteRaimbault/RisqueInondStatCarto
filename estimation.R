
library(dplyr)
library(rgdal)
library(cartography)
library(rgeos)

setwd(paste0(Sys.getenv('MONITORAT'),'/L1StatCarto/DST/'))


# load data
source('excel.R')



#
communes <- readOGR('data','communes')

# correspondance cp <-> code com
corcpinsee <- as.tbl(read.csv('data/correspondance-code-insee-code-postal.csv',sep=';',stringsAsFactors = FALSE))

cpjoin = left_join(communes@data,corcpinsee[,1:2],by=c('INSEE_COMM' = 'Code.INSEE'))
communes@data$cp= sapply(cpjoin$Code.Postal, function(s){strsplit(s,'/')[[1]][1]})
communes@data$dep = sapply(communes$cp, function(s){substr(s,1,2)})
communes = communes[communes$dep%in%c("94","93","95","92","91","77","78","75"),]

#as.character(communes$INSEE_COMM)
#sapply(d$VoletA__A4_2)
cp<-sapply(d$VoletA__A4_2,function(s){substr(s,1,5)});names(cp)<-rownames(d)
d$cp <- cp
#length(which(sapply(cp,function(s){s%in%communes@data$cp})))/nrow(d)

sdf = d%>% group_by(cp)%>%summarise(count=n(),risk=mean(as.numeric(VoletC_C1),na.rm=T))

inondation <- readOGR('data','inondation')

cols <- carto.pal(pal1 = "green.pal",n1 = 3, pal2 = "red.pal",n2 = 3)
choroLayer(spdf = communes,spdfid = "cp",
           df = data.frame(sdf),dfid = 'cp',#"zip",
           var="risk",
           col=cols,
           nclass=6#,
           #breaks = quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           #add=FALSE,lwd = 0.01,
           #legend.pos = "topleft",
           #legend.title.txt = "mean price",
           #legend.values.rnd = 0.5
)

plot(inondation,add=T,col='blue')
#plot(communes,add=T,col='grey')
plot(communes,border = "grey20",add=TRUE)#, lwd=0.1, )



###

nivconn = as.numeric(unlist(d[,12]))
risqinond = as.numeric(d$VoletC_C1)

inds = d$cp%in%communes$cp
distances = sapply(d$cp[inds],function(s){gDistance(gCentroid(communes[communes$cp==s,]),inondation)})

plot(distances,nivconn[inds])
plot(distances,risqinond[inds])

# area of inondable zone within a buffer
areas = gArea(inondation,byid = TRUE)
wareas = sapply(d$cp[inds],function(s){ sum(areas*exp(-gDistance(gCentroid(communes[communes$cp==s,]),inondation,byid = TRUE)/5000))})

plot(wareas,nivconn[inds])
plot(wareas,risqinond[inds])

