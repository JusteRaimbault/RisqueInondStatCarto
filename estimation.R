
library(dplyr)
library(rgdal)
library(cartography)
library(rgeos)
library(ggplot2)

setwd(paste0(Sys.getenv('MONITORAT'),'/L1StatCarto/DST/DST1TD'))


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

sdf = d%>% group_by(cp)%>%
  summarise(count=n(),
            risk=mean(as.numeric(VoletC_C1),na.rm=T),
            conn = mean(as.numeric(VoletB_B1),na.rm=T),
            goodconn = mean(as.numeric(VoletB_B1)[as.numeric(VoletB_B1)>1],na.rm=T)
            )

inondation <- readOGR('data','inondation_idf')

cols <- carto.pal(pal1 = "green.pal",n1 = 3, pal2 = "red.pal",n2 = 3)
choroLayer(spdf = communes,spdfid = "cp",
           df = data.frame(sdf),dfid = 'cp',#"zip",
           var="count",
           col=cols,
           nclass=6,
           breaks = c(0,2,4,6,8,12,27),#quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           add=FALSE,lwd = 0.01,
           legend.pos = "topleft",
           legend.title.txt = "nombre"
           #legend.values.rnd = 0.5
)
#plot(inondation,add=T,col='blue')
plot(communes,border = "grey20",add=TRUE, lwd=0.2)
layoutLayer(title = "Localisation des sondés",
            sources = "Source : Questionnaires Etudiants",
            author = "",
            scale = 0,
            frame = TRUE,
            col = "black",
            coltitle = "white")


###

nivconn = as.numeric(unlist(d[,12]))
risqinond = as.numeric(d$VoletC_C1)

inds = d$cp%in%communes$cp
nivconn=nivconn[inds]
indssdf = sdf$cp%in%communes$cp
distances = sapply(d$cp[inds],function(s){gDistance(gCentroid(communes[communes$cp==s,]),inondation)})
distancessdf = sapply(sdf$cp[indssdf],function(s){gDistance(gCentroid(communes[communes$cp==s,]),inondation)})


plot(distances,nivconn)
plot(distances,risqinond)

plot(distancessdf,sdf$goodconn[indssdf])


##
# test : difference in mean/sd with distance break ?
means=c();sds=c();types=c();dbreaks=c();alldbreaks=c();allconns=c();alltypes=c()
for(dbreak in seq(from=700,to =6000,by=100)){
  means=append(means,mean(nivconn[distances<dbreak]));sds=append(sds,sd(nivconn[distances<dbreak]));types=append(types,"prox");dbreaks=append(dbreaks,dbreak)
  means=append(means,mean(nivconn[distances>dbreak]));sds=append(sds,sd(nivconn[distances>dbreak]));types=append(types,"far");dbreaks=append(dbreaks,dbreak)
  allconns=append(allconns,nivconn[distances<dbreak]);alltypes=append(alltypes,rep("prox",length(nivconn[distances<dbreak])));alldbreaks=append(alldbreaks,rep(dbreak,length(nivconn[distances<dbreak])));
  allconns=append(allconns,nivconn[distances>dbreak]);alltypes=append(alltypes,rep("far",length(nivconn[distances>dbreak])));alldbreaks=append(alldbreaks,rep(dbreak,length(nivconn[distances>dbreak])));
}
g=ggplot(data.frame(conn=means,connsd=sds,type=types,dbreak=dbreaks),aes(x=dbreak,y=conn,col=type,group=type))
g+geom_point()+geom_line()+geom_errorbar(aes(ymin=conn-connsd,ymax=conn+connsd))

g=ggplot(data.frame(conn=allconns,type=alltypes,dbreak=alldbreaks),aes(x=dbreak,y=conn,col=type,group=type))
g+geom_point()+geom_smooth()


# area of inondable zone within a buffer
areas = gArea(inondation,byid = TRUE)
wareas = sapply(d$cp[inds],function(s){ sum(areas*exp(-gDistance(gCentroid(communes[communes$cp==s,]),inondation,byid = TRUE)/5000))})

plot(wareas,nivconn[inds])
plot(wareas,risqinond[inds])


g=ggplot(data.frame(distance=distances,risk=risqinond[inds]),aes(x=distance,y=risk))
g+geom_point()+geom_smooth()

g=ggplot(data.frame(area=wareas,risk=risqinond[inds]),aes(x=area,y=risk))
g+geom_point()+geom_smooth()

g=ggplot(data.frame(distance=distances,conn=nivconn[inds]),aes(x=distance,y=conn))
g+geom_point()+geom_smooth()

g=ggplot(data.frame(area=wareas,conn=nivconn[inds]),aes(x=area,y=conn))
g+geom_point()+geom_smooth()

g=ggplot(data.frame(distance=distancessdf,conn=sdf$goodconn[indssdf]),aes(x=distance,y=conn))
g+geom_point()+geom_smooth()


## Carto connaissance du risque

cols <- carto.pal(pal1 = "red.pal",n1 = 5)
choroLayer(spdf = communes,spdfid = "cp",
           df = data.frame(sdf),dfid = 'cp',#"zip",
           var="conn",
           col=cols,
           nclass=6,
           breaks = seq(from=0.5,to=5.5,by=1.0),#quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           add=FALSE,lwd = 0.01,
           legend.pos = "topleft",
           legend.title.txt = "connaissance",
           legend.values.rnd = 1.0
)
plot(inondation,add=T,col='blue',lwd=0.2)
plot(communes,border = "grey20",add=TRUE, lwd=0.2)
layoutLayer(title = "Connaissance du risque",
            sources = "Source : Questionnaires Etudiants, IAUIdF",
            author = "",
            scale = 0,
            frame = TRUE,
            col = "black",
            coltitle = "white")


## map area and distance
distances = sapply(communes$cp,function(s){gDistance(gCentroid(communes[communes$cp==s,]),inondation)})

cols <- carto.pal(pal1 = "red.pal",n1 = 20)
choroLayer(spdf = communes,spdfid = "cp",
           df = data.frame(cp=communes$cp,distance=distances),dfid = 'cp',#"zip",
           var="distance",
           col=cols,
           nclass=20,
           #breaks = seq(from=0.5,to=5.5,by=1.0),#quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           add=FALSE,lwd = 0.01,
           legend.pos = "topleft",
           legend.title.txt = "connaissance",
           legend.values.rnd = 1.0
)
plot(inondation,add=T,col='blue',lwd=0.2)
plot(communes,border = "grey20",add=TRUE, lwd=0.2)

allareas = sapply(communes$cp,function(s){ sum(areas*exp(-gDistance(gCentroid(communes[communes$cp==s,]),inondation,byid = TRUE)/5000))})

cols <- carto.pal(pal1 = "red.pal",n1 = 20)
choroLayer(spdf = communes,spdfid = "cp",
           df = data.frame(cp=communes$cp,area=allareas),dfid = 'cp',#"zip",
           var="area",
           col=cols,
           nclass=20,
           #breaks = seq(from=0.5,to=5.5,by=1.0),#quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           add=FALSE,lwd = 0.01,
           legend.pos = "topleft",
           legend.title.txt = "connaissance",
           legend.values.rnd = 1.0
)
plot(inondation,add=T,col='blue',lwd=0.2)
plot(communes,border = "grey20",add=TRUE, lwd=0.2)


