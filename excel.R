
library(readxl)

files = list.files('data/questionnaires/validated')

unfilled_string ="Ne saisir que le NOM de famille"

#allnames = c()
d = data.frame()
ldata = list()
for(file in files){
  show(file)
  #sheet1 = read_excel(file,sheet = 1,col_names = FALSE)
  #sheet2 = read_excel(file,sheet = 2,col_names = FALSE)
  #show(sheet1[1,3])
  #show(sheet2[2,3])
  #sheet=data.frame()
  #if(sheet1[1,3]==unfilled_string&&sheet2[2,3]!=unfilled_string){sheet=sheet2[2:nrow(sheet2),3:ncol(sheet2)]}
  #if(sheet2[2,3]==unfilled_string&&sheet1[1,3]!=unfilled_string){sheet=sheet1[,3:ncol(sheet2)]}
  #if(sum(dim(sheet))>0){
  #  allnames=append(allnames,toupper(sheet[1,]))
  #}else{
  #  show(file)
  #}
  sheet = read_excel(paste0('data/questionnaires/validated/',file),sheet = 1,col_names = FALSE)
  #show(sheet[1,3:5])
  #allnames=append(allnames,toupper(sheet[1,]))
  
  currentdata=data.frame(t(sheet))
  #show(dim(currentdata))
  #d=rbind(d,currentdata[3:nrow(currentdata),1:39])
  currentd=currentdata[3:nrow(currentdata),1:39]
  ldata[[file]]=currentd
  #colnames(currentd)<-c(data.frame(sheet)[,1])
  #show(sapply(currentd[,11],function(s){substr(s,1,5)})%in%communes$cp)
}

d = bind_rows(ldata)
colnames(d)<-c(data.frame(sheet)[,1])
rownames(d)<-paste0(d$NomSaisie,1:nrow(d))
descriptionvars = c(data.frame(sheet)[,2])

#d$VoletA__A4_2









