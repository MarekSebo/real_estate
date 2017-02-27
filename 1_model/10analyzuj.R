install.packages("ggplot2");
install.packages("sm");
install.packages("mice");
install.packages("VIM");
install.packages("car");
install.packages("plyr");
install.packages("DAAG");
library(VIM);
library(boot);
library(mice);
library(ggplot2);

#PORADIE
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/scripts_funkcne.r")
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/nacitaj.r")
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/nacitajPonuku.r")
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/spojtabulky2.r") 
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/nacitajValidacny.r") 
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/sameas.r") 
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/cisti.r")
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/NA.r"); 
source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/NAodvazne.r"); 

##############################################
library(sm);

d<-dPoNAodvazne;





#sempl=sample.int(length(d[,1]),size=4000);
#ds<-d[c(1,sempl,length(d[,1]) ),];
print("zatial nie su zavedene casove vahy wt! nemam datumy stiahnutia ponukovych inzeratov.")


#model Xlok only
m<-lm(dPSQ~0+Xlok, data=d,weights=d$w);
summary(m)
kriteria(m,d)

#model REGION only
m<-lm(dPSQ~0+REGION, data=d,weights=d$w);
summary(m)
kriteria(m,d)


#model MUNIC ONLY
m<-lm(dPSQ~0+MUNICIPALITY, data=d,weights=d$w);
summary(m)
kriteria(m,d)

#simulacia IFP modelu (nove d)
imitIFP()
mIFP<-lm(data=d, log(dPSQ)~0+.) 


#mozno nefunguje
kriterialog(mIFP,d);
#!!!!!!!!!pozor
#dm<-d;
#fit<-mIFP
#dv<-zdv
#dv<-data.frame(AREA=as.integer(as.character(dv$PocetIzieb))*dv$RozlohaIzby, balkon=as.factor(as.integer(dv$balkon_rozl>0)), 
 #              lodzia=as.factor(as.integer(dv$lodzia_rozl>0)), terasa=as.factor(as.integer(dv$terasa_rozl>0)),dv)


#znova treba
d<-dPoNAodvazne;

#Konkretny model KLASIK
menaaklas<-c("Garsonka","PocetIzieb","vlast","park_garaz","vytah","rok","podlazie","stav", "naklady",
            "konstr","vybav","stary_rek","RozlohaIzby","balkon_rozl","lodzia_rozl","terasa_rozl", "naklady","balkon","lodzia","terasa","wp");
lokalita<-c("Xlok");
dm<-d[,unique(c("dPSQ",lokalita,menaaklas) )];
#zaloha
zdm<-dm;

klasik<-lm(dPSQ~0+. #+Garsonka:PocetIzieb  
      , data=dm,weights=d$w);
kriteria(klasik,dm)

#LOG model
menaa<-c("Garsonka","PocetIzieb","vlast","park_garaz","vytah","rok","podlazie","stav", #"naklady",
         "konstr","vybav","stary_rek","RozlohaIzby","balkon_rozl","lodzia_rozl","terasa_rozl", "wp");

# Xlok model
lokalita<-c("Xlok");
dm<-d[,unique(c("dPSQ",lokalita,menaa) )];
zdmLOG<-dm;
mLOG<-lm(log(dPSQ)~0+. #+Garsonka:PocetIzieb  
              , data=dm,weights=d$w);

# MUNI model
lokalita<-c("MUNICIPALITY");
dmmuni<-d[,unique(c("dPSQ",lokalita,menaa) )];
zdmLOGmuni<-dmmuni;
mLOGmuni<-lm(log(dPSQ)~0+. #+Garsonka:PocetIzieb  
              , data=dmmuni,weights=d$w);
kriterialog(mLOGmuni,dm)

# StreetModel iba BA
#lokalita<-c("Xlok");
#ktore<-which(nchar(as.character(dm$Xlok))==9)
#dBA<-dm[ktore,setdiff(as.character(attr(mLOG$terms,      "variables")),c("list", "log(dPSQ)" ))  ];
#mLOGBA<-lm(log(dPSQ)~0+. #+Garsonka:PocetIzieb, data=dBA);

#zapis typov premennych v modeli
write.table(sapply(dm, class),file = "C://Users//Lenovo//Disk Google//Machine Learning//DTLN//integracia//scripts_model//typy.csv",
            sep = ";", col.names = NA)
#uloz model
saveRDS(mLOG,file="mLOG.rds")
saveRDS(dm, file="dm.rds")
saveRDS(mLOGmuni, file="mLOGmuni.rds")
saveRDS(dmmuni, file="dmmuni.rds")
saveRDS(REGPJ, file="REGPJ.rds")

#saveRDS(mLOG, file="mLOG.rds")


rm(dm)








##ZACINA NEKONTROLOVANA CAST
#######

library(car)

sl("MUNICIPALITY");

munic<-mod("MUNICIPALITY",d)
munic_hi<-which(munic$coef[match(d$MUNICIPALITY,levels(d$MUNICIPALITY))]>1800 );
munic_med<-which(munic$coef[match(d$MUNICIPALITY,levels(d$MUNICIPALITY))]<=1800 & munic$coef[match(d$MUNICIPALITY,levels(d$MUNICIPALITY))]>1100 );
munic_lo<-which(munic$coef[match(d$MUNICIPALITY,levels(d$MUNICIPALITY))]<=1100 );

mod(prem, d[munic_hi,])
mod(prem, d[munic_lo,])
#modeling :)
prem<-c("PSQ","MUNICIPALITY","PocetIzieb","RozlohaIzby","Garsonka","podlazie","stav","balkon","balkon_rozl"
              ,"lodzia","lodzia_rozl","terasa","terasa_rozl","konstr","vytah");  
md<-d;

lm(PSQ~0+Xlok,data=d,weights=w);

m<-mod(prem,dBA);
confint(m);


MBA<-sort(unique(d$MUNICIPALITY[which(d$REGION=="BA")]));
SBA<-sort(unique(match( as.character(d$ID_STREET[which(d$REGION=="BA")]),levels(d$ID_STREET))));
tab<-table(d$MUNICIPALITY)
tab[setdiff(levels(d$MUNICIPALITY),MBA)];

rm(munic, modlog,modPrvyREGION)

length(SBA)

blava<-rep(0,length(d$PSQ)); blava[which(d$REGION=="BA")]=1;

hustoty<-function(data, response,pred)
{
  response<-as.character(response); pred<-as.character(pred);
  sm.density.compare(data[,response], data[,pred], xlab=pred);
  colfill<-c(2:(2+length(levels(data[,pred])))) ;
  legend(locator(1), levels(data[,pred]), fill=colfill);  
}
hustoty(d,"PSQ","Garsonka");




title(main="MPG Distribution by Car Cylinders")
?sm.density.compare

str(modeldata)
modlog<-glm(log(dPSQ)~0+.,data=modeldata,weights=d$w); #, weights=d$w
BAmodlog<-lm(dPSQ~0+.,data=modeldata,weights=d$w); #, weights=d$w
summary(modlog)$sigma



rm(names_current,sempl,N)