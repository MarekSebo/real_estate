#mozno nahradit popiskovy script tym z NacitajUpravene.r
nother=16;
popisky<-zpopisky;
popisky_povodne<-zpopisky[,1:2];
#NACITANIE
t<-read.csv("C://Users//Lenovo//Disk Google//Data Mining//diplomka files//source//final data//povodne data//byty_ponuka_SR_2015//byty_ponuka_SR_2015.csv", header=TRUE, sep=";", 
            colClasses=c(rep("character",11),"numeric", rep("character", 1), rep("integer",2), "character", "integer", "character","character") );
names(t)[c(1,7:9,12,13,14,17,18,19)]<-c("ID2","DISTRICT","MUNICIPALITY","CADASTRE","AREA","INSERT_DAY","SELL_PRICE","CHAR_VARIETY","CHAR_ID","CHAR_VALUE");
n<-length(t$ID2);

#MASTER VALUE namiesto VAL a ID VALUES
t$CHAR_VALUE<-gsub(",",".",t$CHAR_VALUE);
t$CHAR_VALUE<-replace(t$CHAR_VALUE, is.na(as.character(as.numeric(t$CHAR_VALUE))),"");
t$CHAR_ID<-replace(as.character(as.numeric(t$CHAR_ID)), is.na(as.character(as.numeric(t$CHAR_ID))),"");
masterval<-as.numeric(paste(t$CHAR_VALUE,t$CHAR_ID),sep="")
t<-data.frame(t[,c(1,2,4:17)],masterval)

t$IDZAK<-as.character(t$IDZAK);
t$IDZAK<-gsub("-","",t$IDZAK);
#ULOZ BACKUP
zt<-t;

#ZRATAJ N
ajdi=as.character(1);
N<-0;
for(i in 1:n)
  if(ajdi!=t$ID2[i])
  {
    N=N+1;
    ajdi=t$ID2[i];
  }
print("ponukane byty obsahuju tolkoto verzii"); print(N);
#POZOR, TOTO ROBI PROBLEMY KED su bud chybne IDcka alebo nie su dokonale zoradene!! problem v nacitani premennych, fix needed || vyhodit riadky!!

#VALS A IDS , typy su unikatne pre konretne data, script je univerzalny
t<-zt;
char_numberz<-sort(unique(c(popisky$char_numberz,sort(unique(t$CHAR_VARIETY)), sort(unique(t$ID_REALTY_CHAR)) )) );
char_types<- as.character(rep(NA,length(char_numberz)))
char_types<-as.character(popisky$char_types[match(char_numberz,popisky$char_numberz)  ] );
#unikatny prvok
char_types[is.na(char_types)]<-c("val","val","id","id","id","id","id","id","id","id","id")
char_names<-paste(char_types,as.character(char_numberz),sep="");

#POPISKY
char<-read.csv("C://Users//Lenovo//Disk Google//Data Mining//source//final data//ciselniky//CMN_ciselnik_vlastnosti.csv", header=TRUE, sep=";");
popisky<-data.frame(char_numberz, char_types,char_names, 
                    char$CHAR_VARIETY_VALUE[match(as.integer(char_numberz),as.integer(char$CHAR_VARIETY))], 
                    char$MEASURE_UNIT[match(as.integer(char_numberz),as.integer(char$CHAR_VARIETY))]);
names(popisky)[c(4,5)]<-c("popis","unit")
zpopisky<-popisky;

#VYTVOR PREMENNE
ids_names<-char_names[which(char_types=="id")];
vals_names<-char_names[which(char_types=="val")];
id<-as.data.frame(matrix(as.integer(NA),N,length(ids_names)) );
val<-as.data.frame(matrix(as.numeric(NA),N,length(vals_names)) );

o<-as.data.frame(matrix(NA,N,nother));
names(o)<-names(t[1:nother])
names(id)<-ids_names;
names(val)<-vals_names;

#NACITAJ PREMENNE
byt=0;
ajdi=1;
name=as.name("jozko");
counter<-0;
t$ID2<-as.integer(t$ID2);

startcas<-proc.time();
for(i in 1:n) #1000 riadkov za 6 sekund
{
  
  #NAPLN OTHERS  
  if(ajdi!=t$ID2[i] )
  {
    byt=byt+1;
    o[byt,1:nother]=t[i,1:nother];
    ajdi=t$ID2[i];
  }   
  #VALs
  if( popisky$char_types[match(t$CHAR_VARIETY[i],popisky$char_numberz)] =="val")
  {
    name<-as.character(paste("val",t$CHAR_VARIETY[i],sep=""))
    val[byt,name]=t$masterval[i];
  }
  #IDs
  if( popisky$char_types[match(t$CHAR_VARIETY[i],popisky$char_numberz)] =="id")  
  { 
    name<-as.character(paste("id",t$CHAR_VARIETY[i],sep=""))
    id[byt,name]=t$masterval[i];
  }  
}
print(proc.time()-startcas);
#-----------------------------

###STRUKTURA DAT
#STRUKTURA POVODNYCH PREMENNYCH

for(i in 1:10)
  o[,i]=factor(replace(o[,i],o[,i]=="0",NA));
o[,11]=as.integer(o[,11]);
o[,12]=as.Date(o[,12],format="%d.%m.%y");
o[,14]=as.integer(o[,14]);

#struktura id
for(i in 1:length(id[1,]))
  id[,i]=factor(id[,i]);

#DATUMY ->AGE
AGE_INSERT<-as.integer(as.Date("01.01.2016",format="%d.%m.%Y")-o$INSERT_DAY)
#Price per sq meter
PSQ<-o$SELL_PRICE/o$AREA;

#nova tabulka
o<-data.frame(o,AGE_INSERT,PSQ);

#################HOTOVE DATA#################
#BACKUP ( t a popisky uz su backupnute)
zovalid<-data.frame(o,val,id);

rm(popisky_povodne,PSQ,AGE_INSERT,ajdi,byt,char_names,char_numberz,char_types,ids_names, masterval,name,
   vals_names,counter,nother,id,t,o,val,char,n,N);

