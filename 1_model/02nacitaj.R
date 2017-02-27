#NACITANIE
#najprv zbehni tento script
#potom pozor, treba ulozit premenne, nacitajMEGA ich prepisuje
#mozno treba este nieco viac urobit (pozri oba scripty)
startcas<-proc.time()
t<-read.csv("C://Users//Lenovo//Disk Google//Data Mining//diplomka files//source//final data//povodne data//byty_realiz_SR_2015//byty_realiz_SR_2015_final2.csv", header=TRUE, sep=";", 
            colClasses=c(rep("character",11),"numeric", rep("character", 2), rep("integer",2), "character", "integer", "character","character") );
n<-length(t$ID2);


#MASTER VALUE namiesto VAL a ID VALUES
t$CHAR_VALUE<-replace(as.character(as.numeric(t$CHAR_VALUE)), is.na(as.character(as.numeric(t$CHAR_VALUE))),"");
t$CHAR_ID<-replace(as.character(as.numeric(t$CHAR_ID)), is.na(as.character(as.numeric(t$CHAR_ID))),"");
masterval<-as.numeric(paste(t$CHAR_VALUE,t$CHAR_ID))
t<-data.frame(t[,c(1,2,4:15,17,18)],masterval)

t$IDZAK<-as.character(t$IDZAK);
t$IDZAK<-gsub("-","",t$IDZAK);
ztp<-t;

#VALS A IDS , mena
char_numberz<-sort(unique(t$CHAR_VARIETY));
char_types<-c(rep("val",5),"id","val","val","id", "val",rep("id",13),"val","val",rep("id",5), "val", "val",
              rep("id",6),rep("val",5), "id","id" )
char_names<-paste(char_types,as.character(char_numberz),sep="");

#tuto cast vynechat ked nerobim tento dataset ako prvy / znova ho nacitavam
#{
#POPISKY
char<-read.csv("C://Users//Lenovo//Disk Google//Data Mining//source//final data//ciselniky//CMN_ciselnik_vlastnosti.csv", header=TRUE, sep=";");
popisky<-data.frame(char_numberz, char_types,char_names, 
                    char$CHAR_VARIETY_VALUE[match(as.integer(char_numberz),as.integer(char$CHAR_VARIETY))], 
                    char$MEASURE_UNIT[match(as.integer(char_numberz),as.integer(char$CHAR_VARIETY))]);
names(popisky)[c(4,5)]<-c("popis","unit")
#}

#ZRATAJ N
ajdi=1;
N<-0;
for(i in 1:n)
{
  if(ajdi!=t$ID_REALTY_DATA[i])
  {
    N=N+1;
    ajdi=t$ID_REALTY_DATA[i];
  }
}
print("predane byty obsahuju tolkoto verzii"); print(N);
#POZOR, TOTO ROBI PROBLEMY KED su bud chybne IDcka alebo nie su dokonale zoradene!! problem v nacitani premennych, fix needed || vyhodit riadky!!

#VYTVOR PREMENNE
ids_names<-char_names[which(char_types=="id")];
vals_names<-char_names[which(char_types=="val")];
id<-as.data.frame(matrix(as.integer(NA),N,length(ids_names)) );
val<-as.data.frame(matrix(as.numeric(NA),N,length(vals_names)) );
nother=14;
o<-as.data.frame(matrix(NA,N,nother));
names(o)<-names(t[1:nother])
names(id)<-ids_names;
names(val)<-vals_names;
###########POTIALTO#####aj to je RISKY CODE######

#NACITAJ PREMENNE
byt=0;
ajdi=1;
name=as.name("jozko");
counter<-0;
t$ID_REALTY_DATA<-as.integer(t$ID_REALTY_DATA)
for(i in 1:n)
{
  
  #NAPLN OTHERS  
  if(ajdi!=t$ID_REALTY_DATA[i])
  {
    byt=byt+1;
    o[byt,1:nother]=t[i,1:nother];
    ajdi=t$ID_REALTY_DATA[i];
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
t$ID_REALTY_DATA<-as.character(t$ID_REALTY_DATA)

###STRUKTURA DAT
#STRUKTURA POVODNYCH PREMENNYCH

for(i in 1:10)
{
  o[,i]=replace(o[,i],o[,i]=="0",NA)
  o[,i]=factor(o[,i]);
}
o[,11]=as.integer(o[,11]);
o[,12]=as.Date(o[,12],format="%d.%m.%y");
o[,13]=as.Date(o[,13],format="%d.%m.%y");
o[,14]=as.integer(o[,14]);

#struktura id
for(i in 1:length(id[1,]))
  id[,i]=factor(id[,i]);

#DATUMY ->DAYS
DAYS<-as.integer(o$SELL_DAY-o$INSERT_DAY+1)
AGE_SELL<-as.integer(as.Date("01.01.2016",format="%d.%m.%Y")-o$SELL_DAY)

#Price per sq meter
PSQ<-o$SELL_PRICE/o$AREA

#nova tabulka
#!!!!!!site na tieto data
o<-data.frame(o,DAYS,AGE_SELL,PSQ)
names(o)[1]<-"ID2"
#################HOTOVE DATA#################

#BACKUP (t uz je backupovane)
zovalidp<-data.frame(o,val,id);
zpopisky<-popisky;
rm(DAYS,PSQ,AGE_SELL,ajdi,byt,char_names,char_numberz,char_types,ids_names, masterval,name,vals_names,
   id,o,t,val,n,N,nother,counter);

print(proc.time()-startcas)
