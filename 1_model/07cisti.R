#script {cisti} +implementacia 
#
popisky<-zpopisky;


cisti<-function(d,popisky,povinne_premenne,ChybajucePovinnePremTolerancia)
{
  startcas<-proc.time(); 
for (i in 2:5)
  popisky[,i]<-as.character(popisky[,i]);
d$SAMEAS<-FALSE;
#mozem sem dat aj dPredObohatenim ......
names_current<-names(d);
N<-length(d$ID2);


#TU bude obohacovanie xLOK a kontrola spravnosti existujucich Xlok pomocou REGPJ.csv
#length(which(!is.na(as.integer(ID_STREET) ) & as.integer(as.character(ID_STREET))>100000000 & as.integer(as.character(ID_STREET))<1000000000 ) )
#length(which(!is.na((ID_STREET) ) ) )
#nie su tam ine ako NA hodnoty ktore su divne

#VYHOD PREMENNE
#vyhod premenne typu A
kandidati_vyhod=c("val3","val4","val7","val11","val218", "val69","id84", "id117","id124","id127","id128",
                  "id130","val204","val217","id224");
vyhod_prem=setdiff(kandidati_vyhod, setdiff(kandidati_vyhod, names_current) );
names_current<-setdiff(names_current,vyhod_prem)
d<-d[,names_current] ;


#vyhod premenne aj s riadkami (2.kategoria)
kandidati_vyhod=c("id18", "id32","id114","id126","id131", "id132","id146","id149","id150", "id151", "id200", "id222");
vyhod_prem=setdiff(kandidati_vyhod, setdiff(kandidati_vyhod, names_current ) );
names_current<-setdiff(names_current,vyhod_prem);
ktore<-NULL;
if(!is.null(vyhod_prem))
  for(i in 1:length(vyhod_prem))
    ktore<-union( which(!is.na(d[,vyhod_prem[i]]) ),ktore )
print("vyhodime tolkoto riadkov")
print(length(ktore) )
N<-length(d$ID2)
d<-data.frame( d[setdiff(c(1:N),ktore ),setdiff(names(d),vyhod_prem)  ] );


#Xlok
#zatial ako MUNICIPALITIES
Xlok=as.character(d$REGION); 
d<-data.frame(Xlok, d[,names_current] );
rm(Xlok);
names_current<-union("Xlok",names_current);
#####################################

#
#TRANSFORMACIE
d$val202<-d$val202/d$AREA; #relativna cena za udrzbu
d$val230<-d$val230/d$AREA; #relativna rozloha balkona
d$val245<-d$val245/d$AREA; #realtivna rozloha lodzie
d$val248<-d$val248/d$AREA; #relativna rozloha terasy

#####
#OUTLIERS d
N<-length(d$ID2)
vyhod<-rep(FALSE,length(d[,1]));
#niektore budeme nahradzat NAckami namiesto vyhadzovania
vyh<-0;
k<-0;

#typy bytov - Garsonka, PocetIzieb, RozlohaIzby
d$ID_REALTY_VARIETY<-as.integer(as.character(d$ID_REALTY_VARIETY));
Garsonka<-rep(0,N);
Garsonka[which(as.character(d$ID_REALTY_VARIETY)=="11")]=1;
Garsonka[which(as.character(d$ID_REALTY_VARIETY)=="122")]=1;
PocetIzieb<-as.character(d$ID_REALTY_VARIETY);
PocetIzieb<-replace(PocetIzieb, PocetIzieb=="11" | PocetIzieb=="12", 1);
PocetIzieb<-replace(PocetIzieb, PocetIzieb=="13" | PocetIzieb=="122", 2);
PocetIzieb<-replace(PocetIzieb, PocetIzieb=="14", 3);
PocetIzieb<-replace(PocetIzieb, PocetIzieb=="15", 4);
PocetIzieb<-replace(PocetIzieb, PocetIzieb=="16", 5);
RozlohaIzby<-d$AREA/as.integer(PocetIzieb);
PocetIzieb<-as.factor(as.character(PocetIzieb) );
d<-data.frame(Garsonka,PocetIzieb,RozlohaIzby,d[,names_current]);
d$PocetIzieb<-as.factor(as.character(d$PocetIzieb));
names_current<-union(names_current,c("Garsonka","PocetIzieb","RozlohaIzby"));
rm(Garsonka,PocetIzieb,RozlohaIzby);

vyhod[which(d$RozlohaIzby>=75)]=TRUE;

vyhod[which(d$ID_REALTY_VARIETY==125 | d$ID_REALTY_VARIETY==123 | d$ID_REALTY_VARIETY==124 | d$ID_REALTY_VARIETY==126 | d$ID_REALTY_VARIETY==127) ]=TRUE;
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("TYP BYTU vyhodi(lofty, mezonety, 6+ izbove")
print(k)


#Lokalita - vyhod vatu (REGION mal tu ok hodnoty) - nahrad ju NAckami
#dorobit vyhadzovanie riadkov podla potrieb Xlok
d$STREET[which(nchar(as.character(d$STREET))<4)]=NA;
d$CADASTRE[which(nchar(as.character(d$CADASTRE))<4)]=NA;
d$ID_STREET[which(nchar(as.character(d$ID_STREET))<4)]=NA;
d$DISTRICT[which(nchar(as.character(d$ID_STREET))<3)]=NA;

#ID_UNIT_PRICE vyhod vsetky ine ako 7 (su iba 2 :) )
vyhod[which((as.character(d$ID_UNIT_PRICE)!="7") |  (is.na(d$ID_UNIT_PRICE)) ) ]=TRUE #sedliacky a velmi konzervativny setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("ID_UNIT_PRICE vyhodi")
print(k)

#PSQ
vyhod[which((d$PSQ>4500) |  (d$PSQ<300 ) )]=TRUE #sedliacky a velmi konzervativny setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("PSQ vyhodi")
print(k)

#KATEGORIA BYTU BYTU
vyhod[which(d$id50==2 | d$id50==3 | d$id50==4 )]=TRUE #sedliacky a velmi konzervativny setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("kategoria bytu vyhodi")
print(k)


#AREA
vyhod[which((d$AREA>400 |  d$AREA<10 ) )]=TRUE; #sedliacky a velmi konzervativny setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("AREA vyhodi");
print(k);

#ROKY
vyhod[which((d$val12>2016) |  (d$val12<1900 ) )]=TRUE #sedliacky a velmi konzervativny setting hradieb
vyhod[which((d$val13>2016) |  (d$val13<1900 ) )]=TRUE #sedliacky a velmi konzervativny setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("ROKY (val12,13) vyhodi");
print(k);

#val7 nahrad NAckami (plocha pozemku)!!! toto je diskutabilna cast, mozno ich treba vyhodit
#nena<-length(which(!is.na(d$val7)));
#d$val7[which(!is.na(d$val7))]=NA 
#print("Plocha pozemku nahradi NAckami vsetky cisla. to je max tolkoto (mozu byt neskor vyhodene)");
#print(nena);


#val14 = znalecky posudok VYNECHAME
#summary(d$val14);

#val35, val36 = pocet parkovacich miest v  garazi / na parkovisku
#tu by sme mohli nechavat tie co su >4 a iba ich nahradzat NAckami
nena<-length(which(!is.na(c(d$val35,d$val36))))
d$val35[which((d$val35>4) |  (d$val35<0 ) )]=NA #sedliacky a velmi konzervativny setting hradieb
d$val36[which((d$val36>4) |  (d$val36<0 ) )]=NA #sedliacky a velmi konzervativny setting hradieb
print("Park miesta (val35,36) nahradi NAckami max tolkoto (mozu byt neskor vyhodene)");
print(nena-length(which(!is.na(c(d$val35,d$val36))))  );

#val35,36= pocet parkovacich miest v  garazi / na parkovisku
#vyhadzovanie riadkov
vyhod[which((d$val35>2))]=TRUE # setting hradieb
vyhod[which((d$val36>2))]=TRUE # setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("Park miesta (val35,36) nech su max 2. to vyhodi");
print(k);

#val202= naklady na sluzby a energiu
vyhod[which((d$val202>5) |  (d$val202<0) )]=TRUE # setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("NAKLADY za SLUZBY (val202) vyhodi");
print(k);


vyhod[which((d$val230>0.7) | (d$val230<0))]=TRUE #setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("rozloha balkona (val230) vyhodi");
print(k);

#boxplot(val245);
vyhod[which((d$val245>0.7) | (d$val245<0))]=TRUE #setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("rozloha loggie val 245 vyhodi");
print(k);

#boxplot(val248);
vyhod[which((d$val248>2) | (d$val248<0))]=TRUE #setting hradieb
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("rozloha terasy val248 vyhodi");
print(k);

#val237, val238= poschodie, pocet poschodi celkom
#boxplot(val237)
nena<-length(which(!is.na(d$val237)));
d$val237[which((d$val237>40) | (d$val237< -1))]=NA #setting hradieb
print("poschodie val 237 na NA premeni tolkoto");
print(nena-length(which(!is.na(d$val237))));

nena<-length(which(!is.na(d$val238)));
d$val238[which((d$val238>40) | (d$val238<d$val237))]=NA #setting hradieb
print("pocet poschodi val238 na NA premeni tolkoto");
print(nena-length(which(!is.na(d$val238))));

#vyhod statne byty
vyhod[which((d$id26==5) )]=TRUE
vyhod[which((d$id26==3) )]=TRUE
vyhod[which((d$id26==6) )]=TRUE
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("odstranenie statnych bytov a inych atyp foriem vlastnictva vyhodi");
print(k);

#VYKUROVANIE 
vyhod[which((d$id121>2) )]=TRUE
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("odstranenie inych typov vykurovania vyhodi");
print(k);

#ZDROJ TEPLA
vyhod[which((d$id122==2 |d$id122>3  ) )]=TRUE
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("odstranenie inych zdrojov tepla vyhodi");
print(k);

#TEPLA VODA (vyhod vsetko nestandardne)
vyhod[which((d$id123>1) )]=TRUE
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("odstranenie inych typov teplej vody vyhodi");
print(k);

#ZARIADENIE (vyhod vsetko nestandardne), neskor mozno aj novostavby(teda kod 2 == standard)
vyhod[which((d$id261==1 | d$id261==3) )]=TRUE
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("odstranenie ZARIADENIE (vyhod vsetko nestandardne)  vyhodi");
print(k);

#povinne premenne
#md.pattern(data.frame(d$id116,d$id125,d$id212,d$id214,d$id215,d$id216,d$id223) )
#konzervativny pristup
nacka<-as.data.frame(matrix(0,length(d[,1]),length(povinne_premenne)));
for(i in 1:length(povinne_premenne))
{
  nacka[which(is.na(d[,povinne_premenne[i] ])),i]=1
}
vyhod[which(apply(nacka,1,sum)>ChybajucePovinnePremTolerancia)]=TRUE;
k<-length(which(vyhod==TRUE))-vyh;
vyh<-vyh+k;
print("vyhodenie bytov s chybajucimi povinnymi prem vyhodi");
print(k);


#Vyhodenie riadkov urcenych na vyhodenie
print("vyhodime tolkoto riadkov:");
print(sum(vyhod));

#vyhadzujeme
nechavame=which(vyhod==FALSE);
d<-d[nechavame,names_current];

d$ID_REALTY_VARIETY<-factor(as.character(d$ID_REALTY_VARIETY ))#preskupenie factor levels po vyhodeni niektorych

print(proc.time()-startcas);
return(d);
rm(kandidati_vyhod,nechavame,vyh,vyhod,vyhod_prem,N,nena,ktore,k,startcas,names_current)
}


#implementacia
#kolko moze max chybat z ich hodnot aby sme ich nechali?
dPoCisti<-cisti(dPoSameAs,zpopisky,povinne_premenne=c("id212","id223"),
             ChybajucePovinnePremTolerancia=1);