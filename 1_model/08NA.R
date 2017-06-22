
NAckuj<-function(d)
{
#konzervativne doplnanie NA

#park miesta nahrad NA nulou
d$val35<-replace(d$val35,is.na(d$val35),0);
d$val36<-replace(d$val36,is.na(d$val36),0);
#2 v garazi aj na parkovisku je isto duplikat.vymaz garaz.
d$val35[which(d$val35==d$val36 & d$val35==2)]=0;

#stav 212
d$id212[which(d$id212==5 | d$id212==6)]=3; #170


#pridaj rok vystavby do roku kolaudacie KATEGORICKEHO
d$id213[which(d$val12>=2013 | d$id213==9)]=8;
d$id213[which(d$val12>=2010 & d$val12<2013)]=7;
d$id213[which(d$val12>=2007& d$val12<2010)  ]=6;
d$id213[which(d$val12>=2003& d$val12<2007)  ]=1;
d$id213[which(d$val12>=1995& d$val12<2003)  ]=2;
d$id213[which(d$val12>=1970& d$val12<1995)  ]=3;
d$id213[which(d$val12>=1946& d$val12<1970)  ]=4;
d$id213[which(d$val12>=1000& d$val12<1946)  ]=5;

#hodnoty 2 premen na 0
d$id116<-(as.integer(replace(as.character(d$id116),as.character(d$id116)=="2","0") ) );
d$id214<-(as.integer(replace(as.character(d$id214),as.character(d$id214)=="2","0") ) );
d$id215<-(as.integer(replace(as.character(d$id215),as.character(d$id215)=="2","0") ) );
d$id216<-(as.integer(replace(as.character(d$id216),as.character(d$id216)=="2","0") ) );

#doplnit IDSTREET a DISTRICT podla dat z d[]. pozri scripts.R


#balkonovanie
#balkony s nulovou rozlohou nahradime ziadnymi balkonmi
d$id214[which(d$val230==0 ) ]=0; #332
d$id215[which(d$val245==0 ) ]=0; #365
d$id214[which(d$val248==0 ) ]=0; #179
#nema balkon a NA rozloha => 0 rozloha
d$val230[which(d$id214==0 & is.na(d$val230) ) ]=0; #37463
d$val245[which(d$id215==0 & is.na(d$val245) ) ]=0; #37278
d$val248[which(d$id216==0 & is.na(d$val248) ) ]=0; #50160
#rozloha je uvedena, tak ma balkon
d$id214[which(d$val230>0) ]=1;
d$id215[which(d$val245>0) ]=1;
d$id216[which(d$val248>0) ]=1;

d$id125[which(is.na(d$id125) & d$val238-d$val237==0)]=3; #62
d$id125[which(is.na(d$id125) & d$val237==0)]=1; #7
#ostatne su na medziposchodi
d$id125[which(is.na(d$id125) )]=2; #402    

d<-d[,setdiff(names(d),"val238")]
return(d);
}

#implementacia
dPoNA<-NAckuj(dPoCisti);

