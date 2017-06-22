#sluzba1 = pricingAPI.R

#BEGIN POSTUP
#NASLEDUJUCI POSTUP NEMUSI BYT AKTUALNY:
#pricingAPI<-function(realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
#     objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)
#output = numeric(cena daneho bytu)
#POSTUP:
#transf() vypluje pomenovany data frame. tu sa bude transformovat nazov ulice na ID
#vyber_model() vyberie model
#vyber_Xlok() zredukuje muni a street id na jedno ID Xlok.
#indexy_kalkulacka() vyberie index ktory sa ma dat do..
#pricing() a prikazy za nim ocenia s pomocou vystupu z transf()

#sluzba2 =polygonAPI.R
#polygonAPI<-function(polygon, zadany_model, realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
#        objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)
#output = pomenovany data.frame (zatial: "lok","lok_ID","lat","long","cena")
#transf() transformuje tabulku (inak treba poriesit ked nezada lokalitu. treba ho aj tak pustit)
#vyberie ktore indexy su v polygone
#pricing() oceni s pomocou vystupu z transf()

#dalsie scripty
#transf: vstup -> x
#vyber_model: x=transf(vstup)-> {"muni", "ulice"}
#vyber_Xlok: muni,street, ktory_model -> vektor Xlok
#indexy_kalkulacka: vektorXlok -> vektor indexov
#indexy_polygon: ktory_model, suradnice polygonu -> indexy, ktore chceme ocenit
#pricing: x(vektor), indexy(vektor), ktory_model ->suma koef okrem Xlok(vektor)
#KONIEC POSTUPU

#nastav directory
directory<-getwd() 
print(paste("terajsia directory je:'", getwd(), sep=" ", "'ak treba, upresnite ju v kode (deklaracia premennych)"))


##odtialto mozes kopirovat

#nacitaj velke tabulky
muniSR<-readRDS(file = "muniSRfinal.rds")
uliceBA<-readRDS(file = "uliceBAfinal.rds")
dm<-readRDS(file="dm.rds")
require(Rserve)
require(mgcv)

BAmuni<-unique(uliceBA$MUNI_ID)
vsetky_premenne<-c("MUNI_ID","STREET_ID","Garsonka","PocetIzieb","vlast","park_garaz","vytah", "rok","podlazie","stav",
  "konstr","vybav","stary_rek","RozlohaIzby", "balkon_rozl","lodzia_rozl" ,"terasa_rozl", "wp"  )
spojite_premenne<-c("RozlohaIzby","balkon_rozl","lodzia_rozl","terasa_rozl","wp")
thrash_premenne<-c("MUNI_ID","STREET_ID")
kategoricke_premenne<-setdiff(vsetky_premenne,c(thrash_premenne,spojite_premenne) )

#nacitaj funkcie, scripty
source("scripts_API.R")

#nacitaj scripty
source("pricingAPI.R")
source("polygonAPI.R")



