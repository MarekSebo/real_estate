#sluzba1 = pricingAPI.R
#pricingAPI<-function(realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
#     objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)
#output = numeric(cena daneho bytu)
#POSTUP:
#transf() vypluje pomenovany data frame. tu sa bude transformovat nazov ulice na ID
#vyber_model() vyberie model
#vyber_Xlok() zredukuje muni a street id na jedno ID Xlok.
#indexy_kalkulacka() vyberie index ktory sa ma dat do..
#pricing() a prikazy za nim ocenia s pomocou vystupu z transf()


pricingAPI<-function(realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
     objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)
{
  
#transformacia vstupu z formulara
x<-transf(realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
  objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)

#vyber modelu
ktory_model<-vyber_model(x$MUNI_ID,x$STREET_ID)

#Xlok
Xlok<-vyber_Xlok(ktory_model, x$MUNI_ID,x$STREET_ID)
print(paste("Xlok=",Xlok,sep=" "))
#z ktoreho df vyberame?
if (ktory_model=="muniSR")
  data=muniSR else 
  data=uliceBA
n<-nrow(data)

#indexy, ktore chceme ocenit (tu iba jeden)
index<-indexy_kalkulacka(ktory_model, Xlok)
print(paste("indexy co idu do pricingu: ",index,sep=""))
if(length(index)!=1)
  return(print(paste("zly rozmer indexov, ktore idu do pricingu. ma to byt iba jeden a je ich: ",length(index),sep="")) )
cena<-exp(data[index,"koef_final"] +pricing(ktory_model, index,x))   
return(cena)
}