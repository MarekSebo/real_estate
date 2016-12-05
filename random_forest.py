#nacitanie, typy premennych, y a X

#SPOLOCNY UVOD - idealne dat do file a spustit takto:
#run 'spolocny_uvod_NN_RF.py'

dm=pd.read_csv('dm.csv')

 #SEED
np.random.seed(232)


#vyber premenne ktore treba
premenne=['Xlok', 'Garsonka','PocetIzieb', 'vlast', 'park_garaz', 'vytah', 'rok', 'podlazie', 'stav',
       'konstr', 'vybav', 'stary_rek', 'wp', 'RozlohaIzby', 'balkon_rozl', 'lodzia_rozl', 'terasa_rozl']
y_name=['dPSQ']
spojite_premenne=['RozlohaIzby', 'balkon_rozl', 'lodzia_rozl', 'terasa_rozl']
kategoricke_premenne=[x for x in premenne if x not in spojite_premenne]

dm=dm[premenne+y_name]



#definicie
def kriteria(predictedY, realY):
    print("function 'kriteria': validation criteria on the sets you specified....")
    if(predictedY.shape!= realY.shape):
        return "Error in Kriteria function: šejpy nešejpujú!: predictedY_shape: {}, realY_shape: {} ".format(predictedY.shape, realY.shape)
    n_vzorka=np.shape(realY)[0]
    print("validation on set size of {} began".format(n_vzorka))
    residuals=(realY - predictedY) #vektor rezidui
    RSS = np.sum(residuals ** 2)
    TSS = np.sum((realY - np.mean(realY)) ** 2)
    print("sqrtMSE = ", np.sqrt(RSS / n_vzorka) )
    print("R**2= ", 1 - RSS / TSS)  # prikaz model_moj.score(testX, testY)
    perc_residuals=residuals/realY
    print("mRE = ", np.median(abs(perc_residuals)))
    print("sqrtMSE v percentach = ", np.sqrt(np.sum(perc_residuals**2)/n_vzorka ))
    print("-------------------------------------------------------------------END")




#shuffle
n_all=len(dm[y_name]) # 55 523
shuf=np.arange(n_all)
np.random.shuffle(shuf)

dm=dm.ix[shuf,premenne+y_name ]
dm.index=range(len(dm.ix[:,1]))

#typy premennych urci
dm[ kategoricke_premenne ]=\
    dm[kategoricke_premenne ].apply(lambda x: x.astype('category'))
dm[ spojite_premenne+y_name]=\
    dm[spojite_premenne+y_name].apply(lambda x: x.astype('float32'))
print("tieto premenne povazujeme za spojite: ",dm[spojite_premenne+y_name].dtypes )

#END SPOLOCNY UVOD_NN_RF

#finalny dataframe: potrebujem iba jeden dataset
X_all=dm[spojite_premenne].join(pd.get_dummies(dm[kategoricke_premenne], prefix=kategoricke_premenne ))
y_all=dm[y_name]
print("rozmery poli")
print(y_all.shape)
print(X_all.shape)


print('mozme modelovat!')
print('-------------------------------------------------------------')

from sklearn.ensemble import RandomForestRegressor
from time import time
from sklearn.model_selection import GridSearchCV
import subprocess
from sklearn.externals import joblib

# RF potrebuje pole tvaru (cislo, )
y_all = np.array(y_all).reshape(-1, )


def grid_search_RF(parameters, print_parms, random_state, X, y):
    # #GRID SEARCH. R**2 score, da sa tam dat aj custom....
    print("fitting GridSearch RF with parameters: {} ...".format(parameters))
    cas = time()
    grid_search_modely = GridSearchCV(RandomForestRegressor(random_state=232), parameters, error_score=0, n_jobs=8,
                                      cv=5)
    grid_search_modely.fit(X, y)
    print("gridSearch trval {} sekund".format(time() - cas))

    print("pickling...")
    joblib.dump(grid_search_modely, 'grid_search_modely.pkl')
    print('model je ulozeny ako grid_search_modely.pkl')

    subprocess.call(['speech-dispatcher'])  # start speech dispatcher
    subprocess.call(['spd-say', '" process has finished"'])
    vysledky = pd.DataFrame(grid_search_modely.cv_results_)

    vysledky[print_parms].to_csv('rf_gridsearchCV_vysledky.csv')
    print(vysledky[print_parms])
    print("vysledky zapisane do rf_gridsearchCV_vysledky.csv")




parameters={'n_estimators':[16,128], 'max_depth':[16,64, 128], 'max_features':(1000, None) }
#parameters={'n_estimators':[2,1], 'max_depth':[1,2], 'max_features':(10,20) }
print_parms=['params', 'rank_test_score', 'mean_test_score', 'mean_train_score', 'mean_fit_time']
#print( [prmeter in list(grid_search_modely.cv_results_.keys()) for prmeter in print_parms])

grid_search_RF(parameters, print_parms, 232, X_all,y_all)



vysledky[print_parms]