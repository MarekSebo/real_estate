#MOZNO y a X nie su spravne zarovnane (skontroluj ci sedi priradenie X-y
import pandas as pd
import numpy as np

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
    dm[kategoricke_premenne ].apply(lambda x: x.astype('object')) #category
dm[ spojite_premenne+y_name]=\
    dm[spojite_premenne+y_name].apply(lambda x: x.astype('float32'))
print("tieto premenne povazujeme za spojite: ",dm[spojite_premenne+y_name].dtypes )



# VYTVORENIE TRAIN TEST VALID DATASETOV
#test a valid je ocisteny o unikatne hodnoty premennych

#finalny dataframe: potrebujem iba jeden dataset
X_all=dm[premenne]
y_all=dm[y_name]
print("rozmery poli")
print(y_all.shape)
print(X_all.shape)


perc_valid=0.2

#predely
last_train=int(np.floor((1-perc_valid)*n_all)-1)

#premenne ktore sa nenachadzaju v train datasete
premenne_non_train= [x for x in X_all.columns if (not(( X_all.ix[:last_train,x]!=0).any(axis=0)))]

print(" %d premennych sa nenachadza v train datasete:" % len(premenne_non_train))
#indexy obsahujuce nenulove hodnoty tychto premennych

#urci vsetky indexy valid_test datasetu kde su nulove hodnoty non_train premennych
nuly=np.zeros(last_train,dtype=bool).reshape((-1,1))
ktore=np.array([~(X_all.ix[np.arange(last_train,n_all),premenne_non_train].sum(axis=1)==0) ])


#ktore riadky obsahuju unikatne hodnoty premennych
for col in kategoricke_premenne:
    unikatne_honoty = set(X_all.ix[:last_train, col])
    ktore=ktore + np.array([~(X_all.ix[np.arange(last_train,n_all),col].isin(unikatne_honoty) ) ])
ktore=ktore.reshape((-1,1) )

#boolean vektor, True=vyhodit dany riadok
ktoree=np.concatenate((nuly, ktore))

kolko_vyhod=X_all[ktoree].shape[0]
X_all=X_all[ktoree==False]
y_all=y_all[ktoree==False]
print(X_all.shape)
print(X_all.index[:5])
print(y_all.shape)
print(y_all.index[:5])

#uprav pocet
n_all=len(y_all)
print('kvoli validacii a testovaniu sme vyhodili {} riadkov. zostalo {} riadkov'.format(kolko_vyhod,n_all) )
print("last_train=",last_train)
print("n_train", n_all-last_train+1)

#vytvor datasety
X_train=X_all.ix[:(last_train-1),:]
X_valid=X_all.ix[last_train:,:]

y_train=y_all[:last_train]
y_valid=y_all[last_train:]

print("vytvorili sme (okrem ineho) valid dataset s rozmermi {}  ".format(X_valid.shape))
print("vytvorili sme (okrem ineho) train dataset s rozmermi {}  ".format(X_train.shape))
print("vytvorili sme (okrem ineho) valid y s rozmermi {}  ".format(y_valid.shape))
print("vytvorili sme (okrem ineho) train y s rozmermi {}  ".format(y_train.shape))
print('-------------------------------------------------------------')
print('mozme modelovat!')
print('-------------------------------------------------------------')






#NEURAL NET
import tensorflow as tf
#STLPCE
cont_cols = [tf.contrib.layers.real_valued_column(k) for k in spojite_premenne] #

#https://www.tensorflow.org/versions/r0.11/api_docs/python/contrib.learn.html#DNNRegressor


Xlok_cols = tf.contrib.layers.sparse_column_with_hash_bucket("Xlok", hash_bucket_size=1500)
# other_cat_cols = [tf.contrib.layers.sparse_column_with_hash_bucket(k, hash_bucket_size=8) for k in list(set(kategoricke_premenne) - set('Xlok') )]
#other_cat_cols = [tf.contrib.layers.sparse_column_with_keys(k, keys=list(set(X_train[k]))) for k in list(set(kategoricke_premenne) - set('Xlok') )]
feature_cols = cont_cols+list(Xlok_cols)#+other_cat_cols







#INPUT FN KTORA VRACIA aj target
def input_fn(df):
   continuous_cols = {}
   continuous_cols = {k: tf.constant(df[k].values) for k in spojite_premenne}
   categorical_cols = {k: tf.SparseTensor(
      indices = [[i,0] for i in range (df[k].size) ],
      values = df[k].values,
      shape = [df[k].size,1])
            for k in kategoricke_premenne}
   feature_cols=dict(continuous_cols.items() )
   feature_cols.update(dict(categorical_cols.items()))
   target=tf.constant(y_train.values)
   print("ta spravna input funkcia.")
   return feature_cols, target






def my_input_fn():
    return input_fn(X_train)

print(my_input_fn())
print(len(feature_cols))





regressor = tf.contrib.learn.DNNRegressor(feature_columns=feature_cols, hidden_units=[10, 10]) #[input_fn(X_train),target]
regressor.fit(input_fn=my_input_fn, steps=5000)




