#MOZNO y a X nie su spravne zarovnane (skontroluj ci sedi priradenie X-y)
import pandas as pd
import numpy as np
import tensorflow as tf
import os
from time import time
import subprocess

from loading_RE import DataClass, R2, mRE, sqrtMSE

session_log_name = 'shallow_GPS_lo_LR'
info_freq = 2000
chckpnt = [10000, 25000, 100000] # checkpoints kde sa uklada model
train_perc = 0.90

batch_size = 200
learning_rate = 0.001

# NN
# skryte neurony
num_hidden = [800, 600, 500, 400, 300]
keep_prob = 0.5 # dropout keep prob
keep_prob_last2 = 1 # prob pre posledne dve vrstvy


# NACITANIE DAT
dm = pd.read_csv('dm.csv')
# vyber premenne ktore treba
premenne = ['Xlok', 'Garsonka','PocetIzieb', 'vlast', 'park_garaz', 'vytah', 'rok', 'podlazie', 'stav',
       'konstr', 'vybav', 'stary_rek', 'wp', 'RozlohaIzby', 'balkon_rozl', 'lodzia_rozl', 'terasa_rozl']
y_name = ['dPSQ']
spojite_premenne = ['RozlohaIzby', 'balkon_rozl', 'lodzia_rozl', 'terasa_rozl']
kategoricke_premenne = [x for x in premenne if x not in spojite_premenne]

dm = dm[premenne+y_name]


# shuffle
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

# X, y
X_all=dm[spojite_premenne].join(pd.get_dummies(dm[kategoricke_premenne], prefix=kategoricke_premenne ))
y_all=dm[y_name]

print("rozmery poli")
print("y_shape: ", y_all.shape)
print("X_shape: ", X_all.shape)
n_vars = X_all.shape[1]

n_train = np.floor(len(y_all) * train_perc)
X_train = np.array(X_all.ix[:n_train,:], dtype = 'float32')
y_train = np.array(y_all.ix[:n_train,:], dtype = 'float32')
X_valid = np.array(X_all.ix[n_train:,:], dtype = 'float32')
y_valid = np.array(y_all.ix[n_train:,:], dtype = 'float32')
print(type(y_train))
print("Created train dataset with size {} and target with size {}".format(np.shape(X_train), np.shape(y_train)))
print("Created corresponding validsets with length {}".format(np.shape(y_valid)))


print('mozme modelovat!')
print('-------------------------------------------------------------')



# NNET
graph = tf.Graph()
layers = ['fc'+str(i+1) for i in range(len(num_hidden))]

with graph.as_default():
    tf_dataset = tf.placeholder(tf.float32, shape = (None, n_vars))
    tf_target = tf.placeholder(tf.float32, shape = (None))


    def std_init(n_units_prev, n_units):
        return np.sqrt(6 / (n_vars + num_hidden[0]))

    weights = {
        'fc1': tf.Variable(tf.random_uniform([n_vars , num_hidden[0]], minval = - std_init(n_vars, num_hidden[0]), maxval=std_init(n_vars, num_hidden[0]) )),
        'fc2': tf.Variable(tf.random_uniform([num_hidden[0], num_hidden[1]], minval=- std_init(num_hidden[0], num_hidden[1]),
                              maxval=std_init(num_hidden[0], num_hidden[1]))),
        'fc3': tf.Variable(
            tf.random_uniform([num_hidden[1], num_hidden[2]], minval=- std_init(num_hidden[1], num_hidden[2]),
                              maxval=std_init(num_hidden[1], num_hidden[2]))),
        'fc4': tf.Variable(
            tf.random_uniform([num_hidden[2], num_hidden[3]], minval=- std_init(num_hidden[2], num_hidden[3]),
                              maxval=std_init(num_hidden[2], num_hidden[3]))),
        'fc5': tf.Variable(
            tf.random_uniform([num_hidden[3], num_hidden[4]], minval=- std_init(num_hidden[3], num_hidden[4]),
                              maxval=std_init(num_hidden[3], num_hidden[4]))),
        'out': tf.Variable(tf.random_uniform([num_hidden[-1], 1], minval=- std_init(num_hidden[-1], 1),
                                             maxval=std_init(num_hidden[-1], 1)))
    }


    biases = {
        'fc1': tf.Variable(tf.zeros([num_hidden[0]])),
        'fc2': tf.Variable(tf.zeros([num_hidden[1]])),
        'fc3': tf.Variable(tf.zeros([num_hidden[2]])),
        'fc4': tf.Variable(tf.zeros([num_hidden[3]])),
        'fc5': tf.Variable(tf.zeros([num_hidden[4]])),
        'out': tf.Variable(tf.zeros([1]))
    }
    log = []


    def model(data):
        # input size = n_vars
        log.append('input: ' + str(data.get_shape().as_list()))
        out = data
        # FC1
        out = tf.matmul(out, weights['fc1']) + biases['fc1']
        out = tf.nn.dropout(out, keep_prob)
        out = tf.nn.relu(out)

        # FC2
        out = tf.matmul(out, weights['fc2']) + biases['fc2']
        out = tf.nn.dropout(out, keep_prob)
        out = tf.nn.relu(out)

        # FC3
        out = tf.matmul(out, weights['fc3']) + biases['fc3']
        out = tf.nn.dropout(out, keep_prob)
        out = tf.nn.relu(out)

        # FC4
        out = tf.matmul(out, weights['fc4']) + biases['fc4']
        out = tf.nn.dropout(out, keep_prob_last2)
        out = tf.nn.relu(out)

        # FC5
        out = tf.matmul(out, weights['fc5']) + biases['fc5']
        out = tf.nn.dropout(out, keep_prob_last2)
        out = tf.nn.relu(out)

        # output
        return tf.matmul(out, weights['out']) + biases['out']

    output = model(tf_dataset)
    loss = tf.reduce_mean(tf.square(output - tf_target)) / batch_size
    optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate, beta1=0.9, beta2=0.999, epsilon=1e-08)
    train = optimizer.minimize(loss)

    saver = tf.train.Saver()
    init = tf.initialize_all_variables()


train_data = DataClass(X_train, y_train, batch_size, data_use='train')
valid_data = DataClass(X_valid, y_valid, batch_size, data_use='valid')


with tf.Session(graph=graph) as session:
    step = -1

    # logging
    if not os.path.isdir('logs'):
        os.mkdir('logs')
    filename_ckpt = "logs/{}.ckpt".format(session_log_name)
    filename_txt = "logs/{}.txt".format(session_log_name)
    if os.path.isfile(filename_txt) and os.stat(filename_txt).st_size == 0:
        os.remove(filename_txt)
    if os.path.isfile(filename_txt):
        try:
            saver.restore(session, filename_ckpt)
        except:
            print("You probably have changed the model architecture (or another error occured)."
                  " Please change the 'session_log_name' variable, tooo.")
            session_log_name = input("Type new session_log_name:")
            saver.restore(session, "logs/{}.ckpt".format(session_log_name))
    else:
        session.run(init)

    #docasny krok - loading nefunguje
    step_0 = 0
    # end docasny krok

    (batch_data_valid, batch_targets_valid) = valid_data.next_batch()

    print('-------------------------------------')
    print(' <<-----Training started---------->> ')
    print('-------------------------------------')

    cas = time()
    chckpnt_count = 0
    continue_training = True
    session.run(init)

    while continue_training:
        step += 1

        # training batch
        batch_data, batch_targets = train_data.next_batch()
        feed_dict = {tf_dataset: batch_data, tf_target: batch_targets}
        _, loss_value, predictions = session.run([train, loss, output], feed_dict=feed_dict)

        if step % info_freq == 0:
            # train batch
            print('Minibatch squared loss at step {}: {}'.format(step + step_0, np.sqrt(loss_value)) )
            print('Minibatch R2: ', R2(predictions, batch_targets))
            # valid batch
            valid_prediction = session.run(output, feed_dict={tf_dataset: batch_data_valid})
            print('Validation (batch-sized) squared loss: {}'.format(np.sqrt(loss_value)) )
            print('Validation R2: ', R2(valid_prediction, batch_targets_valid))
            print('-----------------------------------')

        if step in set(chckpnt):
            chckpnt_time = (time() - cas) / 60
            print("{} steps took {} minutes.".format(chckpnt[chckpnt_count], chckpnt_time))
            cas = time()
            subprocess.call(['spd-say', 'Oh yeah! Go Johnny Go, Go! Step {}. Continuing to the next checkpoint. '.format(step + step_0)])

            print("Validation on set size {} began...".format(np.shape(y_valid)))

            results = []
            valid_labels = []
            for offset in range(0, valid_data.total_data_size - batch_size + 1, batch_size):
                data, lab = valid_data.next_batch()
                predict = ((session.run(
                    output,
                    feed_dict={tf_dataset: data}
                )))
                results.append(predict)
                valid_labels.append(lab)

            print('Validation R2 (full) after {} steps, i.e {} epochs: '.format(step, batch_size * step / len(y_train)  ))

            # transf na 1D array
            results = np.array(results)
            valid_labels = np.array(valid_labels)

            results_np = np.zeros(valid_data.total_data_size)
            valid_labels_np = np.zeros(valid_data.total_data_size)
            for i in range(len(results)):
                batch_len = len(results[i])
                for j in range(batch_len):
                    results_np[(i * batch_size + j)] = results[i][j]
                    valid_labels_np[(i * batch_size + j)] = valid_labels[i][j]

            # odstranenie nulovych cien
            dlzka_pred = len(results_np)
            results = results_np[valid_labels_np > 0]
            valid_labels = valid_labels_np[valid_labels_np > 0]
            print(dlzka_pred - len(results), " tolko sme odstranili nulovych valid hodnot :( ")

            # vyhodnotenie
            # print
            print("ukazka head a tail:")
            print(pd.DataFrame({'predicted': results, 'true': valid_labels}).head(10))
            print(pd.DataFrame({'predicted': results, 'true': valid_labels}).tail(150))
            print('Validation accuracy (full) after {} steps: '.format(step + step_0))
            print("R2: ", R2(results, valid_labels))
            print("mRE: ", mRE(results, valid_labels))
            print("sqrtMSE: ", sqrtMSE(results, valid_labels))

            print('batch size: {}'.format(batch_size))
            print('learning rate: {}'.format(learning_rate))
            print('dropout keep: {}, last_2: {}'.format(keep_prob, keep_prob_last2))
            print('-------------------------------------------------')
            print('\n')

            # log to iste
            logfile = open('logs/{}.txt'.format(session_log_name), 'a')
            logfile.write(str(step + step_0) + '\n')
            logfile.write(str(len(log) + 3) + '\n')
            logfile.write('\n'.join(log) + '\n\n\n')

            logfile.write("{} steps took {} minutes.".format(chckpnt[chckpnt_count], chckpnt_time))
            logfile.write('batch size: {}'.format(batch_size) + '\n')
            logfile.write('learning rate: {}'.format(learning_rate) + '\n')
            logfile.write('dropout keep: {}, last_2: {}'.format(keep_prob, keep_prob_last2))
            logfile.write('Validation accuracy (full) after {} steps: '.format(step + step_0) + '\n')
            logfile.write("R2: {}".format(str(R2(results, valid_labels))) + '\n')
            logfile.write("mRE: {}".format(str(mRE(results, valid_labels))) + '\n')
            logfile.write("sqrtMSE: {}".format(str(sqrtMSE(results, valid_labels))) + '\n')
            logfile.write('------------------------------------' + '\n')
            logfile.write('------------------------------------' + '\n')

            logfile.close()

            # uloz model
            save_path = saver.save(session, "logs/{}_chckpnt{}.ckpt".format(session_log_name, chckpnt[chckpnt_count]))
            chckpnt_count += 1

            if(step == chckpnt[-1]):
                continue_training = False


    print('Finito')
