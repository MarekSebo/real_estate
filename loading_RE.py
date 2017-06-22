import numpy as np
import os
import pandas as pd
import tensorflow as tf
from numpy import random


class DataClass(object):
    
    def __init__(self, X, y, batch_size, data_use = "train"):
        self.data = X
        self.targets = y

        self.data_use = data_use
        self.total_data_size = len(y)
        self.batch_size = batch_size
        self.batch_cursor = 0              # pozicia batchu v ramci datasetu
        self.data_size = np.floor(55523 * 0.9)

    def next_batch(self):
        data = self.data[self.batch_cursor:self.batch_cursor + self.batch_size]
        targets = self.targets[self.batch_cursor:self.batch_cursor + self.batch_size]

        self.batch_cursor += self.batch_size
        if self.batch_cursor + self.batch_size > self.data_size:
            self.batch_cursor = 0

        if len(targets) < self.batch_size:
            self.next_batch()

        return data, targets


def R2(predicted_y, real_y):
    if (predicted_y.shape != real_y.shape):
        print ("Error in accuracy function: šejpy nešejpujú!: predicted_y_shape: {}, real_y_shape: {} ".format(
            predicted_y.shape, real_y.shape))
        return
    n_vzorka = np.shape(real_y)[0]
    residuals = (real_y - predicted_y)  # vektor rezidui
    res_sq = np.array(residuals)** 2
    RSS = res_sq.sum()
    TSS = np.sum(np.array(real_y - np.mean(real_y)) ** 2)
    return np.round(100 * (1 - RSS / TSS),decimals=2)  # prikaz model_moj.score(testX, testY)

def mRE(predicted_y, real_y):
    rel_residuals = (predicted_y-real_y)/real_y
    return np.round(np.median(abs(rel_residuals) )*100, decimals=2)

def avg_over(predicted_y, real_y):
    rel_residuals = (predicted_y - real_y) / real_y
    return np.round(np.mean(rel_residuals)*100, decimals = 2)

def sqrtMSE(predicted_y, real_y):
    rel_residuals = (predicted_y - real_y) / real_y
    return np.round(np.sqrt(np.sum(np.array(rel_residuals)**2) / len(real_y) ) * 100, decimals=2)


