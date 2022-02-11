# Akira Nair
# Singh Lab
# January 2, 2022
# Practice Model
# This file constructs a basic deep learning model involving
# a few variables from the clinical tab dataset

# import packages
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import glob
import requests
import json
from tensorflow import keras
#import os
#os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'
import sklearn as sk
#import tensorflow as tf

# use pandas' read_csv function to import tsv files
clinicaldf = pd.read_csv("Data/DraftData/clinical data/nationwidechildrens.org_clinical_patient_luad.txt", sep = "\t")
df = clinicaldf[['bcr_patient_uuid', 'vital_status', 'tobacco_smoking_history_indicator']]
print(clinicaldf.columns)
df = df.drop([0, 1])

# convert vital status outcome into binary classification labels
from sklearn.preprocessing import LabelEncoder
le = LabelEncoder()
Y = le.fit_transform(df['vital_status'])
print("Labels after: ", np.unique(Y))

# choose variables from dataset
X = df.drop(labels = ['bcr_patient_uuid', 'vital_status'], axis=1).convert_dtypes()
X['tobacco_smoking_history_indicator'] = X['tobacco_smoking_history_indicator'].astype('category').cat.codes
#https://stackoverflow.com/questions/32011359/convert-categorical-data-in-pandas-dataframe#:~:text=First%2C%20to%20convert%20a%20Categorical,in%20a%20dataframe%20using%20select_dtypes%20.

#print(X)
#X['tobacco_smoking_pack_years_smoked'] = X['tobacco_smoking_pack_years_smoked'].astype('float')

from sklearn.model_selection import train_test_split
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size = 0.2)
print(X_train.shape)

from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation, Dropout
from tensorflow.keras.callbacks import EarlyStopping
from sklearn.metrics import confusion_matrix

# develop model architecture
model = Sequential()
model.reset_states()
#model._reset_compile_cache()
model.reset_metrics()
model.add(Dense(3, input_dim = len(X.columns), activation = 'relu'))
model.add(Dropout(0.1))
model.add(Dense(1))
model.add(Activation('sigmoid'))
model.compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = ['accuracy'])
print(model.summary())

# develop an early stop 
early_stopping_monitor = EarlyStopping(patience=3)
# fit model to data
devo = model.fit(X_train, Y_train, verbose = 1, epochs = 10, callbacks=[early_stopping_monitor],batch_size = 4, validation_data = (X_test, Y_test))
#apply model to test data set
Y_predictions = (model.predict(X_test) > 0.5)
# generate a confusion matrix 
cm = confusion_matrix(Y_test, Y_predictions)
sns.heatmap(cm, annot=True)
plt.show()