import numpy as np
import pylab as pl
import pandas as pd
import seaborn as sns

d = pd.read_csv('/home/wei/projects/esl/data/vowel.train.txt')
pl.figure(1)
for x_var in ['x.%d' % i for i in range(1, 11)]:
    pl.hold(True); pl.scatter(d[x_var].values, d['y'].values.astype('f'))
pl.xlabel('X'); pl.ylabel('y');