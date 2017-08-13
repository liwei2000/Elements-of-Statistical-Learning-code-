import numpy as np
import seaborn as sns
import pandas as pd
import pylab as pl
from statsmodels.discrete.discrete_model import Logit
from statsmodels.tools.tools import add_constant
# from sklearn.linear_model import LogisticRegression

d = pd.read_csv('/home/wei/projects/esl/data/SAheart.data.txt', index_col='row.names')
preds = d.iloc[:, :-1]
preds = add_constant(preds)
preds.famhist = (preds.famhist == 'Present').astype('f')
lm = Logit(d.chd, preds)
f = lm.fit(disp=False)
print("Running logistic regression of SouthAfrica Heart data:")
print(f.summary())

# generate a scatter plot
sns.pairplot(d)

# use L1 penalization
fr = lm.fit_regularized(method='l1', disp=False, alpha=0.1)
print("Running logistic regression of SouthAfrica Heart data with L1 constraint:")
print(fr.summary())

# vary the L1 penalty
N = 30
alps = np.logspace(0, 4, N)
b = []
for i, alpha in enumerate(alps):
    fr = lm.fit_regularized(method='l1', disp=False, alpha=alpha)
    b.append(fr.params)

b = pd.DataFrame(b, index=alps)
pl.figure(1)
b.plot(title='regularized regression beta')

# use
b = b.set_index(np.nansum(b.abs(), axis=1)).iloc[::-1, 1:]
pl.figure(2)
b.plot(logx=True, title='regularized regression beta')