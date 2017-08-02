import seaborn as sns
import pandas as pd
from statsmodels.discrete.discrete_model import Logit
from statsmodels.tools.tools import add_constant

d = pd.read_csv('/home/wei/projects/esl/data/SAheart.data.txt', index_col='row.names')
preds = d.iloc[:, :-1]
preds = add_constant(preds)
preds.famhist = (preds.famhist == 'Present').astype('f')
lm = Logit(d.chd, preds)
f = lm.fit()
print(f.summary())

# generate a scatter plot
sns.pairplot(d)

