
# coding: utf-8

# In[1]:

import pandas as pd
import numpy as np
from sklearn.svm import SVR
import matplotlib.pyplot as plt


# In[3]:

Ydata = pd.read_csv("E:/Datathon/trip_year_zip.csv",delimiter=',',header = 0)


# In[4]:

Ydata = Ydata.values


# In[6]:

from sklearn import *
Y = Ydata[:,2]
Y = preprocessing.scale(Y)


# In[7]:

PrueX = pd.read_csv("E:/Datathon/demographics.csv",delimiter=',',header = 0)


# In[8]:

PrueX = PrueX.values


# In[9]:

from sklearn import preprocessing
X = []
for i in range(len(Y)):
    for j in range(len(PrueX)):
        if PrueX[j,0] == Ydata[i,0] and PrueX[j,1] == Ydata[i,1]:
            X.append(PrueX[j,2:])
            break
        


# In[11]:

X = np.round(X,decimals=2)
X = preprocessing.scale(X)


# In[39]:

svr_rbf = SVR(kernel='rbf', C=1e3, gamma=0.1)
svr_lin = SVR(kernel='linear', C=1e3)
svr_poly = SVR(kernel='poly', C=1e3, degree=2)
y_rbf = svr_rbf.fit(X[0:19], Y[0:19])
#y_lin = svr_lin.fit(X, Y[0:144])
#y_poly = svr_poly.fit(X, Y[0:144])


# In[51]:

y_rbf = svr_rbf.fit(X, Y[:144])


# In[15]:

PredictX = pd.read_csv("E:/Datathon/demo_2015.csv",delimiter=',',header = 0)


# In[16]:

prex = PredictX.ix[0:35,2:]
prex = preprocessing.scale(prex)


# In[59]:

prex = preprocessing.scale(prex)
predictions = svr_rbf.predict(prex)


# In[19]:

print("Computing regularization path using the LARS ...")
alphas, _, coefs = linear_model.lars_path(X, Y[:144], method='lasso', verbose=True)

xx = np.sum(np.abs(coefs.T), axis=1)
xx /= xx[-1]

plt.plot(xx, coefs.T)
ymin, ymax = plt.ylim()
plt.vlines(xx, ymin, ymax, linestyle='dashed')
plt.xlabel('|coef| / max|coef|')
plt.ylabel('Coefficients')
plt.title('LASSO Path')
plt.axis('tight')
plt.show()


# In[22]:

model_bic = LassoLarsIC(criterion='bic')
t1 = time.time()
model_bic.fit(X, Y[:144])
t_bic = time.time() - t1
alpha_bic_ = model_bic.alpha_

from sklearn import linear_model
model_aic = LassoLarsIC(criterion='aic')
model_aic.fit(X, Y[:144])
alpha_aic_ = model_aic.alpha_


def plot_ic_criterion(model, name, color):
    alpha_ = model.alpha_
    alphas_ = model.alphas_
    criterion_ = model.criterion_
    plt.plot(-np.log10(alphas_), criterion_, '--', color=color,
             linewidth=3, label='%s criterion' % name)
    plt.axvline(-np.log10(alpha_), color=color, linewidth=3,
                label='alpha: %s estimate' % name)
    plt.xlabel('-log(alpha)')
    plt.ylabel('criterion')

plt.figure()
plot_ic_criterion(model_aic, 'AIC', 'b')
plot_ic_criterion(model_bic, 'BIC', 'r')
plt.legend()
plt.title('Information-criterion for model selection (training time %.3fs)'
          % t_bic)
plt.show()


# In[128]:

alpha_bic_


# In[35]:

import sklearn
myModel = sklearn.linear_model.Lasso(alpha=0.00045)
myModel.fit(X,Y[:144])


# In[36]:

myModel.coef_


# In[37]:

prex = preprocessing.scale(prex)
lassopre = myModel.predict(prex)


# In[43]:

plt.plot(lassopre-Y[-36:])
print(np.mean(lassopre-Y[-36:]))
plt.show()


# In[45]:

PredictX = pd.read_csv("E:/Datathon/demo_2015_2020.csv",delimiter=',',header = 0)


# In[50]:

prex = PredictX.ix[0:,2:]
prex = preprocessing.scale(prex)

