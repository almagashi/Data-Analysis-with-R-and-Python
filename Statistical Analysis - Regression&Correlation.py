
# coding: utf-8

# In[92]:


import pandas as pd

uni_data= pd.read_csv('cwurData.csv')

uni_data


# In[93]:


import seaborn as sns

sns.heatmap(uni_data.corr(), annot = True)


# In[94]:


data = uni_data[['publications', 'citations', 'influence']].dropna() 
data.describe()


# In[95]:


sns.pairplot(data)


# In[96]:


#Import useful packages
import pandas #package for data analysis
pandas.set_option('max_rows', 10)
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
import matplotlib
get_ipython().run_line_magic('matplotlib', 'inline')
import statsmodels.api as statsmodels #useful stats package with linear regression functions
import seaborn as sns #very nice plotting package
sns.set(color_codes=True) 



# In[97]:


def regression_model(column_x, column_y):
    #this function uses built in library functions to create a scatter plot,
    #plots of the residuals, compute R-squared, and display the regression eqn

    #fit the regression line using "statsmodels" library:
    X = statsmodels.add_constant(data[column_x])
    Y = data[column_y]
    regressionmodel = statsmodels.OLS(Y,X).fit() #OLS stands for "ordinary least squares"

    #extract regression parameters from model, rounded to 3 decimal places:
    Rsquared = round(regressionmodel.rsquared,3)
    slope = round(regressionmodel.params[1],3)
    intercept = round(regressionmodel.params[0],3)

    #make plots:
    sns.set_style("whitegrid")
    fig, (ax1, ax2) = plt.subplots(ncols=2, sharex=True, figsize=(12,4))
    sns.regplot(x=column_x, y=column_y, data=data, marker="+", ax=ax1) #scatter plot
    sns.residplot(x=column_x, y=column_y, data=data, ax=ax2) #residual plot
    ax2.set(ylabel='Residuals')
    ax2.set_ylim(min(regressionmodel.resid)-1,max(regressionmodel.resid)+1)
    plt.figure(figsize=(5.5,4)) #histogram
    sns.distplot(regressionmodel.resid, kde=True, axlabel='Residuals', color='red') #histogram

    #print the results:
    print("R-squared = ",Rsquared)
    print("Regression equation: "+column_y+" = ",slope,"* "+column_x+" + ",intercept)
    


# In[103]:


regression_model('publications', 'influence')
regressionmodel.summary()
  


# In[104]:


def mult_regression(column_x, column_y):
    ''' this function uses built in library functions to construct a linear 
    regression model with potentially multiple predictor variables. It outputs 
    two plots to assess the validity of the model.'''

    #If there is only one predictor variable, plot the regression line
    if len(column_x)==1:
        plt.figure()
        sns.regplot(x=column_x[0], y=column_y, data=data, marker="+",fit_reg=True,color='orange')
    
    #define predictors X and response Y:
    X = data[column_x]
    X = statsmodels.add_constant(X)
    Y = data[column_y]
    
    #construct model:
    global regressionmodel 
    regressionmodel = statsmodels.OLS(Y,X).fit() #OLS stands for "ordinary least squares"

    #residual plot:
    plt.figure()
    residualplot = sns.residplot(x=regressionmodel.predict(), y=regressionmodel.resid, color='green')
    residualplot.set(xlabel='Fitted values for '+column_y, ylabel='Residuals')
    residualplot.set_title('Residuals vs Fitted values',fontweight='bold',fontsize=14)
    
    #QQ plot:
    qqplot = statsmodels.qqplot(regressionmodel.resid,fit=True,line='45')
    qqplot.suptitle("Normal Probability (\"QQ\") Plot for Residuals",fontweight='bold',fontsize=14)
    


# In[84]:


mult_regression (['publications'], 'influence')
regressionmodel.summary()


# In[105]:


print(data['publications'].corr(data['influence']))


# In[106]:


mult_regression (['citations'], 'influence')
regressionmodel.summary()


# In[87]:


print(data['citations'].corr(data['influence']))


# In[107]:


mult_regression (['publications', 'citations'], 'influence')
regressionmodel.summary()


# In[89]:


mult_regression ('publications', 'citations')
regressionmodel.summary()


# In[110]:


from scipy import stats
t= stats.t.ppf(0.975,2218)
t


# In[111]:


stats.t.sf(t,2198)

