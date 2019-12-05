
# coding: utf-8

# In[56]:


import pandas as pd # imports pandas library to analyze data
foodcoded= pd.read_csv('food_coded.csv',na_values=['Unknown', 'Personal ','3.79 bitch'] ) # reads the csv file and labels the string data as not values 

eating_data = foodcoded[['GPA', 'eating_out']].dropna() # removes the non-valuable data



eating_data.describe() # generate descriptive statistics





# In[57]:


import numpy as np # imports the library as np

from scipy import stats # imports stats from scipy 

mode1 = stats.mode(eating_data['GPA'])[0][0] # finds the mode of GPA in the dataframe from stats
median1 = np.median (eating_data['GPA']) # finds the median of GPA in the dataframe from stats
max1= max (eating_data['GPA'])  # finds the maximum value of GPA
min1= min(eating_data ['GPA']) # finds the minimum value of GPA
range1=max1-min1 # finds the range
std1= np.std(eating_data['GPA']) # finds the standard deviation of GPA from numpy
mean1=np.mean(eating_data['GPA']) # finds the mean of GPA from mean
n= len(eating_data.GPA) # shows the sample size

print('The mode of GPA is:', mode1) 
print('The median of GPA is:', median1)
print('The maximum GPA is:',  max1)
print('The minimum GPA is:', min1)
print('The range of GPA is:', range1)
print('The standard deviation of GPA is:', std1)
print("The mean of the sample is:", mean1)
print('The sample size is:', n)


# In[58]:


import numpy as np # imports stats from scipy

from scipy import stats # imports stats from scipy 

mode2 = stats.mode(eating_data['eating_out'])[0][0] # finds the mode of eating out options in the dataframe from stats
median2 = np.median (eating_data['eating_out']) # finds the median of eating out options in the dataframe from stats
max2= max (eating_data['eating_out']) # finds the maximum value of eating out options
min2= min(eating_data ['eating_out']) # finds the minimum value of eating out options
range2=max2-min2  # finds the range of eating out 

print('The mode of eating out is:', mode2)
print('The median of eating out is:', median2)
print('The maximum eating out is:',  max2)
print('The minimum eating out is:', min2)
print('The range of eating out is:', range2)


# In[68]:


# makes the graph appear within the notebook
get_ipython().run_line_magic('matplotlib', 'inline')
plot=eating_data.GPA.hist() # plots the frequency of GPA
plot.grid(False) # removes the gridlines
plot.set_xlabel('GPA') # names the x-axis as GPA
plot.set_ylabel('Frequency') # names the y-axis as Frequency
plot.set_title('Histogram') # names the Histogram


# In[82]:


# makes the graph appear within the notebook
get_ipython().run_line_magic('matplotlib', 'inline')
plot=eating_data.eating_out.hist(bins=100) # plots the frequency of Eating Out
plot.grid(False)  # removes the gridlines
plot.set_xlabel('Eating Out') # names the x-axis as Eating Out
plot.set_ylabel('Frequency') # names the y-axis as Frequency
plot.set_title('Histogram') # names the Histogram


# In[30]:


df= len(eating_data)-1 # outputs the degrees of freedom
t=stats.t.ppf(0.975, df) # calculates t-score
print(t) 


# In[28]:


SE= std1 / (n)**1/2 # outputs standard error
print(SE)


# In[35]:


CI1= mean1+(SE*t) # outputs upper-bound confidence interval
CI2= mean1-(SE*t) # outputs lower-bound confidence interval
print(CI2, CI1)


# In[70]:


everyday= eating_data[eating_data['eating_out']==5] # filters the sample and assigns it to a variable
mean3= np.mean(everyday['GPA']) # calculates the mean of subset of data
std3= np.std(everyday['GPA']) # calculates the standard deviation of the subset
n3=len(everyday) # assigns the sample size to a variable
print(everyday)
print(mean3)
print(std3)
print(n3)


# In[83]:


# makes the graph appear within the notebook
get_ipython().run_line_magic('matplotlib', 'inline')
plot=everyday.GPA.hist() #plots the GPA frequency from the subset
plot.set_xlabel('GPA') # names the x-axis GPA
plot.set_ylabel('Frequency') # names the y-axis Frequency
plot.set_title('Histogram') # puts a title for the histogram


# In[84]:


not_everyday= eating_data[eating_data['eating_out']!=5] # creates a subset of data 
mean4= np.mean(not_everyday['GPA']) # calculates the mean of the subset
std4= np.std(not_everyday['GPA']) # calculates the standard deviation of the subset
n4=len(not_everyday) # the sample size
print(not_everyday)
print(mean4)
print(std4)
print(n4)


# In[85]:


# makes matplotlib visualization appear inline
get_ipython().run_line_magic('matplotlib', 'inline')
plot=not_everyday.GPA.hist() # plots the gpa frequency of the subset
plot.set_xlabel('GPA') # names x-axis
plot.set_ylabel('Frequency') # names the y-axis
plot.set_title('Histogram') # sets a title to the histogram


# In[89]:


null_value = 0 # sets the null value to 0 as if mean 4 and mean 3 were the same
point_estimate = mean4 - mean3 # point estimate is the difference of means considering that they are not the same
SE1 = (std4**2/n4 + std3**2/n3)**0.5 # calculates the estimates standard error
t_score = (point_estimate - null_value) / SE1 # calculates the t-score
print('t_score:', t_score) 
df = min(n4,n3) - 1 #calulates the degrees of freedom
p_value= stats.t.sf(t_score, df) # calculates the p-value
print('p_value', p_value)


# In[91]:


# Cohen's d tests practical difference
d =  (((n4 - 1)*(std4**2) + (n3-1)*(std3**2))/(n3+n4-2))**0.5
print("Cohen's D", d)
# use hedge's d to correct cohen's d
g = d * (1 -3/(4* (n3 * n4) - 9))
print("Hedge's G",g)

