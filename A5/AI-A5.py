#!/usr/bin/env python
# coding: utf-8

# In[253]:


import nltk
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from nltk.tokenize import word_tokenize
from nltk.stem import PorterStemmer


# ## Defining fields and taking input

# In[254]:


fields = ['common', 'compsci', 'programming', 'ece', 'math', 'ai', 'design', 'ssh',
          'biology', 'sociology', 'psychology', 'economics', 'misc']


# In[255]:


fstems = []
ps = PorterStemmer()
for field in fields:
    stem = ps.stem(field)
    fstems.append(stem)
print(fstems)


# In[256]:


ifile = open('input_text.txt', 'r')
text = ifile.read()
ifile.close()


# In[257]:


tokens = word_tokenize(text)
print(tokens)


# ## Tags

# ### Direct tags

# In[258]:


tags = []


# In[259]:


for token in tokens:
    if token in fields:
        tags.append(token)


# In[260]:


tags


# ### Stem tags

# In[261]:


tstems = []
ps = PorterStemmer()
for token in tokens:
    stem = ps.stem(token)
    tstems.append(stem)
print(tstems)


# In[262]:


for stem in tstems:
    for i in range(len(fields)):
        if(stem == fstems[i]):
            tags.append(fields[i])


# In[263]:


tags


# ### Special cases

# In[264]:


test_fields = ['ai', 'compsci', 'ece', 'ssh', 'misc']
test = ['artificial intelligence', 'computer science', 'electronic', 'social sciences', 'miscellaneous']
test_stems = [ps.stem(word) for word in test]
print(test_stems)


# In[265]:


for i in range(len(test)):
    if(test_stems[i] in text):
        tags.append(test_fields[i])


# In[266]:


tags


# In[ ]:





# ## Stream

# In[267]:


if('stream' in tstems):
    if 'cse' in tstems:
        tags.append('compsci')
    elif 'csb' in tstems:
        tags.append('biology')
    elif 'csd' in tstems:
        tags.append('design')
    elif 'ece' in tstems:
        tags.append('ece')
    elif 'csss' in tstems:
        tags.append('ssh')
    elif 'csam' in tstems:
        tags.append('math')
    elif 'csai' in tstems:
        tags.append('ai')


# In[268]:


tags


# ### Removing duplicate tags

# In[269]:


tags = list(set(tags))
print(tags)


# ## Writing the tags to file

# In[270]:


tagstring = 'taglist(['
for i in range(len(tags)-1):
    tagstring += tags[i] + ', '
tagstring += tags[-1] + ']).'


# In[271]:


print(tagstring)


# In[272]:


f = open('facts.pl', 'w')
f.write(tagstring)
f.close()


# In[ ]:





# In[ ]:




