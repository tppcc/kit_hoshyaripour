#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 27 15:33:36 2023

@author: andreyvlasenko
"""


#import netCDF4 as nc
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import pandas as pd





OH   = []
H2O2 = []
HO2  = []

depth = 240001  #60001
l = 1

for i in range(0,7):
    
    prefix = str(i)
    #path = '../Data/npy_long/' #   '../nachmo_raw_data/Giorgio/npy/'
    path = '../../../../NACHMO_data/npy/'
        
    th2o2 = np.load(path + 'h2o2_'+prefix+'.npy') 
    tho2 = np.load(path + 'ho2_'+prefix+'.npy')
    toh = np.load(path + 'oh_'+prefix+'.npy')
   # depth = len(toh)
    print('depth = ', depth)
    if i ==0:
        oh   =   toh[:,0:depth:l]
        h2o2 =   th2o2[:,0:depth:l]
        ho2  =   tho2[:,0:depth:l]
    else:
        oh =     np.append(   oh,   toh[:,:depth:l],   axis=0)
        ho2 =    np.append(   ho2,  tho2[:,0:depth:l],  axis=0)
        h2o2 =   np.append(   h2o2, th2o2[:,0:depth:l], axis=0)
    # h2o2 = np.transpose(h2o2)
    # oh = np.transpose(oh)
    # ho2 = np.transpose(ho2)
    
#r = np.random.permutation(oh.shape[0])
# ho2  = ho2[r,:]
# h2o2 = h2o2[r,:]
# oh   = oh[r,:]

print('Data merged')
    
      
path  = '../../../concentrations_full/'    
#path  = '../../../concentrations_val/' 

np.save(path + 'h2o2',h2o2) 
np.save(path + 'ho2',ho2)
np.save(path + 'oh',oh)
print("Data Saved")



