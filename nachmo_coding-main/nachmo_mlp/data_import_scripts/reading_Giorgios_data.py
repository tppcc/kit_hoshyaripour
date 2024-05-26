#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 20 17:29:02 2023

@author: g260141
"""

#import netCDF4 as nc
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation



def loading_txt(path,depth):

    c = 0
    
    sigma_level = 0
      # format(time,height,lat,lon)
    for n in range(1,depth):
        if n<10:
            fn = path + '00'+ str(n) + '.txt'
        elif n>9 and n<100:
            fn = path + '0' + str(n) + '.txt'
        else:
            fn = path + str(n) + '.txt'
            
        with open(fn, encoding='ASCII') as f:
            contents = f.readlines()
            
        length =  len(contents) 
        l = length
        data = np.zeros([3,length]) 
        if c == 0: 
            oh   = np.zeros([depth-1,length])
            h2o2 = np.zeros([depth-1,length])
            ho2  = np.zeros([depth-1,length])
        print('n = ', n)
        #contents = np.asarray(contents)
        for i in range(0, len(contents)):
            contents[i] = contents[i].strip()
            if contents[i][24] == '-':
                data[0,i] = float(0)
            else:
                data[0,i] = float(contents[i][24:46])
            if contents[i][48] == '-' or contents[i][49] == '-':
                data[1,i] = float(0)
            else:
                data[1,i] = float(contents[i][48:70])
            if contents[i][72] == '-' or contents[i][73] == '-' or contents[i][74] == '-':
                data[2,i] = float(0)
            else:
                data[2,i] = float(contents[i][72:94])   
    
        oh[c,:]     = data[0,:]
        h2o2[c,:]   = data[1,:]
        ho2[c,:]    = data[2,:]
        c = c+1
        # if c == 1:
        #     oh   = data[0,:l]
        #     oh   = np.asarray(oh[:, None])
        #     h2o2 = data[1,:l]
        #     h2o2   = np.asarray(h2o2[:, None])
        #     ho2  = data[2,:l]
        #     ho2   = np.asarray(ho2[:, None])
        # else:
        #     print('oh.shape = ', oh.shape)
        #     oh = np.append(oh,data[0,:l], axis=1)
        #     h2o2 = np.append(h2o2,data[1,:l], axis=1)
        #     ho2 = np.append(ho2,data[2,:l], axis=1)
            
    return h2o2, oh, ho2
    

OH   = []
H2O2 = []
HO2  = []

path = '../../nachmo_raw_data/Giorgio/mod_txt/dynho_'
h2o2, oh, ho2 = loading_txt(path,150)
path  = '../../../concentrations_giorg/'
np.save(path + 'h2o2',h2o2)
np.save(path + 'ho2',ho2)
np.save(path + 'oh',oh)

# with open(path, encoding='ASCII') as f:
#     contents = f.readlines()
    
# data = np.zeros([len(contents),3])        
# #contents = np.asarray(contents)
# for i in range(0, len(contents)):
#     contents[i] = contents[i].strip()
#     if contents[i][24] == '-':
#         data[i,0] = float(0)
#     else:
#         data[i,0] = float(contents[i][24:46])
#     if contents[i][48] == '-' or contents[i][49] == '-':
#         data[i,1] = float(0)
#     else:
#         data[i,1] = float(contents[i][48:70])
#     if contents[i][72] == '-' or contents[i][73] == '-' or contents[i][74] == '-':
#         data[i,2] = float(0)
#     else:
#         data[i,2] = float(contents[i][72:94])   
    




