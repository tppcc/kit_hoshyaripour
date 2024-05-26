#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import torch
import os
import matplotlib.ticker as ticker

from passmaker import passmaker
from min_max_mean_error import min_max_mean_error
from myticks import myticks

def simple_plotter_2_speceis(c_ref, c_est, species, xticks, passf = None, title = " " , xlabel = None, ylabel = None, namef = None,  fx = 5, fy = 4 ):

  
  counter = 0
  fig, axs = plt.subplots(fx, fy)

  scale = 20/fx*fy

  steps, _  = c_ref[:,:].shape
  print("Steps = ", steps)


  for n in range(0,1):

      for i in range(0,fx):
          for j in range(0,fy):
              
                 
             sub_title= species[counter]
    
                         
             
             if fx == 1:  ax = axs[j]
             if fy == 1:  ax = axs[i] 
             if fx >1 and fy >1 :  ax = axs[i, j]

             ax.plot(c_ref[:,  counter], color = 'r', label = "referece")  
             ax.plot(c_est[:,  counter], color = 'b', label = "estimate")                                       
             ymin, ymax = ax.get_ylim()
             ax.set_title(sub_title, fontsize="10" ,y=.95)


             ax.set_xticks([0,int(steps/2), steps])             
             if j == 0:
                 ax.set_ylabel(ylabel, fontsize="15")
             
             if i == fx-1:
                 ax.set_xlabel(xlabel, fontsize="15")             

                     
               #  ax.set_xticklabels(xticks, fontsize="12")
             else:
                  ax.set_xticklabels(["","", ""], fontsize =1)


             #ax.set_yticks(yticks)
             #ax.set_ylim(bottom=0)
             ax.ticklabel_format(axis='y', useMathText=True)
             counter+=1
             
             
      fig.suptitle(title, fontsize=20) 
      fig.legend(["reference", "estimate"], loc="center right")  
   
      if passf != None:
          passmaker(passf,namef, fig)
      else:
          return fig, ax
      
