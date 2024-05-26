#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Mar 11 11:30:26 2023

@author: andreyvlasenko
"""

import numpy as np


species = ["oh", "ho2", "h2o2"]
path = "../../../concentrations_24/"
path1 = "../../nachmo_raw_data/"
path2 = "../../../concentrations_merge/"
number_of_species = 3


for gas in range(0, number_of_species):
        aux0 = np.load(path  + species[gas] + ".npy")
        aux1 = np.load(path1 + species[gas] + ".npy")
        aux2 = np.concatenate((aux0,aux1),axis = 0)
        np.save(path2 + species[gas] ,aux2)  