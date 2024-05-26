#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Created on Wed Dec 21 12:08:45 2022.

@author: andrey

This code contains nonlinear perceptron emulating stiff system of chemical kinetic equations.
The reacting gases are h2o2, ho2 and oh


Some pieces of code are taken from https://deeplearning.neuromatch.io/tutorials
"""

import numpy as np
import torch
import os


def load_data(path, dtype=torch.float32, species=None, **kwargs):
    """Load data from data files for each chemical species.

    returned data has size (n_trajectories, n_timepts, n_species)
    """
    c = [np.load(os.path.join(path, s) + ".npy") for s in species]  # load chemical concentrations
    for d in c:
        assert d.ndim == 2
    max_c = [d.max() for d in c]
    c_norm = np.stack([c / m for c, m in zip(c, max_c)], axis=2)
    c_norm = torch.tensor(c_norm, dtype=dtype)

    return c_norm, max_c


def load_data_reproduce(path, dtype=torch.float32, species=None, **kwargs):
    """Load data from data files for each chemical species.

    returned data has size (n_trajectories, n_timepts, n_species)
    """
    #c = [np.load(path + s + ".npy") for s in species]  # load chemical concentrations

    c = []
    counter  = 0
    print("c.shape", np.asarray(c).shape)
    max_c = np.load(path + "max_c.npy")  # load chemical concentrations

    for s in species:
        c= c+ [np.load(path + s + ".npy")/max_c[counter]] 
        print("s = ", s)
        counter = counter + 1


    for d in c:
        assert d.ndim == 2
  #  max_c = [d.max() for d in c]
    c_norm = np.stack([c for c, m in zip(c, max_c)], axis=2)
    c_norm = torch.tensor(c_norm, dtype=dtype)

    return c_norm, max_c


def batched_max(x, batch_size=1024):
    """
    Load data in batches and calculate max over first two dimensions.

    Input should be a numpy array or a list of numpy arrays,
    which should have the same sizes on the 3rd and higher dims.
    """
    if isinstance(x, list):
        return np.stack([batched_max(v, batch_size=batch_size) for v in x], axis=0).max(axis=0)
    else:
        assert isinstance(x, np.ndarray)

    n_batches = x.shape[0] // batch_size
    if batch_size * n_batches < x.shape[0]:
        n_batches += 1

    m = np.full(x.shape[2:], -np.inf)
    for i in range(n_batches):
        b = x[batch_size * i:batch_size * (i + 1)].reshape(x.shape[0] * x.shape[1], *x.shape[2:])
        m = np.maximum(m, np.max(b, axis=0))
    return m
