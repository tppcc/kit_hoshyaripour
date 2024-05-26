#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Created on Thu Mar 30 15:00:28 2023.

@author: andreyvlasenko
"""
import torch


def loss_fun(yhat, y, penalty_factor=0.0, first_step=1):
#    yn = torch.min(torch.abs(y[:, :, first_step - 1 :-2] - y[:, :, first_step :]))
    yhat, y = yhat[:, :, first_step - 1 :], y[:, :, first_step - 1 :]
    residual = yhat - y
    _, nspecs,steps= y.shape
#    if len(y.shape) == 3:
#        aux = y[:, :, :-1] - y[:, :, 1 :]
#        yn = torch.zeros(nspecs)
#        yn = torch.max(torch.max(torch.abs(aux),dim=2 ).values,dim =0).values
#        for i in range(0,nspecs):
#            residual[:,i,:] = residual[:,i,:]/torch.sqrt(yn[i])
#       # print("yn = ", yn)
#    else:
#        yn = 1.
   

    L = (residual * residual).mean()
    positivity_violation = -torch.clamp(yhat, max=0.0).mean()
    return L + positivity_violation * penalty_factor
