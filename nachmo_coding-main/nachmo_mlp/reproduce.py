#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Created on Mon Feb 27 13:36:38 2023.

@author: andreyvlasenko
"""
import os
import timeit

import hydra
import lightning.pytorch as pl
import torch
import numpy as np

from chemical_constants_and_parameters import S_oh, S_verwer
from constansts_and_parameters_loader import constants_and_parameters_dataloader
from dataloader import prepare_dataloader
from dataset import RolloutTensorDataset
from models import ChemicalTimeStepper, MLP, RolloutModel, no_grad_stepper
from omegaconf import DictConfig
from torch.utils.data import random_split
from utilities import load_data, load_data_reproduce



dtype = torch.float32


path_to_data = "/work/gg0302/g260141/NACHMO_data/data_for_paper/Verwer_paper/Same_distribution/"
current_epoch = 41
PATH = path_to_data + "model" + str(current_epoch) + ".pt"

@hydra.main(version_base=None, config_path=path_to_data, config_name="hparams" + str(current_epoch))
def main(cfg: DictConfig):

    pl.seed_everything(2024)

#   Loading configs   

    stepper_config = cfg.stepper_config
    net_config = cfg.net_config
    data_config = cfg.data_config
    loader_config = cfg.loader_config

#   Set computing device as cpu (no gpu so far)  

    net_config.device = 'cpu'   #FIXME

#   Selector for the type of c hemical scheme we want to compute. It just sets the number of reacting speceis 

    if data_config.scheme == "Verwer":
        data_config.species = data_config.species_verwer
        cfg.data_config.species = data_config.species_verwer
        Smatrix = S_verwer
        nrates=20
         
    if data_config.scheme == "OH":
        data_config.species = data_config.species_oh
        cfg.data_config.species = data_config.species_oh
        Smatrix = S_oh
        nrates = 4

#   Set the amount of inputs and outputs in the NN
    in_features = len(data_config["species"])
    out_features = nrates if stepper_config["learn_rates"] else in_features  


#   Load the data, ref is the reference data, max_c are the normalization coefficients 

    ref, max_c = load_data_reproduce(path_to_data, dtype=dtype, species=data_config["species"])


#   Normalize Stoichiometry matrix 
    for i in range(len(Smatrix)):
        Smatrix[i, :] = Smatrix[i, :] / max_c[i] 


#   Distributing some constants and Smatrix to devices for estimates on gpu (not needed so far, this is for future, when gpu is enabled)
    parameters_loader = constants_and_parameters_dataloader(net_config.device, Smatrix)

#   Constructing the NN
    net = MLP(in_features, out_features, dtype=dtype, **net_config)
    stepper = ChemicalTimeStepper(
        net, device=net_config.device, species_list=data_config["species"], parameters_loader = parameters_loader, **stepper_config)
    model = RolloutModel(stepper, data_config["trajectory_length"], net_config.device)

#    for var_name in stepper.state_dict():
#        print(var_name, "\t", stepper.state_dict()[var_name])


#   Instantinate (load weights and biased) the NN
    model.load_state_dict(torch.load(PATH, map_location=torch.device('cpu') ) )


    # Compute your concentrations with a NN
    y = no_grad_stepper(model.stepper, ref, net_config.device, n_timepts = 10) # n_timepts is the ammount of timesteps to compute


#   Save your results
    np.save("Y" , y)
    np.save("Y_ref_" , ref)

if __name__ == "__main__":
    main()
