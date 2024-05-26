#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Created on Mon Feb 27 13:36:38 2023.

@author: andreyvlasenko
"""
import os
import numpy as np
import shutil
import torch
import timeit
import yaml

from torch.utils.data import random_split
from chemical_constants_and_parameters import Smatrix
from dataset import RolloutTensorDataset
from dataloader import prepare_dataloader
from models import MLP, ChemicalTimeStepper, RolloutModel
from train import Lit_train

from utilities import load_data
import lightning.pytorch as pl

from argparse import ArgumentParser

from lightning.pytorch.loggers import TensorBoardLogger
from lightning.pytorch.callbacks.early_stopping import EarlyStopping

from omegaconf import DictConfig, OmegaConf
import hydra
import tensorboard as tb



#from tensorboard.plugins.hparams import api as hp
import tensorflow as tf
from tensorflow.python.summary.summary_iterator import summary_iterator

dtype = torch.float32
#loss_config, stepper_config, net_config, data_config, loader_config, train_config, experiment_config, visualization_config = default_config()
data_path = '../../concentrations_full/'              # path to the concentration data for training and testing
action = 'save'
model_dir  = 'models/'
model_name = 'NN_test'
lightning = True




@hydra.main(version_base=None, config_path=".", config_name="config")



def main(cfg: DictConfig):
	c = 0
	factor = 1.
	basedir = "tb_logs/"
	list_of_experiments = list_of_files = os.listdir(basedir) 


	    
	for exp in list_of_experiments:

	    current_dir = basedir + '/' + exp + '/version_0/'      
	    list_of_files = os.listdir(current_dir)    #path.basename('exp1/version_0/checkpoints/')
	   # print("current_dir = ", current_dir)
	  #  print(list_of_files) #os.path.splitext(file_name))
	    file_name = [word for word in list_of_files if word.startswith('e')]

	    path_to_file = current_dir + file_name[0] 
	    aux1 = 0
	    aux2 = 0
	    aux3 = 0
	    aux4 = 0
	    for e in summary_iterator(path_to_file):
	        for v in e.summary.value:
	            if v.tag == 'hp/loss':
	                aux1 = v.simple_value
	            if v.tag == 'hp/L1 OH':
	                aux2 = v.simple_value
	            if v.tag == 'hp/L1 HO2':
	                aux3 = v.simple_value
	            if v.tag == 'hp/L1 H2O2':
	                aux4 = v.simple_value


	    #print("v.simpe =========== ", aux2)
	    if aux2< factor and aux3< factor and aux4< factor and aux1<5.e-3:
	        c +=1
	        print("v.simpe =========== ", aux2, aux3, c)
	        order = "cp -r " + basedir + '/' + exp + " sample2/"
	        print("order = ", order)
	        os.system(order)

	#print(file_name)

if __name__ =="__main__":
	main()


