#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar  7 08:54:57 2024

@author: andreyvlasenko
"""

import torch
import torch.nn as nn
import torch.nn.functional as F

#from chemical_constants_and_parameters import Stochiometry_matrix


class RolloutModel(nn.Module):
    """This class defines a module that iteratively applies a time stepping model."""

    def __init__(self, stepper, n_steps=1, device="cpu"):
        super(RolloutModel, self).__init__()
        self.stepper, self.n_steps, self.device = stepper, n_steps, device

    def rollout(self, x, n_steps, save_every=1):
        rollout = []
        cells,steps = x.shape
        ddNO2  = 1.
        ddNO3  = torch.zeros(cells)  
        ddN2O5 = torch.zeros(cells)  
        ddHO2  = torch.zeros(cells)  
        counter = 0

        for i in range(n_steps):  
            x_prev= x[:,:].clone().detach()
            #aux = 1.5 * x[:,12] + 0.005 * x[:,7] + 0.05 * x[:,5]
            auxp = 0.8 * x[:,12] + 0.05 * x[:,7] + 0.05 * x[:,5]
            HO2_lim_prev = auxp.clone().detach()
            x = self.stepper(x)
            x[x<0.] = 0
            aux = 0.8 * x[:,12] + 0.1 * x[:,7] + 0.1 * x[:,5]
            HO2_lim = aux.clone().detach()
            #x[x>4.] = 4 



            x[:,5] = torch.where(x[:,5] > x_prev[:,5], x_prev[:,5], x[:,5]) 
            x[:,2] = torch.where(x[:,2] - x_prev[:,2] > x_prev[:,5] -  x[:,5], x_prev[:,2] + (x_prev[:,5] -  x[:,5]),x[:,2])  #  SO2, SO4
            x[:,12] = torch.where(x[:,12] > x_prev[:,12], x_prev[:,12],  x[:,12]) # HCHO depletes only 
            x[:,0] = torch.where(x[:,0] -x_prev[:,0] > x_prev[:,12] - x[:,12] , x_prev[:,0], x[:,0]) # CO, HCHO
            x[:,0] = torch.where(x[:,0] < x_prev[:,0], x_prev[:,0], x[:,0]) # CO builds only
            x[:,7]  = torch.where(x[:,7] > x_prev[:,7], x_prev[:,7], x[:,7] )  # ALD depletes only
            x[:,17]  = torch.where(x[:,17] > x_prev[:,17], x_prev[:,17], x[:,17] )  # NO depletes only

            x[:,15] = torch.where(x[:,15] > HO2_lim, HO2_lim, x[:,15]) # molecule concervation for HO2

            dALD =    x[:,7]  - x_prev[:,7]
            dCO  =    x[:,3]  - x_prev[:,3]
            dO3  =    x[:,13] - x_prev[:,13]
      

            dO3D   =    x[:,4]  - x_prev[:,4]
            dO3P   =    x[:,6]  - x_prev[:,6]
            dNO2   =    x[:,16] - x_prev[:,16]
            dNO3   =    x[:,11] - x_prev[:,11]
            dN2O5  =    x[:,10] - x_prev[:,10]
            dHO2   =    x[:,15] - x_prev[:,15]

            x[:,3]  = torch.where(dCO > - 10*dALD, x_prev[:,3], x[:,3] )  # ALD depletes only, produceing CO2, CO2 does not produce anything
            x[:,13] = torch.where(dO3 < 0., x_prev[:,13], x[:,13] ) # Ozone produces and decades mainly due to photosythes rather than due to  NOx chemistry. Hence, Ozone oncentration may either grow or stay the same 


#            if counter > 0:




 #               aux3 = ddNO3*dNO3
 #               aux3 = torch.where(ddNO3 < 0.,  -1., 0) # checking condition that dNO3>0 and ddNO3 < 0. If satisfied, aux3< 0 and aux > 0 elsewhere.
 #               x[:,11] = torch.where(aux3 < 0, 0, x[:,11] ) # N2O5 is a fast reacting species, however its cocnentration is controlled by NO3. NO3 starts from zero,  


  #              aux4 = ddN2O5*dN2O5
  #              aux4 = torch.where(ddN2O5 < 0.,  -1., 0) # checking condition that dNO3>0 and ddNO3 < 0. If satisfied, aux3< 0 and aux > 0 elsewhere.
  #              x[:,10] = torch.where(aux4 < 0, 0, x[:,10] ) # N2O5 is a fast reacting species, however its cocnentration is controlled by NO3. NO3 starts from zero,  




  #              aux5 = ddHO2*dHO2
  #              aux5 = torch.where(ddHO2 < 0.,  -1., 0) # checking condition that dNO3>0 and ddNO3 < 0. If satisfied, aux3< 0 and aux > 0 elsewhere.
  #              x[:,15] = torch.where(aux5 < 0, 0, x[:,15] ) # N2O5 is a fast reacting species, however its cocnentration is controlled by NO3. NO3 starts from zero,  


          #  x[:,15] = torch.where(aux5 < 0, x_prev[:,15], x[:,15] ) # N2O5 is a fast reacting species, however its cocnentration is controlled by NO3. NO3 starts from zero,  

        #    aux4 = ddN2O5*dN2O5
        #    aux4 = torch.where(ddN2O5 < 0., -1.*aux4, aux4) # checking condition that dNO3>0 and ddNO3 < 0. If satisfied, aux3< 0 and aux > 0 elsewhere.
        #    x[:,10] = torch.where(aux4 < 0, x_prev[:,10], x[:,10] ) # N2O5 is a fast reacting species, however its cocnentration is controlled by NO3. NO3 starts from zero, 




         #   x[:,11] = torch.where(ddNO3 > dNO3, x_prev[:,11], x[:,11] ) # N2O5 is a fast reacting species, however its cocnentration is controlled by NO3. NO3 starts from zero,  


           # x[:,10] = torch.where(dN2O5 > dNO3, x_prev[:,10], x[:,10] ) # N2O5 is a fast reacting species, however its cocnentration is controlled by NO3. NO3 starts from zero,   
            
            #x[:,16] = torch.where( dNO2 - ddNO2 >= 0.0001, x_prev[:,16], x[:,16]) 

            ddNO2  = dNO2
            ddNO3  = dNO3
            ddN2O5 = dN2O5
            ddHO2  = dHO2
            counter = counter + 1

           # x[:,16] = torch.where( dNO2 <= 0. and (x[:,16] > x_prev[:,16]), x_prev[:,16], x[:,16]) 



           # x[:,4] = torch.where(dO3D < 0., x_prev[:,4], x[:,4] ) # The same logics as fro ozone
           # x[:,6] = torch.where(dO3P < 0., x_prev[:,6], x[:,6] ) # The same logics as fro ozone




          #  x[:,3]  = torch.where(x[:,14] < 0.025, x_prev[:,3], x[:,3] )  # ALD depletes only

            #HO2_lim = 1.5 * x[:,12] + 0.005 * x[:,7] + 0.05 * x[:,5] 
            #dx = x -x_prev
            #dHO2_lim  =  HO2_lim - HO2_lim_prev

            #aux2 = x[:,15].clone().detach()
            #aux2 = torch.where(dx[:,15] > 0, dx[:,15], -1000.) 



          # x[:,19]  = torch.where(x[:,19] > 2*x[:,15], x_prev[:,19], x[:,19] )



        #    x[:,15] = torch.where(aux2[:] > dHO2_lim, HO2_lim, x[:,15]) 




         #   x[:,15] = torch.where(((dx[:,15] > 0) and (dx[:,15] > dHO2_lim)), HO2_lim, x[:,15]) # molecule concervation for HO2

         #   x[:,15] = torch.where(x[:,15] > HO2_lim, HO2_lim, x[:,15]) # molecule concervation for HO2

         #   x[:,19] = torch.where(x[:,19] > 2*(x[:,12]+2*x[:,15]), x_prev[:,19], x[:,19]) # molecule concervation for HO2



         #   x[:,15] = torch.where(torch.abs(x[:,15] - x_prev[:,15])> 2*torch.abs(HO2_lim- HO2_lim_prev), x_prev[:,15], x[:,15]) # molecule concervation for HO2


         #   x[:,15] = torch.where(x[:,15] > HO2_lim, HO2_lim, x[:,15]) # molecule concervation for HO2
         #   x[:,19]  = torch.where(x[:,19] > 2*x[:,15], x_prev[:,19], x[:,19] ) 


          # if torch.mean(x[:,7]) > torch.mean(x[:,7]): x[:,0] = x_prev[:,0]  # ALD

           # x[x==torch.nan] = 0. #x_prev
           # x[:,5] = 0.1
           # torch.where( x[:,5] > 1, x[:,5], 0 )
           # torch.where( x[:,2] > 1, x[:,2], 0 )
           # for j in range(0,cells): 
           #     if x[j,5] > 1: z
           #        x[j,5] = 1
           #     if x[j,2] > 1: 
           #        x[j,2] = 1
           #     if x[j,1] > 1: 
           #        x[j,1] = 1
           #     if x[j,3] > 1: 
           #        x[j,3] = 1

 #1*x_prev[j] 
                  # print("condition_satisfied")
            save_every = 10# 6000  # FIXME
            if i % save_every == 0:
                rollout.append(x)
               # print("i = i",i)
        return torch.dstack(rollout)

    def forward(self, x):
        return self.rollout(x, self.n_steps)


def no_grad_stepper(stepper, c_ref, device,n_timepts = 1):
    if n_timepts == 1:
        _, n_timepts, _ = c_ref.shape
    model = RolloutModel(stepper, device=device)
    with torch.no_grad():
        ic = c_ref[:, 0, :]  # initial conditions for each cell, all specie
        model_output = model.rollout(ic, n_timepts - 1)
        model_output = torch.movedim(model_output, 1, 2)
        c = torch.cat((c_ref[:, 0:1, :], model_output), 1)  # rollout including ic
    return c


class ChemicalTimeStepper(nn.Module):
    """This class wraps a NN or other model to do time stepping in a chemical system.

    S - stoichiometry matrix. Should have n_processes rows and nspecies columns (we will calculate rS, not Sr)
    """

    def __init__(
        self,
        net,
        device="cpu",
        learn_updates=False,  # whether to learn changes in concentration for one time step
        learn_rates=False,  # whether to learn reaction rates instead of concentrations or their changes
        stoichiometry_matrix=None,  # each row is a reaction, each column is a chemical species
        species_list=None,
        parameters_loader = None,
        dtype=torch.float32,
    ):
        super(ChemicalTimeStepper, self).__init__()
        if learn_rates:
            assert not learn_updates, "conflicting options"
            assert stoichiometry_matrix is not None, "provide stoichiometry matrix"
            assert parameters_loader is not None, "Set Smatrix and epsilon as dataloader"
        self.device = device
        self.parameters_loader = parameters_loader
        self.Smatrix = parameters_loader[1].dataset.tensors[0]
        self.epsilon = parameters_loader[0].dataset.tensors[0]
        self.net = net
        self.learn_updates = learn_updates
        self.learn_rates = learn_rates
        #self.learn_rates, self.S = learn_rates, Smatrix
        if species_list is not None:
            assert net.in_features == len(species_list)
            self.species_list = species_list
        else:
            self.species_list = [f"species {j}" for j in range(net.in_features)]
        if self.Smatrix is not None:
            print("Size of S = ", self.Smatrix.shape)
            print("S = ", self.Smatrix)
            print("Size of epsilon = ", self.epsilon.shape)
            print("epsilon = ", self.epsilon)
            self.Smatrix = torch.transpose(self.Smatrix, 0, 1)#.to(self.device)

    def forward(self, x):
        y = self.net(x)
        if self.learn_updates:
            y += F.prelu(x,self.epsilon)
        elif self.learn_rates:
            #torch.matmul(y, self.Smatrix)
            # y = x + torch.matmul(y, self.S)
            y = F.prelu(
                x + torch.matmul(y, self.Smatrix),self.epsilon
            )
        # y[:,0] = x[:,0] + dx[:,0]
        # y[:,1] = x[:,1] + dx[:,1]*(1 - torch.sigmoid( a*dx[:,0] + b*dx[:,2]) )    #% torch.sin(0.5*3.1428*torch.exp( -a*dx[:,0]*dx[:,0] - b*dx[:,2]*dx[:,2])))
        #  y[:,2] = x[:,2] + dx[:,2]
        #   print('S = ', self.S)
        return y


class OuterProductLayer(nn.Module):
    """Computes all pairwise products of inputs (including self-products) and appends them to the inputs."""

    def __init__(self, in_features):
        super(OuterProductLayer, self).__init__()
        self.in_features = in_features
        self.triu_indices = torch.triu_indices(in_features, in_features, offset=0)
        self.out_features = int(in_features * (in_features + 3) / 2)

    def forward(self, x):
        if x.ndim == 1:
            x = x[None, :]
        products = x.unsqueeze(1) * x.unsqueeze(2)
        products = products[:, self.triu_indices[0], self.triu_indices[1]]  # don't include both a * b and b * a
        products = products.reshape(x.shape[0], -1)
        return torch.cat((x, products), axis=1)


class MLP(nn.Module):
    """This class implements MLPs in Pytorch of an arbitrary number of hidden layers of potentially different sizes."""

    def __init__(
        self,
        in_features,
        out_features,
        n_hidden=None,
        use_bias=True,
        input_products=False,
        nonlinearity=None,
        activation="ReLU",
        dtype=torch.float32,
        device="cpu",
        depth = None,
        debug = None
    ):
        super(MLP, self).__init__()
        self.in_features = in_features
        self.device = device
        # self.nonlinearity = torch.nn.ReLU() if nonlinearity is None else nonlinearity
        if activation == "ReLU":
            self.nonlinearity = torch.nn.ReLU() if nonlinearity is None else nonlinearity
        if activation == "PReLU":
            self.nonlinearity = torch.nn.PReLU() if nonlinearity is None else nonlinearity
        if activation == "Sigmoid":
            self.nonlinearity = torch.nn.Sigmoid() if nonlinearity is None else nonlinearity
        if n_hidden is None:
            n_hidden = []

        layers = []
        n_prev = in_features
        gate = True
        if input_products:
            layers.append(OuterProductLayer(in_features))
            n_prev = layers[-1].out_features
        c = 0
        for h in n_hidden + [out_features]:
            if gate and c == len(n_hidden):
                h = h * 2
            layers.append(nn.Linear(n_prev, h, bias=use_bias, dtype=dtype))
            n_prev = layers[-1].out_features
            c += 1
        self.layers = torch.nn.ModuleList(layers)
        self.in_feature, self.out_features = in_features, out_features

    def forward(self, x):
        x = x
        for L in self.layers[:-1]:
            x = self.nonlinearity(L(x))
        x = self.layers[-1](x)
        if len(x.shape) == 1:
            rates = x[0 : self.out_features]
            gate_vars = x[self.out_features :]
        if len(x.shape) == 2:
            rates = x[:, 0 : self.out_features]
            gate_vars = x[:, self.out_features :]
        # rates = (1 - torch.sigmoid(rates) )   # BP
        rates = rates * (1 - torch.exp(-gate_vars * gate_vars))
        # rates = torch.log(1+ torch.exp(rates))
        return rates