import torch
from torch.utils.data import TensorDataset
from torch.utils.data import DataLoader
#from chemical_constants_and_parameters import Smatrix


def constants_and_parameters_dataloader(device, Smatrix, Ssur = None, dtype = torch.float32 ):
    S = TensorDataset(torch.tensor(Smatrix,dtype = dtype).to(device))
    epsilon = TensorDataset(torch.tensor([0.000000001],dtype = dtype).to(device))
    Ssur = TensorDataset(torch.tensor(Ssur,dtype = dtype).to(device))
    S_dl = DataLoader(S)
    epsilon_dl = DataLoader(epsilon)
    Ssur_dl = DataLoader(Ssur)

    return [epsilon_dl, S_dl, Ssur_dl]
