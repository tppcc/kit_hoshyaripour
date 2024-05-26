import numpy as np
import torch

def Ssurrogate(data, max_c, cut, dtype, subtract_mean=True):

    if subtract_mean:
      diffs = [v - v.mean(axis=0) for v in data]
    else:
      diffs = [np.diff(v, axis=0) for v in data]

    z = np.concatenate(diffs, axis=0)

    u, s, v = np.linalg.svd(z.T, full_matrices=False)

    s[-1*cut:] = 0

    Ssur = u @ np.diag(s)

    return torch.tensor(Ssur, requires_grad=False, dtype = dtype)

#diff = Smatrix_norm_units @ np.linalg.solve(Smatrix_norm_units, new_basis_norm_units) - new_basis_norm_units 
