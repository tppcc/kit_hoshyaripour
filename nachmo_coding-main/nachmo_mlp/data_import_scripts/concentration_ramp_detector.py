# -*- coding: utf-8 -*-
"""
This program finds the rapms in concentrations by sharp gradients. Functions without ramps are smooth and typically (but
not always) have small gradients. Ramps have sharp gradients.
"""

import matplotlib.pyplot as plt
import netCDF4 as nc
import numpy as np


def L2(a, b):
    d = a - b
    L2norm = sum(d * d)
    return L2norm


def my_data_loader(path, hour):
    c = 0

    sigma_level = 0
    # format(time,height,lat,lon)
    for n in range(1, hour):
        if n < 10:
            fn = path + "/NWP_LAM_DOM01_000" + str(n) + ".nc"
        else:
            fn = path + "/NWP_LAM_DOM01_00" + str(n) + ".nc"
        ds = nc.Dataset(fn)
        tmp_oh = ds["OH_full"][:]
        tmp_h2o2 = ds["H2O2_full"][:]
        tmp_ho2 = ds["HO2_full"][:]
        OH_full = tmp_oh.data
        H2O2_full = tmp_h2o2.data
        HO2_full = tmp_ho2.data

        f_oh = np.squeeze(OH_full[:, sigma_level, 13:117, 40:191])
        f_h2o2 = np.squeeze(H2O2_full[:, sigma_level, 13:117, 40:191])
        f_ho2 = np.squeeze(HO2_full[:, sigma_level, 13:117, 40:191])
        c = c + 1
        if c == 1:
            oh = f_oh
            h2o2 = f_h2o2
            ho2 = f_ho2
        else:
            oh = np.append(oh, f_oh, axis=0)
            h2o2 = np.append(h2o2, f_h2o2, axis=0)
            ho2 = np.append(ho2, f_ho2, axis=0)

        return h2o2, oh, ho2


def finding_ramps(h2o2, oh, ho2):
    margin_h2o2 = 5  # criterion of sharpness. The larger values have marings
    margin_oh = 5  # the curves with sharper gradients may pass through the
    margin_ho2 = 5  # gradient filter.

    [time, y, z] = oh.shape
    OH = []
    H2O2 = []
    HO2 = []

    doh = oh[1:, :, :] - oh[0:-1, :, :]  # computing gradients
    dh2o2 = h2o2[1:, :, :] - h2o2[0:-1, :, :]
    dho2 = ho2[1:, :, :] - ho2[0:-1, :, :]

    for i in range(0, y):
        print("y = ", y, "i = ", i)
        for j in range(0, z):
            c = 0
            for k in range(0, time - 2):
                aux1 = doh[k, i, j] / doh[k + 1, i, j]  # getting the gradient sharpness
                aux2 = dh2o2[k, i, j] / dh2o2[k + 1, i, j]
                aux3 = dho2[k, i, j] / dho2[k + 1, i, j]

                if aux1 != 0 and aux2 != 0 and aux2 != 0:
                    #    =========  derivatives have the same sign
                    if np.abs(aux1) < 1 / margin_oh or np.abs(aux1) > margin_oh:
                        c = c + 1
                    if np.abs(aux2) < 1 / margin_h2o2 or np.abs(aux2) > margin_h2o2:
                        c = c + 1
                    if np.abs(aux3) < 1 / margin_ho2 or np.abs(aux3) > margin_ho2:
                        c = c + 1

                    #    =========  derivatives have opposite sign

                    if (aux1 > -1 / margin_oh or aux1 < -margin_oh) and aux1 < 0:
                        c = c + 1
                    if (aux2 > -1 / margin_h2o2 or aux2 < -margin_h2o2) and aux2 < 0:
                        c = c + 1
                    if (aux3 > -1 / margin_ho2 or aux3 < -margin_ho2) and aux2 < 0:
                        c = c + 1

            if (any(oh[:, i, j]) != 0 and any(h2o2[:, i, j]) != 0 and any(ho2[:, i, j]) != 0) == False:
                c = c + 1

            # print('c = ', c)
            if c != 0:
                H2O2 = H2O2 + [(h2o2[:, i, j])]
                HO2 = HO2 + [(ho2[:, i, j])]
                OH = OH + [(oh[:, i, j])]

    H2O2 = np.asarray(H2O2)
    HO2 = np.asarray(HO2)
    OH = np.asarray(OH)
    return H2O2, HO2, OH


print(" Starting comput")

path = "your_path"
hours = 2  # length of the data time sequence

ex1_h2o2, ex1_oh, ex1_ho2 = my_data_loader(path, hours)
ex2_h2o2, ex2_oh, ex2_ho2 = my_data_loader(path, hours)

ex1_H2O2, ex1_HO2, ex1_OH = finding_ramps(ex1_h2o2, ex1_oh, ex1_ho2)
ex2_H2O2, ex2_HO2, ex2_OH = finding_ramps(ex1_h2o2, ex1_oh, ex1_ho2)


lat = 20  # len(OH_full[1,0,:,1])
lon = 20  # len(OH_full[1,0,1,:])


# %%


for i in range(0, 100):
    plt.plot(ex1_HO2[i, :], "b", label="out1")
    plt.plot(ex2_HO2[i, :], "r", label="out2")
    plt.legend(loc="upper left")
    plt.title("h2o2(" + str(i) + ")")
    ax = plt.gca()
    plt.xlabel("time steps")
    plt.xlabel("value")
    plt.show()


# j = 3
# for i in range(0,100):
#     plt.plot(f_h2o2[:,i,j])
#     plt.title(['i= '+str(i)])
#     plt.show()
# for j in range(0,int(y/10)):
#  plt.plot(f_h2o2[:,i,j])
#  plt.plot(f_h2o2[:,i,j])
#  plt.show()

L_max = np.zeros([lat, lon])

# np.save('oh', oh)


# for clat in range(0,lat):
#     print(clat)
#     for clon in range(0,lon):
#         L_aux= np.zeros([lat,lon])
#         print(clon)
#         for i in range(0,lat):
#             for j in range(0,lon):
#                 current_point = h2o2[:,clat,clon]
#                 if i!=clat and j!=clon:
#                     tmp = L2(current_point,h2o2[:,i,j])
#                     if tmp>L_max[i,j]:
#                         L_max[clat,clon]=tmp


# yi = np.argmax(np.ndarray.flatten(L_max))

# fh2o2=(np.ndarray.flatten(h2o2))

# plt.plot()


# for clat in range(1,lat):
#     for clon in range(1,lon):
#         L_max=0
#         for i in range(1,lat):
#             for j in range(1,lon):
#                 current_point = h2o2(clat,clon)
#                 if i!=clat and j!=clon:
#                     L_aux = L2(current_point,h2o2(i,j))
