import netCDF4 as nc
import numpy as np

# Reads concentraion ncl files
#
# ncl concentration format(time,height,lat,lon)


def L2(a, b):
    d = a - b
    L2norm = sum(d * d)
    return L2norm


c = 0


first_ncl_file = 1
last_ncl_file = 4


for n in range(first_ncl_file, last_ncl_file):
    if n < 10:
        fn = "rand_init/NWP_LAM_DOM01_000" + str(n) + ".nc"  # change path and name of the file if needed
    else:
        fn = "rand_init/NWP_LAM_DOM01_00" + str(n) + ".nc"
    ds = nc.Dataset(fn)
    tmp_oh = ds["OH_full"][:]
    tmp_h2o2 = ds["H2O2_full"][:]
    tmp_ho2 = ds["HO2_full"][:]
    OH_full = tmp_oh.data
    H2O2_full = tmp_h2o2.data
    HO2_full = tmp_ho2.data

    f_oh = np.squeeze(OH_full[:, 0, 13:117, 40:191])  # we do small offset from the bounda-
    f_h2o2 = np.squeeze(H2O2_full[:, 0, 13:117, 40:191])  # ries to remove cells with zero va-
    f_ho2 = np.squeeze(HO2_full[:, 0, 13:117, 40:191])  # lues
    c = c + 1
    if c == 1:
        oh = f_oh
        h2o2 = f_h2o2
        ho2 = f_ho2
    else:
        oh = np.append(oh, f_oh, axis=0)
        h2o2 = np.append(h2o2, f_h2o2, axis=0)
        ho2 = np.append(ho2, f_ho2, axis=0)
