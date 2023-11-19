import xarray as xr
import numpy as np
import pandas as pd
import os
import argparse

'''
Function        :   ifs2icon_vert
Description     :   This function Interpolates the IFS/CAMS model level output to ICON Model level file, 
                    note that horizontal interpolation is not done here, see ICON TOOLS from DWD for horizontal interpolation.
                    The vertical interpolation is done via cdo (climate data operator) by MPI.
Parameter       :   fname_tq(str)                       - File name containing the model level variables, ['t', 'q']
                                                          is referenced to compute the GPH on model levels
                    fname_var(str)                      - File name containing variable to be interpolated
                    fname_surface(str)                  - File name containing Surface Pressure & Surface Geopotential ['sp', 'z']
                    fname_ifs_model_level(str)          - File name of the IFS model level description csv [e.g. L90, L137]
                                                          export from https://confluence.ecmwf.int/display/UDOC/L137+model+level+definitions
                                                          remove the unit from header before passing the csv
                    fname_icon_model_level(str)         - File name of the ICON model level [typically "HHL_${icon_grid}_l${nlevp1}.grb2"]
                                                          export from https://confluence.ecmwf.int/display/UDOC/L137+model+level+definitions,
                                                          remove unit from header
                    target_level(int)                   - Number of target levels, must match the icon_model_level
                    fname_ifs_hgrid                     - File name of the IFS horizontal grid used for horizontal interpolation
                    fname_icon_hgrid                    - File name of the ICON horizontal grid used for horizontal interpolation
                    output_grid_name                    - Name of the target ICON horizontal grid, will be used to name the final file
                    var_nametable (str)                 - csv of variable names to be interpolated, can be infered by 'cdo showname ${fname_var}', header: source_name, target_name; this allows the CAMS/IFS variable name to be translated directly to ICON
Note            :   The file of Variable, surface and tq must contain only ONE timestep
Usage           :   Call the program by 
                    python ifs2icon_vert.py <fname_tq> <fname_var> <fname_surface> <ifs_model_level> 
                    <icon_model_level> <target_level> <var_name1> <var_name2> ...
                    Replace <fname_tq>, <fname_var>, <fname_surface>, <ifs_model_level>, <icon_model_level>, 
                    <target_level>, <var_nametable>, with the actual values you want to pass to the class constructor.

for testing purposes
/Users/corneliustai/Desktop/hiwi/test_ifs2icon
python ifs2icon_vert.py tq_field.nc o3_field.nc surface_parameter.nc ifs_model_l60.csv HHL_0024_R02B06_G_l91.grb2 91 go3

cd /work/bb1070/b380982/CAMS
python ~/ifs2icon_vert.py tq_field.nc o3_field.nc surface_parameter.nc ifs_model_l60.csv HHL_0024_R02B06_G_l91.grb2 91 hno3


'''

class ifs2icon_vert:
    def __init__(self, fname_tq, fname_var, fname_surface, fname_ifs_model_level, fname_icon_model_level, target_level, fname_ifs_hgrid, fname_icon_hgrid, output_grid_name, var_nametable):         # Declare some global variables
        var_nametable = pd.read_csv(var_nametable)
        self.cwd = os.getcwd()
        self.ifs_model_level = ifs_model_level
        self.fname_tq = fname_tq
        self.fname_var = fname_var
        self.fname_surface = fname_surface
        self.ifs_model_level = fname_ifs_model_level
        self.icon_model_level = fname_icon_model_level
        self.target_level = target_level
        self.source_namelist = var_nametable.source_name
        self.target_namelist = var_nametable.target_name
        self.fname_ifs_hgrid = fname_ifs_hgrid
        self.fname_icon_hgrid = fname_icon_hgrid
        self.output_grid_name = output_grid_name

                self.r = 287.06         #Gas constant
        self.amd = 28.97        #Molar weight of dry air
        self.fname_var_hinterpolated = os.path.basename(self.fname_var)     #Change the name to target_%s
        output_grid_name, _ = os.path.splitext(os.basename(fname_var))
        self.fname_var_hinterpolated= "%s/remap_%s_%s" % (self.cwd, output_grid_name,self.fname_var_hinterpolated)

    def main(self):
        self.hi_variable()
        self.hi_parameters()
        z_full = self.gp_ml()
        self.fname_z_full = os.path.basename(self.fname_tq)
        self.fname_z_full = "%s/geopotential_%s" % (self.cwd, self.fname_z_full)
        #Write attribute and convert to netCDF
        variable_attrs = {
            'long_name': 'geopotential',
            'standard_name': 'geopotential',
            'units': 'm^2s^-2',
        }
        # Create the new xarray dataset
        z_dataset = xr.Dataset(data_vars={'z': z_full})
        z_dataset['z'].attrs = variable_attrs
        z_dataset = z_dataset.transpose("time", "level", "latitude", "longitude")       #Match CF-convention (time, height, lat, lon)
        z_dataset.to_netcdf(self.fname_z_full, format = "NETCDF4")
        self.interpolation()

    def pres_ml(self):      # Compute the Pressure at half levels, return p_half and sz (surface geopotential)
        sp, sz = self.get_surface_parameter()
        n, a, b = self.get_pressure_parameter()
        p_half = sp.expand_dims(dim={"level": n}).copy()
        for level in n:
            p_half.loc[dict(level = level)] = a[level-1] +  p_half.loc[dict(level = level)] * b[level-1]
        return p_half, sz

    def get_surface_parameter(self):        # Get surface Pressure and Geopotential Field
        da = xr.open_dataset(self.fname_surface_hinterpolated)
        sp, sz = da.sp, da.z
        return sp, sz

    def get_pressure_parameter(self):      # Get the n, a,b parameter for computing pressure on ML
        model_level_parameter = pd.read_csv(self.ifs_model_level, delimiter=",", skiprows=[1], usecols=[0,1,2])
        n, a, b = model_level_parameter.n, model_level_parameter.a, model_level_parameter.b
        return n, a, b

    def get_meteorology_parameter(self):        # Get ['t', 'q'] data array
        da = xr.open_dataset(self.fname_tq_hinterpolated)
        t, q = da.t, da.q
        return t, q

    def gp_ml(self):       # Compute GP on model levels via stepwise integration
        # Integration upwards from lowest level to level 1
        p_half, sz = self.pres_ml()
        t, q = self.get_meteorology_parameter()
        moist_t = t * (1. + 0.609133 * q)

        z_full, z_half = xr.zeros_like(p_half), xr.zeros_like(p_half)           # Define an 0s array as place holder for geopotential level

        for level in [p_half.level.values[-1]]:      #Lowest level, defined from boundary condition
            dlog_p = np.log(p_half.loc[dict(level=level)]/ p_half.loc[dict(level=level - 1)])
            alpha = 1. - ((p_half.loc[dict(level=level - 1)] / (p_half.loc[dict(level=level)] - p_half.loc[dict(level=level - 1)])) * dlog_p)
            r_moist_t = moist_t.loc[dict(level=level)] * self.r
            # Compute the geopotential on full level from Surface Pressure, then update the current half level
            z_f = sz + (alpha * r_moist_t.values)
            z_h = sz + (dlog_p * r_moist_t.values)

            z_full.loc[dict(level=level)] = z_f
            z_half.loc[dict(level=level)] = z_h

        for level in p_half.level.values[:0:-1]:       #Exclude lowest and top level
            dlog_p = np.log(p_half.loc[dict(level=level)]/ p_half.loc[dict(level=level - 1)])
            alpha = 1. - ((p_half.loc[dict(level=level - 1)] / (p_half.loc[dict(level=level)] - p_half.loc[dict(level=level - 1)])) * dlog_p)

            r_moist_t = moist_t.loc[dict(level=level)] * self.r
            # Compute the geopotential on full level from previous half level output, then update the current half level
            # z_f is the geopotential of this full level
            # integrate from previous (lower) half-level z_h to the
            # full level
            z_f = z_h + (alpha * r_moist_t.values)

            # z_h is the geopotential of 'half-levels'
            # integrate z_h to next half level
            z_h = z_h + (dlog_p * r_moist_t.values)

            z_full.loc[dict(level=level)] = z_f
            z_half.loc[dict(level=level)] = z_h

        for level in [p_half.level.values[0]]:
            dlog_p = np.log(p_half.loc[dict(level=level)] / 0.1)
            alpha = np.log(2)
            r_moist_t = moist_t.loc[dict(level=level)] * self.r
            # Compute the geopotential on full level from Surface Pressure, then update the current half level
            z_f = z_h + (alpha * r_moist_t.values)
            z_h = z_h + (dlog_p * r_moist_t.values)

            z_full.loc[dict(level=level)] = z_f
            z_half.loc[dict(level=level)] = z_h

        return z_full

    def interpolation(self):
        # Note: the following code is written via bash
        os.system("module load cdo")
        # Check icon grid, convert to netcdf if grb is found
        fname_icon_model_level_nc, _ = os.path.splitext(os.path.basename(self.icon_model_level))
        fname_icon_model_level_nc = fname_icon_model_level_nc + '.nc'
        fname_icon_model_level_nc = os.path.join(os.path.dirname(self.icon_model_level), fname_icon_model_level_nc)
        if 'grb' in self.icon_model_level:
            os.system('cdo -f nc copy %s %s' %(self.icon_model_level, fname_icon_model_level_nc))

        # calculate level of full levels (HFL) of ICON data <- create half level ICON grid
        target_minus1 = self.target_level - 1
        os.system("cdo -divc,2.0 -add -sellevidx,1/%s %s -sellevidx,2/%s %s %s/HFL_ICON.nc" %(target_minus1, fname_icon_model_level_nc, self.target_level, fname_icon_model_level_nc, self.cwd))
        # calculate the HFL of IFS data
        os.system("cdo divc,9.80665 -selname,z %s HFL_CAMS.nc" %(self.fname_z_full))
        # interpolate vertically from source (CAMS) to target (ICON) HFLs
        for varname in self.target_namelist:
            os.system("cdo -intlevelx3d,%s/HFL_CAMS.nc -selname,%s %s/%s %s/HFL_ICON.nc %s/icon_level_remap_%s_%s.nc" %(self.cwd, varname, self.cwd, self.fname_var_hinterpolated, self.cwd, self.cwd, self.output_grid_name,varname))
            
    
    def hi_variable(self):     #Variable Namelist for Horizontal Interpolation
        os.system("rm %s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd))     # Clean up previous file
        os.system("rm %s/NAMELIST_ifs2icon_horizontal" %(self.cwd))
        #Declare Namelist
        for source_var in self.source_namelist:
            for target_Var in self.target_namelist:
                with open("%s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd), "a") as file:
                    file.write("&input_field_nml\n inputname    = \"%s\"\n outputname    = \"%s\"\n intp_method     = 3\n/\n!" %(source_var, target_var))

        text = """&remap_nml
                in_grid_filename  = '%s/%s'
                in_filename       = '%s/%s'
                in_type           = 2
                out_grid_filename = '%s/%s'
                out_filename      = '%s/%s'
                out_type          = 2
                out_filetype      = 5
                l_have3dbuffer    = .false.
                ncstorage_file    = "ncstorage.tmp"
                /""".format(self.cwd, self.fname_ifs_hgrid, self.cwd, self.fname_var, self.cwd, self.fname_icon_hgrid, self.cwd, self.fname_var_hinterpolated)

        with open("%s/NAMELIST_ifs2icon_horizontal" %(self.cwd), "w") as file:
            file.write(text)


    def hi_parameters(self):     #Namelist for boundary condition Horizontal Interpolation
        self.fname_tq_hinterpolated = "remap_%s" %(os.basename(self.fname_tq))
        self.fname_surface_hinterpolated = "remap_%s" %(os.basename(self.fname_surface))
        #TQ
        os.system("rm %s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd))     # Clean up previous file
        os.system("rm %s/NAMELIST_ifs2icon_horizontal" %(self.cwd))
        #Declare Namelist
        for var in ['t', 'q']:
            with open("%s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd), "a") as file:
                file.write("&input_field_nml\n inputname    = \"%s\"\n outputname    = \"%s\"\n intp_method     = 3\n/\n!" %(var, var))

        text = """&remap_nml
                in_grid_filename  = '%s/%s'
                in_filename       = '%s/%s'
                in_type           = 2
                out_grid_filename = '%s/%s'
                out_filename      = '%s/%s'
                out_type          = 2
                out_filetype      = 5
                l_have3dbuffer    = .false.
                ncstorage_file    = "ncstorage.tmp"
                /""".format(self.cwd, self.fname_ifs_hgrid, self.cwd, self.fname_tq, self.cwd, self.fname_icon_hgrid, self.cwd, self.fname_tq_hinterpolated)

        with open("%s/NAMELIST_ifs2icon_horizontal" %(self.cwd), "w") as file:
            file.write(text)
        #Cast to bash
        os.system('/home/b/b380982/Icon_tools/dwd_icon_tools/icontools/iconremap --remapnml NAMELIST_ifs2icon_horizontal --input_field_nml NAMELIST_ICON_VARIABLE_TABLE')

        #SP,SZ
        os.system("rm %s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd))     # Clean up previous file
        os.system("rm %s/NAMELIST_ifs2icon_horizontal" %(self.cwd))
        #Declare Namelist
        for var in ['sp', 'sz']:
            with open("%s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd), "a") as file:
                file.write("&input_field_nml\n inputname    = \"%s\"\n outputname    = \"%s\"\n intp_method     = 3\n/\n!" %(var, var))

        text = """&remap_nml
                in_grid_filename  = '%s/%s'
                in_filename       = '%s/%s'
                in_type           = 2
                out_grid_filename = '%s/%s'
                out_filename      = '%s/%s'
                out_type          = 2
                out_filetype      = 5
                l_have3dbuffer    = .false.
                ncstorage_file    = "ncstorage.tmp"
                /""".format(self.cwd, self.fname_ifs_hgrid, self.cwd, self.fname_tq, self.cwd, self.fname_icon_hgrid, self.cwd, self.fname_surface_hinterpolated)

        with open("%s/NAMELIST_ifs2icon_horizontal" %(self.cwd), "w") as file:
            file.write(text)
        #Cast to bash
        os.system('/home/b/b380982/Icon_tools/dwd_icon_tools/icontools/iconremap --remapnml NAMELIST_ifs2icon_horizontal --input_field_nml NAMELIST_ICON_VARIABLE_TABLE')


def main():
    parser = argparse.ArgumentParser(
        description="Command line interface for the ifs2icon_vert class")

    parser.add_argument("fname_tq", type=str, help="File name containing model level variables ['t', 'q']")
    parser.add_argument("fname_var", type=str, help="File name containing variable to be interpolated")
    parser.add_argument("fname_surface", type=str, help="File name containing Surface Pressure & Surface Geopotential ['sp', 'z']")
    parser.add_argument("ifs_model_level", type=str, help="File name of the IFS model level description csv")
    parser.add_argument("icon_model_level", type=str, help="File name of the ICON model level")
    parser.add_argument("target_level", type=int, help="Number of target levels, must match the icon_model_level")
    parser.add_argument("fname_ifs_hgrid", type=str, help="File name containing IFS Horizontal grid file")
    parser.add_argument("fname_icon_hgrid", type=str, help="File name containing ICON Horizontal grid file")
    parser.add_argument("output_grid_name", type=str, help="Name of the ICON horizontal grid")
    parser.add_argument("var_nametable", type=int, help="csv table of variable names to be interpolated")

    args = parser.parse_args()

    # Create an instance of the class with the parsed arguments
    instance = ifs2icon_vert(args.fname_tq, args.fname_var, args.fname_surface, args.ifs_model_level, args.icon_model_level, args.target_level, args.var_namelist)
    instance.main()

if __name__ == "__main__":
    main()
