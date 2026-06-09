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
                    <icon_model_level> <target_level> ...
                    Replace <fname_tq>, <fname_var>, <fname_surface>, <ifs_model_level>, <icon_model_level>, 
                    <target_level> etc., with the actual values you want to pass to the class constructor.

for testing purposes
/Users/corneliustai/Desktop/hiwi/test_ifs2icon
python ifs2icon_vert.py tq_field.nc o3_field.nc surface_parameter.nc ifs_model_l60.csv HHL_0024_R02B06_G_l91.grb2 91 ...

cd /work/bb1070/b380982/CAMS_ICON_INTERPOLATION
python ~/ifs2icon_vert.py tq_field.nc o3_field.nc surface_parameter.nc ifs_model_l60.csv HHL_0024_R02B06_G_l91.grb2 91 ...


'''

# [FIX] Extracted iconremap path as a module-level constant for consistency.
#       The original code had two different casings: 'Icon_tools' vs 'icon_tools'.
ICONREMAP_BIN = '/home/b/b380982/icon_tools/dwd_icon_tools/icontools/iconremap'


class ifs2icon_vert:
    def __init__(self, fname_tq, fname_var, fname_surface, fname_ifs_model_level, fname_icon_model_level, target_level, fname_ifs_hgrid, fname_icon_hgrid, output_grid_name, var_nametable):
        # [FIX] Added skipinitialspace=True to handle the space after the comma
        #       in the CSV header ("source_name, target_name").
        var_nametable = pd.read_csv(var_nametable, skipinitialspace=True)
        self.cwd = os.getcwd()
        # [FIX] Removed erroneous line: self.ifs_model_level = ifs_model_level
        #       'ifs_model_level' was not a parameter and would have raised NameError.
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
        # [FIX] os.basename -> os.path.basename (os.basename does not exist).
        self.fname_var_hinterpolated = os.path.basename(self.fname_var)
        # [FIX] Removed the erroneous line:
        #         output_grid_name, _ = os.path.splitext(os.basename(fname_var))
        #       This shadowed the output_grid_name parameter with the stem of
        #       fname_var, producing a wrong filename like "remap_o3_field_o3_field.nc"
        #       instead of "remap_R02B06_o3_field.nc".
        self.fname_var_hinterpolated = "%s/remap_%s_%s" % (self.cwd, output_grid_name, self.fname_var_hinterpolated)

    def main(self):             #Main call for the ifs2icon_vert_class
        self.hi_variable()
        self.hi_parameters()
        z_full = self.gp_ml()
        # [FIX] os.basename -> os.path.basename
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

        # [FIX] Changed slice from [:0:-1] to [-2:0:-1].
        #       The original [:0:-1] produced [N, N-1, ..., 1], re-processing the
        #       lowest level N (already handled above) and including the top level 1
        #       (handled separately below).  The correct slice [-2:0:-1] yields
        #       [N-1, N-2, ..., 2], i.e. intermediate levels only.
        for level in p_half.level.values[-2:0:-1]:       #Exclude lowest and top level
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
        # [FIX] Removed os.system("module load cdo").
        #       'module load' only affects the subshell of that single os.system call
        #       and has no effect on subsequent calls.  CDO must be available in PATH
        #       before running this script (e.g. via the job submission script).

        # Check icon grid, convert to netcdf if grb is found
        # [FIX] os.basename -> os.path.basename
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
            os.system("cdo -intlevelx3d,%s/HFL_CAMS.nc -selname,%s %s/%s %s/HFL_ICON.nc %s/icon_level_remap_%s_%s.nc" %(self.cwd, varname, self.cwd, self.fname_var_hinterpolated, self.cwd, self.cwd, self.output_grid_name, varname))

    def hi_variable(self):     #Variable Namelist for Horizontal Interpolation
        os.system("rm -f %s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd))     # Clean up previous file
        os.system("rm -f %s/NAMELIST_ifs2icon_horizontal" %(self.cwd))
        # [FIX] Replaced nested loop (Cartesian product) with zip() to pair
        #       source and target names row-by-row from the nametable.
        # [FIX] Fixed variable name: target_Var -> target_var (consistent casing).
        for source_var, target_var in zip(self.source_namelist, self.target_namelist):
            with open("%s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd), "a") as file:
                file.write("&input_field_nml\n inputname    = \"%s\"\n outputname    = \"%s\"\n intp_method     = 3\n/\n!" %(source_var, target_var))

        # [FIX] Changed .format() to % operator.  The original string used %s
        #       placeholders but called .format(), which does not substitute into %s.
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
                /""" % (self.cwd, self.fname_ifs_hgrid, self.cwd, self.fname_var, self.cwd, self.fname_icon_hgrid, self.cwd, self.fname_var_hinterpolated)

        with open("%s/NAMELIST_ifs2icon_horizontal" %(self.cwd), "w") as file:
            file.write(text)

        # [FIX] Added the iconremap call.  The original hi_variable() wrote the
        #       namelists but never executed iconremap, so the variable field was
        #       never horizontally interpolated.
        os.system('%s --remapnml NAMELIST_ifs2icon_horizontal --input_field_nml NAMELIST_ICON_VARIABLE_TABLE' % ICONREMAP_BIN)


    def hi_parameters(self):     #Namelist for boundary condition Horizontal Interpolation
        # [FIX] os.basename -> os.path.basename (two occurrences)
        self.fname_tq_hinterpolated = "remap_%s" %(os.path.basename(self.fname_tq))
        self.fname_surface_hinterpolated = "remap_%s" %(os.path.basename(self.fname_surface))
        #TQ
        os.system("rm -f %s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd))     # Clean up previous file
        os.system("rm -f %s/NAMELIST_ifs2icon_horizontal" %(self.cwd))
        #Declare Namelist
        for var in ['t', 'q']:
            with open("%s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd), "a") as file:
                file.write("&input_field_nml\n inputname    = \"%s\"\n outputname    = \"%s\"\n intp_method     = 3\n/\n!" %(var, var))

        # [FIX] Changed .format() to % operator (same issue as hi_variable).
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
                /""" % (self.cwd, self.fname_ifs_hgrid, self.cwd, self.fname_tq, self.cwd, self.fname_icon_hgrid, self.cwd, self.fname_tq_hinterpolated)

        with open("%s/NAMELIST_ifs2icon_horizontal" %(self.cwd), "w") as file:
            file.write(text)
        #Cast to bash
        os.system('%s --remapnml NAMELIST_ifs2icon_horizontal --input_field_nml NAMELIST_ICON_VARIABLE_TABLE' % ICONREMAP_BIN)

        #SP,SZ
        os.system("rm -f %s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd))     # Clean up previous file
        os.system("rm -f %s/NAMELIST_ifs2icon_horizontal" %(self.cwd))
        # [FIX] Changed 'sz' to 'z'.  The IFS/CAMS surface geopotential variable
        #       is named 'z', not 'sz'.  The get_surface_parameter() method reads
        #       da.z, so the name after remapping must also be 'z'.
        for var in ['sp', 'z']:
            with open("%s/NAMELIST_ICON_VARIABLE_TABLE" %(self.cwd), "a") as file:
                file.write("&input_field_nml\n inputname    = \"%s\"\n outputname    = \"%s\"\n intp_method     = 3\n/\n!" %(var, var))

        # [FIX] Changed .format() to % operator.
        # [FIX] Changed in_filename from self.fname_tq to self.fname_surface.
        #       The original passed the T/Q file as input for the surface-parameter
        #       remap, so iconremap would not find sp and z in that file.
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
                /""" % (self.cwd, self.fname_ifs_hgrid, self.cwd, self.fname_surface, self.cwd, self.fname_icon_hgrid, self.cwd, self.fname_surface_hinterpolated)

        with open("%s/NAMELIST_ifs2icon_horizontal" %(self.cwd), "w") as file:
            file.write(text)
        #Cast to bash
        os.system('%s --remapnml NAMELIST_ifs2icon_horizontal --input_field_nml NAMELIST_ICON_VARIABLE_TABLE' % ICONREMAP_BIN)


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
    # [FIX] Changed type from int to str — this is a file path, not a number.
    parser.add_argument("var_nametable", type=str, help="csv table of variable names to be interpolated")

    args = parser.parse_args()

    # [FIX] Added missing arguments (fname_ifs_hgrid, fname_icon_hgrid,
    #       output_grid_name) and corrected args.var_namelist -> args.var_nametable
    #       to match the argparse definition above.
    instance = ifs2icon_vert(
        args.fname_tq,
        args.fname_var,
        args.fname_surface,
        args.ifs_model_level,
        args.icon_model_level,
        args.target_level,
        args.fname_ifs_hgrid,
        args.fname_icon_hgrid,
        args.output_grid_name,
        args.var_nametable
    )
    instance.main()

if __name__ == "__main__":
    main()
