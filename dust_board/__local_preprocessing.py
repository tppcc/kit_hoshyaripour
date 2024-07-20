import os
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.pyplot as plt
import numpy as np
import pytz
import xarray as xr
import datetime
import matplotlib.colors as mcolors
import math

def LocalTime(time_obj):
    r"""
    Convert DateTime object to <DOW>, <DD> <MM> <YY> <HH><Z>
    :param time_obj:
    :return: String of time in <DOW>, <DD> <MM> <YY> <HH><Z> CEST
    :rtype: str
    """
    # Convert UTC to CEST (Central European Summer Time)
    cest = pytz.timezone('Europe/Berlin')
    dt_cest = time_obj.astimezone(cest)

    # Format the datetime object
    return dt_cest.strftime("%a, %d %b. %Y %H%Z")


def Grib2nc(fpath, source_grid, target_grid):
    #os.system('module load cdo')
    # Assign grid to
    os.system(
        f'grib_set -r -s packingType=grid_simple {fpath} {fpath}.grib1')
    os.system(
    #    # f'cdo -f nc4 -remapnn,f{target_grid} -setgrid,f{source_grid} icon_pollen_description.txt f{fpath} f{fpath}.nc')
        f'cdo -f nc4 -remapnn,{target_grid} -setgrid,{source_grid} {fpath}.grib1 {fpath}.nc')

    fpath_nc = fpath + '.nc'

    return fpath_nc

def LoadCmap(fpath):
    cmap_data = np.loadtxt(fpath, skiprows=2, delimiter=' ')

    if cmap_data.max() > 10.:
        cmap = mcolors.ListedColormap(cmap_data/255.)
    else:
        cmap = mcolors.ListedColormap(cmap_data)

    return cmap

def Dust2PM(field, parameters_dict):
    r"""
    Convert Dust mass concentration to PM2.5 and PM 10 based on the CDF of log-normal mode
    :param field: Dust field to be computed
    :type: dict-like(xr.DataArray)
    :param mean: Mean of the dust distribution
    :type: float
    :param sigma: The Standard Deviation of the dust distribution
    :type: float
    :return: Computed PM2.5 field
    :rtype: xr.DataArray
    :return: Computed PM10 field
    :rtype: xr.DataArray
    """

    env_dict = parameters_dict

    vnames = [x for x in env_dict.__getattribute__(('dust_log_nor'))]

    pm25 = xr.zeros_like(field[vnames[0]])
    pm10 = xr.zeros_like(field[vnames[0]])

    for vname in parameters_dict.dust_log_norm:
        mean, sigma = parameters_dict.dust_log_norm[vname]
        if vname in field:
            pm25 = pm25 + LogNorm2CDF(field[vname], mean, sigma, 2.5e-6)
            pm10 = pm10 + LogNorm2CDF(field[vname], mean, sigma, 1e-5)

    # Format the datetime object
    return pm25, pm10


def LogNorm2CDF(da, mean, sigma, d_upper):
    r"""
    Compute the CDF of a log-normal distrubution function in SI UNIT
    :param field: Total concentration field
    :type: xr.DataArray
    :param mean: Mean of the dust distribution
    :type: float
    :param sigma: The Standard Deviation of the dust distribution
    :type: float
    :param d_upper: Upper limit of the CDF
    :return: Computed CDF
    :rtype: xr.DataArray
    """

    cdf = da * 0.5 * (1 + math.erf( (math.log(d_upper / mean)) / (math.sqrt(2) * math.log(sigma)) ))

    return cdf

def Plotting(da, vname, model, parameters_dict):
    r"""
    Plot the data array and pass it to <local_directory>/plots/
    :param da:
    :param model:
    :param parameters_dict:
    :return: None
    """
    env_dict = parameters_dict
    print(env_dict.model_init_time)

    plot_dir = os.path.join(env_dict.local_directory[model], vname, 'plots')
    if not os.path.exists(plot_dir):
        os.makedirs(plot_dir)

    time = da.time.values

    cmap = LoadCmap(env_dict.contourf_colormap[vname])

    for i, t in enumerate(time):
        try:
            plot_time = np.datetime_as_string(t, unit='m')
            plot_data = da.sel(time=t).squeeze()

            if vname == 't_2m':
                plot_data = plot_data - 273.15  # Convert K to C

            title_init_time = datetime.datetime.fromisoformat(env_dict.model_init_time).strftime('%Y-%m-%d %H:%M')
            title_current_time = t.astype('datetime64[s]').tolist().strftime('%Y-%m-%d %H:%M')

            if vname == 'taod_dust':
                levels = np.linspace(0.05,0.75,15)
                extend = 'max'
            else:
                levels = np.linspace(plot_data.min() - (plot_data.min() * 0.2),
                                     plot_data.max() + (plot_data.max() * 0.2), 20)
                extend = 'both' 

            plt.figure(figsize=[12, 6])
            ax = plt.axes(projection=ccrs.PlateCarree())  # Define the map projection
            ax.coastlines('110m', color='0.4')  # Add coastlines
            ax.add_feature(cfeature.LAND, facecolor='#bebebe')
            ax.add_feature(cfeature.OCEAN, facecolor='#37648b')
            cf = ax.contourf(plot_data.lon, plot_data.lat, plot_data.values, levels=levels, cmap=cmap, extend=extend)
            plt.title(f'{env_dict.long_names[vname]}', loc='center', fontsize=10)
            plt.title('Init. time: ' + title_init_time, loc='left', fontsize=10)
            plt.title('Current: ' + title_current_time, loc='right', fontsize=10)
            plt.xlabel('Longitude')
            plt.ylabel('Latitude')
            plt.colorbar(cf, shrink=0.8, extend='both',extendrect=True)

            plt.savefig(os.path.join(plot_dir, f'{vname}_{plot_time}.jpg'), bbox_inches='tight')
            plt.close()

        except Exception as e:
            print(f'An error occurred when plotting for {vname} at {i}th time step: {e}')

def DataProcessing(model, parameters_dict):
    r"""
    Entry Point 2 for dust_board routine
    Load the downloaded file, load as xarray array, concat along time and return as object
    :param model: Model name
    :param parameters_dict: class of dictionary of Environmental Parameters
    :return:
    :rtype: list(xr.DataArray)
    """

    env_dict = parameters_dict
    source_grid = env_dict.source_grid[model]
    target_grid = env_dict.target_grid[model]

    # Use an explicit dictionary to store variables instead of using locals()
    data_arrays = {}

    # Collect variable names for the given model
    vnames = [x for x in env_dict.__getattribute__((model + '_variable'))]
    for vname in vnames:
        fdirs = os.path.join(env_dict.local_directory[model], vname)
        fnames = [x for x in os.listdir(fdirs) if '.grib2' in x and '.nc' not in x and '.grib1' not in x]
        fnames.sort()
        fnames = [os.path.join(fdirs, x) for x in fnames]

        da_list = []
        for fpath in fnames:
            if os.path.exists(fpath+'.nc') == False:
                fpath_nc = Grib2nc(fpath, source_grid,
                                   target_grid)  # Assuming Grib2nc is a function defined elsewhere
            else:
                fpath_nc = fpath + '.nc'
            da = xr.load_dataset(fpath_nc, engine='netcdf4')
            #varkey = list(da.keys())
            print(da)
            da_list.append(da[env_dict.short_names[vname]])

        # Concatenate along time dimension and store in dictionary
        data_arrays[vname] = xr.concat(da_list, dim='time')

        # Assuming Plotting is a function defined elsewhere that you want to use
        Plotting(data_arrays[vname], vname, model, env_dict)

    return data_arrays


def PreprocessingMeteogram(da, target_lon, target_lat):
    r"""
    Interpolate input field into a point based product
    :param da: Data Array to be interpolated
    :type: xr.DataArray
    :param target_lon:
    :type: float
    :param target_lat:
    :type: float
    :return: interpolated_da xr.DataArray
    :rtype: xr.DataArray
    """
    return da.interp({'lat': target_lat, 'lon': target_lon})


def Meteogram(plot_dir, target_lon, target_lat, t_2m_dict, asob_s_dict, aswdifd_s_dict,
              pm25_dict, pm10_dict, parameters_dict):
    r"""
    Entry Point 3 for dust_board routine
    Produce a point based product Meteogram. Currently:
    1. 2-M Temperature
    2. Surface Net Radiation
    3. Surface Diffused Downards Radiation
    :param t2m_dict: Dictionary of Data Array, key: Model Name
    :type: dict(da)
    :param vname:
    :param parameters_dict:
    :return:
    """
    env_dict = parameters_dict
    models = env_dict.model_url

    vnames = ['t_2m', 'asob_s', 'aswdifd_s']
    plot_y_label = ['Temperature [$^oK$]', 'Net Surface Radiation [W m$^{-2}$]',
                    'Downward Surface Diffused Radiation [W m$^{-2}$]']
    title_init_time = LocalTime(datetime.datetime.fromisoformat(env_dict.model_init_time))

    fig, axes = plt.subplots(3, 1, figsize=[10, 25])
    #plt.rc('font', size=20)

    for i in range(len(vnames)):
        vname = vnames[i]
        for model in models:
            da = locals()[vname + '_dict'][model]
            interpolated_da = PreprocessingMeteogram(da, target_lon, target_lat)
            # Convert from K to C if var = t_2m
            if i == 0:
                interpolated_da = interpolated_da - 273.15
            axes[i].plot(interpolated_da.time, interpolated_da,
                         color=env_dict.meteogram_colour[model], linewidth=2,
                         label=env_dict.model_long_names[model])
            axes[i].set_title(env_dict.long_names[vname], loc='center')
            axes[i].set_ylabel(plot_y_label[i])
            axes[i].set_xlabel('Time [h]')
            axes[i].legend()

    i += 1
    pm_colour = ['red', 'blue', 'green', 'orange']
    pm_label = ['PM2.5 (ICON-ART)', 'PM2.5 (ICON-ART-EU)', 'PM10 (ICON-ART)', 'PM10 (ICON-ART-EU)']
    plot_y_label = 'Number concentration'
    j = 0
    for vname in ['pm25', 'pm10']:
        for model in ['dwd_art', 'dwd_art_eu']:
            da = locals()[vname + '_dict'][model]
            interpolated_da = PreprocessingMeteogram(da, target_lon, target_lat)
            axes[i].plot(interpolated_da.time, interpolated_da,
                         color=env_dict.meteogram_colour[model], linewidth=2,
                         label=env_dict.model_long_names[model])
            j += 1
    axes[i].set_title('PM 2.5 and PM 10 concentration', loc='center')
    axes[i].set_ylabel(plot_y_label)
    axes[i].set_xlabel('Time [h]')
    axes[i].legend()
    plt.title('Init. time: ' + title_init_time, loc='left', size=14)
    plt.savefig(plot_dir, bbox_inches='tight')
