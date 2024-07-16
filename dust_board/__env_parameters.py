import datetime


class EnvParameters:
    r"""
    Class of dictionary of environmental parameters to run the download script
    """

    def __init__(self):
        # Initialise datetime.datetime object and transform to format matching DWD server
        self.model_init_time = datetime.datetime.now().replace(hour=0, minute=0).isoformat(
            timespec='minutes')
        # Local destination path for different model file (Will be automatically created if not exist)
        self.local_directory = {
            'dwd_art': '/lsdf/kit/imk-tro/projects/MOD/Gruppe_Grams/nk2448/2024_Kitweather/dust_board/dwd_art',
            'dwd_art_eu': '/lsdf/kit/imk-tro/projects/MOD/Gruppe_Grams/nk2448/2024_Kitweather/dust_board/dwd_art_eu',
            'dwd_nwp': '/lsdf/kit/imk-tro/projects/MOD/Gruppe_Grams/nk2448/2024_Kitweather/dust_board/dwd_nwp',
            'dwd_nwp_eu': '/lsdf/kit/imk-tro/projects/MOD/Gruppe_Grams/nk2448/2024_Kitweather/dust_board/dwd_nwp_eu'
        }
        # URL to the variable name page of the models
        self.model_url = {
            #'dwd_art': 'https://opendata.dwd.de/weather/nwp/icon/grib/00/',
            #'dwd_art_eu': 'https://opendata.dwd.de/weather/nwp/icon-eu/grib/00/',
            'dwd_art': 'https://files.dwd-api.de/browser/',
            #'dwd_art_eu': 'https://files.dwd-api.de/browser/'
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_art_variable = {
            'taod_dust': '?prefix=m/icon-art/p/TAOD_DUST/r/%s/s/' % (self.model_init_time),
            'dust_total_mc': '?prefix=m/icon-art/p/DUST_TOTAL_MC/lvt1/150/lv1/120/r/%s/s/' % (self.model_init_time),
            't_2m': '?prefix=m/icon-art/p/T_2M/r/%s/s/' % (self.model_init_time),
            'asob_s': '?prefix=m/icon-art/p/ASOB_S/r/%s/s/' % (self.model_init_time),
            'aswdifd_s': '?prefix=m/icon-art/p/ASWDIFD_S/r/%s/s/' % (self.model_init_time)
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_art_eu_variable = {
            'taod_dust': '?prefix=m/icon-art-eu/p/TAOD_DUST/r/%s/s/' % (self.model_init_time),
            #'dust_total_mc': '?prefix=m/icon-art-eu/p/DUST_TOTAL_MC/lvt1/150/lv1/74/r/%s/s/',
            #'t_2m': '?prefix=m/icon-art-eu/p/T_2M/r/%s/s/' % (self.model_init_time),
            #'asob_s': '?prefix=m/icon-art-eu/p/ASOB_S/r/%s/s/' % (self.model_init_time),
            #'aswdifd_s': '?prefix=m/icon-art-eu/p/ASWDIFD_S/r/%s/s/' % (self.model_init_time)
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_nwp_variable = {
            'asob_s': 'asob_s/',
            'aswdifd_s': 'aswdifd_s/',
            't_2m': 't_2m/'
        }
        # relative path from vairable name page to list of model variable files
        self.dwd_nwp_eu_variable = {
            'asob_s': 'asob_s/',
            'aswdifd_s': 'aswdifd_s/',
            't_2m': 't_2m/'
        }
        # Long names of the variables, used in Plotting routine
        self.long_names = {
            'taod_dust': 'Column integrated Atmospheric Optical Depth',
            'dust_total_mc': 'Total Dust',
            't_2m': '2M Surface Temperature',
            'asob_s': 'Net surface direct solar radiation',
            'aswdifd_s': 'Surface downwards diffused solar radiation',
        }
        # Short names of the variables, used in Preprocessing routine
        self.short_names = {
            'taod_dust': 'AOD_DUST',
            'dust_total_mc': 'param0.20.0',
            't_2m': '2t',
            'asob_s': 'ASOB_S',
            'aswdifd_s': 'ASWDIFD_S',
        }
        # Path to source file grid (ICON), can be obtained in MPI server. ICON-NWP: R3B6
        self.source_grid = {
            'dwd_art': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/icon_grid_0036_R03B06_G.nc',
            'dwd_art_eu': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/icon_grid_0051_R03B07_N02.nc',
            'dwd_nwp': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/icon_grid_0005_R03B06_R.nc',
            'dwd_nwp_eu': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/icon_grid_0051_R03B07_N02.nc'
        }
        # Path to destination grid, see cdo documentary for the format of the file used in remapnn
        self.target_grid = {
            'dwd_art': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/dwd_nwp.txt',
            'dwd_art_eu': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/dwd_nwp_eu.txt',
            'dwd_nwp': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/dwd_nwp.txt',
            'dwd_nwp_eu': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/dwd_nwp_eu.txt'
        }
        # Meteogram line colour
        self.meteogram_colour = {
            'dwd_art': 'red',
            'dwd_art_eu': 'blue',
            'dwd_nwp': 'green',
            'dwd_nwp_eu': 'orange'
        }
        # Contour colormap
        self.contourf_colormap = {
            'taod_dust': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/taod_dust.rgb',
            'dust_total_mc': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/taod_dust.rgb',
            't_2m': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/kit-weather-t2m.rgb',
            'asob_s': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/kit-weather-bw.rgb',
            'aswdifd_s': '/pfs/data5/home/kit/imktro/nk2448/2024_Kitweather/dust_board/kit-weather-bw.rgb'
        }
        # Long Name of the model, used in Plotting routine
        self.model_long_names = {
            'dwd_art': 'ICON-ART',
            'dwd_art_eu': 'ICON-ART-EU',
            'dwd_nwp': 'ICON-NWP',
            'dwd_nwp_eu': 'ICON-NWP-EU'
        }
        # Enable Multi-processing (NOT YET IMPLEMENTED)
        self.multi_process = {
            'switch': False,
            'n_process': 1
        }
        # Information of Dust A-C for PM2.5 and PM10 computation list: [mean, sd]
        self.dust_log_nor = {
            'DUSTA0': [6.445e-7, 1.700],
            'DUSTB0': [3.454e-6, 1.600],
            'DUSTC0': [8.672e-6, 1.500]
        }
