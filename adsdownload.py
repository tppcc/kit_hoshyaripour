import cdsapi
import numpy as np
import argparse
import os

'''
Function        :   ads_download
Description     :   This function is a command line interface for downloading ADS dataset. It allows users to specify various parameters such as model name, 
                    date range, time resolution, area of interest, optional high-resolution area, level information, and variable names for downloading datasets from ADS.

Parameters      :   path(str)              - Directory at which the downloaded file is stored
                    model_name(str)        - Name of the model file to be downloaded, e.g. cams-global-reanalysis-eac4.
                    date_range(str)        - Start and end date of the desired download script, formatted as YYYY-MM-DD,YYYY-MM-DD.
                    time_res(str)          - Time hourly resolution of the desired download script, e.g. 00,06,12,18.
                    area(str)              - Area of interest defined as MAXLAT,MINLON,MINLAT,MAXLON anticlockwise starting from the north-most point, e.g. 90,-180,-90,180.
                    -hres(str, optional)   - Horizontal resolution, will use model resultion if no argument is given. E.g. 0.4,0.4
                    -level(str, optional)  - Optional parameter. Surface/Model level/Pressure level of interest. Will assume surface parameter if no argument is given
                                             For model level: (1) 1/60 (optional: 1/60/2 third number being resolution), selects all model levels between the given values if resolution is not given. 
                                             For Pressure Levels: (2) 1, 300, 1000 in hPa.
                    variables(str)         - Variable names to be downloaded, e.g. carbon_monoxide, hydrogen_peroxide.

Note            :   For proper usage, ensure that the files containing variables, surface, and tq contain only ONE timestep.
Usage           :   Call the program using:
                        python download_ads_dataset.py <model_name> <date_range> <time_res> <area> [-hres <area>] [-level <level>] <variables> ...
                    Replace <model_name>, <date_range>, <time_res>, <area>, [-hres <area>], [-level <level>], <variables>, etc., with the actual values needed for execution.
'''


class adsdownload:
    def __init__(self, path, model_name, date_range, time_res, area, hres, level,
                 variables):  # Initialise class adsdownload
        self.path = path
        self.model_name = model_name
        # Split date time
        self.date_range = date_range.replace("", "").split(",")
        self.time_res = [x + ":00" for x in time_res.replace("", "").split(",")]
        self.area = [int(x) for x in area.replace("", "").split(",")]

        # Horizontal resolution
        if hres == None:
            self.hres = hres
        elif hres != None:
            self.hres = hres.replace("", "").split(",")
        # Height resolution
        if level == None:
            self.level = level
        elif "/" in level:
            self.level = level.replace("", "").split("/")
            self.pressure_or_level = "model"
            self.level = self.ml_split(*self.level)
        elif "," in level:
            self.level = level.replace("", "").split(",")
            self.pressure_or_level = "pressure"

        self.variables = variables.replace("", "").split(",")

    def ml_split(self, min_level, max_level, res=1):
        ml = np.arange(int(min_level), int(max_level) + 1, int(res))
        return ml.tolist()

    def cdsapirc_check(self):
        home_dir = os.path.expanduser("~")
        if os.path.isfile("%s/.cdsapirc" % (home_dir)) == False:
            raise Exception(".cdsapirc does not exist in %s" % (home_dir))

    def main(self):
        # Perform cdsapirc file check
        self.cdsapirc_check()
        # Set up a loop for each day in the selected date range
        start_date, end_date = self.date_range
        date_delta = (np.datetime64(end_date) - np.datetime64(start_date)).astype('timedelta64[D]').item().days
        for dt in np.arange(0, date_delta + 1, 1):
            current_date = (np.datetime64(start_date) + np.timedelta64(dt, 'D')).astype(str)
            date = "%s/%s" % (current_date, current_date)
            fname = "%s_%s_level_%s.nc" % (self.model_name, self.pressure_or_level, current_date)
            fname = os.path.join(self.path, fname)

            if self.hres == None:
                # Initialise cdsapi instance
                c = cdsapi.Client()

                c.retrieve(
                    '%s' % (self.model_name),
                    {
                        'format': 'netcdf',
                        'time': self.time_res,
                        'date': date,
                        'area': self.area,
                        '%s_level' % (self.pressure_or_level): self.level,
                        'variable': self.variables,
                    },
                    '%s' % (fname))
            elif self.hres != None:
                # Initialise cdsapi instance
                c = cdsapi.Client()

                c.retrieve(
                    '%s' % (self.model_name),
                    {
                        'format': 'netcdf',
                        'time': self.time_res,
                        'date': date,
                        'area': self.area,
                        '%s_level' % (self.pressure_or_level): self.level,
                        'variable': self.variables,
                        'grid': self.hres
                    },
                    '%s' % (fname))

    def test_main(self):
        # Perform cdsapirc file check
        self.cdsapirc_check()
        # Set up a loop for each day in the selected date range
        start_date, end_date = self.date_range
        date_delta = (np.datetime64(end_date) - np.datetime64(start_date)).astype('timedelta64[D]').item().days
        for dt in np.arange(0, date_delta + 1, 1):
            current_date = (np.datetime64(start_date) + np.timedelta64(dt, 'D')).astype(str)
            date = "%s/%s" % (current_date, current_date)
            fname = "%s_%s_level_%s.nc" % (self.model_name, self.pressure_or_level, current_date)
            fname = os.path.join(self.path, fname)
            print(date, fname, self.time_res, self.area, self.hres, self.level, self.hres, self.pressure_or_level,
                  self.variables)


def main():
    parser = argparse.ArgumentParser(description="Command line interface for downloading ADS dataset")
    parser.add_argument("path", type=str,
                        help="Directory at which the downloaded file is stored")
    parser.add_argument("model_name", type=str,
                        help="Name of the model file to be downloaded, e.g. cams-global-reanalysis-eac4")
    parser.add_argument("date_range", type=str,
                        help="Start and end date of the desired download script, e.g. YYYY-MM-DD,YYYY-MM-DD")
    parser.add_argument("time_res", type=str,
                        help="Time hourly resolution of the desired download script, e.g. 00,06,12,18")
    parser.add_argument("area", type=str,
                        help="Area of interest, MAXLAT,MINLON,MINLAT,MAXLON anticlockwise starting from the north-most point e.g. 90,-180,-90,180")
    parser.add_argument("-hres", default=None, type=str,
                        help="(Optional: default = None) model resolution if no argument is given, e.g. 0.5,0.5")
    parser.add_argument("-level", default=None, type=str,
                        help="(Optional: default = None [Not mandatory if only Surface parameter is selected])Surface/Model level/Pressure level of interest, (1): 1/60 (optional: 1/60/2 third number being resolution), only for model level, will select all model level between the given values if arguments are not given; (2) 1, 300, 1000 in hPa, only for Pressure Levels")
    parser.add_argument("variables", type=str,
                        help="Variable names to be downloaded, e.g. carbon_monoxide,hydrogen_peroxide")
    args = parser.parse_args()

    instance = adsdownload(args.path, args.model_name, args.date_range, args.time_res, args.area, args.hres, args.level,
                           args.variables)
    instance.main()


if __name__ == "__main__":
    main()
