from dust_board import *

# http://icon-downloads.mpimet.mpg.de/dwd_grids.xml#grid5

# Initialise Environmental Dictionary to be passed to the model_load function
environmental_dict = EnvParameters()

############# Fully adaptive, only need to change EnvParameters in .__env_parameters.py

# Fetch nwp variable that exists in all models.
for model in environmental_dict.model_url:
    ModelLoad(model, environmental_dict)
    locals()['da_' + model] = DataProcessing(model, environmental_dict)


############# End of adaptive script

# Plotting Meteogram
t_2m_dict = {
    #'dwd_nwp': da_dwd_nwp['t_2m'],
    #'dwd_nwp_eu': da_dwd_nwp_eu['t_2m'],
    'dwd_art': da_dwd_art['t_2m'],
    #'dwd_art_eu': da_dwd_art_eu['t_2m']
}
aswdifd_s_dict = {
    #'dwd_nwp': da_dwd_nwp['aswdifd_s'],
    #'dwd_nwp_eu': da_dwd_nwp_eu['aswdifd_s'],
    'dwd_art': da_dwd_art['aswdifd_s'],
    #'dwd_art_eu': da_dwd_art_eu['aswdifd_s']
}
asob_s_dict = {
    #'dwd_nwp': da_dwd_nwp['asob_s'],
    #'dwd_nwp_eu': da_dwd_nwp_eu['asob_s'],
    'dwd_art': da_dwd_art['asob_s'],
    #'dwd_art_eu': da_dwd_art_eu['asob_s']
}

Meteogram('fr_stadt.jpg', 7.83212, 48.00152, t_2m_dict, asob_s_dict, aswdifd_s_dict, environmental_dict)
Meteogram('ka_nordwest.jpg', 8.35565, 49.02859, t_2m_dict, asob_s_dict, aswdifd_s_dict, environmental_dict)
Meteogram('st_cannstatt.jpg', 9.22986, 48.80893, t_2m_dict, asob_s_dict, aswdifd_s_dict, environmental_dict)
Meteogram('kn_biberach.jpg', 9.79987, 48.08985, t_2m_dict, asob_s_dict, aswdifd_s_dict, environmental_dict)
Meteogram('hn_stadt.jpg', 9.22474, 49.16553, t_2m_dict, asob_s_dict, aswdifd_s_dict, environmental_dict)