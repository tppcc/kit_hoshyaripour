## Path to the models /work/gg0302/g260141/NACHMO_data/data_for_paper/Verwer_paper
## How to run jupyter notebooks:

Copy and follow this link: https://jupyterhub.dkrz.de/hub/login?next=%2Fhub%2Fhome

## How to visualizer results using tesorboard

tensorboard --logdir /your_dir_with_tblogs/$1 --port 888

## How to download ICON

ICON is here https://gitlab.dkrz.de/icon/icon-model

CABBA is here (download the suplementaty material from this Article) https://gmd.copernicus.org/articles/12/1365/2019/

## Code content
- chemical_constants_and_parameters.py &nbsp;  &nbsp; &nbsp; _Contains stoichiometry matrices_
- dataloader.py.    &nbsp;  &nbsp; &nbsp; _arranges the dataset in batches and distributes it among the devices (cpus, gpus)_.
- dataset.py        &nbsp;  &nbsp; &nbsp;     _Prepares a dataset consisting of input (chemical states) and outputs (chemical state sequences)_
- exec.bash                    &nbsp;  &nbsp; &nbsp;   _Contains slurm commands for multiprocessor run_
- loss_function.py             &nbsp;  &nbsp; &nbsp;   _contains loss function._
- main.py                       &nbsp;  &nbsp; &nbsp;  _Th driver program_.
- metrics.py             &nbsp;  &nbsp; &nbsp;   _Computes MAE and MRE for validation data_ .
- models.py                     &nbsp;  &nbsp; &nbsp;  _Creates the MLP and the stepper._
- model_calculation.py         &nbsp;  &nbsp; &nbsp;   _Evaluates model several timesteps ahead, preserving autograd tape (code dependencies)._
- train.py                     &nbsp;  &nbsp; &nbsp;  _Contains training and logger subroutines._ 
- utilities.py                 &nbsp;  &nbsp; &nbsp;   _Loads the data from the harddrive, normalizes it. Returns tensors containing concentrations and normalization factors. This data is used by_ `dataset.py`
- visualization.py &nbsp;  &nbsp; &nbsp; _ Visualizes NN outputs on the fly during the training _ 

# How to run NACHMO-MLP on strand

cd into you work-directory (the home directory has limited space)

`cd /gpfs/work/<your-user-name>`

## 1. Installing packages (if needed)

### Installing mniniconda (if needed)
Download the miniconda file:

`wget -O Miniconda.sh https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh`

Install miniconda:

`bash ./Miniconda.sh`

Install mamba:

`conda install mamba -n base -c conda-forge`


You After the installation you have to log out and log in to strand again to activate the conda environment. You
can check if conda is installed by running `conda --version`. Also the prompt should now start with (base).

Clone NACHMO-MLP repository:

`git clone git@codebase.helmholtz.cloud:m-dml/nachmo_coding.git`

Create the conda environment:

`cd nachmo_coding/nachmo_mlp/`

and then run:

`mamba env create -f dev_env.yaml`

To activate the environment run `conda activate gpuenv`.

## 2. Setting `config.yaml` file

Goto `/gpfs/work/<your-user-name>`  and then `cd nachmo_coding/nachmo_mlp/`

open `config.yaml` In this file find the lists given below and change the default values, if needed. Each item cooresonds to a NN hyperparamter or to the training settings. The names are self-explainable. Anyhow, see comments starting with `#` opposite to corresponding item for more info.  

- **hardw_settings**:
     - devices: 1.   &nbsp;  &nbsp; &nbsp; # _num of GPU's per node_ 
     - accelerator: "gpu" &nbsp;  &nbsp; &nbsp; # _change to "cpu" if run on cpu nodes_
     - strategy: "ddp"
     - num_of_nodes: 1

- **train_config**:
     - lr: 1e-6 &nbsp;  &nbsp; &nbsp; # _learning rate_
     - n_epochs: 1240 &nbsp;  &nbsp; &nbsp; # _number of epochss_

- **loader_config**:
     - batch_size: 256
     - num_workers: 1 &nbsp;  &nbsp; &nbsp; # _don't touch_ 
 
- **experiment_config**:
     - val_frac: 0.11 &nbsp;  &nbsp; &nbsp; # _fraction of validation data_
     - test_frac: 0.09  &nbsp;  &nbsp; &nbsp; # _fraction of test data_

- **data_config**:
     - scheme: "Verwer" &nbsp;  &nbsp; &nbsp; # _this flag set to compute Verwer scheme. For dynamic OH chane it to "OH"_
     - species: 
     - species_oh: ["OH", "HO2", "H2O2"]  &nbsp;  &nbsp; &nbsp; #  _don't touch !_
     - species_verwer: ["CO","HNO3","SO4","XO2",... &nbsp;  &nbsp; &nbsp; #  don't touch !
     - trajectory_length: 2  &nbsp;  &nbsp; &nbsp; # _length of multistep training_
- **net_config**:
     - n_hidden: [40,40,40] &nbsp;  &nbsp; &nbsp; # _Number of elements in this list corresponds to NN depth, the value of the element is the amount of neurons in the corresponding level_
    - input_products: True  &nbsp;  &nbsp; &nbsp; # _flag defining whether we provide all concentration products as input_
    - activation: "ReLU"
    - device: "cuda"  &nbsp;  &nbsp; &nbsp; # _If you compute on CPU node, change to "cpu"_ 
    - depth: 900 #59999  &nbsp;  &nbsp; &nbsp; # _Corresponds to the time length (in time steps) of the training data. Set to the maximal number of timesteps if you whant ot train on the whole set_
    - debug: False

- **stepper_config**:
    - learn_updates: True. &nbsp;  &nbsp; &nbsp; # 
    - learn_rates: False
    - stoichiometry_matrix: False &nbsp;  &nbsp; &nbsp; # If set to the `True`, set `learn_rates: True` and `learn_updates: False`.

- **exp_name**: "Exp" &nbsp;  &nbsp; &nbsp; # _Name of the experiment_
- **log_name**: "nul" &nbsp;  &nbsp; &nbsp; # _Name of the directory where the NN stores checkpoints, model, results and hyperparameters._
- **path_to_data**:



## 3. Setting path to the data

Copy the `Verwer` and `dyn_OH` data from `/work/gg0302/g260141/NACHMO_data/Verwer` and `/work/gg0302/g260141/NACHMO_data/dyn_OH` into `/gpfs/work/<your-user-name>`.

 In the `main.py` find the "if block" `if data_config.scheme == "Verwer"` . At the end of this block find the variable `data_path =` and set the pass to Verwer data set in `/gpfs/work/<your-user-name>`.  Then find the "if block" `if data_config.scheme == "OH"`" and set the pass to dynamic OH data set in `/gpfs/work/<your-user-name>`. Save the changes. 

 ## 4. Running the code

 If you run the code on a frontnode, run the command `python main.py`, but make sure that `devices` and `num_of_nodes` in the `config.yaml` are set to one. If you do not do this, prepare to get tones of the holly anger from our admins.

If you run the code using `slurm` command, open `exec.bash` file and substitute `srun /gpfs/home/vlasenko/miniconda3/envs/gpuenv/bin/python main.py` with `srun /gpfs/home/<your-user-name>/miniconda3/envs/gpuenv/bin/python main.py`. Ejoy.

# Generate your own data
Follow the instructions in [running KPP from python](KPP_python/readme.md).
