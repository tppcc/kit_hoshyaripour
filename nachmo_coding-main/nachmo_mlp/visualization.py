#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Created on Thu Mar 30 14:32:28 2023.

@author: andreyvlasenko
"""

import matplotlib.pyplot as plt
import numpy as np
import torch


def plot_for_each_species(
    y, max_c, species_list=None, vars_list=None, title=None, ncells=None, observing_period=None, rel_error=False
):
    """Plot one or more variables for multiple chemical species. one plot is created for each chemical species.

    Plot reference and inferred concentration trajectories, starting from initial reference conditions

    Args:
         y(torch.tensor): array containing computed and reference concentrations. Note, dimensions of y should be (n_timepoints, n_species, n_vars)
         species_list(list): list of species names
         vars_list:
         title(string): title of a plot
         ncells(int):   number of cells to visualize
         observing_period(int): num of time steps to compute and visualize


    Returns:
            concentration_plots(figure object): plots of computed concentrations
    """

    #  for i in range(0,3):
    #     print("max_c = ", max_c[i])

    if len(y.shape) == 2:  # only 1 var, no variable axis
        y = y.reshape(*y.shape, 1)  # add the variable axis
    n_timepts, n_species, n_vars = y.shape

    if not rel_error:
        for i in range(0, n_species):
            # print("max_c = ", max_c[i] )
            y[:, i, :] = y[:, i, :] * max_c[i]

    aux = torch.empty(n_timepts + ncells + 1, n_species, n_vars).fill_(float("nan"))
    for i in range(0, ncells):
        aux[i * observing_period + i : (i + 1) * observing_period + i, :, :] = y[
            i * observing_period : (i + 1) * observing_period, :, :
        ]
    y = aux

    if species_list is None:
        species_list = [f"species {j}" for j in n_species]
    if vars_list is None:
        vars_list = [f"y[:, {k}]" for k in n_vars]

    concentration_plots = []
    for j in range(n_species):
        assert len(y.shape) == 3, "y must have 2 dimensions"
        fig, ax = plt.subplots()
        for k in range(n_vars):
            plt.plot(y[:, j, k], label=vars_list[k])
        if title is None:
            plt.title(species_list[j])
        else:
            plt.title(f"{title} for {species_list[j]}")
        ax.set_xlabel("Time step")
        plt.legend()
        concentration_plots = concentration_plots + [fig]
        plt.close()
    #        plt.show()
    return concentration_plots


def ode_plot(
    stepper,
    max_c,
    c,
    c_ref,
    y_outliers,
    nsteps=None,
    ncells=None,
    alp=0.025,
    concentrations_or_tendencies="concentrations",
):
    """Plot reference and inferred concentration trajectories, starting from initial reference conditions.

    Args:
         stepper - single step NN model
         nsteps - number of steps to visualize
         ncells - number of cells to visualize
         alp    - scatter plot's brightness parameter
         concentrations_or_tendencies(string): if set to "concentrations" it visualizes concentrations, othervise it plots tendencies.


    Returns:
            scatter_plots(figure object): scatter plots of computed concentrations (concentration derivatives) versus the corresponding reference
            concentration_plots(figure object): plots of computed concentrations
    """

    if nsteps > 1 and ncells > 0:
        c_ref = c_ref[:ncells, :nsteps, :]
        c = c[:ncells, :nsteps, :]
    n_cells, n_timepts, n_species = c_ref.shape

    assert c.shape == c_ref.shape, "incorrect shape for rollout "

    absolute_error = c - c_ref
    relative_error = absolute_error / c_ref
    relative_error[abs(relative_error) > 2] = 2.0  # float('nan')

    zero_array = (c_ref.reshape(-1, c.shape[-1])) * 0.0

    y = torch.dstack((c.reshape(-1, c.shape[-1]), c_ref.reshape(-1, c.shape[-1])))  # stack cells along time axis

    y_abs_err = torch.dstack((absolute_error.reshape(-1, c.shape[-1]), zero_array))  # stack cells along time axis

    y_rel_err = torch.dstack((relative_error.reshape(-1, c.shape[-1]), zero_array))  # stack cells along time axis

    time_period = "obtained for " + str(n_timepts) + " steps (" + str(nsteps / 500) + " sec)"

    absolute_error_plots = plot_for_each_species(
        y_abs_err,
        max_c,
        species_list=stepper.species_list,
        vars_list=["absolute error", ""],
        title="Absolute error in concentration " + time_period,
        ncells=ncells,
        observing_period=nsteps,
    )

    relative_error_plots = plot_for_each_species(
        y_rel_err,
        max_c,
        species_list=stepper.species_list,
        vars_list=["relative error", ""],
        title="relative error in concentration " + time_period,
        ncells=ncells,
        observing_period=nsteps,
        rel_error=True,
    )
    # print("**********  y_outliers.shape = ",y_outliers.shape)
    outlier_plots = plot_for_each_species(
        y_outliers,
        max_c,
        species_list=stepper.species_list,
        vars_list=["emulated", "reference"],
        title="Largest outlies in concentrations defined by absolute error(1), absolute error grow(2) " + time_period,
        ncells=2,
        observing_period=int(len(y_outliers) / 2),
    )

    concentration_plots = plot_for_each_species(
        y,
        max_c,
        species_list=stepper.species_list,
        vars_list=["emulated", "reference"],
        title="Time integrated concentration " + time_period,
        ncells=ncells,
        observing_period=nsteps,
    )

    scatter_plots = plot_scatter_plots(
        c,
        c_ref,
        concentrations_or_tendencies="concentrations",
        species_list=stepper.species_list,
        observing_period=nsteps,
        alp=0.025,
    )

    return concentration_plots, scatter_plots, absolute_error_plots, relative_error_plots, outlier_plots


def plot_scatter_plots(
    c,
    c_ref,
    concentrations_or_tendencies,
    species_list=None,
    observing_period=None,
    alp=0.025,
):
    # Plots scatter plots for given individual_cells and observing_time_period.
    # Args:
    #    c(torch.tensor): computed concentration
    #    c_ref(torch.tensor): reference concentration
    #    concentrations_or_tendencies(string): if set to "concentrations" it visualizes
    #                                          concentrations, othervise it plots tendencies.
    #    observing_time_period(integer):  lenght of observing period
    #
    # Returns:
    #        fig(object): scatter plot

    dc = c[:, 1:, :] - c[:, :-1, :]
    dc_ref = c_ref[:, 1:, :] - c_ref[:, :-1, :]

    c = c.reshape(-1, c.shape[-1])
    c_ref = c_ref.reshape(-1, c_ref.shape[-1])
    dc = dc.reshape(-1, dc.shape[-1])
    dc_ref = dc_ref.reshape(-1, dc_ref.shape[-1])

    scatter_plots = []

    for gas in range(0, len(species_list)):
        a = torch.linspace(0, 10, 11) * 0.2 - 1

        fig, ax = plt.subplots()
        if concentrations_or_tendencies == "concentrations":
            plt.scatter(c_ref[:, gas], c[:, gas], alpha=alp)

            plt.title("Concentrations of " + species_list[gas] + " predicted " + str(observing_period) + " steps ahead")
        else:
            a = a = torch.linspace(0, 10, 11) * 0.2 - 1
            a = a * np.max(np.abs(dc_ref))
            # print("np.abs(dreference)", np.max(np.abs(dreference)))
            plt.scatter(dc_ref[:, gas], dc[:, gas], alpha=alp)
            plt.title("Tendencies of " + species_list[gas] + " predicted " + str(observing_period) + " steps ahead")
        plt.plot(a, a, "r")
        ax.set_xlabel("reference")
        ax.set_ylabel("estimates")
        # plt.show()
        scatter_plots = scatter_plots + [fig]
        plt.close()
    return scatter_plots


def metrics_surface_figs(species_list, err_grow, f2p_grow, discrete_steps, absolute_error):
    c = 0
    Err_fig = []
    print("&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&")
    for f in [err_grow, f2p_grow, absolute_error]:
        for gas in range(0, len(species_list)):
            fig, axs = plt.subplots(2, 2, figsize=(9, 9))
            fig.subplots_adjust(left=0.02, bottom=0.06, right=0.95, top=0.94, wspace=0.05)
            step = 0
            print("gas = ", gas)
            for ax in axs.flat:
                # Create the colormap
                cmap = "RdBu_r"
                cbar_lim = np.max(f[:, :, step, gas])
                im = ax.imshow(f[:, :, step, gas], origin="lower", cmap=cmap, vmin=-cbar_lim, vmax=cbar_lim)
                if c == 0:
                    ax.set_title(
                        "Error grow in "
                        + species_list[gas]
                        + " at step "
                        + str(discrete_steps[step])
                        + "("
                        + str(discrete_steps[step] / 500)
                        + " sec)"
                    )
                if c == 1:
                    ax.set_title(
                        "Fit for purpose (relative error) of "
                        + species_list[gas]
                        + "estimates at step "
                        + str(discrete_steps[step])
                        + "("
                        + str(discrete_steps[step] / 500)
                        + " sec)"
                    )
                if c == 2:
                    ax.set_title(
                        "Absolute Error of "
                        + species_list[gas]
                        + "estimates at step "
                        + str(discrete_steps[step])
                        + "("
                        + str(discrete_steps[step] / 500)
                        + " sec)"
                    )

                fig.colorbar(im, ax=ax)
                step += 1
            Err_fig = Err_fig + [fig]
        c += 1

    plt.close()
    print("len(Err_fig) = ", len(Err_fig))
    return Err_fig





def scatter_plots_verwer(y,ref,species,cfg):
    fig, axs = plt.subplots(5, 4)
    s = 100
    alp = 0.125

    c = torch.linspace(0, 10, 11) * 0.2 - 1
            

    [n_exp,nsteps,nspecies] = ref.shape
    half = int(nsteps/2)



    counter = 0


    if cfg.data_config.scheme == "Verwer":
        hcells = 4
        vcells = 5

    if cfg.data_config.scheme == "OH":
        hcells = 3
        vcells = 1    

    for i in range(0,vcells):
        for j in range(0,hcells):
        
            y[:,-1,counter] =y[:,-1,counter]/max(ref[:,-1,counter])
            y[:,half,counter] =y[:,half,counter]/max(ref[:,half,counter])
            
            
            ref[:,-1,counter] =ref[:,-1,counter]/max(ref[:,-1,counter])
            ref[:,half,counter] =ref[:,half,counter]/max(ref[:,half,counter])
            
            axs[i, j].scatter(ref[:,-1,counter], y[:,-1,counter], alpha=alp, color = 'b')        
            axs[i, j].scatter(ref[:,half,counter], y[:,half,counter], alpha=alp, color = 'g')
            
            axs[0, 0].scatter(ref[1,-1,counter]*0, y[1,-1,counter]*0, alpha=1, color = 'b', label='Full')        
            axs[0, 0].scatter(ref[1,half,counter]*0, y[1,half,counter]*0, alpha=1, color = 'g', label='Half')
            
            axs[i, j].plot(c, c, "r")
            
          #  print("counter = ", counter)
            axs[i, j].set_title(species[counter])
            if i == 0 and j == 0: axs[i, j].legend()
            counter+=1

            if j == 0 : axs[i, j].set(xlabel='truth', ylabel='estimates')
            
            if j >0 and  i == 4 : axs[i, j].set(xlabel='truth')
            
            
            
    return fig










