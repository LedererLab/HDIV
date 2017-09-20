# Inference for high-dimensional nested regression

Methods for statistical inference in high-dimensional models under endogeneity of regressors of interest.


## Authors

**David Gold, Johannes Lederer, Jing Tao** - University of Washington


## Reference

[Inference for high-dimensional nested regression](https://arxiv.org/abs/1708.05499)

Cite as "Inference for high-dimensional nested regression, *Gold, Lederer, and Tao,* arXiv:1708.05499, 2017"


## Software

The present repository contains the software used to conduct the simulation studies cited in the paper "Inference for high-dimensional nested regression".
Please note that the software is designed for use on a cluster under the [Slurm Workload Manager](https://slurm.schedmd.com/).
The simulation routine may be adapted for serial execution, or for parallel execution under a different workload manager.
Our primary objective in hosting this repository is the transparent presentation of the processes used to obtain our empirical results.
As is, the software is not suitable for practical analytical objectives, though it may be adapted for such purposes.

The following subsections detail the installation and use of the software on a compute cluster with Slurm.

### Installation

After connecting to your cluster, you should clone the present git repository with
```
git clone git@github.com:LedererLab/HDIV.git
```
Note that you may need to enter a short interactive Slurm session to use git, for instance with
```
srun --pty --time="60" --mem-per-cpu="100" /bin/bash
```
Depending on your cluster's configuration, you may first need to install (from an available [binary](https://git-scm.com/downloads)) or build (from [source](https://github.com/git/git)) git using the interactive session.

You will need the following R packages to run the simulations:

```
dplyr
purrr
MASS
mvtnorm
Matrix
methods
lpSolve
glmnet
```

### Configuring the simulations

The simulations are conducted under a variety of parameter configurations.
Each parameter configuration determines a data-generation mechanism that is used to generate random samples to which the HDIV estimation method may be applied.

To configure the simulations, first navigate to the `HDIV/src` directory.
Then, in a Slurm interactive session, run

```
Rscript config/make_configs.r
```

This will write the parameter configurations specified in the `make_configs.r` file to a csv file in the `config` folder.
Note that, depending on the configuration of your cluster, you may need to load an appropriate Slurm module, for instance with
```
module load R
```
or similar, in order for the `Rscript` command to be available.

Next, while still in the `src` directory, run
```
./configure.sh
```
This will launch a series of Slurm jobs that each generate the model's regression parameters and writes them to the appropriate files.


### Running the simulations
