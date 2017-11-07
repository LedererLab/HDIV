# Inference for High-dimensional Nested Regression

Statistical inference in high-dimensional models under endogeneity of regressors of interest.



## Contents and usage

The present repository contains the software used to conduct the simulation studies cited in the paper "Inference for high-dimensional nested regression."
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

To configure the simulations, navigate to the `HDIV`
```
src/configure.sh
```
This will launch a series of Slurm jobs that each generate the model's regression parameters for given a configuration and writes the regression parameters to disk.
The script also creates the `err`, `out`, and `res` folders required to run the simulations.


### Running the simulations

The present software uses the Slurm Workload Manager to run separate trials in parallel.
To run the simulations for a contiguous range of configurations, first navigate to the `HDIV` directory.
Then, use the `src/run.sh` script with first and second command line arguments denoting the first and last configuration numbers of the desired range.
If the second argument is omitted, the range will be taken to consist solely of the first argument.
An example usage is
```
src/run.sh 1 10
```



## Reference

[Inference for high-dimensional nested regression](https://arxiv.org/abs/1708.05499)

Authors: **David Gold, Johannes Lederer, Jing Tao** &mdash; University of Washington

Cite as "Inference for high-dimensional nested regression, *Gold, Lederer, and Tao,* arXiv:1708.05499, 2017"

## Repository authors

* **[David Gold](dag89@uw.edu)** &mdash; Graduate student in Statistics, University of Washington &mdash; *main repository author*

* **[Johannes Lederer](ledererj@uw.edu)** &mdash; Assistant Professor in Statistics, University of Washington



