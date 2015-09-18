# PowerSim
Code and output supporting a working StableMarkets post about p-values.

The code creates a data-generating process. The simulation then continually generates data from this DGP, runs a univariate regression, and extracts the p-value of the beta coefficient.

The regression is repeated for betas of different sizes in the DGP (effect size) and multiple sample sizes of the DGP.
