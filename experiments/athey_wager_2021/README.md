### Replication Files

This directory contains replication code for the paper:

Susan Athey and Stefan Wager.
<b>Policy Learning With Observational Data.</b> <i>Econometrica 89.1 (2021): 133-161.</i>
[<a href="https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA15732">paper</a>,
<a href="https://arxiv.org/abs/1702.02896">arxiv</a>]

All simulation experiments can be replicated by running provided scripts. To produce Figure 2, run `figure_2_run.R` followed by `figure_2_make_plot.R`, and similarly for Table 3. The file `gain_analysis.R` contains all the code used to analyze the GAIN dataset and produce Table 2 alonge with the agreement_* figures (not included in the final version of the paper); unfortunately, however, we are unable to publicly post the dataset at this time.

The analyses in the paper were run with policytree version 1.0 and grf version 1.2. Further information on the computational environment used is provided as a comment at the top of each script.
