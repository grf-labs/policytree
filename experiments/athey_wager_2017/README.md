### Replication Files

This directory contains replication code for the paper:

Susan Athey and Stefan Wager.
<b>Efficient Policy Learning.</b> 2017.
[<a href="https://arxiv.org/abs/1702.02896">arxiv</a>]

All simulation experiments can be replicated by running provided scripts. To produce Figure 2, run `figure_2_run.R` followed by `figure_2_make_plot.R`, and similarly for Table 3. The file `gain_analysis.R` contains all the code used to analyze the GAIN dataset; unfortunately, however, we are unable to publicly post the dataset at this time.

The analyses in the paper were run with policytree version 1.0 and grf version 1.2. Further information on the computational environment used is provided as a comment at the top of each script.
