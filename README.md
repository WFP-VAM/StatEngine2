
#New Version of StatEngine

This is version is designed to produce estimates from surveys using complex survey sampling designs of all types. At it's core, it is wrapper around the R-Survey package built by Thomas Lumley.

##Scripts
###Input
+CreateSvyDesign: Entry poinnt, called after data from database is coalesced into 'InputDB' list in R. This contains all the parameters necessary to produce a survey design object returned by the R Survey Package. If multiple surveys in data, then a 'multiSvyDsgnObj' will be created. This contains each survey as individual survey objects and a stacked object where the multiple surveys are coalesced into a single survey design wherin each survey is treated a separate strata. Additional inputs can be passed to perform calibration and post-stratification.
+conditionVars: Creates new variables in the survey design object from existing variables but transformed by conditions specified by the user (i.e. log transform)
###For Calculations (core functions are calcSumStats & calc2WayCmp)
+calsSumStats: Calculates summary statistics on survey design object on target variables specified by conditionVars
+calcTrends: like calcSumStats but over time with automatic fitting of linear models for trends over time (possible to do is to add quadratic terms and change lm on log-odds to glm for binomials)
+calcANOVA: performs ANOVA and regression analysis to inform which if any variables and associated interactions are significant
+calcMultCmp: Performs post-hoc interaction analysis to uncover relationships between specific categories amongst variables (i.e do any significant differences exist between interaction terms--interaction terms can be groups); works to compare groups within surveys
+calc2WayCmp: yet to be written! Performs Diff-in-diff to see differences between groups over time.
###For Calculations (core functions are calcSumStats & calc2WayCmp)
+plotSumStats: plotting summary stats

As to goal of the StatEngine is to drive dashboards via automated tabulation of results from survey data. Plotting functions are probably unnecessary. However a database should be created where the reults can be cached and a wrapper function to retrieve those results from said database before calling the StatEngine.





