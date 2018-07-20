#####################################
# Assignment SPICT GROUP
#####################################


#1 Load data from Sardine in the Adriatic (CatchPIL.csv) and fit spict model. The biomass index was build by collapsinge the ECHO WEST acoustic index (WAA * NAA). Check all diagnostics (convergence, residuals and retro)!

#2 If no or poor convergence, put initial values for K and priors. To see how to do it, check in the SPICT Vignette how to do this.


#3 Is the timing of the survey correct as being set on an annual basis? # Set it to the correct one and rerun to see if it improves the fit.
# Ask Ilaria Costantini the exact months when the survey was actually performed. Hint, it might not converge with the strating values of K and the priors turned on.
# up to 2014 survey was in September
# from 2015 included survey performed in June/July


#4 If you have time, Run a short term forecast where you provide a short term forecast (+1 and +3 years) with predicted catch under the following scenario:
#Keep fishing at the F of the terminal year (Fstatus quo)
#Fishing at Fmsy or F0.1


# 5 At the end of the presentations: 
# Compare SPCIT fit to a4a fit. Is the perception of the stock status similiar or different in the recent years?


#############################################################
# Assignment a4a GROUP
#############################################################

# 1 import data from SARDINE FLStock and FLindex
# verify the correct setting of m.spawn and harvest.spawn. 

# 2 Find the best Fmodel, Qmodel and possibly SR model
# Use all the available diagnostics

# 3 Determine the best fitting model, make sure that the Hessian matrix is computed and the a4a model converged.
# Through simulation compute uncertainity on the estimated parameters

# 4 Change the harvest.spwn = 0.5 in the final run to check the effect on the assessment estimates

# 5 Compute reference points for this stock and compare Fbar to the reference point of choice (F0.1 or Fmsy or something else)

# If you have time, Run a short term forecast where you provide a short term forecast (+1 and +3 years) with predicted catch under the following scenario:
#  Keep fishing at the F of the terminal year (Fstatus quo)
#Fishing at Fmsy or F0.1

# At the end of the presentations: 
# Compare a4a fit to SPCIT fit. Is the perception of the stock status similiar or different in the recent years?

