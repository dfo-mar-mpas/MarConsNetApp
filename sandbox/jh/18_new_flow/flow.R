# Fish Condition Weight

# 1. Locate the data and either use data frame
# from source or create a project_glider() function in MarConsNetData.

# In this case, I could have created MarConsNetData function, but
# instead I'm using the data frame from the source.

tar_load(gsdet)

# 2. Update dataTable in MarConsNetData

# In this case, I used a target (not a function) so I have a tar_load in
# the get function

# 3. Add indicator to indicator_binning in MarConsNetAnalysis

# Done.

# 4. Apply to plot_trend_status

