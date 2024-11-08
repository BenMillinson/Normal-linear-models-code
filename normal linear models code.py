import pandas as pd
import numpy as np
from scipy import stats
import statsmodels.api as sm
import statsmodels.formula.api as smf
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split

# Decimal place options (equivalent to options(digits = x) in R)
pd.set_option('display.float_format', '{:.2f}'.format)  # Adjust the float display if needed

# -----------------
# Correlations
# -----------------
# Pearson correlation
m1_corr = stats.pearsonr(df_x['variable_x'], df_x['variable_y'])

# -----------------
# Regressions
# -----------------
# Simple linear regression
m1_reg = smf.ols('variable_x ~ variable_y', data=df_x).fit()
# Multiple linear regression with additional predictors
m2_reg = smf.ols('x ~ y + a + b', data=df_x).fit()

# -----------------
# Making predictions with regressions
# -----------------
# New data for predictions
new_data = pd.DataFrame({'variable_x': [x], 'variable_y': [y]})
predictions = m1_reg.predict(new_data)

# -----------------
# Confidence intervals with t-distribution
# -----------------
# 95% confidence interval for t-distribution with 25 degrees of freedom
lower_bound = stats.t.ppf(0.025, df=25)
upper_bound = stats.t.ppf(0.975, df=25)

# -----------------
# Confidence intervals from models
# -----------------
# 95% CI
conf_int_95 = m1_reg.conf_int(alpha=0.05)
# 99% CI
conf_int_99 = m1_reg.conf_int(alpha=0.01)

# -----------------
# Dummy variables in regression
# -----------------
# Fit model with a categorical predictor
m_x = smf.ols('variable_x ~ C(variable_y)', data=df_x).fit()
pred_new_data = m_x.predict(pd.DataFrame({'variable_y': ["x", "y"]}))

# -----------------
# T-tests
# -----------------
# Independent t-test, paired or unpaired
t_test = stats.ttest_ind(df_x['variable_x'], df_x['group_x'], equal_var=True)
# Test hypothesis with a specific mean difference
t_test_hyp = stats.ttest_1samp(df_x['variable_x'] - df_x['group_x'], popmean=-5)

# -----------------
# Area under t-distribution curve
# -----------------
# Probability of observing a t-value greater than 1.5 with df=10
p_value_one_tail = stats.t.sf(1.5, df=10)
p_value_two_tail = stats.t.sf(1.5, df=10) * 2

# -----------------
# Confidence intervals from a t-test
# -----------------
# Calculate standard error
se = np.sqrt(((n_x - 1) * s_x**2 + (n_y - 1) * s_y**2) / (n_x + n_y - 2)) * np.sqrt(1/n_x + 1/n_y)
# 95% CI for the difference in means
diff = xbar - ybar
ci_95 = (diff - se * stats.t.ppf(0.975, df=n_x + n_y - 2), 
         diff + se * stats.t.ppf(0.975, df=n_x + n_y - 2))

# -----------------
# ANOVAs
# -----------------
# One-way ANOVA
aov = smf.ols('variable_x ~ C(group_x)', data=df_x).fit()
anova_table = sm.stats.anova_lm(aov, typ=2)

# -----------------
# Multilevel models (random effects)
# -----------------
# For mixed-effects models, use the `statsmodels` library's mixed linear model
from statsmodels.regression.mixed_linear_model import MixedLM

# Random intercept model
m_x = MixedLM.from_formula('var_x ~ x1', groups='subject', data=df_x).fit()
# Random slopes and intercepts model
m_y = MixedLM.from_formula('var_x ~ x1', groups='subject', re_formula="1 + x1", data=df_x).fit()

# -----------------
# Confidence intervals using normal distribution
# -----------------
stdev = x
mu = y
ci_norm_95 = [stats.norm.ppf(0.025, loc=mu, scale=stdev), 
              stats.norm.ppf(0.975, loc=mu, scale=stdev)]
# Adjust the ppf values for different confidence levels if needed

