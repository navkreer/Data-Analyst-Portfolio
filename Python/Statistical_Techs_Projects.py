import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
import statsmodels.api as sm
from statsmodels.stats.diagnostic import het_breuschpagan
import statsmodels.graphics.gofplots as smgof
from scipy.stats import shapiro

## Project 1: Analysis of Quantity of Sleep in Groups using ANOVA

# Data Processing
data = pd.read_csv("C:/Users/navxk/Desktop/Sleep.csv")

measurements = data['Ave.Sleep.per.Day']
treatments = np.tile(['2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017'], 6)
blocking = data['Age.Group']
Sleep = pd.DataFrame({'measurements': measurements, 'treatments': treatments, 'blocking': blocking})

# Split data by year
years = [str(i) for i in range(2003, 2018)]
yeardata = {year: data[data['Year'] == year]['Ave.Sleep.per.Day'] for year in years}

# Split data by age group
agegroups = ['15 to 24 years', '25 to 34 years', '35 to 44 years', '45 to 54 years', '55 to 64 years', '65 years and over']
agedata = {agegroup: data[data['Age.Group'] == agegroup]['Ave.Sleep.per.Day'] for agegroup in agegroups}

agemeans = {age_group: np.mean(agedata[age_group]) for age_group in agegroups}
print(agemeans)

# Boxplot for the entire dataset by treatments (years)
plt.figure(figsize=(10, 6))
sns.boxplot(x=treatments, y=measurements)
plt.title("Average Sleep Duration of all Age Groups in the US from 2003 - 2017")
plt.xlabel("Year")
plt.ylabel("Average Hours of Sleep")
plt.xticks(rotation=90)
plt.show()

# Plot for each age group
for age_group, data in agedata.items():
    plt.figure(figsize=(10, 6))
    plt.plot(years, [np.mean(data[data['Year'] == year]) for year in years], marker='o')
    plt.title(f"Average Sleep Duration of {age_group} in the US from 2003 - 2017")
    plt.xlabel("Year")
    plt.ylabel("Average Hours of Sleep")
    plt.xticks(rotation=90)
    plt.show()

# ANOVA Test
model = ols('measurements ~ treatments + blocking', data=Sleep).fit()
anovaresults = anovalm(model)
residuals = model.resid

# Q-Q plot for normality
smgof.qqplot(residuals, line ='45')
plt.title("Normal Q-Q Plot of the Residuals")
plt.show()

# Shapiro-Wilk test
shapirotest = shapiro(residuals)
print(f"Shapiro-Wilk Test for Normality: {shapirotest}")

# Levene's Test for variance homogeneity (using scipy.stats)
from scipy.stats import levene
levenetreatment = levene(*[data[data['Year'] == year]['Ave.Sleep.per.Day'] for year in years])
leveneagegroup = levene(*[data[data['Age.Group'] == group]['Ave.Sleep.per.Day'] for group in agegroups])

print(f"Levene's test for treatments: {levenetreatment}")
print(f"Levene's test for blocking (age groups): {leveneagegroup}")

# Residuals vs Fitted plot
fitted_values = model.fittedvalues
plt.figure(figsize=(10, 6))
plt.scatter(fitted_values, residuals)
plt.axhline(0, color='red', linestyle='--')
plt.xlabel("Fitted Values")
plt.ylabel("Residuals")
plt.title("Treatment Residuals vs Fitted Plot")
plt.show()

# Residuals vs Fitted plot for age groups
fittedagevalues = np.tile(list(agemeans.values()), 15)
plt.figure(figsize=(10, 6))
plt.scatter(fittedagevalues, residuals)
plt.axhline(0, color='red', linestyle='--')
plt.xlabel("Fitted Values")
plt.ylabel("Residuals")
plt.title("Block Residuals vs Fitted Plot")
plt.show()

print(anovaresults)

# Tukey's HSD Test
from statsmodels.stats.multicomp import pairwise_tukeyhsd
tukey = pairwise_tukeyhsd(Sleep['measurements'], Sleep['treatments'], alpha=0.05)
print(tukey.summary())

# Generate Tukey labels
tukeylabels = pd.DataFrame(tukey.summary().data)[['group1', 'group2', 'meandiff', 'p-adj', 'reject']]
print(tukeylabels)

# Create boxplot with Tukey HSD labels
plt.figure(figsize=(10, 6))
boxplot = sns.boxplot(x='treatments', y='measurements', data=Sleep)
plt.title("Average Sleep Duration of all Age Groups in the US from 2003 - 2017")
plt.xlabel("Year")
plt.ylabel("Average Hours of Sleep")

# Overlay Tukey labels
for i in range(len(tukeylabels)):
    x_pos = i
    label = tukeylabels['reject'][i]
    boxplot.text(x_pos, boxplot.get_ylim()[1], label, ha='center', color='red', fontweight='bold', fontsize=12)

plt.show()

## Project 2: Sample Survey Analysis of Caffeine Intake
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
import statsmodels.api as sm
from scipy.stats import norm
from sklearn.utils import resample

# Load the data
data = pd.read_csv("CaffeineDataset.csv")

# Sleep quality bar graph
plt.figure(figsize=(8, 6))
sns.histplot(data['sleepquality'], bins=10, kde=False, color="lightblue", edgecolor="black")
plt.xlabel("Sleep Quality")
plt.ylabel("Count")
plt.title("Visual Representation of Sleep Quality Data")
plt.show()

# Hours of sleep histogram
plt.figure(figsize=(8, 6))
sns.histplot(data['hoursofsleep'], bins=10, kde=False, color="lightblue", edgecolor="black")
plt.xlabel("Hours of Sleep")
plt.ylabel("Count")
plt.title("Visual Representation of Hours of Sleep")
plt.show()

# Sleep satisfaction histogram
plt.figure(figsize=(8, 6))
sns.histplot(data['sleepsatisfaction'], bins=10, kde=False, color="lightblue", edgecolor="black")
plt.xlabel("Sleep Satisfaction (10 max satisfaction)")
plt.ylabel("Count")
plt.title("Visual Representation of Sleep Satisfaction")
plt.show()

# Box plot of caffeine consumption
plt.figure(figsize=(8, 6))
sns.boxplot(x=data['totalmg'], color="lightblue", edgecolor="black")
plt.xlabel("Caffeine Consumption in mg")
plt.title("Box Plot of Caffeine Consumption")
plt.show()

# Fatigue bar graph
plt.figure(figsize=(8, 6))
sns.countplot(x=data['fatigue'], palette="Blues", edgecolor="black")
plt.xlabel("Individual's Often Feeling Fatigue Throughout The Day")
plt.ylabel("Count")
plt.title("Visual Representation of Feelings of Fatigue")
plt.show()

# Proportion of undergraduates consuming caffeine
n = 54
N = 25000
caffeineEntries = data[data['totalmg'] > 0.0]
X = len(caffeineEntries)
phat = X / n
Ssquared = (n * phat * (1 - phat)) / (n - 1)
varphat = ((N - n) / N) * (Ssquared / n)
SE = np.sqrt(varphat)
lower = phat - norm.ppf(0.975) * SE
upper = phat + norm.ppf(0.975) * SE

print(f"Proportion of undergraduates consuming caffeine: {phat}")
print(f"Confidence interval: ({lower}, {upper})")

# Proportion of undergraduates consuming more than 400 mg of caffeine
caffeineEntries400 = data[data['totalmg'] > 400.0]
X400 = len(caffeineEntries400)
phat400 = X400 / n
Ssquared400 = (n * phat400 * (1 - phat400)) / (n - 1)
varphat400 = ((N - n) / N) * (Ssquared400 / n)
SE400 = np.sqrt(varphat400)
lower400 = phat400 - norm.ppf(0.975) * SE400
upper400 = phat400 + norm.ppf(0.975) * SE400

print(f"Proportion of undergraduates consuming more than 400 mg: {phat400}")
print(f"Confidence interval: ({lower400}, {upper400})")

# Proportion of undergraduates experiencing difficulty falling asleep who also consume caffeine (Ratio Estimation)
sleepdifficulty = (data['sleepdifficultynum'] >= 3).astype(int)
caffeineusage = (data['totalmg'] > 0).astype(int)
sumy = np.sum(sleepdifficulty)
sumx = np.sum(caffeineusage)
rhat = sumy / sumx

meanx = np.mean(caffeineEntries['totalmg'])
Sr = np.sum((sleepdifficulty - rhat * caffeineusage) ** 2) / (n - 1)

lowerratio = rhat - norm.ppf(0.975) * np.sqrt((1 / meanx ** 2) * ((N - n) / N) * (Sr / n))
upperratio = rhat + norm.ppf(0.975) * np.sqrt((1 / meanx ** 2) * ((N - n) / N) * (Sr / n))

print(f"Ratio estimate: {rhat}")
print(f"Confidence interval for ratio: ({lowerratio}, {upperratio})")

# Bootstrap for Mean of Caffeine Consumption
cafwo55 = data.drop(index=range(55, 67))
totalmg = cafwo55['totalmg']
Nresamples = 2000
bootmean = np.zeros(Nresamples)
bootsd = np.zeros(Nresamples)

for i in range(Nresamples):
    bootdata = resample(totalmg, n_samples=n, replace=True)
    bootmean[i] = np.mean(bootdata)
    bootsd[i] = np.std(bootdata)

bootstrap = pd.DataFrame({'bootmean': bootmean, 'bootsd': bootsd})
print(bootstrap.head())

# Visualizing the bootstrap distribution of the sample mean
plt.figure(figsize=(8, 6))
sns.histplot(bootstrap['bootmean'], bins=20, kde=False, color="blue")
plt.title("Bootstrap Distribution of the Sample Mean Amount of Caffeine Consumed")
plt.xlabel("Mean of the Sample")
plt.ylabel("Frequency")
plt.show()

# Compute 95% confidence interval using the bootstrap
meanboot = np.mean(bootstrap['bootmean'])
CI = np.percentile(bootstrap['bootmean'], [2.5, 97.5])
print(f"95% confidence interval for the mean caffeine consumption: {CI}")

# Compute Margin of Error using Proportion
def proMoE(N, n, p, conflevel):
    a = norm.ppf(conflevel + (1 - conflevel) / 2)
    b = np.sqrt((p * (1 - p) / (n - 1)) * ((N - n) / N))
    return a * b

# Compute Margin of Error using Mean
def meanMoE(N, n, sd, conflevel):
    tst = stats.t.ppf(conflevel + (1 - conflevel) / 2, df=n - 1)
    vpcf = np.sqrt((sd ** 2 / n) * ((N - n) / N))
    return tst * vpcf

# Proportion of undergrads with <= 6 hours of sleep
lt6 = cafwo55[cafwo55['hoursofsleep'] <= 6]
p_lt6 = len(lt6) / len(cafwo55)
print(f"Proportion of undergrads with <= 6 hours of sleep: {plt6}")

ub7 = plt6 + proMoE(N, n, plt6, 0.95)
lb7 = plt6 - proMoE(N, n, plt6, 0.95)
print(f"Confidence interval for proportion with <= 6 hours of sleep: ({lb7}, {ub7})")

# Proportion of undergrads struggling to be productive without caffeine
numagree = 13 + 14
propagree = numagree / n
print(f"Proportion of undergrads struggling without caffeine: {propagree}")

ub8 = propagree + proMoE(N, n, propagree, 0.95)
lb8 = propagree - proMoE(N, n, propagree, 0.95)
print(f"Confidence interval for struggling without caffeine: ({lb8}, {ub8})")

# Mean fatigue level
fatiguenum = cafwo55['fatiguenum'].astype(int)
mean_fatigue = np.mean(fatiguenum)
ub9 = np.mean(fatiguenum) + meanMoE(N, n, np.std(fatiguenum), 0.95)
lb9 = np.mean(fatiguenum) - meanMoE(N, n, np.std(fatiguenum), 0.95)
print(f"Confidence interval for mean fatigue level: ({lb9}, {ub9})")

# Mean hours before bed undergrads consume caffeine
ub = np.mean(cafwo55['hoursbeforebednocaff']) + meanMoE(N, n, np.std(cafwo55['hoursbeforebednocaff']), 0.95)
lb = np.mean(cafwo55['hoursbeforebednocaff']) - meanMoE(N, n, np.std(cafwo55['hoursbeforebednocaff']), 0.95)
print(f"Confidence interval for hours before bed with caffeine: ({lb}, {ub})")

# Proportion of undergrads experiencing negative symptoms after caffeine consumption
prop11 = 15 / (5 + 34 + 15)
ub11 = prop11 + proMoE(N, n, prop11, 0.95)
lb11 = prop11 - proMoE(N, n, prop11, 0.95)
print(f"Confidence interval for negative symptoms after caffeine: ({lb11}, {ub11})")
