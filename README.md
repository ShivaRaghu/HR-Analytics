## HR Analytics: Job Change of Data Scientists

**Introduction** The companies actively involved in big data and analytics spend money on employees to train and hire them for data scientist positions. However, according to me it seems some candidates leave the company once trained. Hence to reduce the cost on training, company want to predict which candidates are really interested in working for the company and which candidates may look for new employment once trained. On the basis of the characteristics of the employees the HR of the want to understand the factors affecting the decision of an employee for staying or leaving the current job.

**Description of dataset**: The dataset I am planning to use is from kaggle.  The dataset has already been divided into testing and training sets. Training data has 14 features on 19158 observations and 2129 observations with 13 features in testing dataset. 
Some notes about the data: The data is imbalanced, most features are categorical, some with cardinality and missing imputation can be part of pipeline (https://www.kaggle.com/arashnic/hr-analytics-job-change-of-data-scientists?select=sample_submission.csv)

**Goal:** 
1.	Classify the employees into staying or leaving category using predictive analytics classification models
2.	Identify important factors affecting the decision making of staying or leaving using MeanDecreaseGini from RandomForest model.
3.	Create a process in the form of questionnaire to identify employees who wish to stay versus leave using CART model.


**Decision Tree**
![](https://github.com/ShivaRaghu/HR-Analytics.github.io/blob/main/rpart%20plot%20best%202.png)

**Summarize findings to stakeholders:**
Classification models (CART, RandomForest, LASSO, RIDGE) had identified following three variables as significant for the decision making of an employee whether to leave or work for the company.

Variable 1: *Experience*
Insight: Acc. To the RF model, experience is the most important predictor. Employees with less than one year, 1 to 5 year and 6 to 10 year experience tend to leave the job more often than others.
![](https://github.com/ShivaRaghu/HR-Analytics.github.io/blob/main/experience.png)

Recommendation: This could be due to various reasons, and also people with more experience (11+ years) probably are good candidates to screen for when hiring for training that are more likely to stay and work for company.Plus there is a need to explore why people with less than one year or  1-5 year are more likely to leave.

Variable 2: *Last.new.job
(Difference in years between previous job and current job)*

Insight: Lastnewjob is the second most important predictor for employee’s decision according to the random forest model. Someone who is in the current role for 4+ years will more likely to work for company than someone who is in current role for less than an year.

Recommendation: As data suggests that employees who are in the company for less than an year or 1 or 2 years are more likely to leave as compared to someone who is in the company for 4+ years. Hence there is a need to try to understand those employees better with more surveys or more work life balance opportunities as new employees are generally people who are also starting family and trying to balance job with spouse/kids.

Variable 3: *Discipline Major*
Insight: Major Discipline is the 3rd major important predictor of employee’s decision

Recommendation: The data suggests that employees with discipline major “STEM” are more likely to leave than other disciplines(Business, Humanities, Arts, Others). Therefore if an organization want to try to keep an employee then it might be a good idea to have a balance of candidates with other disciplines along with STEM.





