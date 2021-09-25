# College-Enrollment-Prediction

**GOAL** 
To predict Enrollment in a college using Regression , Decision tree and comparing best model

**About**
In the fall of 2019, the administration of a large private university requested that the Office of Enrollment Management and the Office of Institutional Research work together to identify prospective students who would most likely enroll as new freshmen in the Fall 2020 semester. Historically, inquiries numbered about 90,000+ students, and the university enrolled from 2400 to 2800 new freshmen each Fall semester. It was decided that inquiries for Fall 2019 would be used to build the model to help shape the Fall 2020 freshman class. The data set INQ2019 was built over a period of a several months in consultation with Enrollment Management.

**Libraries used**
ggplot2, caret, dplyr, car, e1071, pROC, modeest, tidyverse, rpart,rpart.plot

**Conclusion**

I will choose regression model over decision tree model by comparing area under the curve ROC but the tradeoff I must think of will be simplification for regression model. I had to clean data and remove certain variables for regression analysis whereas for decision trees I did not.
For regression : Area under the curve : 0.9786
For decision tree: Area under the curve: 0.8714
