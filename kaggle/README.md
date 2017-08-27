Kaggle competition
=========


# Can we predict voting outcomes?


## Competition details


### Can we accurately predict voting outcomes by using informal polling questions?

Please note: this competition is only open to students of 15.071x - The Analytics Edge.

What predicts voting outcomes? In this competition, you'll be using data from Show of Hands, an informal polling platform for use on mobile devices and the web, to see what aspects and characteristics of people's lives predict how they will be voting for the presidential election.

Show of Hands has been downloaded over 300,000 times across Apple and Android app stores, and users have cast more than 75 million votes. In this problem, we'll use data from thousands of users and one hundred different questions to see which responses predict voting outcomes.


### Acknowledgements

This competition is brought to you by 15.071x, edX, and Show of Hands.

- Started: 2:12 pm, Tuesday 24 May 2016 UTC 
- Ends: 11:59 pm, Monday 13 June 2016 UTC (20 total days) 
- Points: this competition does not award ranking points 
- Tiers: this competition does not count towards tiers


## Get the data


### Data Files

- Questions.pdf (41.07 kb)
- test2016.csv (523.16 kb)
- train2016.csv (2.10 mb)
- sampleSubmission2016.csv (20.16 kb)


### File descriptions

Here is a description of the files you have been provided for this competition: 

* **train2016.csv** - the training set of data that you should use to build your models
* **test2016.csv** - the test set that you will be evaluated on. It contains all of the independent variables, but not the dependent variable.
* **sampleSubmission2016.csv** - a sample submission file in the correct format.
* **Questions.pdf** - the question test corresponding to each of the question codes, as well as the possible answers.


### Data fields

* **USER_ID** - an anonymous id unique to a given user
* **YOB** - the year of birth of the user
* **Gender** - the gender of the user, either Male or Female
* **Income** - the household income of the user. Either not provided, or one of "under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "$100,001 - $150,000", or "over $150,000".
* **HouseholdStatus** - the household status of the user. Either not provided, or one of "Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", or "Single (w/kids)".
* **EducationalLevel** - the education level of the user. Either not provided, or one of "Current K-12", "High School Diploma", "Current Undergraduate", "Associate's Degree", "Bachelor's Degree", "Master's Degree", or "Doctoral Degree".
* **Party** - the political party for whom the user intends to vote for. Either "Democrat" or "Republican
* **Q124742, Q124122, . . . , Q96024** - 101 different questions that the users were asked on Show of Hands. If the user didn't answer the question, there is a blank. For information about the question text and possible answers, see the file Questions.pdf.


## Make a submission


### File Format

Your submission should be in CSV format. You can upload this in a zip/gz/rar/7z archive if you prefer.


### # of Predictions

We expect the solution file to have 1,392 predictions. The file should have a header row. Please see the sample submission file on the data page for an example of a valid submission.


## Information


### Evaluation

The evaluation metric for this competition is Categorization Accuracy. This is the number of category predictions that match the actual category, divided by the total number of observations. Take the following example:

| USER_ID | Actual       | Prediction   |
| ------- | ------------ | ------------ |
| 123     | "Democrat"   | "Democrat"   |
| 124     | "Republican" | "Democrat"   |
| 130     | "Republican" | "Republican" |
| 145     | "Democrat"   | "Republican" |

In this case, two predictions match the actual category out of four total observations. Consequently, the Categorization Accuracy in this case is 0.5. 


### Submission Format

**For every author in the dataset**, submission files should contain two columns: USER_ID and PREDICTION. Use comma-delimited submission files. 

The file should contain a header and have the following format:

```
USER_ID,PREDICTION
123,Democrat
124,Democrat
130,Republican
145,Republican
etc.
```


## Rules


### Competition Rules


#### One account per participant

You cannot sign up to Kaggle from multiple accounts and therefore you cannot submit from multiple accounts.


#### No private sharing outside teams

Privately sharing code or data outside of teams is not permitted. It's okay to share code if made available to all participants on the forums.


#### Team Mergers

Team mergers are not allowed in this competition.


#### Team Limits

The maximum size of a team is 1 participant.


#### Submission Limits

You may submit a maximum of 5 entries per day.
You may select up to 5 final submissions for judging.


#### Competition Timeline

- Start Date: **5/24/2016 2:12:57 PM UTC**
- Merger Deadline: **None**
- First Submission Deadline: **None**
- End Date: **6/13/2016 11:59:00 PM UTC**


#### Rules Acceptance

You accepted these rules at 9:59 pm, Sunday 29 May 2016 UTC.