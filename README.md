# ADS Project 5: 

Term: Spring 2017

+ Team 14
+ Projec title: Lorem ipsum dolor sit amet
+ Team members
	+ Chen, Liangbin lc3190@columbia.edu
	+ Jiang, Yi 
	+ Li, Yaqin yl3578@columbia.edu
	+ Zhao, Boya bz2294@columbia.edu
+ Project summary: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) 
Boya Zhao: feature extraction(about Similiarity of length and differnt type of word), model training, writing final report and presentation.

Liangbin Chen: feature extraction(about the interrogative pairs and Similiarity), model training and help preparing presentation.

Yaqin Li: feature extraction(about sentiment, modal, privative), model training and help preparing presentation.

Yi Jiang: Give some idea in discussion
Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Where else but Quora can a physicist help a chef with a math problem and get cooking tips in return? Quora is a place to gain and share knowledge—about anything. It’s a platform to ask questions and connect with people who contribute unique insights and quality answers. This empowers people to learn from each other and to better understand the world.

Over 100 million people visit Quora every month, so it's no surprise that many people ask similarly worded questions. Multiple questions with the same intent can cause seekers to spend more time finding the best answer to their question, and make writers feel they need to answer multiple versions of the same question. Quora values canonical questions because they provide a better experience to active seekers and writers, and offer more value to both of these groups in the long term.

Currently, Quora uses a Random Forest model to identify duplicate questions. In this competition, Kagglers are challenged to tackle this natural language processing problem by applying advanced techniques to classify whether question pairs are duplicates or not. Doing so will make it easier to find high quality answers to questions resulting in an improved experience for Quora writers, seekers, and readers.

Firstly, We try to generate some features based on Similiarity of 2 questions(eg. length, word type, interrogative, sentiment, modal, privative).

Then, we try different models(SVM, Random forest, ANN, gbm and adaboost) and different set of features. We select the models and features set with best test error performance. 

As the result, we reduce the test error rate to about 20%. 

Because the time limit, we do not have time to further improve the performance.
There are some possible potential improvement for this problem:
1 try to calcuate the word frequency and decrease the dimensions to acceptible level.
2 try to use more advanced models.
