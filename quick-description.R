quick_description <- HTML("<p>This application is a quick explorer to look at the
relationship between a logistic regression model for disease prediction and the ROC curve.
We start with the idea of determining <b>poor glycaemic control</b> (PGC) based on 
fasting plasma blood glucose values.  The 'gold standard' we use is based on an HbA1C
cut-off.  The default is HbA1c &#8805 69.4 mol/mmol (8.5% DCCT units).
[<a href='http://www.ncbi.nlm.nih.gov/pubmed/23665365'>1</a>, 
<a href='http://bit.ly/1I51EnM'>2</a>]</p>  There are two open questions.  First, what is
what is the bese blood glucose value to use to predict PGC?  Second, what if I change
the HbA1c cut-off?

<p>The layout of the dashboard is as follows.  This panel has three tabs. 
<b>Description:</b> What you are reading now. <b>Scatterpot:</b> a scatterplot of 
actual HbA1c and blood glucose readings. <b>Logistic:</b> a logistic regression graphic 
showing the relationship between dichtomisied HbA1c values and continuosu Blood glucose.
The vertical red-dotted line showns the currently selected blood glucose value for predicting PGC.
The points are colored to show true positive (TP), false positive (FP), true negative (TN) and 
false negative (FN) values.

The graphic on the right shows the ROC curve for for predicting PGC using
blood glucose values.[<a href='https://en.wikipedia.org/wiki/Receiver_operating_characteristic'>3</a>]
The red-dot corresponds to the choice of the blood
glucose value used to determine PGC.  The bottom left panel shows the
binary classification matrix for predicting PGC based on blood glucose.
[<a href='https://en.wikipedia.org/wiki/Confusion_matrix'>4</a>]
the bottom right panel allows you to vary the blood glucose value you
use for classifying PGC; and the HbA1c value you use as the Gold Standard
for PGC")

          