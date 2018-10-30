---
title: 'survxai: an R package for structure-agnostic explanations of survival models'
authors:
- affiliation: 1, 2
  name: Aleksandra Grudziaz
  orcid: 0000-0002-5963-6260
- affiliation: 2
  name: Alicja Gosiewska
  orcid: 0000-0001-6563-5742
- affiliation: 1, 2
  name: Przemyslaw Biecek
  orcid: 0000-0001-8423-1823
date: "23 August 2018"
output: pdf_document
bibliography: paper.bib
tags:
- survival analysis
- explainable artificial intelligence
- predictive modeling
- interpretable machine learning
affiliations:
- index: 1
  name: Faculty of Mathematics and Information Science, Warsaw University of Technology
- index: 2
  name: Faculty of Mathematics, Informatics, and Mechanics, University of Warsaw
---

# Introduction

<!-- A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience-->

Predictive models are widely used in supervised machine learning. Three most common classes of such models are: *regression models*, where the target variable is continuous numeric, *classification models*, where the target variable is binary or categorical and *survival models*, where the target is some censored variable. Common examples of censored variables are times to death (but some cases lived for x years and are still alive), cessation of service by customer, or failure of machine components.

Modern survival models are often complex in structure; for example survival neural networks [@ELEUTERI2003855] or survival random forest [@randomForestSRC]. These models may be described by thousands of coefficients. Often such flexibility leads to high performance, but makes models opaque and hard to understand. This is acceptable in cases where only the model accuracy is important, but in cases that involve human decisions, it may not be informative enough. To trust model predictions one needs to see which features are important and how model predictions would change if some feature was changed.

The area of model interpretability or explanability has quickly gained the attention of machine learning experts. Understanding of complex models leads not only to higher trust in model predictions but also better models. Better, means that the models are more robust and obtains higher accuracy on validation data. See examples in the `DALEX`  [@2018arXiv180608915B] or `iml` [@Molnar2018] R packages.

Existing tools for model agnostic explanations are focused on regression models and classification problems, as in both cases model predictions that may be summarised by a single number. Survival models require different approach as predictions are in a form of survival functions. Demand for such explainers has led to some model specific solutions, like `iSurvive` [@pmlr-v70-dempsey17a] for continuous time hidden Markov models. Yet, there is currently a lack of structure agnostic tools for survival models.

The `survxai` fills this gap. This R package is designed to deliver local and global explanations for survival models, in a structure-agnostic fashion. In the package documentation we demonstrate examples for survival random forest models and for Cox models.
The `survxai` package consists of new implementations and visualisations of explainers, designed for survival models. Functions are well documented and the package is supplemented with unit tests, and illustrations.
Regardless of the complexity of the model, the methods implemented in the `survxai` package maintain a certain level of interpretability, important in medical applications [@collett2015modelling], churn analysis [@lu2003modeling] and others.


<!-- A clear statement of need that illustrates the purpose of the software-->
# Explanations of survival models
 
The R package `survxai` is a tool for creating explanations of survival models. It is structure-agnostic, and thus works for both complex and simple survival models. It also allows for comparisons between two or more models.
 
Currently, four classes of model explainers are implemented. Two for local explanations (for a single prediction), and two for global explanations (for a whole model and population).

The package `survxai` is available on CRAN. It can be install using the command `install.packages('survxai')`. The development version of the package can be found at [`https://github.com/MI2DataLab/survxai`](https://github.com/MI2DataLab/survxai).

**Local methods** are the explanations of a single observation.
 
- **The Ceteris Paribus** profile presents model responses around a single point in the feature space [@ceterisParibus]. See Figure 1 for an example. Each panel is related to a single variable. Each single panel shows how a model prediction (survival curve) would change if only a single variable were changed. It is useful for *what-if* reasoning. Each curve in a panel is related to a different value of the selected variable.
The Ceteris Paribus profile illustrates how the survival curve may change with the changing of values of variable.

![Ceteris Paribus plot for survival random forest model with three variables. The black dashed survival curve corresponds to an observation of interest. The left panel shows the survival curves for different values of bilirubin. Colors correspond to mean survival curves of observations from quintiles. From red which is the first quintile to blue which is the last one. The middle panel shows that prediction for sex=0 is worse than for sex=1 for times less than 7.5. The right panel analogously shows survival curves for different levels of the variable stage.](img/ceteris_paribus.png)

- **The Break Down** plot presents variable contributions to a model prediction [@2018arXiv180401955S]. See Figure 2 for an example. The Break Down of predictions for survival models help to understand which factors drive survival probabilities for a single observation.

![Break Down plot for survival random forest model. Variables bili and stage have the highest impact on the final prediction.](img/breakdown.png)
 
**Global methods** are explanations for performance and model structure.
 
- **The Variable Response** plot is designed to better understand the relation between a variable and a model output. See Figure 3 for an example. The variable response plot illustrates how the mean survival curve changes along with the changing values of the variable. It is inspired by Partial Dependence Plots [@RJ-2017-016].

![Variable response plots for three models and variable sex. In survival random forest, the sex variable affects model predictions in a different way than in  other models.](img/variable_response.png)
 
- **The Model Performance** curves present prediction error for the chosen survival model, depending on time. See Figure 4 for an example. For computing prediction error, we use the expected Brier Score [@BSScore]. At a given time point t, the Brier score for a single observation is the squared difference between observed survival status and a model-based prediction of surviving time $t$.

![Model performance plots for three models. In random forest model, predictions are less accurate after year 4.](img/model_performance.png)

<!-- Mentions (if applicable) of any ongoing research projects using the software or recent scholarly publications enabled by it -->

# Conclusions and future work

Explainers implemented in the `survxai` package allow for exploration of one or more models in a feature-by-feature fashion. This approach will miss interactions between variables that may be handled by the models. The main problem with interactions is that number of interactions grows rapidly with the number of features what makes it hard to present in a readable form.

# Acknowledgments

The work was supported by NCN Opus grant 2016/21/B/ST6/02176.

<!-- A list of key references including a link to the software archive -->

# References

