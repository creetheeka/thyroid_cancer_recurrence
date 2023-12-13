PH125.9x Data Science Capstone: Differentiated Thyroid Cancer Recurrence

The primary goal of this project is to train machine learning models such as naive Bayes, random forest, support vector machine, k-nearest neighbors, generalized linear model, and gradient boosting machine to predict the likelihood of thyroid cancer recurrence in patients diagnosed with differentiated thyroid cancer. The dataset is available at https://archive.ics.uci.edu/dataset/915/differentiated+thyroid+cancer+recurrence (Borzooei, Shiva and Tarokhian, Aidin. (2023). Differentiated Thyroid Cancer Recurrence. UCI Machine Learning Repository. https://doi.org/10.24432/C5632J). This dataset is analyzed by utilizing the various tools learned throughout the courses in the PH125.x data science series. The objective is to determine a model that yields the highest score for recall/sensitivity and AUC (Area Under the Curve) among the trained models. This dataset is part of research in the field of AI and Medicine and does not contain any sensitive data.

In this project, several key steps are followed to achieve the objective by exploratory analysis of data including checking for NAs, proportion of target variable, distribution of feature variables through data visualization, and training and validating the models using the dataset provided. All models are trained on the training set which is 80% of the dataset. The validation set is derived from 20% of the dataset. Repeated cross-validation method is employed to estimate the performance of the model. Confusion matrix results are analyzed at each step and eventually, the model with the best metrics for predicting the recurrence of thyroid cancer from the validation set is selected as the best model for this project. After training and validating various models, gradient boosting machine (GBM) yielded the best results when compared to other models trained for this project. 

Even though GBM model yielded the best metrics for this project, various hyperparameters could be fine-tuned to generate better results. Future work would include additional research on much larger datasets and train advanced algorithms that could involve intensive computing. Also, other packages besides caret could be utilized to try various techniques and explore advanced metrics besides cross-validation to develop an ideal model.

To run the R script and to generate the pdf, download the Rmd file and select Knit>Knit to PDF in the R studio. The script will download the dataset from UCI archive. 

Please note when executing R script in RStudio, maximize the application window, so the script does not throw an error about "Error in plot.new() : figure margins too large".

RStudio 2023.09.1+494 "Desert Sunflower" Release (cd7011dce393115d3a7c3db799dda4b1c7e88711, 2023-10-16) for windows
Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.09.1+494 Chrome/116.0.5845.190 Electron/26.2.4 Safari/537.36

R version info:
platform       x86_64-w64-mingw32               
arch           x86_64                           
os             mingw32                          
crt            ucrt                             
system         x86_64, mingw32                  
status                                          
major          4                                
minor          3.2                              
year           2023                             
month          10                               
day            31                               
svn rev        85441                            
language       R                                
version.string R version 4.3.2 (2023-10-31 ucrt)
nickname       Eye Holes     
