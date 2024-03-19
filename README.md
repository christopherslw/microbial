# Microbial Response to Climate Change
This project is a statistical analysis and multiple comparison of microbial responses to climate change in Tibetan alpine meadows.

## Abstract
  Microbial necromass carbon (MNC) plays a significant role in nutrient cycling and is an especially large contributor to soil organic matter. (Cai et al., 2023) explored the dynamics of MNC across different warming groups in a Tibetan alpine meadow, and found that low-level (+0–1.5°C) warming mostly enhanced the accumulation of MNC compared to the control group. However, the effects of soil layer and interactions between soil physiochemical properties and their contribution to bacterial necromass carbon (BNC) in particular were not explored as in depth. In this paper, I aimed to address this by examining BNC production across different soil layers and by establishing a linear model to describe the relationship between soil properties and BNC levels. Through statistical analyses, including one-way ANOVA and linear regression modeling, we found a significant difference in BNC levels among different soil layers. The results indicated that soil physiochemical properties partially explain the variability observed in BNC levels. Overall, my findings can provide valuable insights into the variation of BNC with soil levels and highlight the importance of abiotic factors in shaping BNC dynamics.


## Introduction
  The Tibetan Plateau is estimated to contain 2% to 3% of the earth's soil organic carbon (Shang et al., 2016). The data collected by (Cai et al., 2023) contained information from a Tibetan alpine meadow about different abiotic factors such as soil physiochemical properties (pH, soil moisture, etc.), different biotic factors (bacterial necromass carbon, fungal necromass carbon, etc.), as well as different treatment groups and soil levels. The relationship between the treatment groups the direct and indirect effects that warming had on the total MNC and its contribution to soil organic carbon was explored in (Cai et al., 2023). To deepen the understanding of the interactions between aboitic and biotic factors on the accumulation of BNC specifically, the complete effects of soil layer must be understood. Soil layer is a large contributor to the availability of nutrients including organic matter distribution in an ecosystem. As a result, soil layer can have large effects on microbial activity and the accumulation of bacterial necromass carbon in an ecosystem. Classical modelling of carbon sequestration places assumptions that soil organic carbon (SOC) is mainly derived from plant root and litter residues (Kallenbach et al., 2016). However, recent evidence suggests the importance of microbial sources of carbon, where microbial processes and life cycles are essential to the stability of soil SOC. By improving our understanding of microbial contributions to soil carbon pools, we can shed further light on the transformation of SOC (Bardgett et al., 2008; Liang et al., 2016).
  
  By specifically investigating the variations in bacterial necromass carbon across different soil layers, this study aims to shed light on the specific effects of soil layer on BNC dynamics. This knowledge is crucial for unraveling the complex interactions between abiotic and biotic factors and their combined influence on microbial activity and the accumulation of BNCithin the ecosystem.Understanding the intricate relationships between soil layers and BNC accumulation can have significant implications for nutrient cycling processes, carbon sequestration, and overall ecosystem sustainability. Moreover, by describing the specific effects of soil layer on BNC, this study contributes to the growing body of knowledge surrounding microbial contributions to soil carbon dynamics.

## Statistical Methods

#### ANOVA: Comparing the mean BNC between soil layers

  To assess the variance of means between soil layers, I performed a one way analysis of variance (ANOVA). In analysis of variance, the total variation in the data is comprised of two components: the variation between groups and the variation within groups. The data was collected from root samples which were randomly selected from a subplot of 0.2m x 0.2m in the Field Observation and Research Station of Alpine Grassland Ecosystem of Tibet, China (Cai, et al., 2023). The data was log transformed, and did not yield a significant p value (p > 0.05) for the Shapiro Wilk test, indicating that the data is normally distributed. Finally, the homoscedasticity was assessed with the Levene Test, which also did not yield a significant p value (p > 0.05), indicating equal variance. Thus all the assumptions are met. The null hypothesis of this ANOVA is that there is no difference in any of the means across soil layer groups. Thus, the alternative hypothesis is that there is a mean difference in at least one of the means across soil layers. For significant results, we proceed with multiple comparison using Tukey's method. 


#### Model: Predicting BNC 
  I used a linear model to describe the effects of the soil layer and the physiochemical properties of the soil on BNC, which was used to predict BNC given the features. The regression coefficients are estimated using the least squares method given by: $$\hat{\beta} = (X^\top X)^{-1} X^\top Y$$ where Y is the vector of observed values for the dependent variable, $X^\top$ is the design matrix X. This minimizes the sum of squared residuals resulting in the best fit line through the data. 
  
  All predictors have linear relationships with BNC, and the residuals are normally distributed (Shapiro Wilk p > 0.05) and as seen in plot (b) of Figure 3. However, the predictors are highly correlated. In order to deal with this, I performed principal component analysis (PCA) on the predictors that are soil physiochemical properties (pH, SM, STN). The principal components are calculated based on the covariance or correlation matrix of the preprocessed data, where the covariance matrix measures the pairwise covariances between variables, while the correlation matrix measures the pairwise correlations. Next, eigendecomposition is performed to yield a set of eigenvalues and their corresponding eigenvectors. Then the eigenvalues are sorted in descending order to determine their importance and the principal component score is calculated by the dot product: $$PC_{ij} = \mathbf{x}_i \cdot \mathbf{v}_j$$ where $\mathbf{x}_i$ represents the centered and scaled values of the ith observation, and $\mathbf{v}_j$ denotes the jth eigenvector. Principal components are orthogonal to each other, meaning they are uncorrelated. This allows for the multicollinearity assumption to be met. For this model, I used the first two principal components which collectively describe 95% of the variance. Finally, the homoscedasticity is assessed with the Levene test (p > 0.05) and the residual vs fitted plot of Figure 3, implying equal variance. Therefore, the assumptions are met for each test. The null hypothesis of this is that there is no significant relationship between the predictors and the response variable. Thus, the alternative hypothesis is that there is a significant relationship between the predictors and the response variable.

## Results

#### ANOVA Results
After performing the ANOVA with the aov function in R, I found a significant difference between the means of log BNC across all soil groups (one-way ANOVA; F-value = 87.6, p-value < 0.05). Thus, I am able to reject the null hypothesis and conclude that there is a significant difference between the means of the BNC in each soil layer. This indicates a significant effect of soil layers on BNC. Additionally, a Tukey HSD test was performed to identify pairwise differences between soil layers. The mean log BNC in the 10-20 cm soil layer is significantly lower compared to the 0-10 cm soil layer (mean difference = -0.726, p < 0.05). Next, the mean log BNC in the 20-30 cm soil layer is significantly lower compared to the 0-10 cm soil layer (mean difference = -0.998, p < 0.05). Finally, the mean log BNC in the 20-30 cm soil layer is significantly lower compared to the 10-20 cm soil layer (mean difference = -0.272, p = 0.003). The 95% confidence interval for every difference does not include zero, indicating a significant difference between soil layer. We then proceed to the Tukey's method for multiple comparison, which yields significant results between each soil level (p-values all less than 0.01). The biggest observed difference was between soil level 1 and 3 (0.9976538, p-value<0.000001). 

#### Regression Results
I used linear regression to explore the relationships between the BNC and some of the physiochemical properties of the soil (pH, soil moisture(SM), and soil total nitrogen (STN)) as shown in Figure 4. The results are the following correlation coefficients calculated with Pearson's product-moment correlation: (a) -0.869225, (b) 0.7562659, and (c) 0.8062282. As seen from the correlation test results, each predictor had high correlation with BNC, indicating that each property is closely related to the BNC.


## Discussion

#### Effects of abiotic factors on BNC production
The study aimed to investigate the factors influencing Bacterial Necromass Carbon (BNC) and develop a predictive model for BNC using linear regression analysis. The findings of our study provide valuable insights into the dynamics of BNC and its association with soil characteristics. The model exhibited a strong fit, with an R-squared value of 0.8191, suggesting that approximately 82% of the variance in BNC can be explained by the selected predictors. The linear regression analysis of each predictor revealed a significant relationship between BNC and the predictors, namely pH, soil moisture (SM), and soil total nitrogen (STN). Notably, pH emerged as a particularly influential variable with a strong negative correlation with BNC (cor = -0.869225). This finding suggests that higher pH levels may inhibit the accumulation of BNC in the soil. The significant relationships between BNC and soil factors emphasize the importance of considering soil characteristics in predicting and monitoring BNC levels. Incorporating these findings into soil management practices can contribute to more sustainable agricultural practices.

#### Soil layer leads to differing BNC levels
Furthermore, the one-way ANOVA results indicated a significant effect of soil levels on BNC, revealing that different soil layers exhibited variations in BNC levels with the differences being statistically significant (p < 0.05). Additionally, the Tukey HSD test revealed that for every group compared to another group, the difference in means is significant. This suggests that BNC distribution is influenced by the stratification of soil layers, highlighting the importance of soil depth when studying BNC dynamics. The observed differences in BNC among soil layers can be attributed to variations in organic matter content, nutrient availability, and microbial activity. Deeper soil layers may provide a more favorable environment for microbial decomposition and bacterial necromass production, resulting in higher BNC levels compared to surface layers. However, further investigations are needed. 

Our findings have important implications for understanding the ecological role of BNC and its potential as an indicator of soil health and microbial activity. The significant relationships between BNC and soil factors emphasize the importance of considering soil characteristics in predicting and monitoring BNC levels. Incorporating these findings into soil management practices can contribute to more effective soil conservation and sustainable agricultural practices.

#### Limitations
There are several limitations within the study. First, the data were collected from only the Tibetan reigon and may not be fully representative of other regions. Additionally, other unmeasured factors not included in our model may also influence BNC levels. As noted above, only abiotic variables were considered in this study, although biotic variables are the main factors that mediate the formation of MNC. (Shao et al., 2018; Yang et al., 2022). Future studies could consider incorporating additional variables and expanding the study to different locations to enhance the generalizability of the findings.


## References

#### Articles
Bardgett, R. D., Freeman, C., & Ostle, N. J. (2008). Microbial contributions to climate change through carbon cycle feedbacks. The ISME Journal, 2, 805– 814. https://doi-org.proxy.library.ucsb.edu/10.1038/ISMEJ.2008.58

Cai, M., Zhao, G., Zhao, B., Cong, N., Zheng, Z., Zhu, J., Duan, X., & Zhang, Y. (2023). Climate warming alters the relative importance of plant root and microbial community in regulating the accumulation of soil microbial necromass carbon in a Tibetan alpine meadow. Global Change Biology, 29, 3193– 3204. https://doi-org.proxy.library.ucsb.edu/10.1111/gcb.16660

Kallenbach, C. M., Frey, S. D., & Grandy, A. S. (2016). Direct evidence for microbial-derived soil organic matter formation and its ecophysiological controls. Nature Communications, 7, 13630. https://doi-org.proxy.library.ucsb.edu/10.1038/ncomms13630

Liang, C., Kao-Kniffin, J., Sanford, G. R., Wickings, K., Balser, T. C., & Jackson, R. D. (2016). Microorganisms and their residues under restored perennial grassland communities of varying diversity. Soil Biology and Biochemistry, 103, 192– 200. https://doi-org.proxy.library.ucsb.edu/10.1016/j.soilbio.2016.08.002

Shang, W., Wu, X. D., Zhao, L., Yue, G. Y., Zhao, Y. H., Qiao, Y. P., & Li, Y. Q. (2016). Seasonal variations in labile soil organic matter fractions in permafrost soils with different vegetation types in the Central Qinghai-Tibet plateau. Catena, 137, 670– 678. https://doi-org.proxy.library.ucsb.edu/10.1016/j.catena.2015.07.012

#### Packages
readr: Wickham, H., Hester, J., & Francois, R. (2018). readr: Read Rectangular Text Data. R package version 1.3.1. Retrieved from https://CRAN.R-project.org/package=readr

tidyverse: Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D. A., François, R., ... & Yutani, H. (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686. DOI: 10.21105/joss.01686

ggplot2: Wickham, H., Chang, W., Henry, L., Pedersen, T. L., Takahashi, K., Wilke, C., ... & Yasui, Y. (2020). Welcome to the ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics. R package version 3.3.5. Retrieved from https://CRAN.R-project.org/package=ggplot2

psych: Revelle, W. (2020). psych: Procedures for Psychological, Psychometric, and Personality Research. R package version 2.1.3. Retrieved from https://CRAN.R-project.org/package=psych

car: Fox, J., & Weisberg, S. (2019). An {R} Companion to Applied Regression (3rd ed.). Thousand Oaks CA: Sage. Retrieved from https://socialsciences.mcmaster.ca/jfox/Books/Companion/

multcomp: Hothorn, T., Bretz, F., & Westfall, P. (2008). Simultaneous inference in general parametric models. Biometrical Journal, 50(3), 346-363. DOI: 10.1002/bimj.200810425

grid: Murrell, P. (2020). grid: Grid Graphics. R package version 3.3.3. Retrieved from https://CRAN.R-project.org/package=grid

gridExtra: Auguie, B. (2017). gridExtra: Miscellaneous Functions for "Grid" Graphics. R package version 2.3. Retrieved from https://CRAN.R-project.org/package=gridExtra
