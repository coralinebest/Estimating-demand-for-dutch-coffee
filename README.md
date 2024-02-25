# Estimating demand for dutch coffee

## Overview

In this project, I aim to analyze a panel dataset recording monthly data over seven years (1990-1996) related to the roasted coffee market. The dataset covers various variables such as coffee prices, consumption, wages, income per capita, price of tea, and more. The primary objective is to understand the factors influencing the consumption of roasted coffee and explore market dynamics.

## Methods
I perform a simple linear regression to analyze the correlation between the logarithm of coffee consumption and the logarithm of coffee prices. Subsequently, I refine the regression model by integrating quarter dummy variables to account for seasonal variations. This allows for a reassessment of the association between coffee consumption and prices, now considering additional controls such as quarters, wages, income per capita, and the price of tea.

To address potential endogeneity issues, I introduce an instrumental variable, specifically the price of coffee beans. This instrumental variable is utilized in a two-stage regression to estimate the causal impact of coffee prices on consumption.

In the evaluation of market dynamics, I calculate both the Lerner index and the adjusted Lerner index. These metrics serve as tools to gauge market power, providing valuable insights into the competitive landscape.
