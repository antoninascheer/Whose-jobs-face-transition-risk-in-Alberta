# Whose jobs face transition risk in Alberta?
## Understanding sectoral employment precarity in an oil-rich Canadian province

Antonina Scheer, Moritz Schwarz, Debbie Hopkins, Ben Caldecott

This repository presents the code used to undertake the econometric analysis presented in the article "Whose jobs face transition risk in Alberta? Understanding sectoral employment precarity in an oil-rich Canadian province" published in Climate Policy (DOI: [10.1080/14693062.2022.2086843](https://www.doi.org/10.1080/14693062.2022.2086843)). 

Abstract: 

>Labour markets of oil-exporting regions will be impacted by a global transition to low-carbon energy as oil demand reduces to meet the aims of the Paris Agreement. Together with direct job losses in the oil and gas industry, indirect employment effects on other sectors should also be considered to ensure a just transition. We explore these direct and indirect employment impacts that could result from the low-carbon transition by analysing the effect of oil price fluctuations on the labour market of Alberta, a Canadian province economically reliant on oil sands extraction. We employ a mixed methods approach, contextualizing our quantitative analysis with first-hand experiences of career transition s using interviews with oil sands workers. We estimate a vector autoregression for province-wide insights and explore sector-specific dynamics using time series regressions. We find that the price discount on Canadian oil sands, which is determined by local factors like crude oil quality and pipeline capacity, does not significantly affect employment, while the global oil price does. This finding puts in doubt claims of long-term employment benefits from new pipelines. We find that at a provincial scale, oil price fluctuations lead to employment levels also fluctuating. Our analysis at the sectoral level shows that these job fluctuations extend beyond oil and gas to other sectors, such as construction and some service sectors. These findings suggest that the province’s current economic dependence on oil creates job precarity because employment in various sectors is sensitive to a volatile oil market. Furthermore, due to this sectoral sensitivity to oil price changes, workers in these sectors may be especially at risk in a low-carbon transition and warrant special attention in the development of provincial and national just transition policies. Transitional assistance can support workers directly, while economic diversification in Alberta can reduce reliance on international oil markets and thereby ensure stable opportunities in existing and new sectors.


Included in this folder is code to enable data preparation, VECM analysis, ARDL analysis, and associated function:
 
1. Data preparation code, which downloads relevant datasets from public online sources and saves it in folders that can be prepared by the code user. 
2. Code and associated functions for the vector error correction model, which we use to evaluate the effect of oil price changes on province-wide employment levels. 
3. Code and associated functions for the autoregressive distributed lag models, which we use to evaluate the effect of oil price changes on employment levels in several seperate economic sectors.
4. Code to view output table for ARDL results
