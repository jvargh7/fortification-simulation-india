# fortification-simulation-india
Development of a dashboard simulating India’s nutrition scenario and potential of fortification interventions

Conference: Nutrition 2018, Boston MA

Jithin Sam Varghese, Rutuja Chhajed, Sumathi Swaminathan, Tinku Thomas, Vinod Kamat, Anura Kurpad  

The dashboard can be viewed at https://tatanin.org/#/dashboard

Objective:  

To develop an interactive and intuitive web dashboard for government stakeholders, and researchers to understand the Indian nutrition scenario in terms of supply-demand (agricultural produce-food intake), health outcomes and optimum levels of priority nutrients while fortifying foods.  

Methods:  

Publicly available datasets on crop production and health outcomes were extracted and pre-processed. A commercial dataset on consumer food expenditure (National Sample Survey, Round 68 Sch.1.0) was purchased from the Ministry of Statistics and Programme Implementation. The food expenditure was translated into food and nutrient intake using Indian Food Composition Tables 2017, USDA SR 28 and proprietary recipe databases. Distribution of food intake within a household was calculated based on age and gender specific consumer units. The fortification simulation algorithms were developed based on the IOM Dietary Reference Intakes to assess risk of both inadequacy as well as adverse effects for any nutrient, level and vehicle combination. The application was prototyped in Shiny by RStudio and later scaled using Javascript and PHP with MySQL as the database.  

Results:  

The dashboard presents data for production of 61 crops, 151 foods, 14 nutrients and 90 indicators relevant for health and nutrition. The Fortification Simulation tab provides functionalities to modify the fortificant level and coverage of interventions for up to 3 fortification vehicles simultaneously. The EAR cut-point method and Probability approach to assess risk of deficient intakes were successfully implemented in R 3.3.3. The fortification simulation algorithm was effectively translated from R to PHP. The algorithm estimates that risk of iron inadequacy in men would decrease from 64.7% to 1.3% in India, while the risk of adverse events would increase from 0.005% to 24.4% with the mandatory fortification of rice, wheat and salt at 3mg, 3mg and 100mg per 100 grams of food consumed.  

Conclusion:  

An integrated dashboard of the state of Indian nutrition is now available to inform policy. This is the first application of its kind which uses food expenditure data tailored to a developing country context and uses the entire distribution of intakes. The dashboard will be refined further to include other datasets associated with understanding the nutrition situation in India.  


Funding Source: Tata Trusts  
