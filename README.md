Finding opportunities for locating new businesses: The case of organic dairy in the USA.
================
Juan C. S. Herrera
February, 2019



![alt text](https://github.com/juancsherrera/TDISupplychain/blob/master/README_figs/usaodsupplychain.gif)

Title: Finding opportunities for locating new businesses, The case of organic dairy business in the USA.

Motivation and description of the problem: Locating a new business (or a new branch) requires minimizing costs of running the business, and maximizing profits. In this project I am assuming that the costs of inputs and the final costs charged to consumers are the same all over the USA, this is supported by economic theories of arbitrage. With this restriction, in order to minimize costs of both the supply chain of input materials and getting access to consumers, a new business would need to choose a geographic location that minimizes the distance to both consumers and suppliers.

In order to tackle this challenge I use a highly perishable item: dairy, particularly organic dairy. I model the supply chain based on two parts: 1. organic dairy farmers that produce raw milk. 2. organic dairy handlers that transform raw milk into pasteurized milk or dairy products, such as cheese, pasteurized milk, and yogurt. 3. Potential location of consumers.

Data: For (1, and 2) The dataset is the United States Department of Agriculture Organic Integrity Database that contains information for every organic product certified by the USDA. This dataset contains an address for each operation (handler and farmer) but not a GPS coordinate. I used the google maps API to retrieve the GPS coordinates. For (3) I used the publicly available dataset of Whole Foods supermarket locations. I scraped the website in order to obtain the address, and again, used the Google Maps API to obtain GPS coordinates.

Methods: I use network science methodologies to infer dynamic supply chains that connect farmers -&gt; handlers -&gt; supermarkets. These supply chains are inferred based on modeling connections between the GPS coordinates as I Don't have data on which handlers purchase raw milk from which farmers, and which handlers supply the supermarket Whole Foods with dairy products. For this I modeled connections based on the following restrictions: 1. farmers supply handlers on a 50 mile radius. 2. handlers supply Whole foods supermarkets located in a 300 mile radius. Since the dataset has dynamic components for farmers and handlers but not for whole foods I assume that whole foods consumers were always (since 2002). Farmers can only supply to handlers that are in the area in their same pr previous years of establishing a new business.

Conclusions: Farmer level: At the farmer level the best bet is to locate as close as possible to a handling facility. It would help to locate close to a newer handling facility with fewer handlers around as your business will have less competition. Handler Level: The best bet is to locate in the midpoint between a critical mass of farmers and a supermarket. A very promising area is close to the northeastern coast but not directly on the coast as it will be too far from farmers. Supermarket level: The best bet is to locate close to handlers. However this is a problem of the design as we don't have data on consumers other than current supermarkets. Hence more data is needed for a conclusive solution.

Shortcomings: I plan to expand the analysis by including data on average cost of land in order to come up with a cost function that strengthens this analysis. The GPS coordinates for all operations (farmers, handlers, and supermarkets) do not include the size of the operation this is a severe shortcoming as being close to a bigger operation of the competence will make it harder to compete. The connections are euclidean and don't consider actual traveled distance using highways. This can be corrected with more data (or more time to get this from the Google Maps API without exceeding the free quota).
