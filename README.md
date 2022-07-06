# Statistical Measures of Inequality in Low-Income and Communities of Color from Underground Storage Tanks

## Code and Analysis

This repository accompanies the paper suubmitted to the journal of Environmental justice and provides all necessary code to recreate the analyses described.


### How to reproduce our work

The analyses for this work were all done using R version 4.1.2 and RStudio version RStudio: 2022.02.0+443 "Prairie Trillium". Any R version later than 4.0 should work fine.

Once you have downloaded this repository, you should download the geodatabase with the 2020 edition of EJSCREEN data, which is available by either navigating to the 2020 folder from the [EJSCREEN data webpage](https://www.epa.gov/ejscreen/download-ejscreen-data) or you can [CLICK HERE](https://gaftp.epa.gov/EJSCREEN/2020/EJSCREEN_2020_USPR.gdb.zip) for a direct gdb download. You should unzip the files into the 'ORD_EJ_USTs/data/EJSCREEN/' folder. This should be the only data you need to download although you can also provide a more updated [USTFinder](https://www.epa.gov/ust/ust-finder) dataset if you would like.

Once you have this repository downloaded and the EJSCREEN data in the correct folder, you can begin to work through the 'ORD_EJ_USTs/scripts/' folder. The scripts are numbered in the order of progression you will need to run them. (DATA COMMONS CITATION FOR USTFINDER)