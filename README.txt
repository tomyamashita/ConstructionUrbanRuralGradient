This README.txt file was generated on 2023-01-10 by Thomas J. Yamashita


GENERAL INFORMATION

1. Title of Dataset: Differences in Mammal Community Response to Highway Construciton along an Urban-Rural Gradient
	This data is associated with the manuscript, titled Differences in Mammal Community Response to Highway Construciton along an Urban-Rural Gradient, available here: XXXX

2. Author Information
	Thomas J. Yamashita
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville
		tjyamashta@gmail.com
		Shared first authorship
		Corresponding Author
	Jason V. Lombardi
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville
		Shared first authorship
	Zachary M. Wardle
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville
	Michael E. Tewes
		Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville
	John H. Young Jr. 
		Environmental Affairs Division, Texas Department of Transportation

3. Date of Data Collection: 
	Camera Data: July 2019 - November 2020

4. Geographic location of data collection: Eastern Cameron County, Texas, USA along Farm-to-Market 1847

5. Funding Sources: Texas Department of Transportation


DATA & FILE OVERVIEW

1. File List: 
	DataOrganize_ConEarly.txt: DataOrganize text file containing camera name, species name, year, month, day, hour, minute, second, and number of individuals in the photograph for the construction period (February 2020 - November 2020)
	DataOrganize_PreCon.txt: DataOrganize text file containing camera name, species name, year, month, day, hour, minute, second, and number of individuals in the photograph for the pre-construction period July 2019 - February 2020)
	CTtable.xslx: CT Table describing active and total camera trap nights for the pre-construction and construction periods
	EnvData.xlsx: File containing information about which crossings, the side of the highway and the level of urbanization for each camera
	README.txt: This file

2. Relationship between files: 
	The camera names are the same in the above files and can be used to link the data


METHODOLOGICAL INFORMATION

1. Description of the methods used for collection/generation and processing of data: 
	Methodology for collection and processing of the data can be found in the manuscript

2. Quality Assurance Procedures: 
	Photographs were processed by trained personnel and went through multiple rounds of checks to ensure accurate detection of all animals 

3. People involved with data collection, processing, and analysis: 
	Thomas J. Yamashita, Zachary M. Wardle, Jason V. Lombardi


DATA SPECIFIC INFORMATION FOR: DataOrganize_ConEarly.txt
1. Data Type: text file

2. Number of Variables: 9

3. Number of Rows: 187462

4. Variable List: 
	Column 1: Camera name
	Column 2: Species in photograph
	Column 3: Year photograph was taken
	Column 4: Month photograph was taken
	Column 5: Day photograph was taken
	Column 6: Hour photograph was taken
	Column 7: Minute photograph was taken
	Column 8: Second photograph was taken
	Column 9: Number of individuals of the given species in the photograph


DATA SPECIFIC INFORMATION FOR: DataOrganize_PreCon.txt
1. Data Type: text file

2. Number of Variables: 9

3. Number of Rows: 466698

4. Variable List: 
	Column 1: Camera name
	Column 2: Species in photograph
	Column 3: Year photograph was taken
	Column 4: Month photograph was taken
	Column 5: Day photograph was taken
	Column 6: Hour photograph was taken
	Column 7: Minute photograph was taken
	Column 8: Second photograph was taken
	Column 9: Number of individuals of the given species in the photograph


DATA SPECIFIC INFORMATION FOR: CTtable.xlsx
1. Data Type: Microsoft excel xlsx file

2. Number of Variables: 12

3. Number of Rows: 60

4. Variable List: 
	Camera: Camera Name
	Site: Location and side of highway of camera
	Station: Location of camera
	timeperiod: Pre-construction (1) or construction (2) period
	Setup_date: Date camera was set up
	Retrieval_date: Date camera was retrieved
	ProblemX_from: (multiple columns): Start date that a camera was inactive. X starts at 1 and increases by 1. Higher numbers indicate that a camera had more problems
	ProblemX_to: (multiple columns): End date that a camera was inactive. Just like above, X starts at 1 and increases by 1 as a camera has more problems


DATA SPECIFIC INFORMATION FOR: EnvData.xlsx
1. Data Type: Microsoft excel .xlsx file

2. Number of Variables: 4

3. Number of Rows: 40

4. Variable List: 
	Camera: Camera name
	Site: Location and side of highway of camera
	Station: Location of camera
	Urbanization: The urbanization level of the camera (urban, peri-urban, and rural). Details are described in the text


DATA SPECIFIC INFORMATION FOR: MammalResponseConstructionUrbanRural.R
1. Data Type: R script

5. Other Information: This script provides code for conducting all analyses used in this manuscript
