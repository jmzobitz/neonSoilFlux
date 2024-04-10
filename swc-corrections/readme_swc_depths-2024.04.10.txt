Measurement depths are not currently being reported correctly in the sensor positions file in the Soil water content and water salinity data product (DP1.00094.001). This readme file and an associated file containing the sensor installation depths (swc_depths.csv) have been added to the data product download package until the data product algorithms are corrected.

In swc_depths.csv: 
•	domainID: A three-character alphanumeric code, referring to the NEON domain (D01-D20)
•	siteID: A four-character code, referring to the NEON site
•	horizontalPosition.HOR: A three-character alphanumeric code for the measurement locations within one horizontal plane. 001 corresponds to soil plot 1, 002 to soil plot 2, and so on.
•	verticalPosition.VER: A three-character alphanumeric code for the measurement locations within one vertical plane ranging from 501 to 508. 501 corresponds to the shallowest measurement level and 508 corresponds to the deepest measurement level.
•	sensorDepth: Sensor installation depth (m), negative values indicate below the soil surface
•	startDateTime: depth start date (YYYY-MM-DDTHH:mm:SSZ)
•	endDateTime: depth end date (YYYY-MM-DDTHH:mm:SSZ). NA = no end date (i.e., sensor still at that depth)