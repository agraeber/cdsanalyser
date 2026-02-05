How to find CDS views for DB table fields

How can we get the date fron the CDS views, if we do not know the name of the CDS view name?
We developers an all consultants are very familiar with the names of database tables and fields, as we do it since decades. But how to find the matching CDS view field?

This repository contains a report and a FIORI-OData-Service in Package ZCDS_SEARCH
In package ZCDS_SEARCH_BSPyou find a deployed BSP App.

Prerequisites:
Create Application Job Catalog Entry ZCDSSEARCH_BUILD_INDEX with class ZCL_CDSSEARCH_BUILD_INDEX
Create Application Job Template ZCDSSEARCH_BUILD_INDEX_TEMPL for ZCDSSEARCH_BUILD_INDEX

Start of analysis
Analyse once a great amount of CDS views, selected by name and put the results into a database table.
Run this once for all the CDS view you want to analyse

Create Application job based on Template ZCDSSEARCH_BUILD_INDEX_TEMPL and execute it.
or
Start report ZCDSSEARCH_BUILD_INDEX_VIA_JOB which does it for you (On-Prem only)

Use of result
Data is being provided by OData service ZUI_CDSFIELDINDEX_V4.
Use the Preview of the service, create your own FIORI App
Example BSP_APP ZCDSANALYSE (for local A4H system) is included.

OnPrem:
Use Report - ZGET_CDS_FOR_DB_FIELD - Search and Display
An old fashioned report which selects for any given CDS view field all database fields or vica versa in a very short time.

Find the detailled description in:
https://community.sap.com/t5/technology-blogs-by-members/how-to-find-cds-views-for-db-table-fields/ba-p/13880899
