# CDS & Table Field Search Tool (Clean Core Enabler)

This tool bridges the gap between traditional SAP database table knowledge and the modern **Clean Core** approach. It allows developers and consultants to find corresponding **CDS View entities and fields** based on familiar database tables and fields in seconds.

> [!CAUTION]
> **Cloud Readiness & Compatibility** > This tool is unfortunatley **NOT Cloud Ready (not for SAP BTP ABAP Environment / Steampunk)**.  
> Although it uses modern components like Application Jobs and OData V4, the underlying data sources and system tables for CDS metadata are **NOT released** for cloud development. It is designed for use on **SAP S/4HANA On-Premise** or **Private Cloud** environments.

## Overview

As we move toward Clean Core, accessing data via CDS views instead of direct table access is essential. However, finding the right CDS view for a known DB table can be challenging. This repository provides a high-performance indexing engine and search UI to solve this.

### Project Structure

The solution is organized into the following components:

- **Package `ZCDS_SEARCH`**: Backend core, indexing logic, and OData V4 service.
- **Package `ZCDS_SEARCH_BSP`**: Deployed Fiori BSP Application (`ZCDSANALYSE`).

---

## 🚀 Prerequisites & Setup

To enable the background indexing engine, the following objects are included in the repository:

1. **Application Job Catalog Entry**:  
   `ZCDSSEARCH_BUILD_INDEX` (bound to class `ZCL_CDSSEARCH_BUILD_INDEX`).
2. **Application Job Template**:  
   `ZCDSSEARCH_BUILD_INDEX_TEMPL` based on the catalog entry above.

---

## 🏗 Start of Analysis (Building the Index)

Before the first search, you must analyze the CDS views in your system to populate the search index.

1. **Execute the Indexer**:
   - **Standard**: Create and execute an **Application Job** based on Template `ZCDSSEARCH_BUILD_INDEX_TEMPL`.
   - **On-Premise Shortcut**: Run Report `ZCDSSEARCH_BUILD_INDEX_VIA_JOB` to trigger the indexing process via the Application Job.
2. **Result**: The tool analyzes the requested CDS views and stores the mapping results in a dedicated database table for instant access.

---

## 🔍 How to Use

### Modern UI (Fiori / OData)

The primary way to consume the data is via the **OData V4 Service**: `ZUI_CDSFIELDINDEX_V4`.

- Use the Service Preview to build your own dashboard.
- A pre-deployed **Fiori App (`ZCDSANALYSE`)** is included (configured for A4H/Developer systems).

### Classic UI (On-Premise Only)

For a quick search within the SAP GUI, use Report: **`ZGET_CDS_FOR_DB_FIELD`**.  
This high-speed report allows you to:

- Find all database fields for any given CDS view field.
- Find the matching CDS view field for any given database table/field.

---

## 📖 Detailed Documentation

For a full technical deep dive and the story behind this tool, please refer to the SAP Community blog post:  
🔗 [How to find CDS views for DB table fields](https://community.sap.com/t5/technology-blogs-by-members/how-to-find-cds-views-for-db-table-fields/ba-p/13880899)
