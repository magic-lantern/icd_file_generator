## Introduction

This repository contains a tool that is used to create a sample data set of N rows with X columns of data. User can specify D columns of ICD diagnosis codes, P columns of ICD procedure codes, and G columns of extra data.

Either ICD9 or ICD10 codes can be generated.

### ICD 9
ICD 9 code list was obtained from CMS on October 2017 via: https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html - specific file downloaded was [Version 32 Full and Abbreviated Code Titles – Effective October 1, 2014 [ZIP, 1MB]](https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip) 

ICD 9 Files included in with this code are:

* CMS32_DESC_LONG_DX.txt - List of ICD9 diagnosis codes along with a long description
* CMS32_DESC_LONG_SG.txt - List of ICD9 procedure codes along with a long description

### ICD 10

ICD 10 code list was obtained from CMS on October 2017 via: https://www.cms.gov/Medicare/Coding/ICD10/2017-ICD-10-CM-and-GEMs.html - specific files downloaded were 
[2017 Code Descriptions in Tabular Order [ZIP, 2MB]](https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2017-ICD10-Code-Descriptions.zip) and [2017 ICD-10-PCS Codes File [ZIP, 831KB]] (https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2017-New-Procedure-Codes.zip)

ICD 10 Files included with this code are:

* icd10cm_codes_2017.txt - List of ICD10 diagnosis codes from "2017 2017 Code Descriptions in Tabular Order"
* icd10pcs_codes_2017.txt - List of ICD10 procedure codes from "2017 ICD-10-PCD Codes File"
