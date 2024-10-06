*&---------------------------------------------------------------------*
*& Report ZBUILD_CDS_DBTAB_FLD_INDEX
*&---------------------------------------------------------------------*
*& Analyses the selected CDS views and stores the analysis result
*  in the DB
*&---------------------------------------------------------------------*
REPORT zdelete_cds_dbtab_fld_index.

delete from ZCDSFIELDINDEX where base_field NE 'DUMMY'.
