*&---------------------------------------------------------------------*
*& Report ZBUILD_CDS_DBTAB_FLD_INDEX
*&---------------------------------------------------------------------*
*& Analyses the selected CDS views and stores the analysis result
*  in the DB
*&---------------------------------------------------------------------*
REPORT zbuild_cds_dbtab_fld_index.

TABLES ddldependency.
INCLUDE zbuild_cds_dbtab_fld_id_lcl.

SELECT-OPTIONS s_vname FOR ddldependency-ddlname.

INITIALIZATION.
  DATA(app) = NEW lcl_build_cds_dbtab_fld_index(  ).

START-OF-SELECTION.

  IF s_vname[] IS INITIAL.
    RETURN.
  ELSE.
    app->extract_data( s_vname[] ).
  ENDIF.

INITIALIZATION.
  s_vname-sign = 'I'.
  s_vname-option = 'CP'.
  s_vname-high = ''.
  s_vname-low = 'I_*'.
  INSERT s_vname INTO TABLE s_vname.
  s_vname-low = 'P_*'.
  INSERT s_vname INTO TABLE s_vname.
