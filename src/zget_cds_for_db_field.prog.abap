*&---------------------------------------------------------------------*
*& Report ZGET_CDS_FOR_DB_FIELD
*&---------------------------------------------------------------------*
*& Displays the analysed results
*&---------------------------------------------------------------------*
REPORT zget_cds_for_db_field.
TABLES Zcdsfieldindex.
INCLUDE zget_cds_for_db_FIELD_lcl.

SELECTION-SCREEN BEGIN OF BLOCK cds WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS s_ddlnam FOR Zcdsfieldindex-entity_name.
  SELECT-OPTIONS s_ddelem FOR Zcdsfieldindex-element_name.
SELECTION-SCREEN END OF BLOCK cds.
SELECTION-SCREEN BEGIN OF BLOCK db WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS s_table FOR Zcdsfieldindex-base_object.
  SELECT-OPTIONS s_field FOR Zcdsfieldindex-base_field.
SELECTION-SCREEN END OF BLOCK db.
SELECT-OPTIONS s_contr  FOR Zcdsfieldindex-compatibility_contract DEFAULT 'C1'.

INITIALIZATION.
  DATA(app) = NEW lcl_get_cds_for_db_field(  ).

START-OF-SELECTION.
  app->get_data( i_ddlnam_rg = s_ddlnam[]
                 i_ddelem_rg = s_ddelem[]
                 i_table_rg  = s_table[]
                 i_field_rg  = s_field[]
                 i_contr = s_contr[] ).
  app->create_fieldcatalog(  ).
  app->display_result(  ).
