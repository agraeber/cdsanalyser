*&---------------------------------------------------------------------*
*& Report ZGET_CDS_FOR_DB_FIELD
*&---------------------------------------------------------------------*
*& Displays the analysed results
*&---------------------------------------------------------------------*
REPORT zget_cds_for_db_field.
TABLES zcdsfieldindex.
INCLUDE zget_cds_for_db_field_lcl.

SELECTION-SCREEN BEGIN OF BLOCK cds WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS s_ddlnam FOR zcdsfieldindex-entity_name.
SELECT-OPTIONS s_ddelem FOR zcdsfieldindex-element_name.
SELECTION-SCREEN END OF BLOCK cds.
SELECTION-SCREEN BEGIN OF BLOCK db WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS s_table FOR zcdsfieldindex-base_object.
SELECT-OPTIONS s_field FOR zcdsfieldindex-base_field.
SELECTION-SCREEN END OF BLOCK db.
SELECT-OPTIONS s_contr  FOR zcdsfieldindex-compatibility_contract DEFAULT 'C1'.
PARAMETERS p_rele AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN BEGIN OF BLOCK entity WITH FRAME TITLE TEXT-003.
PARAMETERS: rb_all   RADIOBUTTON GROUP enti DEFAULT 'X',
              rb_rapbo RADIOBUTTON GROUP enti,
              rb_vh    RADIOBUTTON GROUP enti.
SELECTION-SCREEN END OF BLOCK entity.

INITIALIZATION.
  DATA(app) = NEW lcl_get_cds_for_db_field(  ).

START-OF-SELECTION.
  " Validate at least one search criterion is provided
  IF s_ddlnam[] IS INITIAL AND s_ddelem[] IS INITIAL AND
     s_table[] IS INITIAL AND s_field[] IS INITIAL.
    MESSAGE 'Please specify at least one search criterion' TYPE 'E'.
    RETURN.
  ENDIF.

  " Build release state filter if requested
  DATA released_range TYPE RANGE OF zcdsfieldindex-release_state.
  IF p_rele = abap_true.
    released_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'RELEASED' ) ).
  ENDIF.

  " Execute search with optimized parameters
  app->get_data( i_ddlnam_rg     = s_ddlnam[]
                 i_ddelem_rg     = s_ddelem[]
                 i_table_rg      = s_table[]
                 i_field_rg      = s_field[]
                 i_contr         = s_contr[]
                 i_released_rg   = released_range[]
                 i_get_rap_bo    = rb_rapbo
                 i_get_value_help = rb_vh ).

  app->create_fieldcatalog( ).
  app->display_result( ).

*&---------------------------------------------------------------------*
*& Form TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA header TYPE slis_t_listheader.
  DATA header_line TYPE slis_listheader.

  header_line-typ  = 'H'.
  header_line-info = 'CDS Field Index Search Results'.
  APPEND header_line TO header.

  header_line-typ  = 'S'.
  header_line-key  = 'Execution Time:'.
  WRITE sy-uzeit TO header_line-info.
  APPEND header_line TO header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = header.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING p_ucomm     TYPE sy-ucomm
                        p_selfield  TYPE slis_selfield.

  CASE p_ucomm.
    WHEN '&IC1'.  " Double-click
      IF p_selfield-fieldname = 'ENTITY_NAME'.
        " Navigate to CDS view
        SET PARAMETER ID 'DDOBJNAME' FIELD p_selfield-value.
        CALL TRANSACTION 'SE11' AND SKIP FIRST SCREEN.
      ELSEIF p_selfield-fieldname = 'BASE_OBJECT'.
        " Navigate to base table/view
        SET PARAMETER ID 'DDOBJNAME' FIELD p_selfield-value.
        CALL TRANSACTION 'SE11' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.
