*&---------------------------------------------------------------------*
*& Include zget_cds_for_db_field_lcl
*&---------------------------------------------------------------------*
CLASS lcl_get_cds_for_db_field DEFINITION.
  PUBLIC SECTION.
    METHODS get_data
      IMPORTING i_ddlnam_rg   TYPE table
                i_ddelem_rg   TYPE table
                i_table_rg    TYPE table
                i_field_rg    TYPE table
                i_contr       TYPE table
                i_released_rg TYPE table.

    METHODS create_fieldcatalog.
    METHODS display_result.

  PRIVATE SECTION.
    DATA result       TYPE STANDARD TABLE OF Zcdsfieldindex WITH DEFAULT KEY.
    DATA fieldcatalog TYPE slis_t_fieldcat_alv.
ENDCLASS.


CLASS lcl_get_cds_for_db_field IMPLEMENTATION.
  METHOD get_data.
    SELECT * FROM zcdsfieldindex
      INTO TABLE @me->result
      WHERE entity_name            IN @i_ddlnam_rg
        AND element_name           IN @i_ddelem_rg
        AND base_object            IN @i_table_rg
        AND base_field             IN @i_field_rg
        AND compatibility_contract IN @i_contr
        AND release_state          IN @i_released_rg.
  ENDMETHOD.


  METHOD create_fieldcatalog.
    DATA fieldcat TYPE slis_fieldcat_alv.

    fieldcat-fieldname = 'ENTITY_NAME'.
    fieldcat-seltext_l = 'CDS Name'.
    fieldcat-emphasize = 'C100'.
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'ELEMENT_NAME'.
    fieldcat-seltext_l = 'View Field Name'.
    fieldcat-emphasize = 'C100'.
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'BASE_OBJECT'.
    fieldcat-seltext_l = 'Table/DDL'.
    fieldcat-emphasize = 'C300'.
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'BASE_FIELD'.
    fieldcat-seltext_l = 'Table Field'.
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'COMPATIBILITY_CONTRACT'.
    fieldcat-seltext_l = 'Comp. Contract'.
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'RELEASE_STATE'.
    fieldcat-seltext_l = 'Release state'.
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'USE_IN_SAP_CLOUD_PLATFORM'.
    fieldcat-seltext_l = 'Use in Cloud Dev?'.
    APPEND fieldcat TO fieldcatalog.


  ENDMETHOD.


  METHOD display_result.
    DATA fieldlayout TYPE slis_layout_alv.

    fieldlayout-colwidth_optimize = 'X'.
    " sort t_base_field by base_object.
    IF result IS NOT INITIAL.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          it_fieldcat   = fieldcatalog
          is_layout     = fieldlayout
        TABLES
          t_outtab      = result
        EXCEPTIONS
          program_error = 1
          OTHERS        = 2.
    ELSE.
      MESSAGE 'No result found for given CDS View / DB-Table ' TYPE 'S'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
