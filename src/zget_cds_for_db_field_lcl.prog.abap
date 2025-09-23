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
                i_released_rg TYPE table
                i_get_all type abap_bool
                i_get_rap_bo type abap_bool
                i_get_value_help type abap_bool
                .

    METHODS create_fieldcatalog.
    METHODS display_result.

  PRIVATE SECTION.
    DATA result       TYPE STANDARD TABLE OF Zcdsfieldindex WITH DEFAULT KEY.
    DATA fieldcatalog TYPE slis_t_fieldcat_alv.
ENDCLASS.


CLASS lcl_get_cds_for_db_field IMPLEMENTATION.
  METHOD get_data.
  
IF i_get_rap_bo EQ abap_true.
  DATA entity_name_range TYPE RANGE OF zjhcdsfieldindex-release_state.
  entity_name_range = VALUE #( ( sign = 'I' option = 'CP' low = 'I*TP' ) ( sign = 'I' option = 'CP' low = 'I*TP_2' ) ( sign = 'I' option = 'CP' low = 'I*TP_3' ) ).
ELSEIF i_get_value_help EQ abap_true.
  entity_name_range = VALUE #( ( sign = 'I' option = 'CP' low = '*VH' ) ).
ENDIF.
  SELECT * FROM zcdsfieldindex
    INTO TABLE @result
    WHERE entity_name            IN @i_ddlnam_rg
    AND entity_name               IN @entity_name_range
    AND element_name           IN @i_ddelem_rg
    AND base_object            IN @i_table_rg
    AND base_field             IN @i_field_rg
    AND compatibility_contract IN @i_contr
    AND release_state          IN @i_released_rg
    AND use_in_sap_cloud_platform IN @use_sap_cloud_range
    ORDER BY entity_name, element_name.
    

  METHOD create_fieldcatalog.
    DATA fieldcat TYPE slis_fieldcat_alv.

    fieldcat-fieldname = 'ENTITY_NAME'.
    fieldcat-seltext_l = 'CDS Name'.
    fieldcat-emphasize = 'C100'.
    fieldcat-hotspot   = abap_true.    
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'ELEMENT_NAME'.
    fieldcat-seltext_l = 'View Field Name'.
    fieldcat-emphasize = 'C100'.
fieldcat-hotspot   = abap_false.        
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'BASE_OBJECT'.
    fieldcat-seltext_l = 'Table/DDL'.
    fieldcat-emphasize = 'C300'.
fieldcat-hotspot   = abap_true.        
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'BASE_FIELD'.
    fieldcat-seltext_l = 'Table Field'.
fieldcat-hotspot   = abap_false.       
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'COMPATIBILITY_CONTRACT'.
    fieldcat-seltext_l = 'Comp. Contract'.
    fieldcat-hotspot   = abap_false.   
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'RELEASE_STATE'.
    fieldcat-seltext_l = 'Release state'.
    fieldcat-hotspot   = abap_false.   
    APPEND fieldcat TO fieldcatalog.

    fieldcat-fieldname = 'USE_IN_SAP_CLOUD_PLATFORM'.
    fieldcat-seltext_l = 'Use in Cloud Dev?'.
    fieldcat-hotspot   = abap_false.   
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
        i_callback_program      = repid
        i_callback_top_of_page  = 'TOP_OF_PAGE'
        i_callback_user_command = 'USER_COMMAND'          
        TABLES
          t_outtab      = result
        EXCEPTIONS
          program_error = 1
          OTHERS        = 2.
    ELSE.
      MESSAGE 'No result found for given CDS View / DB-Table ' TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  FORM user_command USING r_ucomm     LIKE sy-ucomm
                                                  rs_selfield TYPE slis_selfield.

  IF r_ucomm <> '&IC1'. " Hotspot-Klick
    RETURN.
  ENDIF.

  ASSIGN result[ rs_selfield-tabindex ] TO FIELD-SYMBOL(<alv_line>).
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF rs_selfield-fieldname = 'ENTITY_NAME'.
    DATA(url) = CONV char255( |https://<myservername>:443/sap/bc/adt/ddic/ddl/sources/{ <alv_line>-entity_name }/source/main?version=active&sap-client=205| ).
  ELSEIF rs_selfield-fieldname = 'BASE_OBJECT'.
    url = |https://<myservername>:443/sap/bc/adt/ddic/tables/{ <alv_line>-base_object }/source/main?version=active&version=active&sap-client=205|.
  ENDIF.

  CALL FUNCTION 'CALL_BROWSER'
    EXPORTING
      url    = url
    EXCEPTIONS
*     frontend_not_supported = 1
*     frontend_error = 2
*     prog_not_found = 3
*     no_batch = 4
      OTHERS = 9.
ENDFORM.
ENDCLASS.
" 