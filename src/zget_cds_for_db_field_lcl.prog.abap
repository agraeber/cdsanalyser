*&---------------------------------------------------------------------*
*& Include zget_cds_for_db_field_lcl
*&---------------------------------------------------------------------*
CLASS lcl_get_cds_for_db_field DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_entity_range TYPE RANGE OF zcdsfieldindex-entity_name,
           ty_element_range TYPE RANGE OF zcdsfieldindex-element_name,
           ty_object_range TYPE RANGE OF zcdsfieldindex-base_object,
           ty_field_range TYPE RANGE OF zcdsfieldindex-base_field,
           ty_contract_range TYPE RANGE OF zcdsfieldindex-compatibility_contract,
           ty_release_range TYPE RANGE OF zcdsfieldindex-release_state.

    METHODS get_data
      IMPORTING i_ddlnam_rg     TYPE ty_entity_range
                i_ddelem_rg     TYPE ty_element_range
                i_table_rg      TYPE ty_object_range
                i_field_rg      TYPE ty_field_range
                i_contr         TYPE ty_contract_range
                i_released_rg   TYPE ty_release_range
                i_get_rap_bo    TYPE abap_bool DEFAULT abap_false
                i_get_value_help TYPE abap_bool DEFAULT abap_false.

    METHODS create_fieldcatalog.
    METHODS display_result.

  PRIVATE SECTION.
    DATA result       TYPE STANDARD TABLE OF zcdsfieldindex WITH DEFAULT KEY.
    DATA fieldcatalog TYPE slis_t_fieldcat_alv.

    METHODS add_field_to_catalog
      IMPORTING i_fieldname TYPE fieldname
                i_label     TYPE scrtext_l
                i_hotspot   TYPE abap_bool DEFAULT abap_false
                i_emphasize TYPE char4 OPTIONAL.
ENDCLASS.


CLASS lcl_get_cds_for_db_field IMPLEMENTATION.
  METHOD get_data.
    DATA entity_name_range TYPE RANGE OF zcdsfieldindex-entity_name.

    " Build entity name filter based on entity type selection
    IF i_get_rap_bo = abap_true.
      " RAP Business Objects (Transactional Processing views)
      entity_name_range = VALUE #( sign   = 'I'
                                   option = 'CP'
                                   ( low = 'I*TP' )
                                   ( low = 'I*TP_2' )
                                   ( low = 'I*TP_3' ) ).
    ELSEIF i_get_value_help = abap_true.
      " Value Help views
      entity_name_range = VALUE #( ( sign = 'I' option = 'CP' low = '*VH' ) ).
    ENDIF.

    " Execute optimized query
    SELECT entity_name,
           element_name,
           base_object,
           base_field,
           compatibility_contract,
           release_state,
           use_in_sap_cloud_platform
      FROM zcdsfieldindex
      INTO CORRESPONDING FIELDS OF TABLE @result
      WHERE entity_name            IN @i_ddlnam_rg
        AND entity_name            IN @entity_name_range
        AND element_name           IN @i_ddelem_rg
        AND base_object            IN @i_table_rg
        AND base_field             IN @i_field_rg
        AND compatibility_contract IN @i_contr
        AND release_state          IN @i_released_rg
      ORDER BY entity_name, element_name.

    IF sy-subrc <> 0.
      CLEAR result.
    ENDIF.
  ENDMETHOD.

  METHOD create_fieldcatalog.
    " Optimized field catalog creation using helper method
    add_field_to_catalog( i_fieldname = 'ENTITY_NAME'
                         i_label     = 'CDS Name'
                         i_hotspot   = abap_true
                         i_emphasize = 'C100' ).

    add_field_to_catalog( i_fieldname = 'ELEMENT_NAME'
                         i_label     = 'View Field Name'
                         i_emphasize = 'C100' ).

    add_field_to_catalog( i_fieldname = 'BASE_OBJECT'
                         i_label     = 'Table/DDL'
                         i_hotspot   = abap_true
                         i_emphasize = 'C300' ).

    add_field_to_catalog( i_fieldname = 'BASE_FIELD'
                         i_label     = 'Table Field' ).

    add_field_to_catalog( i_fieldname = 'COMPATIBILITY_CONTRACT'
                         i_label     = 'Comp. Contract' ).

    add_field_to_catalog( i_fieldname = 'RELEASE_STATE'
                         i_label     = 'Release State' ).

    add_field_to_catalog( i_fieldname = 'USE_IN_SAP_CLOUD_PLATFORM'
                         i_label     = 'Use in Cloud Dev?' ).
  ENDMETHOD.


  METHOD display_result.
    DATA fieldlayout TYPE slis_layout_alv.

    IF result IS INITIAL.
      MESSAGE 'No results found for the specified criteria' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    fieldlayout-colwidth_optimize = abap_true.
    fieldlayout-zebra = abap_true.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        it_fieldcat             = fieldcatalog
        is_layout               = fieldlayout
        i_callback_program      = sy-repid
        i_callback_top_of_page  = 'TOP_OF_PAGE'
        i_callback_user_command = 'USER_COMMAND'
      TABLES
        t_outtab                = result
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error displaying ALV grid' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD add_field_to_catalog.
    DATA fieldcat TYPE slis_fieldcat_alv.

    fieldcat-fieldname = i_fieldname.
    fieldcat-seltext_l = i_label.
    fieldcat-hotspot   = i_hotspot.
    fieldcat-emphasize = i_emphasize.

    APPEND fieldcat TO fieldcatalog.
  ENDMETHOD.

ENDCLASS.
