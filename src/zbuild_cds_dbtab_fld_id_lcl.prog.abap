*&---------------------------------------------------------------------*
*& Include          ZBUILD_CDS_DBTAB_FLD_ID_LCL
*&---------------------------------------------------------------------*
CLASS lcl_build_cds_dbtab_fld_index DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS extract_data
      IMPORTING i_view_name TYPE table.

  PRIVATE SECTION.
    CONSTANTS co_object_type_view TYPE string VALUE 'VIEW' ##NO_TEXT.

    TYPES: BEGIN OF ty_s_base_field,
             entity_name            TYPE string,
             element_name           TYPE string,
             base_object            TYPE string,
             base_field             TYPE string,
             is_calculated          TYPE abap_bool,
             tabclass               TYPE tabclass,
             cdsview                TYPE objectname,
             elefield               TYPE string,
             vleng                  TYPE ddleng,
             vdatatype              TYPE datatype_d,
             vscrtext_l             TYPE scrtext_l,
             tleng                  TYPE ddleng,
             tdatatype              TYPE datatype_d,
             tscrtext_l             TYPE scrtext_l,
             dup_cnt                TYPE i,
             field_dup              TYPE string,
             appcmp                 TYPE uffctr,
             appcmpname             TYPE ufps_posid,
             appcmptext             TYPE c LENGTH 60,
             compatibility_contract TYPE ars_w_api_state-compatibility_contract,

           END OF ty_s_base_field.

    TYPES ty_base_fields TYPE STANDARD TABLE OF ty_s_base_field.

    DATA base_fields           TYPE ty_base_fields.
    DATA base_field_duplicates TYPE ty_base_fields.

    METHODS save_result_to_db IMPORTING i_base_fields TYPE ty_base_fields.

    METHODS add_contract_data
      IMPORTING i_entity_name TYPE lcl_build_cds_dbtab_fld_index=>ty_s_base_field-entity_name.

    METHODS determine_duplicate
      IMPORTING
        i_basefield           TYPE lcl_build_cds_dbtab_fld_index=>ty_s_base_field
      RETURNING
        VALUE(r_base_field_d) TYPE lcl_build_cds_dbtab_fld_index=>ty_s_base_field.

    METHODS delete_duplicates.

ENDCLASS.


CLASS lcl_build_cds_dbtab_fld_index IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD extract_data.
    SELECT objectname FROM ddldependency
      INTO TABLE @DATA(db_object_names)
      WHERE ddlname IN @i_view_name AND objecttype = @co_object_type_view.

    LOOP AT db_object_names ASSIGNING FIELD-SYMBOL(<db_object_name>).
      CLEAR: base_fields,
             base_field_duplicates.

      " Extract DD name for given CDS View
      SELECT SINGLE cds_ddl FROM ddl_object_names INTO @DATA(cdsddl) WHERE cds_db_view = @<db_object_name>-objectname.

      " Extract business module for given CDS view.
      SELECT SINGLE a~ApplicationComponent,
                    a~ApplicationComponentName,
                    b~ApplicationComponentText
        INTO ( @DATA(appcmp),
               @DATA(appcmpname),
               @DATA(appcmptext) )
        FROM cds_views_pkg_appcomp AS a
               INNER JOIN
                 application_component_text AS b ON a~ApplicationComponent = b~ApplicationComponent
        WHERE a~DDLSourceName = @cdsddl
          AND b~Language      = 'E'.

      " Extract CDS view field name, data type,length and text
      "  select tabname leng datatype scrtext_l into t_vldt from dd03vt where
      "     tabname = p_vname

      DATA(finder) = NEW cl_dd_ddl_field_tracker( iv_ddlname = cdsddl ).
      TRY.
          DATA(field_infos) = finder->get_base_field_information( ).
        CATCH cx_dd_ddl_read INTO DATA(error). " TODO: variable is assigned but never used (ABAP cleaner)

      ENDTRY.

      MOVE-CORRESPONDING field_infos TO base_fields.
      MODIFY base_fields FROM VALUE #( appcmp     = appcmp
                                       appcmpname = appcmpname
                                       appcmptext = appcmptext )
             TRANSPORTING appcmp appcmpname appcmptext WHERE appcmp IS INITIAL.

      SORT base_fields BY base_field.

      LOOP AT base_fields ASSIGNING FIELD-SYMBOL(<basefield>) WHERE base_field IS NOT INITIAL.
        me->determine_duplicate( <basefield> ).
      ENDLOOP.

      me->delete_duplicates( ).

      SORT base_fields BY is_calculated.

      LOOP AT base_fields ASSIGNING <basefield>. " into  w_base_field.
        DATA(entity_name) = <basefield>-entity_name.

        READ TABLE base_field_duplicates INTO data(base_field_d) WITH KEY base_field = <basefield>-base_field.
        IF sy-subrc = 0.
          <basefield>-field_dup = 'X'.
        ELSE.
          <basefield>-field_dup = ''.
        ENDIF.

        <basefield>-cdsview = <db_object_name>.

        SELECT SINGLE leng datatype scrtext_l
          INTO ( <basefield>-vleng, <basefield>-vdatatype, <basefield>-vscrtext_l )
          FROM dd03vt
          WHERE tabname = <db_object_name> AND fieldname = <basefield>-element_name AND ddlanguage = 'E'.

        SELECT SINGLE leng datatype scrtext_l
          INTO ( <basefield>-tleng, <basefield>-tdatatype, <basefield>-tscrtext_l )
          FROM dd03vt
          WHERE tabname = <basefield>-base_object AND fieldname = <basefield>-base_field AND ddlanguage = 'E'.

        IF <basefield>-is_calculated = '' OR <basefield>-base_field <> ''.
          CONCATENATE <basefield>-element_name ' ' 'As' ' ' '"' <basefield>-base_field '"' ',' '"' <basefield>-tscrtext_l INTO <basefield>-elefield.
        ELSE.
          CONCATENATE <basefield>-element_name ',' '"' <basefield>-vscrtext_l INTO <basefield>-elefield SEPARATED BY space.
        ENDIF.

      ENDLOOP.

      me->add_contract_data( entity_name ).

      me->save_result_to_db( base_fields ).

    ENDLOOP.
  ENDMETHOD.

  METHOD save_result_to_db.
    TYPES tty_cdsfieldindex TYPE TABLE OF zcdsfieldindex WITH DEFAULT KEY.

    DATA(insertion_entries) = VALUE tty_cdsfieldindex( FOR entry IN i_base_fields
                                                       ( entity_name            = entry-entity_name
                                                         element_name           = entry-element_name
                                                         base_object            = entry-base_object
                                                         base_field             = entry-base_field
                                                         compatibility_contract = entry-compatibility_contract
                                                         appcmp                 = entry-appcmp
                                                         appcmpname             = entry-appcmpname
                                                         appcmptext             = entry-appcmptext ) ).
    MODIFY zcdsfieldindex FROM TABLE insertion_entries.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD add_contract_data.
    SELECT SINGLE compatibility_contract
      FROM ars_w_api_state
      INTO @DATA(compatibility_contract)
      WHERE object_id   = @i_entity_name
        AND object_type = 'DDLS'.
    IF sy-subrc = 0.
      MODIFY base_fields
             FROM VALUE #( compatibility_contract = compatibility_contract )
             TRANSPORTING compatibility_contract
             WHERE compatibility_contract <> 'XX'.  "dummy, valid for each entry
    ENDIF.
  ENDMETHOD.

  METHOD determine_duplicate.


    READ TABLE base_field_duplicates INTO      r_base_field_d  WITH KEY base_field = i_basefield-base_field.

    IF sy-subrc = 0.
      r_base_field_d-dup_cnt += 1.
    ELSE.
      r_base_field_d-dup_cnt = 1.
    ENDIF.

    r_base_field_d-base_field = i_basefield-base_field.
    APPEND r_base_field_d TO base_field_duplicates.

  ENDMETHOD.


  METHOD delete_duplicates.

    IF base_field_duplicates IS NOT INITIAL.
      DELETE base_field_duplicates WHERE dup_cnt <= 1.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
