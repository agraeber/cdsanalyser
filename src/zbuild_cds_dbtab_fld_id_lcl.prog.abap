*&---------------------------------------------------------------------*
*& Include          ZBUILD_CDS_DBTAB_FLD_ID_LCL
*&---------------------------------------------------------------------*
CLASS lcl_build_cds_dbtab_fld_index DEFINITION.
  PUBLIC SECTION.
    METHODS base_table_extract
      IMPORTING i_view_name TYPE table.
  PRIVATE SECTION.
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


ENDCLASS.

CLASS lcl_build_cds_dbtab_fld_index IMPLEMENTATION.
  METHOD base_table_extract.

    SELECT objectname FROM ddldependency
      INTO TABLE @DATA(db_object_names)
      WHERE ddlname IN @i_view_name AND objecttype = 'VIEW'.


    LOOP AT db_object_names ASSIGNING FIELD-SYMBOL(<db_object_name>).

      " Extract DD name for given CDS View
      SELECT SINGLE cds_ddl FROM ddl_object_names INTO @DATA(cdsddl) WHERE cds_db_view = @<db_object_name>-objectname.

      " Extract business module for given CDS view.
      SELECT SINGLE a~applicationcomponent,
                    a~applicationcomponentname,
                    b~applicationcomponenttext
        INTO ( @DATA(appcmp), @DATA(appcmpname), @DATA(appcmptext) )
        FROM cds_views_pkg_appcomp AS a
               INNER JOIN
                 application_component_text AS b ON a~applicationcomponent = b~applicationcomponent
        WHERE a~ddlsourcename = @cdsddl
          AND b~language      = 'E'.

      " Extract CDS view field name, data type,length and text
      "  select tabname leng datatype scrtext_l into t_vldt from dd03vt where
      "     tabname = p_vname

      DATA(finder) = NEW cl_dd_ddl_field_tracker( iv_ddlname = cdsddl ).
      TRY.
          DATA(field_infos) = finder->get_base_field_information( ).
        CATCH cx_dd_ddl_read INTO DATA(error).

      ENDTRY.

      DATA t_base_field       TYPE STANDARD TABLE OF ty_s_base_field.
      DATA t_base_field_d     TYPE STANDARD TABLE OF ty_s_base_field. " find duplicate fields.

      MOVE-CORRESPONDING field_infos TO t_base_field.

      SORT t_base_field BY base_field.

      LOOP AT t_base_field ASSIGNING FIELD-SYMBOL(<basefield>) WHERE base_field IS NOT INITIAL.

        READ TABLE t_base_field_d INTO DATA(base_field_d) WITH KEY base_field = <basefield>-base_field.

        IF sy-subrc EQ 0.
          base_field_d-base_field  = <basefield>-base_field.
          base_field_d-dup_cnt    += 1.
          APPEND base_field_d TO t_base_field_d.
        ELSE.
          base_field_d-base_field = <basefield>-base_field.
          base_field_d-dup_cnt    = 1.
          APPEND base_field_d TO t_base_field_d.
        ENDIF.

      ENDLOOP.

      IF t_base_field_d IS NOT INITIAL.
        DELETE t_base_field_d WHERE dup_cnt LE 1.
      ENDIF.

      SORT t_base_field BY is_calculated.

      LOOP AT t_base_field ASSIGNING <basefield>. " into  w_base_field.
        DATA(entity_name) = <basefield>-entity_name.

        READ TABLE t_base_field_d INTO base_field_d WITH KEY base_field = <basefield>-base_field.
        IF sy-subrc EQ 0.
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

      SELECT SINGLE compatibility_contract FROM ars_w_api_state
      INTO @DATA(compatibility_contract)
      WHERE object_id EQ @entity_name
        AND object_type EQ 'DDLS'.
      IF sy-subrc EQ 0.
        MODIFY t_base_field
          FROM VALUE #( compatibility_contract = compatibility_contract )
          TRANSPORTING compatibility_contract
          WHERE compatibility_contract NE 'XX'.
      ENDIF.

      TYPES tty_zjhcdsfieldindex TYPE TABLE OF zcdsfieldindex WITH DEFAULT KEY.

      DATA(insertion_entries) = VALUE tty_zjhcdsfieldindex( FOR entry IN t_base_field
                                                            ( entity_name            = entry-entity_name
                                                              element_name           = entry-element_name
                                                              base_object            = entry-base_object
                                                              base_field             = entry-base_field
                                                              compatibility_contract = entry-compatibility_contract ) ).
      MODIFY zcdsfieldindex FROM TABLE insertion_entries.
      COMMIT WORK AND WAIT.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
