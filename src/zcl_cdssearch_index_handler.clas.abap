CLASS zcl_cdssearch_index_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_view_range TYPE RANGE OF ddlname.

    METHODS constructor.

    METHODS extract_data
      IMPORTING VALUE(i_view_name) TYPE ty_view_range. 
    METHODS delete_data.

  PRIVATE SECTION.
    CONSTANTS co_object_type_view TYPE string VALUE 'VIEW' ##NO_TEXT.

    TYPES: BEGIN OF ty_s_base_field,
             entity_name               TYPE string,
             element_name              TYPE string,
             base_object               TYPE string,
             base_field                TYPE string,
             is_calculated             TYPE abap_bool,
             tabclass                  TYPE tabclass,
             cdsview                   TYPE objectname,
             elefield                  TYPE string,
             vleng                     TYPE ddleng,
             vdatatype                 TYPE datatype_d,
             vscrtext_l                TYPE scrtext_l,
             tleng                     TYPE ddleng,
             tdatatype                 TYPE datatype_d,
             tscrtext_l                TYPE scrtext_l,
             dup_cnt                   TYPE i,
             field_dup                 TYPE string,
             appcmp                    TYPE uffctr,
             appcmpname                TYPE ufps_posid,
             appcmptext                TYPE c LENGTH 60,
             compatibility_contract    TYPE ars_w_api_state-compatibility_contract,
             release_state             TYPE ars_release_state,
             use_in_key_user_apps      TYPE  ars_use_in_key_user_apps,
             use_in_sap_cloud_platform TYPE ars_use_in_sap_cp,
           END OF ty_s_base_field.

    TYPES ty_base_fields TYPE STANDARD TABLE OF ty_s_base_field.

    DATA base_fields           TYPE ty_base_fields.
    DATA base_field_duplicates TYPE ty_base_fields.

    METHODS save_result_to_db IMPORTING i_base_fields TYPE ty_base_fields.

    METHODS add_contract_data
      IMPORTING i_entity_name TYPE ty_s_base_field-entity_name.

    METHODS determine_duplicate
      IMPORTING
        i_basefield           TYPE ty_s_base_field
      RETURNING
        VALUE(r_base_field_d) TYPE ty_s_base_field.

    METHODS delete_duplicates.

ENDCLASS.


CLASS zcl_cdssearch_index_handler IMPLEMENTATION.
  METHOD constructor.
    " Initialization if needed in future
  ENDMETHOD.

  METHOD extract_data.
    IF i_view_name IS INITIAL.
      i_view_name = VALUE #( sign = 'I' option = 'CP' ( low = 'I_*' ) (  low = 'P_*' ) ).
    ENDIF.

    me->delete_data( ).

    SELECT objectname FROM ddldependency
      INTO TABLE @DATA(db_object_names)
      WHERE ddlname IN @i_view_name AND objecttype = @co_object_type_view.

    LOOP AT db_object_names ASSIGNING FIELD-SYMBOL(<db_object_name>).
      CLEAR: base_fields,
             base_field_duplicates.

      " Extract DD name for given CDS View
      SELECT SINGLE cds_ddl FROM ddl_object_names INTO @DATA(cdsddl) WHERE cds_db_view = @<db_object_name>-objectname.

      " Extract business module for given CDS view.
      SELECT SINGLE a~applicationcomponent,
                    a~applicationcomponentname,
                    b~applicationcomponenttext
        INTO ( @DATA(appcmp),
               @DATA(appcmpname),
               @DATA(appcmptext) )
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
        CATCH cx_dd_ddl_read.
          " Skip this view if field information cannot be retrieved
          CONTINUE.
      ENDTRY.

      MOVE-CORRESPONDING field_infos TO base_fields.
      MODIFY base_fields FROM VALUE #( appcmp     = appcmp
                                       appcmpname = appcmpname
                                       appcmptext = appcmptext )
             TRANSPORTING appcmp appcmpname appcmptext WHERE appcmp IS INITIAL.

      " Optimize duplicate detection using hashed table
      SORT base_fields BY base_field.
      
      DATA: duplicate_counts TYPE HASHED TABLE OF ty_s_base_field
                             WITH UNIQUE KEY base_field.
      
      LOOP AT base_fields INTO DATA(bf) WHERE base_field IS NOT INITIAL.
        DATA(dup_entry) = VALUE ty_s_base_field( base_field = bf-base_field dup_cnt = 1 ).
        INSERT dup_entry INTO TABLE duplicate_counts.
        IF sy-subrc <> 0.
          READ TABLE duplicate_counts ASSIGNING FIELD-SYMBOL(<dup_count>)
               WITH TABLE KEY base_field = bf-base_field.
          IF sy-subrc = 0.
            <dup_count>-dup_cnt = <dup_count>-dup_cnt + 1.
          ENDIF.
        ENDIF.
      ENDLOOP.
      
      " Store only actual duplicates (count > 1)
      base_field_duplicates = VALUE #( FOR entry IN duplicate_counts
                                       WHERE ( dup_cnt > 1 ) 
                                       ( entry ) ).

      SORT base_fields BY is_calculated.

      " Prepare data for bulk SELECT operations
      DATA: view_fields  TYPE TABLE OF ty_s_base_field,
            table_fields TYPE TABLE OF ty_s_base_field.

      DATA(entity_name) = VALUE #( base_fields[ 1 ]-entity_name OPTIONAL ).

      " Optimize: Use hashed table for duplicate lookup
      DATA duplicate_hash TYPE HASHED TABLE OF ty_s_base_field
                          WITH UNIQUE KEY base_field.
      duplicate_hash = base_field_duplicates.

      LOOP AT base_fields ASSIGNING FIELD-SYMBOL(<basefield>).
        " Check for duplicate using hashed table (O(1) lookup)
        <basefield>-field_dup = COND #( WHEN line_exists( duplicate_hash[ base_field = <basefield>-base_field ] )
                                        THEN 'X' 
                                        ELSE '' ).
        <basefield>-cdsview = <db_object_name>.

        " Collect for batch SELECT
        INSERT <basefield> INTO TABLE view_fields.
        IF <basefield>-base_object IS NOT INITIAL.
          INSERT <basefield> INTO TABLE table_fields.
        ENDIF.
      ENDLOOP.

      " Batch SELECT for view field information
      IF view_fields IS NOT INITIAL.
        SELECT tabname, fieldname, leng, datatype, scrtext_l
          FROM dd03vt
          FOR ALL ENTRIES IN @view_fields
          WHERE tabname = @view_fields-cdsview
            AND fieldname = @view_fields-element_name
            AND ddlanguage = 'E'
          INTO TABLE @DATA(view_field_info).
      ENDIF.

      " Batch SELECT for table field information
      IF table_fields IS NOT INITIAL.
        SELECT tabname, fieldname, leng, datatype, scrtext_l
          FROM dd03vt
          FOR ALL ENTRIES IN @table_fields
          WHERE tabname = @table_fields-base_object
            AND fieldname = @table_fields-base_field
            AND ddlanguage = 'E'
          INTO TABLE @DATA(table_field_info).
      ENDIF.

      " Update base_fields with retrieved information
      LOOP AT base_fields ASSIGNING <basefield>.
        " Update view field information
        READ TABLE view_field_info INTO DATA(vf_info)
          WITH KEY tabname = <basefield>-cdsview
                   fieldname = <basefield>-element_name.
        IF sy-subrc = 0.
          <basefield>-vleng = vf_info-leng.
          <basefield>-vdatatype = vf_info-datatype.
          <basefield>-vscrtext_l = vf_info-scrtext_l.
        ENDIF.

        " Update table field information
        IF <basefield>-base_object IS NOT INITIAL.
          READ TABLE table_field_info INTO DATA(tf_info)
            WITH KEY tabname = <basefield>-base_object
                     fieldname = <basefield>-base_field.
          IF sy-subrc = 0.
            <basefield>-tleng = tf_info-leng.
            <basefield>-tdatatype = tf_info-datatype.
            <basefield>-tscrtext_l = tf_info-scrtext_l.
          ENDIF.
        ENDIF.

        " Build element field string
        IF <basefield>-is_calculated = '' OR <basefield>-base_field <> ''.
          <basefield>-elefield = |{ <basefield>-element_name } As "{ <basefield>-base_field }","{ <basefield>-tscrtext_l }|.
        ELSE.
          <basefield>-elefield = |{ <basefield>-element_name }, "{ <basefield>-vscrtext_l }|.
        ENDIF.
      ENDLOOP.

      me->add_contract_data( entity_name ).

      me->save_result_to_db( base_fields ).

    ENDLOOP.
  ENDMETHOD.

  METHOD save_result_to_db.
    TYPES tty_cdsfieldindex TYPE TABLE OF zcdsfieldindex WITH DEFAULT KEY.

    CHECK i_base_fields IS NOT INITIAL.

    DATA(insertion_entries) = VALUE tty_cdsfieldindex( FOR entry IN i_base_fields
                                                       ( entity_name               = entry-entity_name
                                                         element_name              = entry-element_name
                                                         base_object               = entry-base_object
                                                         base_field                = entry-base_field
                                                         compatibility_contract    = entry-compatibility_contract
                                                         release_state             = entry-release_state
                                                         use_in_key_user_apps      = entry-use_in_key_user_apps
                                                         use_in_sap_cloud_platform = entry-use_in_sap_cloud_platform
                                                         appcmp                    = entry-appcmp
                                                         appcmpname                = entry-appcmpname
                                                         appcmptext                = entry-appcmptext ) ).
    TRY.
        MODIFY zcdsfieldindex FROM TABLE insertion_entries.
        COMMIT WORK AND WAIT.
      CATCH cx_sy_open_sql_db.
        ROLLBACK WORK.
        RAISE EXCEPTION TYPE cx_sy_open_sql_db.
    ENDTRY.
  ENDMETHOD.

  METHOD add_contract_data.
    SELECT SINGLE compatibility_contract, release_state, use_in_key_user_apps, use_in_sap_cloud_platform
      FROM ars_w_api_state
      INTO @DATA(ars)
      WHERE object_id   = @i_entity_name
        AND object_type = 'DDLS'.
    IF sy-subrc = 0.
      MODIFY base_fields
             FROM VALUE #( compatibility_contract    = ars-compatibility_contract
                           release_state             = ars-release_state
                           use_in_key_user_apps      = ars-use_in_key_user_apps
                           use_in_sap_cloud_platform = ars-use_in_sap_cloud_platform )
             TRANSPORTING compatibility_contract release_state use_in_key_user_apps use_in_sap_cloud_platform
             WHERE compatibility_contract <> 'XX'.  " dummy, valid for each entry
    ENDIF.
  ENDMETHOD.

  METHOD determine_duplicate.
    " This method is now obsolete - duplicate detection optimized in extract_data
    " Kept for compatibility but not used
    r_base_field_d = i_basefield.
  ENDMETHOD.


  METHOD delete_duplicates.
    " This method is now obsolete - duplicate detection optimized in extract_data
    " Kept for compatibility
  ENDMETHOD.

  METHOD delete_data.
    " Delete all existing index data
    DELETE FROM zcdsfieldindex.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
