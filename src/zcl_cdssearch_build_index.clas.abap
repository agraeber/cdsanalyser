CLASS zcl_cdssearch_build_index DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apj_dt_exec_object.
    INTERFACES if_apj_rt_exec_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_cdssearch_build_index IMPLEMENTATION.

  METHOD if_apj_dt_exec_object~get_parameters.
    " Define job parameters
    et_parameter_def = VALUE #(
      ( selname    = 'S_VNAME'
        kind       = if_apj_dt_exec_object=>select_option
        datatype   = 'DDLNAME'
        length     = 40
        param_text = 'CDS View Name Pattern'
        changeable_ind = abap_true )
    ).

    " Default parameter values - standard CDS view prefixes
    et_parameter_val = VALUE #(
      ( selname = 'S_VNAME' sign = 'I' option = 'CP' low = 'I_*' )
      ( selname = 'S_VNAME' sign = 'I' option = 'CP' low = 'P_*' )
      ( selname = 'S_VNAME' sign = 'I' option = 'CP' low = 'R_*' )
      ( selname = 'S_VNAME' sign = 'I' option = 'CP' low = 'A_*' )
    ).
  ENDMETHOD.

  METHOD if_apj_rt_exec_object~execute.
    DATA view_name_range TYPE RANGE OF ddlname.
    DATA message_text TYPE string.

    TRY.
        " Validate input parameters
        IF it_parameters IS INITIAL.
          message_text = 'No parameters provided for CDS index build job'.
          et_message_tab = VALUE #( ( msgty = 'E' msgid = '00' msgno = '001' msgv1 = message_text ) ).
          RETURN.
        ENDIF.

        " Build CDS view name range from parameters (optimized with VALUE constructor)
        view_name_range = VALUE #( FOR param IN it_parameters
                                   WHERE ( selname = 'S_VNAME' )
                                   ( sign   = param-sign
                                     option = param-option
                                     low    = param-low
                                     high   = param-high ) ).

        " Validate that we have at least one view pattern
        IF view_name_range IS INITIAL.
          message_text = 'No CDS view patterns specified'.
          et_message_tab = VALUE #( ( msgty = 'E' msgid = '00' msgno = '001' msgv1 = message_text ) ).
          RETURN.
        ENDIF.

        " Log job start
        message_text = |Starting CDS index build for { lines( view_name_range ) } pattern(s)|.
        et_message_tab = VALUE #( ( msgty = 'I' msgid = '00' msgno = '001' msgv1 = message_text ) ).

        " Execute index building
        DATA(index_handler) = NEW zcl_cdssearch_index_handler( ).
        index_handler->extract_data( view_name_range ).

        " Log successful completion
        message_text = 'CDS index build completed successfully'.
        INSERT VALUE #( msgty = 'S' msgid = '00' msgno = '001' msgv1 = message_text )
               INTO TABLE et_message_tab.

      CATCH cx_root INTO DATA(error).
        " Handle any errors during execution
        message_text = error->get_text( ).
        et_message_tab = VALUE #( ( msgty = 'E'
                                    msgid = '00'
                                    msgno = '001'
                                    msgv1 = 'CDS index build failed:'
                                    msgv2 = message_text(50) ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
