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

    TRY.
        " Validate input parameters
        IF it_parameters IS INITIAL.
          " No parameters provided - skip execution
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
          " No view patterns specified - skip execution
          RETURN.
        ENDIF.

        " Execute index building
        DATA(index_handler) = NEW zcl_cdssearch_index_handler( ).
        index_handler->extract_data( view_name_range ).

      CATCH cx_root INTO DATA(error).
        " Error during execution - log to application log if needed
        " For now, simply return (job framework will handle the exception)
        RETURN.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

