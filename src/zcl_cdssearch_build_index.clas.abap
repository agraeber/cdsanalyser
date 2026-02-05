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
    et_parameter_def = VALUE #(
      ( selname = 'S_VNAME'
        kind    = if_apj_dt_exec_object=>select_option
        datatype = 'DDLNAME'
        length = 40
        param_text = 'CDS View Name Pattern' )
    ).

    et_parameter_val = VALUE #(
      ( selname = 'S_VNAME' sign = 'I' option = 'CP' low = 'I_*' )
      ( selname = 'S_VNAME' sign = 'I' option = 'CP' low = 'P_*' )
    ).
  ENDMETHOD.

  METHOD if_apj_rt_exec_object~execute.
    DATA view_name_range TYPE RANGE OF ddlname.

    " Korrekter Parameter-Name: it_parameters
    LOOP AT it_parameters INTO DATA(parameter) WHERE selname = 'S_VNAME'.
      APPEND VALUE #( sign   = parameter-sign
                      option = parameter-option
                      low    = parameter-low
                      high   = parameter-high ) TO view_name_range.
    ENDLOOP.

    IF view_name_range IS NOT INITIAL.
      DATA(index_handler) = NEW zcl_cdssearch_index_handler( ).
      index_handler->extract_data( view_name_range ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
