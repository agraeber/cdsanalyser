*&---------------------------------------------------------------------*
*& Report ZBUILD_CDS_DBTAB_FLD_INDEX
*&---------------------------------------------------------------------*
*& Analyses the selected CDS views and stores the analysis result
*  in the DB
*&---------------------------------------------------------------------*
REPORT zcdssearch_build_index_via_job.
TABLES ddldependency.

SELECT-OPTIONS s_vname FOR ddldependency-ddlname.

INITIALIZATION.
  s_vname[] = VALUE #( sign   = 'I'
                       option = 'CP'
                       ( low = 'I_*' )
                       ( low = 'P_*' )
                       ( low = 'R_*' )
                       ( low = 'A_*' ) ).

START-OF-SELECTION.
  DATA job_parameters    TYPE cl_apj_rt_api=>tt_job_parameter_value.

  DATA(job_parameter) = VALUE cl_apj_rt_api=>ty_job_parameter_value( name = 'S_VNAME' ).
  LOOP AT s_vname[] INTO DATA(selection_line).
    INSERT VALUE #( sign   = selection_line-sign
                    option = selection_line-option
                    low    = selection_line-low
                    high   = selection_line-high ) INTO TABLE job_parameter-t_value.
  ENDLOOP.

  " 2. Startzeitpunkt definieren (Sofort-Start)
  DATA(job_start_info) = VALUE cl_apj_rt_api=>ty_start_info( start_immediately = abap_true ).

  TRY.
      " 3. Job einplanen
      cl_apj_rt_api=>schedule_job( EXPORTING iv_job_template_name   = 'ZCDSSEARCH_BUILD_INDEX_TEMPL'
                                             is_start_info          = job_start_info
                                             iv_job_text            = 'CDS Index Build'
                                             it_job_parameter_value = job_parameters
                                   IMPORTING ev_jobname             = DATA(job_name)
                                             ev_jobcount            = DATA(job_count) ).

      MESSAGE |Job { job_name } ({ job_count }) wurde erfolgreich eingeplant.| TYPE 'S'.

    CATCH cx_apj_rt INTO DATA(exception).
      MESSAGE exception->get_text( ) TYPE 'E'.
  ENDTRY.
