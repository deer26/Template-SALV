CLASS zcl_hk_salv_template DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA go_salv_table TYPE REF TO cl_salv_table .
    DATA gr_data TYPE REF TO data .

    METHODS constructor IMPORTING !if_cprog  TYPE syst_cprog DEFAULT sy-cprog .

    METHODS refresh .
    METHODS create_header
      IMPORTING
        !if_header_text TYPE any
        !if_action_text TYPE any
        !if_logo        TYPE bds_typeid OPTIONAL
        !if_footer_text TYPE any OPTIONAL .
    METHODS display .
    METHODS hotspot_column
      IMPORTING
        !if_columnname TYPE lvc_fname .
    METHODS hide_button
      IMPORTING
        !if_button TYPE string .
    METHODS set_column_color
      IMPORTING
        !if_columnname TYPE lvc_fname
        !if_color      TYPE char1 .
    METHODS factory
      IMPORTING
        !if_list_display         TYPE sap_bool DEFAULT if_salv_c_bool_sap=>false
        !if_title                TYPE lvc_title DEFAULT sy-title
        !if_enable_all_functions TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !if_striped_pattern      TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !if_delete_columns       TYPE char100 OPTIONAL
        !if_aggregation_columns  TYPE char100 OPTIONAL
        !if_sort_columns         TYPE char100 OPTIONAL
        !if_hotspot_columns      TYPE char100 OPTIONAL
        !if_hide_buttons         TYPE char100 OPTIONAL
        !if_layout_key_handle    TYPE slis_handl OPTIONAL
        !if_layout_restriction   TYPE salv_de_layout_restriction DEFAULT if_salv_c_layout=>restrict_none
        !if_popup_type           TYPE salv_de_constant OPTIONAL
        !if_selection_mode       TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>row_column
        !io_container            TYPE REF TO cl_gui_container OPTIONAL
        !if_container_name       TYPE string OPTIONAL
        !if_layout               TYPE slis_vari OPTIONAL
        !if_no_display           TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !if_form_prog            TYPE syst_cprog DEFAULT sy-cprog
        !if_pfstatus             TYPE sypfkey OPTIONAL
        !if_color_column         TYPE lvc_fname OPTIONAL
        !if_temp_pfstatus        TYPE sypfkey DEFAULT 'STANDARD'
        !if_temp_report          TYPE syrepid DEFAULT 'SAPLSALV'
        !if_no_mandt             TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
      CHANGING
        !ct_table                TYPE STANDARD TABLE .
    METHODS delete_column
      IMPORTING
        !if_columnname TYPE lvc_fname .
    METHODS aggregation_column
      IMPORTING
        !if_columnname TYPE lvc_fname .
    METHODS sort_column
      IMPORTING
        !if_columnname TYPE lvc_fname .
    METHODS edit_column
      IMPORTING
        !if_columname   TYPE lvc_fname
        !if_long_text   TYPE scrtext_l OPTIONAL
        !if_medium_text TYPE scrtext_m OPTIONAL
        !if_short_text  TYPE scrtext_s OPTIONAL
        !if_sign        TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !if_zero        TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true .
    METHODS show_button
      IMPORTING
        !if_button TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gf_cprog TYPE syst_cprog .
    DATA gf_form_prog TYPE syst_cprog .
    DATA go_container TYPE REF TO cl_gui_custom_container .

    METHODS on_user_command
      FOR EVENT added_function OF cl_salv_events
      IMPORTING
        !e_salv_function .
    METHODS on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
    METHODS on_before_user_command
      FOR EVENT before_salv_function OF cl_salv_events
      IMPORTING
        !e_salv_function .
    METHODS on_after_user_command
      FOR EVENT after_salv_function OF cl_salv_events
      IMPORTING
        !e_salv_function .
    METHODS on_top_of_page
      FOR EVENT top_of_page OF cl_salv_events_table
      IMPORTING
        !r_top_of_page
        !page
        !table_index .
    METHODS on_end_of_page
      FOR EVENT end_of_page OF cl_salv_events_table
      IMPORTING
        !r_end_of_page
        !page .
    METHODS on_data_changed_finished ##RELAX
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
        !e_modified
        !et_good_cells .
    METHODS on_data_changed ##RELAX
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        !er_data_changed
        !e_onf4
        !e_onf4_before
        !e_onf4_after
        !e_ucomm
      .
    METHODS on_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
    METHODS delete_columns
      IMPORTING
        !if_columnnames TYPE char100 .
    METHODS aggregation_columns
      IMPORTING
        !if_columnnames TYPE char100 .
    METHODS sort_columns
      IMPORTING
        !if_columnnames TYPE char100 .
    METHODS hotspot_columns
      IMPORTING
        !if_columnnames TYPE char100 .
    METHODS register_events .
    METHODS export_pdf.
    METHODS hide_buttons
      IMPORTING !if_buttons TYPE char100.
ENDCLASS.



CLASS ZCL_HK_SALV_TEMPLATE IMPLEMENTATION.


  METHOD sort_columns.
    DATA lt_columnnames TYPE TABLE OF lvc_fname .

    TRY .
        SPLIT if_columnnames AT ',' INTO TABLE lt_columnnames .

        LOOP AT lt_columnnames REFERENCE INTO DATA(lr_columnnames) .
          sort_column( lr_columnnames->* ) .
        ENDLOOP .

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD sort_column.

    TRY .
        DATA(lo_sort) = go_salv_table->get_sorts( ).
        lo_sort->add_sort( columnname = if_columnname )->set_subtotal( ).

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD show_button.
    DATA: lo_functions TYPE REF TO cl_salv_functions.

* Get all functions
*   lo_functions ?=   go_salv_table->get_functions( ).
    lo_functions =   go_salv_table->get_functions( ).
    DATA(lt_func_list) = lo_functions->get_functions( ).

* Now show the MYFUNCTION
    LOOP AT lt_func_list REFERENCE INTO DATA(lr_func_list).
      IF lr_func_list->r_function->get_name( ) = if_button.
        lr_func_list->r_function->set_visible( abap_true ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_column_color.
    DATA lo_col_tab TYPE REF TO cl_salv_column_table .

    TRY .
        lo_col_tab ?= go_salv_table->get_columns( )->get_column( if_columnname ).
        lo_col_tab->set_color( VALUE #( col = if_color ) ) . "#EC CI_CONV_OK

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD register_events.

    TRY .
        DATA(lo_events) = go_salv_table->get_event( ) .

        SET HANDLER on_user_command FOR lo_events .
        SET HANDLER on_double_click FOR lo_events .
        SET HANDLER on_before_user_command FOR lo_events .
        SET HANDLER on_after_user_command FOR lo_events .
        SET HANDLER on_top_of_page FOR lo_events .
        SET HANDLER on_end_of_page FOR lo_events .
        SET HANDLER on_link_click FOR lo_events .

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD refresh.

    TRY .
        go_salv_table->refresh( s_stable = VALUE #( row = abap_true col = abap_true ) ).

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD on_user_command.

    PERFORM salv_on_user_command IN PROGRAM (gf_form_prog) IF FOUND
      USING go_salv_table e_salv_function.
  ENDMETHOD.


  METHOD on_top_of_page.

    PERFORM salv_on_top_of_page IN PROGRAM (gf_form_prog) IF FOUND
      USING go_salv_table r_top_of_page page table_index .


  ENDMETHOD.


  METHOD on_link_click.

    PERFORM salv_on_link_click IN PROGRAM (gf_form_prog) IF FOUND
      USING go_salv_table row column .

  ENDMETHOD.


  METHOD on_end_of_page.

    PERFORM salv_on_end_of_page IN PROGRAM (gf_form_prog) IF FOUND
      USING go_salv_table r_end_of_page page.

  ENDMETHOD.


  METHOD on_double_click.
    PERFORM salv_on_double_click IN PROGRAM (gf_form_prog) IF FOUND
      USING go_salv_table row column .
  ENDMETHOD.


  METHOD on_data_changed_finished.
    DATA lt_row_id TYPE int4_table .

    CHECK e_modified EQ abap_true .

    LOOP AT et_good_cells REFERENCE INTO DATA(lr_cells) .
      COLLECT lr_cells->row_id INTO lt_row_id .
    ENDLOOP .

    PERFORM salv_on_data_changed_finished IN PROGRAM (gf_form_prog) IF FOUND
      USING lt_row_id[] .

    refresh( )  .

  ENDMETHOD.


  METHOD on_data_changed.
    DATA lt_row_id TYPE int4_table .
    DATA lf_row_id LIKE LINE OF lt_row_id .

    FIELD-SYMBOLS:
      <ls_row>   TYPE any,
      <lt_table> TYPE STANDARD TABLE.

    TRY .
        LOOP AT er_data_changed->mt_good_cells REFERENCE INTO DATA(lr_good_cells) .
          COLLECT lr_good_cells->row_id INTO lt_row_id .
        ENDLOOP .
        CHECK sy-subrc EQ 0 .

        UNASSIGN <lt_table> .
        ASSIGN er_data_changed->mp_mod_rows->* TO <lt_table> .
        CHECK sy-subrc EQ 0
          AND <lt_table> IS ASSIGNED .

        LOOP AT <lt_table> ASSIGNING <ls_row> .
          READ TABLE lt_row_id INTO lf_row_id INDEX sy-tabix .
          CHECK sy-subrc EQ 0 .

          PERFORM salv_on_data_changed IN PROGRAM (gf_form_prog) IF FOUND
            USING er_data_changed
                  lf_row_id
                  <ls_row>
                  e_onf4
                  e_onf4_before
                  e_onf4_after
                  e_ucomm.
        ENDLOOP .

      CATCH cx_root INTO DATA(lo_root) ##CATCH_ALL.
        MESSAGE lo_root->get_longtext( ) TYPE 'I'.
    ENDTRY .

  ENDMETHOD.


  METHOD on_before_user_command.
    PERFORM salv_on_before_user_command IN PROGRAM (gf_form_prog) IF FOUND
      USING go_salv_table e_salv_function.
  ENDMETHOD.


  METHOD on_after_user_command.
    export_pdf( ) .

    PERFORM salv_on_after_user_command IN PROGRAM (gf_form_prog) IF FOUND
      USING go_salv_table e_salv_function.
  ENDMETHOD.


  METHOD hotspot_columns.
    DATA lt_columnnames TYPE TABLE OF lvc_fname .

    TRY .
        SPLIT if_columnnames AT ',' INTO TABLE lt_columnnames .

        LOOP AT lt_columnnames REFERENCE INTO DATA(lr_columnnames) .
          hotspot_column( lr_columnnames->* ) .
        ENDLOOP .

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD hotspot_column.
    DATA lo_column  TYPE REF TO cl_salv_column_table.
    TRY .
        lo_column ?= go_salv_table->get_columns( )->get_column( if_columnname ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        TRY .
            DATA(lo_columns) = go_salv_table->get_columns( ) .

            LOOP AT lo_columns->get( ) REFERENCE INTO DATA(lr_column) WHERE columnname CP if_columnname .
              lo_column ?= go_salv_table->get_columns( )->get_column( lr_column->columnname ).
              lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
            ENDLOOP .
          CATCH cx_root INTO lo_error ##CATCH_ALL.
            MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
        ENDTRY .
    ENDTRY .
  ENDMETHOD.


  METHOD hide_buttons.
    DATA lt_buttons TYPE TABLE OF string .

    TRY .
        SPLIT if_buttons AT ',' INTO TABLE lt_buttons .

        LOOP AT lt_buttons REFERENCE INTO DATA(lr_button) .
          hide_button( lr_button->* ) .
        ENDLOOP .
      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD hide_button.
    DATA: lo_functions TYPE REF TO cl_salv_functions.

* Get all functions
*   lo_functions ?=   go_salv_table->get_functions( ).
    lo_functions =   go_salv_table->get_functions( ).
    DATA(lt_func_list) = lo_functions->get_functions( ).


* Now hide the MYFUNCTION
    LOOP AT lt_func_list REFERENCE INTO DATA(lr_func_list).
      IF lr_func_list->r_function->get_name( ) = if_button.
        lr_func_list->r_function->set_visible( abap_false ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD factory.
    DATA ls_key TYPE salv_s_layout_key .
    DATA lf_container_name TYPE char80 .
    DATA lo_error TYPE REF TO cx_root .
    DATA ls_layout_key TYPE salv_s_layout_key.


    gf_form_prog = if_form_prog .
*--
    TRY .
        gr_data = REF #( ct_table ) .

        IF if_container_name IS NOT INITIAL OR io_container IS NOT INITIAL .
          IF if_container_name IS NOT INITIAL .

            IF cl_salv_table=>is_offline( ) EQ if_salv_c_bool_sap=>false .
              lf_container_name = if_container_name .

              CREATE OBJECT go_container
                EXPORTING
                  container_name = lf_container_name.
            ENDIF .

            CALL METHOD cl_salv_table=>factory
              EXPORTING
                r_container    = go_container
                container_name = if_container_name
              IMPORTING
                r_salv_table   = go_salv_table
              CHANGING
                t_table        = ct_table.

          ELSEIF io_container IS NOT INITIAL.
            CALL METHOD cl_salv_table=>factory
              EXPORTING
                r_container  = io_container
              IMPORTING
                r_salv_table = go_salv_table
              CHANGING
                t_table      = ct_table.
          ENDIF .
        ELSE .
          CALL METHOD cl_salv_table=>factory
            EXPORTING
              list_display = if_list_display
            IMPORTING
              r_salv_table = go_salv_table
            CHANGING
              t_table      = ct_table.

          IF if_pfstatus IS INITIAL.
            CALL METHOD go_salv_table->set_screen_status
              EXPORTING
                report        = if_temp_report
                pfstatus      = if_temp_pfstatus
                set_functions = cl_salv_model_base=>c_functions_all.
          ELSE.
            CALL METHOD go_salv_table->set_screen_status
              EXPORTING
                report        = gf_cprog
                pfstatus      = if_pfstatus
                set_functions = cl_salv_model_base=>c_functions_all.
          ENDIF.


        ENDIF .

        CHECK go_salv_table IS BOUND .

*--
        IF if_layout IS NOT INITIAL.
          ls_layout_key-report = gf_cprog.
          DATA(lo_layout) = go_salv_table->get_layout( ).

          lo_layout->set_key(  ls_layout_key  ).

          lo_layout->set_initial_layout( if_layout ).
        ENDIF.

*--
        IF if_hide_buttons IS NOT INITIAL.
          hide_buttons( if_hide_buttons ).
        ENDIF.

*--
        IF if_aggregation_columns IS NOT INITIAL.
          aggregation_columns( if_aggregation_columns ).
        ENDIF.
*--
        IF if_sort_columns IS NOT INITIAL.
          sort_columns( if_sort_columns ).
        ENDIF.

        IF if_no_mandt EQ abap_true.
          delete_column( 'MANDT' ).
        ENDIF.

        IF if_delete_columns IS NOT INITIAL .
          delete_columns( if_delete_columns ) .
        ENDIF .

*--
        IF if_hotspot_columns IS NOT INITIAL.
          hotspot_columns( if_hotspot_columns  ).
        ENDIF.

        register_events( ) .

*--
        IF if_title IS NOT INITIAL .
          go_salv_table->get_display_settings( )->set_list_header( if_title ) .
        ENDIF .

*--
        IF if_enable_all_functions EQ abap_true .
          go_salv_table->get_functions( )->set_all( ).
        ENDIF .

*--
        go_salv_table->get_selections( )->set_selection_mode( if_selection_mode ) .

*--
        go_salv_table->get_columns( )->set_optimize( if_salv_c_bool_sap=>true ) .

*--
        go_salv_table->get_display_settings( )->set_striped_pattern( if_striped_pattern ) .

*--
        ls_key-report = gf_cprog .
        ls_key-handle = if_layout_key_handle .
        go_salv_table->get_layout( )->set_key( ls_key ) .
        go_salv_table->get_layout( )->set_save_restriction( if_layout_restriction ) .
        go_salv_table->get_layout( )->set_default( if_salv_c_bool_sap=>true ).

*--
        IF if_color_column IS NOT INITIAL.
          DATA(lo_columns) = go_salv_table->get_columns( ).
          lo_columns->set_color_column( if_color_column ).
        ENDIF.


**--
        CASE if_popup_type .
          WHEN 100." small
            go_salv_table->set_screen_popup(
              start_column = 10
              end_column   = 80
              start_line   = 3
              end_line     = 10 ).
          WHEN 200 . "medium
            go_salv_table->set_screen_popup(
              start_column = 10
              end_column   = 120
              start_line   = 3
              end_line     = 15 ).
          WHEN 400 ."large
            go_salv_table->set_screen_popup(
              start_column = 10
              end_column   = 250
              start_line   = 3
              end_line     = 25 ).
        ENDCASE .

*--
        me->go_salv_table = go_salv_table ##NEEDED. " for events


*--
        IF if_no_display EQ abap_false .
          go_salv_table->display( ) .
        ENDIF .


      CATCH cx_root INTO lo_error ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY.
  ENDMETHOD.


  METHOD export_pdf.
    DATA: lf_size         TYPE i,
          lf_pdf_data     TYPE xstring,
          lf_spoolid      TYPE tsp01-rqident,
          lf_fextn        TYPE string,
          lf_fname        TYPE string,
          lf_fpath        TYPE string,
          lf_fullpath     TYPE string,
          lf_default_path TYPE string.

    IF sy-binpt NE 'C' .
      RETURN .
    ENDIF .

    CLEAR sy-batch ##WRITE_OK.
    CLEAR sy-binpt ##WRITE_OK.

    IF sy-spono IS INITIAL .
      RETURN .
    ENDIF .

    TRY .
        lf_spoolid = sy-spono .                         "#EC CI_CONV_OK

*--
        CALL METHOD cl_gui_frontend_services=>get_desktop_directory
          CHANGING
            desktop_directory    = lf_default_path
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.

          lf_fextn = '.pdf'  ##NO_TEXT.

          CALL METHOD cl_gui_frontend_services=>file_save_dialog
            EXPORTING
              default_extension    = lf_fextn
              default_file_name    = lf_fpath
              file_filter          = lf_fextn
              initial_directory    = lf_default_path
            CHANGING
              filename             = lf_fname
              path                 = lf_fpath
              fullpath             = lf_fullpath
            EXCEPTIONS
              cntl_error           = 1
              error_no_gui         = 2
              not_supported_by_gui = 3
              OTHERS               = 4.

          IF sy-subrc <> 0 OR  lf_fname IS INITIAL .
            RETURN .
          ENDIF.

*--
          CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
            EXPORTING
              src_spoolid              = lf_spoolid
              no_dialog                = 'X'
              pdf_destination          = 'X'
            IMPORTING
              pdf_bytecount            = lf_size
              bin_file                 = lf_pdf_data
            EXCEPTIONS
              err_no_abap_spooljob     = 1
              err_no_spooljob          = 2
              err_no_permission        = 3
              err_conv_not_possible    = 4
              err_bad_destdevice       = 5
              user_cancelled           = 6
              err_spoolerror           = 7
              err_temseerror           = 8
              err_btcjob_open_failed   = 9
              err_btcjob_submit_failed = 10
              err_btcjob_close_failed  = 11
              OTHERS                   = 12.
          IF sy-subrc EQ 0.
            IF lf_pdf_data IS NOT INITIAL .
              DATA(lt_pdf) = cl_bcs_convert=>xstring_to_solix( lf_pdf_data ).

              cl_gui_frontend_services=>gui_download(
                EXPORTING
                  bin_filesize = lf_size
                  filename     = lf_fullpath
                  filetype     = 'BIN'
                CHANGING
                  data_tab     = lt_pdf ).
            ENDIF .
          ELSE.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.


      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL .
        MESSAGE lo_error->get_text( ) TYPE 'I' .
    ENDTRY .
  ENDMETHOD.


  METHOD edit_column.
    TRY .

        DATA(lo_column) = go_salv_table->get_columns( )->get_column( if_columname ).
        IF if_long_text IS NOT INITIAL.
          lo_column->set_long_text( if_long_text ).
        ENDIF.
        IF if_medium_text IS NOT INITIAL.
          lo_column->set_medium_text( if_medium_text ).
        ENDIF.
        IF if_short_text IS NOT INITIAL.
          lo_column->set_short_text( if_short_text ).
        ENDIF.
        lo_column->set_sign( if_sign ).
        lo_column->set_zero( if_zero ).
      CATCH cx_root INTO  DATA(lo_error) ##CATCH_ALL .
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD display.

    TRY .
        go_salv_table->display( ) .

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD delete_columns.
    DATA lt_columnnames TYPE TABLE OF lvc_fname .

    TRY .
        SPLIT if_columnnames AT ',' INTO TABLE lt_columnnames .

        LOOP AT lt_columnnames REFERENCE INTO DATA(lr_columnnames) .
          delete_column( lr_columnnames->* ) .
        ENDLOOP .

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD delete_column.

    TRY .
        go_salv_table->get_columns( )->get_column( if_columnname )->set_technical( abap_true ) .

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        TRY .
            DATA(lo_columns) = go_salv_table->get_columns( ) .

            LOOP AT lo_columns->get( ) REFERENCE INTO DATA(lr_column) WHERE columnname CP if_columnname .
              go_salv_table->get_columns( )->get_column( lr_column->columnname )->set_technical( abap_true ) .
            ENDLOOP .
          CATCH cx_root INTO lo_error ##CATCH_ALL.
            MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
        ENDTRY .
    ENDTRY .
  ENDMETHOD.


  METHOD create_header.
    DATA : ls_address      TYPE  bapiaddr3,
           lt_return       TYPE TABLE OF bapiret2,
           lo_layout_grid1 TYPE REF TO cl_salv_form_layout_data_grid,
           lo_layout_grid2 TYPE REF TO cl_salv_form_layout_data_grid,
           lo_layout_grid3 TYPE REF TO cl_salv_form_layout_data_grid,
           lo_layout_grid4 TYPE REF TO cl_salv_form_layout_data_grid.


    DATA(lo_top_element) = NEW cl_salv_form_layout_grid( 10 ).
    lo_top_element->create_header_information(
      row     = 1
      column  = 1
      text    = if_header_text
      tooltip = if_header_text ).

    lo_top_element->create_action_information(
      row     = 2
      column  = 1
      text    = if_action_text
      tooltip = if_action_text ).

    DATA(lo_grid) = lo_top_element->create_grid( row    = 3
                                                 column = 1 ).
    DATA(lo_textview1) = lo_grid->create_text(
      row    = 1
      column = 1
      text   = TEXT-i01 "Date
    ).

    DATA(lo_textview2) = lo_grid->create_text(
      row    = 1
      column = 2
      text   = |{ sy-datum DATE = ENVIRONMENT }|
    ).

    DATA(lo_textview3) = lo_grid->create_text(
      row    = 2
      column = 1
      text   = TEXT-i02 "Time
    ).


    DATA(lo_textview4) = lo_grid->create_text(
      row    = 2
      column = 2
      text   = |{ sy-uzeit TIME = ENVIRONMENT }|
    ).

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username      = sy-uname
        cache_results = 'X'
      IMPORTING
        address       = ls_address
      TABLES
        return        = lt_return.

    DATA(lf_msj) = |{ TEXT-i03 } { ls_address-firstname } { ls_address-lastname }|."Hello,Welcome !

    lo_grid->create_text(
      row    = 3
      column = 1
      text   = lf_msj
    ).

    lo_layout_grid1 ?= lo_textview1->get_layout_data( ).
    lo_layout_grid2 ?= lo_textview2->get_layout_data( ).
    lo_layout_grid3 ?= lo_textview3->get_layout_data( ).
    lo_layout_grid4 ?= lo_textview4->get_layout_data( ).

    lo_layout_grid1->set_h_align( if_salv_form_c_h_align=>left ).
    lo_layout_grid2->set_h_align( if_salv_form_c_h_align=>left ).
    lo_layout_grid3->set_h_align( if_salv_form_c_h_align=>left ).
    lo_layout_grid4->set_h_align( if_salv_form_c_h_align=>left ).

    go_salv_table->set_top_of_list( lo_top_element ).

    IF if_logo IS NOT INITIAL.
      DATA(lo_logo) = NEW cl_salv_form_layout_logo( ).
* set left content
      lo_logo->set_left_content( lo_top_element ).

* set Right Image
      lo_logo->set_right_logo( if_logo ).

*   set the top of list using the header for Online.
      go_salv_table->set_top_of_list( lo_logo ).

    ENDIF.

    IF if_footer_text IS NOT INITIAL.

      DATA(lo_eol) = NEW cl_salv_form_header_info( text = if_footer_text ).
      go_salv_table->set_end_of_list( lo_eol ).

    ENDIF.
  ENDMETHOD.


  METHOD constructor.
*** **** INSERT IMPLEMENTATION HERE **** ***
*======================================================================*
*  CR / ID / Date     : IE-xxx / dd.mm.20yy
*  Title              : <solution title>
*  Responsible        : <Consultant>, <Developer> / itelligence AG
*======================================================================
*  Change history
*     Date     | User     | CR / ID, changes
* -----------  ---------  ---------------------------------------------
*  dd.mm.20yy   <Username>  CR nnn / IE-xxx: .......
*
*======================================================================*
    gf_cprog = if_cprog .
  ENDMETHOD.


  METHOD aggregation_columns.
    DATA lt_columnnames TYPE TABLE OF lvc_fname .

    TRY .
        SPLIT if_columnnames AT ',' INTO TABLE lt_columnnames .

        LOOP AT lt_columnnames REFERENCE INTO DATA(lr_columnnames) .
          aggregation_column( lr_columnnames->* ) .
        ENDLOOP .

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD aggregation_column.

    TRY .
        DATA(lo_aggrs) = go_salv_table->get_aggregations( ).
        lo_aggrs->add_aggregation( columnname = if_columnname ).

      CATCH cx_root INTO DATA(lo_error) ##CATCH_ALL.
        MESSAGE lo_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.
ENDCLASS.
