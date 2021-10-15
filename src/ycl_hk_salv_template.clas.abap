CLASS ycl_hk_salv_template DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA gr_salv_table TYPE REF TO cl_salv_table .
    DATA gt_data TYPE REF TO data .

    METHODS refresh .
    METHODS create_header IMPORTING !iv_header_text TYPE any
                                    !iv_action_text TYPE any
                                    !iv_logo        TYPE bds_typeid OPTIONAL
                                    !iv_footer_text TYPE any OPTIONAL.
    METHODS display .
    METHODS hotspot_column
      IMPORTING
        !iv_columnname TYPE lvc_fname .
    METHODS hide_button
      IMPORTING
        !iv_button TYPE string .
    METHODS set_column_color
      IMPORTING
        !iv_columnname TYPE lvc_fname
        !iv_color      TYPE char1 .
    METHODS factory
      IMPORTING
        !iv_list_display         TYPE sap_bool DEFAULT if_salv_c_bool_sap=>false
        !iv_title                TYPE lvc_title DEFAULT sy-title
        !iv_enable_all_functions TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !iv_striped_pattern      TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !iv_delete_columns       TYPE char100 OPTIONAL
        !iv_aggregation_columns  TYPE char100 OPTIONAL
        !iv_sort_columns         TYPE char100 OPTIONAL
        !iv_hotspot_columns      TYPE char100 OPTIONAL
        !iv_hide_buttons         TYPE char100 OPTIONAL
        !iv_layout_key_handle    TYPE slis_handl OPTIONAL
        !iv_layout_restriction   TYPE salv_de_layout_restriction DEFAULT if_salv_c_layout=>restrict_none
        !iv_popup_type           TYPE salv_de_constant OPTIONAL
        !iv_selection_mode       TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>row_column
        !ir_container            TYPE REF TO cl_gui_container OPTIONAL
        !iv_container_name       TYPE string OPTIONAL
        !iv_layout               TYPE slis_vari OPTIONAL
        !iv_no_display           TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !iv_cprog                TYPE syst_cprog DEFAULT sy-cprog
        !iv_form_prog            TYPE syst_cprog
        !iv_pfstatus             TYPE sypfkey
        !iv_color_column         TYPE lvc_fname OPTIONAL
      CHANGING
        !ct_table                TYPE table .
    METHODS delete_column
      IMPORTING
        !iv_columnname TYPE lvc_fname .
    METHODS aggregation_column
      IMPORTING
        !iv_columnname TYPE lvc_fname .
    METHODS sort_column
      IMPORTING
        !iv_columnname TYPE lvc_fname .
    METHODS edit_column
      IMPORTING
        !iv_columname   TYPE lvc_fname
        !iv_long_text   TYPE scrtext_l OPTIONAL
        !iv_medium_text TYPE scrtext_m OPTIONAL
        !iv_short_text  TYPE scrtext_s OPTIONAL
        !iv_sign        TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true
        !iv_zero        TYPE sap_bool DEFAULT if_salv_c_bool_sap=>true .
    METHODS show_button
      IMPORTING
        !iv_button TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gv_cprog TYPE syst_cprog .
    DATA gv_form_prog TYPE syst_cprog .
    DATA gr_container TYPE REF TO cl_gui_custom_container .

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
    METHODS on_data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
        !e_modified
        !et_good_cells .
    METHODS on_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        !er_data_changed
        !e_onf4
        !e_onf4_before
        !e_onf4_after
        !e_ucomm .
    METHODS on_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
    METHODS delete_columns
      IMPORTING
        !iv_columnnames TYPE char100 .
    METHODS aggregation_columns
      IMPORTING
        !iv_columnnames TYPE char100 .
    METHODS sort_columns
      IMPORTING
        !iv_columnnames TYPE char100 .
    METHODS hotspot_columns
      IMPORTING
        !iv_columnnames TYPE char100 .
    METHODS register_events .
    METHODS export_pdf.
    METHODS hide_buttons
      IMPORTING !iv_buttons TYPE char100.
ENDCLASS.



CLASS ycl_hk_salv_template IMPLEMENTATION.


  METHOD delete_column.

    TRY .
        gr_salv_table->get_columns( )->get_column( iv_columnname )->set_technical( abap_true ) .

      CATCH cx_root INTO DATA(lr_error).
        TRY .
            DATA(lr_columns) = gr_salv_table->get_columns( ) .

            LOOP AT lr_columns->get( ) INTO DATA(ls_column) WHERE columnname CP iv_columnname .
              gr_salv_table->get_columns( )->get_column( ls_column-columnname )->set_technical( abap_true ) .
            ENDLOOP .
          CATCH cx_root INTO lr_error.
            MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
        ENDTRY .
    ENDTRY .
  ENDMETHOD.


  METHOD delete_columns.
    DATA lt_columnnames TYPE TABLE OF lvc_fname .

    TRY .
        SPLIT iv_columnnames AT ',' INTO TABLE lt_columnnames .

        LOOP AT lt_columnnames INTO DATA(ls_columnnames) .
          delete_column( ls_columnnames ) .
        ENDLOOP .

      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD display.

    TRY .
        gr_salv_table->display( ) .

      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD edit_column.
    TRY .

        DATA(lo_column) = gr_salv_table->get_columns( )->get_column( iv_columname ).
        IF iv_long_text IS NOT INITIAL.
          lo_column->set_long_text( iv_long_text ).
        ENDIF.
        IF iv_medium_text IS NOT INITIAL.
          lo_column->set_medium_text( iv_medium_text ).
        ENDIF.
        IF iv_short_text IS NOT INITIAL.
          lo_column->set_short_text( iv_short_text ).
        ENDIF.
        lo_column->set_sign( iv_sign ).
        lo_column->set_zero( iv_zero ).
      CATCH cx_root INTO  DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD export_pdf.
    DATA: lv_size         TYPE i,
          lv_pdf_data     TYPE xstring,
          lv_spoolid      TYPE tsp01-rqident,
          lv_fextn        TYPE string,
          lv_fname        TYPE string,
          lv_fpath        TYPE string,
          lv_fullpath     TYPE string,
          lv_default_path TYPE string.

    IF sy-binpt NE 'C' .
      RETURN .
    ENDIF .

    CLEAR sy-batch .
    CLEAR sy-binpt .

    IF sy-spono IS INITIAL .
      RETURN .
    ENDIF .

    TRY .
        lv_spoolid = sy-spono .

*--
        CALL METHOD cl_gui_frontend_services=>get_desktop_directory
          CHANGING
            desktop_directory    = lv_default_path
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.

          lv_fextn = '.pdf' .

          CALL METHOD cl_gui_frontend_services=>file_save_dialog
            EXPORTING
              default_extension    = lv_fextn
              default_file_name    = lv_fpath
              file_filter          = lv_fextn
              initial_directory    = lv_default_path
            CHANGING
              filename             = lv_fname
              path                 = lv_fpath
              fullpath             = lv_fullpath
            EXCEPTIONS
              cntl_error           = 1
              error_no_gui         = 2
              not_supported_by_gui = 3
              OTHERS               = 4.

          IF sy-subrc <> 0 OR  lv_fname IS INITIAL .
            RETURN .
          ENDIF.

*--
          CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
            EXPORTING
              src_spoolid              = lv_spoolid
              no_dialog                = 'X'
              pdf_destination          = 'X'
            IMPORTING
              pdf_bytecount            = lv_size
              bin_file                 = lv_pdf_data
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
            IF lv_pdf_data IS NOT INITIAL .
              DATA(lt_pdf) = cl_bcs_convert=>xstring_to_solix( lv_pdf_data ).

              cl_gui_frontend_services=>gui_download(
                EXPORTING
                  bin_filesize = lv_size
                  filename     = lv_fullpath
                  filetype     = 'BIN'
                CHANGING
                  data_tab     = lt_pdf ).
            ENDIF .
          ELSE.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.


      CATCH cx_root INTO DATA(lr_error) ##CATCH_ALL .
        MESSAGE lr_error->get_text( ) TYPE 'I' .
    ENDTRY .
  ENDMETHOD.


  METHOD factory.
    DATA ls_key TYPE salv_s_layout_key .
    DATA lv_container_name TYPE char80 .
    DATA lr_error TYPE REF TO cx_root .
    DATA ls_layout_key TYPE salv_s_layout_key.

    gv_cprog = iv_cprog .
    gv_form_prog = iv_form_prog .
*--
    TRY .
        gt_data = REF #( ct_table ) .

        IF iv_container_name IS NOT INITIAL OR ir_container IS NOT INITIAL .
          IF iv_container_name IS NOT INITIAL .

            IF cl_salv_table=>is_offline( ) EQ if_salv_c_bool_sap=>false .
              lv_container_name = iv_container_name .

              CREATE OBJECT gr_container
                EXPORTING
                  container_name = lv_container_name.
            ENDIF .

            CALL METHOD cl_salv_table=>factory
              EXPORTING
                r_container    = gr_container
                container_name = iv_container_name
              IMPORTING
                r_salv_table   = gr_salv_table
              CHANGING
                t_table        = ct_table.

          ELSEIF ir_container IS NOT INITIAL.
            CALL METHOD cl_salv_table=>factory
              EXPORTING
                r_container  = ir_container
              IMPORTING
                r_salv_table = gr_salv_table
              CHANGING
                t_table      = ct_table.
          ENDIF .
        ELSE .
          CALL METHOD cl_salv_table=>factory
            EXPORTING
              list_display = iv_list_display
            IMPORTING
              r_salv_table = gr_salv_table
            CHANGING
              t_table      = ct_table.


          CALL METHOD gr_salv_table->set_screen_status
            EXPORTING
              report        = iv_cprog
              pfstatus      = iv_pfstatus
              set_functions = cl_salv_model_base=>c_functions_all.
        ENDIF .

        CHECK gr_salv_table IS BOUND .

*--
        IF iv_layout IS NOT INITIAL.
          ls_layout_key-report = iv_cprog.
          DATA(lr_layout) = gr_salv_table->get_layout( ).

          lr_layout->set_key(  ls_layout_key  ).

          lr_layout->set_initial_layout( iv_layout ).
        ENDIF.

*--
        IF iv_hide_buttons IS NOT INITIAL.
          hide_buttons( iv_hide_buttons ).
        ENDIF.

*--
        IF iv_aggregation_columns IS NOT INITIAL.
          aggregation_columns( iv_aggregation_columns ).
        ENDIF.
*--
        IF iv_sort_columns IS NOT INITIAL.
          sort_columns( iv_sort_columns ).
        ENDIF.


        IF iv_delete_columns IS NOT INITIAL .
          delete_columns( iv_delete_columns ) .
        ENDIF .

*--
        IF iv_hotspot_columns IS NOT INITIAL.
          hotspot_columns( iv_hotspot_columns  ).
        ENDIF.

        register_events( ) .

*--
        IF iv_title IS NOT INITIAL .
          gr_salv_table->get_display_settings( )->set_list_header( iv_title ) .
        ENDIF .

*--
        IF iv_enable_all_functions EQ abap_true .
          gr_salv_table->get_functions( )->set_all( ).
        ENDIF .

*--
        gr_salv_table->get_selections( )->set_selection_mode( iv_selection_mode ) .

*--
        gr_salv_table->get_columns( )->set_optimize( if_salv_c_bool_sap=>true ) .

*--
        gr_salv_table->get_display_settings( )->set_striped_pattern( iv_striped_pattern ) .

*--
        ls_key-report = gv_cprog .
        ls_key-handle = iv_layout_key_handle .
        gr_salv_table->get_layout( )->set_key( ls_key ) .
        gr_salv_table->get_layout( )->set_save_restriction( iv_layout_restriction ) .
        gr_salv_table->get_layout( )->set_default( if_salv_c_bool_sap=>true ).

*--
        gr_salv_table->get_screen_status(
          IMPORTING
            report   = DATA(lv_report)
            pfstatus = DATA(lv_pfstatus) ).

*--
        IF iv_color_column IS NOT INITIAL.
          DATA(lo_columns) = gr_salv_table->get_columns( ).
          lo_columns->set_color_column( iv_color_column ).
        ENDIF.


**--
        CASE iv_popup_type .
          WHEN 100." small
            gr_salv_table->set_screen_popup(
              start_column = 10
              end_column   = 80
              start_line   = 3
              end_line     = 10 ).
          WHEN 200 . "medium
            gr_salv_table->set_screen_popup(
              start_column = 10
              end_column   = 120
              start_line   = 3
              end_line     = 15 ).
          WHEN 400 ."large
            gr_salv_table->set_screen_popup(
              start_column = 10
              end_column   = 250
              start_line   = 3
              end_line     = 25 ).
        ENDCASE .

*--
        me->gr_salv_table = gr_salv_table .          " for events


*--
        IF iv_no_display EQ abap_false .
          gr_salv_table->display( ) .
        ENDIF .


      CATCH cx_root INTO lr_error .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY.
  ENDMETHOD.


  METHOD hotspot_column.
    DATA lr_column  TYPE REF TO cl_salv_column_table.
    TRY .
        lr_column ?= gr_salv_table->get_columns( )->get_column( iv_columnname ).
        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_root INTO DATA(lr_error) .
        TRY .
            DATA(lr_columns) = gr_salv_table->get_columns( ) .

            LOOP AT lr_columns->get( ) INTO DATA(ls_column) WHERE columnname CP iv_columnname .
              lr_column ?= gr_salv_table->get_columns( )->get_column( ls_column-columnname ).
              lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
            ENDLOOP .
          CATCH cx_root INTO lr_error .
            MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
        ENDTRY .
    ENDTRY .
  ENDMETHOD.


  METHOD hotspot_columns.
    DATA lt_columnnames TYPE TABLE OF lvc_fname .

    TRY .
        SPLIT iv_columnnames AT ',' INTO TABLE lt_columnnames .

        LOOP AT lt_columnnames INTO DATA(ls_columnnames) .
          hotspot_column( ls_columnnames ) .
        ENDLOOP .

      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD on_after_user_command.
    export_pdf( ) .

    PERFORM salv_on_after_user_command IN PROGRAM (gv_form_prog) IF FOUND
      USING gr_salv_table e_salv_function.
  ENDMETHOD.


  METHOD on_before_user_command.
    PERFORM salv_on_before_user_command IN PROGRAM (gv_form_prog) IF FOUND
      USING gr_salv_table e_salv_function.
  ENDMETHOD.


  METHOD on_data_changed.
    DATA lt_row_id TYPE int4_table .
    DATA lv_row_id LIKE LINE OF lt_row_id .
    DATA ls_good_cells LIKE LINE OF er_data_changed->mt_good_cells .

    FIELD-SYMBOLS:
      <fs_row>   TYPE any,
      <fs_table> TYPE STANDARD TABLE.

    TRY .
        LOOP AT er_data_changed->mt_good_cells INTO ls_good_cells .
          COLLECT ls_good_cells-row_id INTO lt_row_id .
        ENDLOOP .
        CHECK sy-subrc EQ 0 .

        UNASSIGN <fs_table> .
        ASSIGN er_data_changed->mp_mod_rows->* TO <fs_table> .
        CHECK sy-subrc EQ 0
          AND <fs_table> IS ASSIGNED .

        LOOP AT <fs_table> ASSIGNING <fs_row> .
          READ TABLE lt_row_id INTO lv_row_id INDEX sy-tabix .
          CHECK sy-subrc EQ 0 .

          PERFORM salv_on_data_changed IN PROGRAM (gv_form_prog) IF FOUND
            USING er_data_changed
                  lv_row_id
                  <fs_row> .
        ENDLOOP .

      CATCH cx_root .
    ENDTRY .

  ENDMETHOD.


  METHOD on_data_changed_finished.
    DATA ls_cells TYPE lvc_s_modi .
    DATA lt_row_id TYPE int4_table .
    DATA lv_row_id LIKE LINE OF lt_row_id .

    CHECK e_modified EQ abap_true .

    LOOP AT et_good_cells INTO ls_cells .
      COLLECT ls_cells-row_id INTO lt_row_id .
    ENDLOOP .

    PERFORM salv_on_data_changed_finished IN PROGRAM (gv_form_prog) IF FOUND
      USING lt_row_id[] .

    refresh( )  .

  ENDMETHOD.


  METHOD on_double_click.
    PERFORM salv_on_double_click IN PROGRAM (gv_form_prog) IF FOUND
      USING gr_salv_table row column .
  ENDMETHOD.


  METHOD on_end_of_page.

    PERFORM salv_on_end_of_page IN PROGRAM (gv_form_prog) IF FOUND
      USING gr_salv_table r_end_of_page page.

  ENDMETHOD.


  METHOD on_link_click.

    PERFORM salv_on_link_click IN PROGRAM (gv_form_prog) IF FOUND
      USING gr_salv_table row column .

  ENDMETHOD.


  METHOD on_top_of_page.

    PERFORM salv_on_top_of_page IN PROGRAM (gv_form_prog) IF FOUND
      USING gr_salv_table r_top_of_page page table_index .


  ENDMETHOD.


  METHOD on_user_command.

    PERFORM salv_on_user_command IN PROGRAM (gv_form_prog) IF FOUND
      USING gr_salv_table e_salv_function.
  ENDMETHOD.


  METHOD refresh.

    TRY .
        gr_salv_table->refresh( s_stable = VALUE #( row = abap_true col = abap_true ) ).

      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD register_events.

    TRY .
        DATA(lr_events) = gr_salv_table->get_event( ) .

        SET HANDLER on_user_command FOR lr_events .
        SET HANDLER on_double_click FOR lr_events .
        SET HANDLER on_before_user_command FOR lr_events .
        SET HANDLER on_after_user_command FOR lr_events .
        SET HANDLER on_top_of_page FOR lr_events .
        SET HANDLER on_end_of_page FOR lr_events .
        SET HANDLER on_link_click FOR lr_events .

      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD set_column_color.
    DATA lr_col_tab TYPE REF TO cl_salv_column_table .

    TRY .
        lr_col_tab ?= gr_salv_table->get_columns( )->get_column( iv_columnname ).
        lr_col_tab->set_color( VALUE #( col = iv_color ) ) .

      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD hide_buttons.
    DATA lt_buttons TYPE TABLE OF string .

    TRY .
        SPLIT iv_buttons AT ',' INTO TABLE lt_buttons .

        LOOP AT lt_buttons INTO DATA(ls_button) .
          hide_button( ls_button ) .
        ENDLOOP .
      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD hide_button.
    DATA: lo_functions TYPE REF TO cl_salv_functions.

* Get all functions
    lo_functions ?=   gr_salv_table->get_functions( ).
    DATA(lt_func_list) = lo_functions->get_functions( ).

* Now hide the MYFUNCTION
    LOOP AT lt_func_list INTO DATA(ls_func_list).
      IF ls_func_list-r_function->get_name( ) = iv_button.
        ls_func_list-r_function->set_visible( abap_false ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD show_button.
    DATA: lo_functions TYPE REF TO cl_salv_functions.

* Get all functions
    lo_functions ?=   gr_salv_table->get_functions( ).
    DATA(lt_func_list) = lo_functions->get_functions( ).

* Now show the MYFUNCTION
    LOOP AT lt_func_list INTO DATA(ls_func_list).
      IF ls_func_list-r_function->get_name( ) = iv_button.
        ls_func_list-r_function->set_visible( abap_true ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_header.
    DATA : ls_address      TYPE  bapiaddr3,
           lt_return       TYPE TABLE OF bapiret2,
           lo_layout_grid1 TYPE REF TO cl_salv_form_layout_data_grid,
           lo_layout_grid2 TYPE REF TO cl_salv_form_layout_data_grid,
           lo_layout_grid3 TYPE REF TO cl_salv_form_layout_data_grid,
           lo_layout_grid4 TYPE REF TO cl_salv_form_layout_data_grid.


    DATA(lo_top_element) = NEW cl_salv_form_layout_grid( 10 ).
    DATA(lo_header) = lo_top_element->create_header_information(
      row     = 1
      column  = 1
      text    = iv_header_text
      tooltip = iv_header_text ).

    DATA(lo_action) = lo_top_element->create_action_information(
      row     = 2
      column  = 1
      text    = iv_action_text
      tooltip = iv_action_text ).

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

    DATA(lv_msj) = |{ TEXT-i03 } { ls_address-firstname } { ls_address-lastname }|."Hello,Welcome !

    DATA(lo_textview5) = lo_grid->create_text(
      row    = 3
      column = 1
      text   = lv_msj
    ).

    lo_layout_grid1 ?= lo_textview1->get_layout_data( ).
    lo_layout_grid2 ?= lo_textview2->get_layout_data( ).
    lo_layout_grid3 ?= lo_textview3->get_layout_data( ).
    lo_layout_grid4 ?= lo_textview4->get_layout_data( ).

    lo_layout_grid1->set_h_align( if_salv_form_c_h_align=>left ).
    lo_layout_grid2->set_h_align( if_salv_form_c_h_align=>left ).
    lo_layout_grid3->set_h_align( if_salv_form_c_h_align=>left ).
    lo_layout_grid4->set_h_align( if_salv_form_c_h_align=>left ).

    gr_salv_table->set_top_of_list( lo_top_element ).

    IF iv_logo IS NOT INITIAL.
      DATA(lo_logo) = NEW cl_salv_form_layout_logo( ).
* set left content
      lo_logo->set_left_content( lo_top_element ).

* set Right Image
      lo_logo->set_right_logo( iv_logo ).

*   set the top of list using the header for Online.
      gr_salv_table->set_top_of_list( lo_logo ).

    ENDIF.

    IF iv_footer_text IS NOT INITIAL.

      DATA(lo_eol) = NEW cl_salv_form_header_info( text = iv_footer_text ).
      gr_salv_table->set_end_of_list( lo_eol ).

    ENDIF.
  ENDMETHOD.


  METHOD aggregation_columns.
    DATA lt_columnnames TYPE TABLE OF lvc_fname .

    TRY .
        SPLIT iv_columnnames AT ',' INTO TABLE lt_columnnames .

        LOOP AT lt_columnnames INTO DATA(ls_columnnames) .
          aggregation_column( ls_columnnames ) .
        ENDLOOP .

      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD aggregation_column.

    TRY .
        DATA(lo_aggrs) = gr_salv_table->get_aggregations( ).
        lo_aggrs->add_aggregation( columnname = iv_columnname ).

      CATCH cx_root INTO DATA(lr_error).
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD sort_column.

    TRY .
        DATA(lo_sort) = gr_salv_table->get_sorts( ).
        lo_sort->add_sort( columnname = iv_columnname )->set_subtotal( ).

      CATCH cx_root INTO DATA(lr_error).
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


  METHOD sort_columns.
    DATA lt_columnnames TYPE TABLE OF lvc_fname .

    TRY .
        SPLIT iv_columnnames AT ',' INTO TABLE lt_columnnames .

        LOOP AT lt_columnnames INTO DATA(ls_columnnames) .
          sort_column( ls_columnnames ) .
        ENDLOOP .

      CATCH cx_root INTO DATA(lr_error) .
        MESSAGE lr_error->get_longtext( ) TYPE 'I' DISPLAY LIKE 'E' .
    ENDTRY .
  ENDMETHOD.


ENDCLASS.
