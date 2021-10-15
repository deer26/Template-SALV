*&---------------------------------------------------------------------*
*& Include ysalv_template_example_f01
*&---------------------------------------------------------------------*

FORM get_default_layout CHANGING c_layout   TYPE disvariant-variant.

  c_layout = cl_salv_layout_service=>get_default_layout( s_key = VALUE #( report = sy-repid ) )-layout .

ENDFORM.                    " get_default_layout

FORM f4_layouts CHANGING c_layout TYPE disvariant-variant.

  c_layout = cl_salv_layout_service=>f4_layouts( s_key    = VALUE #( report = sy-repid )  )-layout.

ENDFORM.

FORM get_data.
  SELECT  carrid,
           connid,
           fldate,
           price,
           currency,
           planetype,
           seatsmax,
           seatsocc,
           paymentsum,
           seatsmax_b,
           seatsocc_b,
           seatsmax_f,
           seatsocc_f,
           '@AV@' AS icon,
           CASE WHEN seatsmax_b < 21 THEN '@01@ '
                WHEN seatsmax_b EQ 21 THEN '@BF@'
                ELSE '@00@' END AS statu
     FROM sflight AS s
   INTO CORRESPONDING FIELDS OF TABLE @gt_tab  .

  " for make colorfully row addition

  LOOP AT gt_tab REFERENCE INTO DATA(lo_tab).

    IF lo_tab->seatsmax_b < 21.
      lo_tab->color = VALUE #( ( color-col = 5 color-int = 0 color-inv = 0 )  ).
    ELSEIF lo_tab->seatsmax_b > 30.
      lo_tab->color = VALUE #( ( color-col = 4 color-int = 0 color-inv = 0 )  ).
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM set_data.



  sy-title = 'Kartal goool gool'.

  CREATE OBJECT gr_salv.

  gr_salv->factory(
    EXPORTING
      iv_delete_columns      = 'MANDT,SEATSMAX_F,SEATSOCC_F'
      iv_hotspot_columns     = 'CARRID,ICON'
      iv_hide_buttons        = 'DEL,GEL'
      iv_pfstatus            = 'STAT' " create status with name stat and put DEL,GEL,SAVe
      iv_aggregation_columns = 'SEATSMAX_B,SEATSOCC_B'
      iv_sort_columns        = 'CARRID,CONNID'
      iv_layout              = p_layout
      iv_color_column        = 'COLOR'
      iv_form_prog           = sy-repid
    CHANGING
      ct_table               = gt_tab[] ).


  gr_salv->delete_column( iv_columnname = 'SEATSOCC' ).

  gr_salv->set_column_color( iv_columnname = 'PRICE' iv_color = col_positive ).

  gr_salv->edit_column(
    iv_columname   = 'ICON'
    iv_long_text   = 'Uçuşlar'
    iv_medium_text = 'Uçuşlar'
    iv_short_text  = 'Uçuşlar'
    iv_sign        = if_salv_c_bool_sap=>true
    iv_zero        = if_salv_c_bool_sap=>false
  ).
  gr_salv->edit_column(
    iv_columname   = 'STATU'
    iv_long_text   = 'Statu'
    iv_medium_text = 'Statu'
    iv_short_text  = 'Statu'
    iv_sign        = if_salv_c_bool_sap=>true
    iv_zero        = if_salv_c_bool_sap=>false
  ).
  gr_salv->create_header(
    iv_header_text = |Öylesine bir rapor({ lines( gt_tab ) })|
    iv_action_text = 'BioNTech SE'
    iv_logo        = 'LOGO'
    iv_footer_text = 'BioNTech SE' ).

  gr_salv->display( ) .

ENDFORM.
FORM salv_on_user_command USING pr_salv_table TYPE REF TO cl_salv_table
                                pv_salv_function TYPE salv_de_function .

  DATA(lo_sel) = pr_salv_table->get_selections( ).
  DATA(lt_row) = lo_sel->get_selected_rows( ).

  CASE pv_salv_function .
    WHEN 'SAVE' .
      gr_salv->hide_button( 'SAVE' ).
      gr_salv->show_button( 'DEL' ).
      gr_salv->show_button( 'GEL' ).
      gr_salv->refresh( ).
    WHEN 'DEL' .
      gr_salv->hide_button( 'DEL' ).
      gr_salv->show_button( 'SAVE' ).
      DELETE gt_tab WHERE carrid = 'AA'.
      gr_salv->create_header( iv_header_text = |Öylesine bir rapor({ lines( gt_tab ) })|
                              iv_action_text = 'Biontech Se'
                              iv_logo        = 'LOGO' ).
      gr_salv->refresh( ).
    WHEN 'GEL' .
      gr_salv->hide_button( 'DEL' ).
      gr_salv->hide_button( 'SAVE' ).
      gr_salv->hide_button( 'GEL' ).
    WHEN 'SU01'.

      CALL TRANSACTION 'SU01' AND SKIP FIRST SCREEN.
  ENDCASE .

ENDFORM .
FORM salv_on_link_click USING pr_salv_table TYPE REF TO cl_salv_table
                                pv_row TYPE salv_de_row
                                pv_column TYPE salv_de_column ##CALLED .
  IF pv_column EQ 'CARRID'.

    READ TABLE gt_tab INTO DATA(ls_tab) INDEX pv_row.
    IF sy-subrc EQ 0.
      SELECT * FROM scarr INTO TABLE @DATA(lt_tab) WHERE carrid EQ @ls_tab-carrid.
      IF sy-subrc EQ 0.
        DATA(lo_salv) = NEW ycl_hk_salv_template( ).
        lo_salv->factory(
          EXPORTING
            iv_title          = 'SCARR Table'
            iv_hide_buttons   = 'DEL,GEL,SAVE'
            iv_pfstatus       = 'STAT'
            iv_delete_columns = 'MANDT'
            iv_popup_type     = '100'
            iv_form_prog      = sy-repid
          CHANGING
            ct_table          = lt_tab ).
        lo_salv->display( ) .

      ENDIF.
    ENDIF.
  ELSEIF pv_column EQ 'ICON'.
    READ TABLE gt_tab INTO ls_tab INDEX pv_row.
    IF sy-subrc EQ 0.
      SELECT * FROM spfli INTO TABLE @DATA(lt_spfli) WHERE carrid EQ @ls_tab-carrid AND connid EQ @ls_tab-connid.
      IF sy-subrc EQ 0.
        DATA(lo_spfli) = NEW ycl_hk_salv_template( ).
        lo_spfli->factory(
          EXPORTING
            iv_title          = 'SPFLI Table'
            iv_delete_columns = 'MANDT'
            iv_hide_buttons   = 'DEL,GEL,SAVE,SU01'
            iv_pfstatus       = 'STAT'
            iv_popup_type     = '200'
            iv_form_prog      = sy-repid
          CHANGING
            ct_table          = lt_spfli ).

        lo_spfli->display( ) .

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM .
*FORM salv_on_double_click USING pr_salv_table TYPE REF TO cl_salv_table
*                                pv_row TYPE salv_de_row
*                                pv_column TYPE salv_de_column ##CALLED .
*  BREAK-POINT .
*ENDFORM .
*
*FORM salv_on_before_user_command USING pr_salv_table TYPE REF TO cl_salv_table
*                                       e_salv_function TYPE salv_de_function ##CALLED .
*  BREAK-POINT .
*ENDFORM .
*
*FORM salv_on_after_user_command USING pr_salv_table TYPE REF TO cl_salv_table
*                                      e_salv_function TYPE salv_de_function ##CALLED .
*  BREAK-POINT .
*ENDFORM .
*
*FORM salv_on_top_of_page USING pr_salv_table TYPE REF TO cl_salv_table
*                               pr_top_of_page TYPE REF TO cl_salv_form
*                               pv_page TYPE sypagno
*                               pv_table_index TYPE syindex ##CALLED .
*  BREAK-POINT .
*ENDFORM .

*FORM salv_on_end_of_page USING pr_salv_table TYPE REF TO cl_salv_table
*                               pr_end_of_page TYPE REF TO cl_salv_form
*                               pv_page TYPE sypagno ##CALLED .
*  BREAK-POINT .
*ENDFORM .
