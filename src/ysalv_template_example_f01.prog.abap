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

  CREATE OBJECT go_salv.

  go_salv->factory(
    EXPORTING
      if_delete_columns      = 'MANDT,SEATSMAX_F,SEATSOCC_F'
      if_hotspot_columns     = 'CARRID,ICON'
      if_hide_buttons        = 'DEL,GEL'
      if_pfstatus            = 'STAT' " create status with name stat and put DEL,GEL,SAVe
      if_aggregation_columns = 'SEATSMAX_B,SEATSOCC_B'
      if_sort_columns        = 'CARRID,CONNID'
      if_layout              = p_layout
      if_color_column        = 'COLOR'
      if_form_prog           = sy-repid
    CHANGING
      ct_table               = gt_tab[] ).


  go_salv->delete_column( if_columnname = 'SEATSOCC' ).

  go_salv->set_column_color( if_columnname = 'PRICE' if_color = col_positive ).

  go_salv->edit_column(
    if_columname   = 'ICON'
    if_long_text   = 'Uçuşlar'
    if_medium_text = 'Uçuşlar'
    if_short_text  = 'Uçuşlar'
    if_sign        = if_salv_c_bool_sap=>true
    if_zero        = if_salv_c_bool_sap=>false
  ).
  go_salv->edit_column(
    if_columname   = 'STATU'
    if_long_text   = 'Statu'
    if_medium_text = 'Statu'
    if_short_text  = 'Statu'
    if_sign        = if_salv_c_bool_sap=>true
    if_zero        = if_salv_c_bool_sap=>false
  ).
  go_salv->create_header(
    if_header_text = |Öylesine bir rapor({ lines( gt_tab ) })|
    if_action_text = 'BioNTech SE'
    if_logo        = 'LOGO'
    if_footer_text = 'BioNTech SE' ).

  go_salv->display( ) .

ENDFORM.
FORM salv_on_user_command USING pr_salv_table TYPE REF TO cl_salv_table
                                pv_salv_function TYPE salv_de_function .

  DATA(lo_sel) = pr_salv_table->get_selections( ).
  DATA(lt_row) = lo_sel->get_selected_rows( ).

  CASE pv_salv_function .
    WHEN 'SAVE' .
      go_salv->hide_button( 'SAVE' ).
      go_salv->show_button( 'DEL' ).
      go_salv->show_button( 'GEL' ).
      go_salv->refresh( ).
    WHEN 'DEL' .
      go_salv->hide_button( 'DEL' ).
      go_salv->show_button( 'SAVE' ).
      DELETE gt_tab WHERE carrid = 'AA'.
      go_salv->create_header( if_header_text = |Öylesine bir rapor({ lines( gt_tab ) })|
                              if_action_text = 'Biontech Se'
                              if_logo        = 'LOGO' ).
      go_salv->refresh( ).
    WHEN 'GEL' .
      go_salv->hide_button( 'DEL' ).
      go_salv->hide_button( 'SAVE' ).
      go_salv->hide_button( 'GEL' ).
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
        DATA(lo_salv) = NEW zcl_hk_salv_template( ).
        lo_salv->factory(
          EXPORTING
            if_title          = 'SCARR Table'
            if_hide_buttons   = 'DEL,GEL,SAVE'
            if_pfstatus       = 'STAT'
            if_delete_columns = 'MANDT'
            if_popup_type     = '100'
            if_form_prog      = sy-repid
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
        DATA(lo_spfli) = NEW zcl_hk_salv_template( ).
        lo_spfli->factory(
          EXPORTING
            if_title          = 'SPFLI Table'
            if_delete_columns = 'MANDT'
            if_hide_buttons   = 'DEL,GEL,SAVE,SU01'
            if_pfstatus       = 'STAT'
            if_popup_type     = '200'
            if_form_prog      = sy-repid
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
