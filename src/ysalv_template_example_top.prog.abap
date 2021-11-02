*&---------------------------------------------------------------------*
*& Include ysalv_template_example_top
*&---------------------------------------------------------------------*

DATA : BEGIN OF gt_tab OCCURS 0.
         INCLUDE TYPE sflight.
DATA :   icon  TYPE icon_d,
         statu TYPE icon_d,
         color TYPE lvc_t_scol, " for color row
       END OF gt_tab,
       go_salv TYPE REF TO zcl_hk_salv_template.
