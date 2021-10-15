*&---------------------------------------------------------------------*
*& Include ysalv_template_example_scr
*&---------------------------------------------------------------------*

PARAMETERS: p_layout TYPE disvariant-variant.

INITIALIZATION.
  PERFORM get_default_layout CHANGING p_layout.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_layouts CHANGING p_layout.
