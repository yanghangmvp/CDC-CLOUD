CLASS zzcl_query_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gcs_sorting_order,
        descending TYPE string VALUE 'desc',
        ascending  TYPE string VALUE 'asc',
      END OF   gcs_sorting_order .

    CLASS-METHODS paging
      IMPORTING
        !io_paging TYPE REF TO if_rap_query_paging
      CHANGING
        !ct_data   TYPE STANDARD TABLE .
    CLASS-METHODS filtering
      IMPORTING
        !io_filter TYPE REF TO if_rap_query_filter
      CHANGING
        !ct_data   TYPE STANDARD TABLE .
    CLASS-METHODS orderby
      IMPORTING
        !it_order TYPE if_rap_query_request=>tt_sort_elements
      CHANGING
        !ct_data  TYPE STANDARD TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_QUERY_UTILS IMPLEMENTATION.


  METHOD filtering.
    TRY.
        DATA(lt_filter) = io_filter->get_as_ranges(  ).
      CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_range).
        "handle exception
    ENDTRY.
    FIELD-SYMBOLS: <fs_data> TYPE any,
                   <fs_fval> TYPE any.
    DATA: lv_index TYPE sy-tabix.

    LOOP AT lt_filter INTO DATA(ls_filter).
      TRANSLATE ls_filter-name TO UPPER CASE.            "#EC TRANSLANG
      LOOP AT ct_data ASSIGNING <fs_data>.
        lv_index = sy-tabix.
        ASSIGN COMPONENT ls_filter-name OF STRUCTURE <fs_data> TO <fs_fval>.
        CHECK sy-subrc EQ 0.
        IF <fs_fval> NOT IN ls_filter-range.
          DELETE ct_data INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.


  METHOD orderby.
    DATA: lt_otab  TYPE abap_sortorder_tab,
          ls_oline TYPE abap_sortorder.
    DATA: ls_order LIKE LINE OF it_order.

    LOOP AT it_order INTO ls_order.
      ls_oline-name = ls_order-element_name.
      TRANSLATE ls_oline-name TO UPPER CASE.             "#EC TRANSLANG
      ls_oline-descending = ls_order-descending.
      APPEND ls_oline TO lt_otab.
      CLEAR ls_oline.
    ENDLOOP.

    SORT ct_data BY (lt_otab).
  ENDMETHOD.


  METHOD paging.
    DATA(lv_skip) =  io_paging->get_offset(  ).
    DATA(lv_top) = io_paging->get_page_size(  ).
    DATA: lv_from TYPE i,
          lv_to   TYPE i.
    DATA: lo_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_result> TYPE STANDARD TABLE,
                   <fs_rec>    TYPE any.

    CREATE DATA lo_data LIKE ct_data.
    ASSIGN lo_data->* TO <fs_result>.

    IF lv_skip IS NOT INITIAL.
      lv_from = lv_skip + 1. "start from record
    ELSE.
      lv_from = 1.
    ENDIF.
    IF lv_top EQ if_rap_query_paging=>page_size_unlimited OR lv_top IS INITIAL.
      lv_to = lines( ct_data ).
    ELSE.
      lv_to   = lv_from + lv_top - 1.
    ENDIF.

    LOOP AT ct_data ASSIGNING <fs_rec> FROM lv_from TO lv_to.
      APPEND <fs_rec> TO <fs_result>.
    ENDLOOP.

    ct_data = <fs_result>.
  ENDMETHOD.
ENDCLASS.
