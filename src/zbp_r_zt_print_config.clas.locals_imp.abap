CLASS lhc_zr_zt_print_config DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR zr_zt_print_config
        RESULT result,
      createxsdfile FOR DETERMINE ON SAVE
        IMPORTING keys FOR zr_zt_print_config~createxsdfile.
ENDCLASS.

CLASS lhc_zr_zt_print_config IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD createxsdfile.
    DATA : lv_error   TYPE abap_boolean,
           lv_message TYPE string.

*&---获取UI 界面实体数据内容
    READ ENTITIES OF zr_zt_print_config IN LOCAL MODE
    ENTITY config ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(results).

*&---出来数据文件
    LOOP AT results ASSIGNING FIELD-SYMBOL(<result>).
      lv_error = abap_false.
      TRY.
          "FDP utils
          " 通过数据服务获取对应的xsd 文件
          " 获取数据服务类
          DATA(lo_fdp_util) = cl_fp_fdp_services=>get_instance( CONV zzeservicename( <result>-servicedefinitionname ) ).
          " 获取数据服务xsd 文件
          <result>-xsdfile = lo_fdp_util->get_xsd(  ).
          " 设置xsd 文件名
          <result>-xsdfilename = |{ <result>-servicedefinitionname }.xsd |.
          " 设置xsd 存储文件类型
          <result>-xsdtype = 'application/xml'.
        CATCH cx_fp_fdp_error INTO DATA(lx_fdp).
          lv_message = lx_fdp->get_longtext(  ).
*              lv_message = lx_ads->get_longtext(  ).
          lv_error = abap_true.
      ENDTRY.
      " 报错消息处理
      IF lv_error = abap_true.
        APPEND VALUE #(  uuid = <result>-uuid
            %msg = new_message(
            id       = '00'
            number   = 000
            severity = if_abap_behv_message=>severity-error
            v1       = lv_message
            )
        )  TO reported-config.

      ENDIF.
*      ENDIF.
    ENDLOOP.
    " 若无错则保存数据服务xsd 文件到template 附加xsdfile 文件字段中
    IF lv_error = abap_false.
      " 更新数据实体把产生xsd 内容和对应字段更新到对应实体上
      MODIFY ENTITIES OF zr_zt_print_config IN LOCAL MODE
          ENTITY config UPDATE FIELDS ( xsdfilename xsdtype xsdfile )
              WITH VALUE #( FOR file IN results ( %tky        = file-%tky
                                                  xsdfilename = file-xsdfilename
                                                  xsdtype     = file-xsdtype
                                                  xsdfile     = file-xsdfile ) ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
