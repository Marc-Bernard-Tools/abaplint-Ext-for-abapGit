CLASS zcl_abaplint_abapgit_ext_logo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* abaplint Extension for abapGit
*
* https://github.com/Marc-Bernard-Tools/abaplint-Ext-for-abapGit
*
* Copyright 2023 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_logo TYPE string VALUE 'abaplint_logo.png'.

    CLASS-METHODS get_logo_mime
      RETURNING
        VALUE(rv_xdata) TYPE xstring
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_logo_html
      IMPORTING
        !iv_title      TYPE string OPTIONAL
      RETURNING
        VALUE(rv_html) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_mime TYPE string VALUE 'ZABAPLINT_LOGO'.

ENDCLASS.



CLASS ZCL_ABAPLINT_ABAPGIT_EXT_LOGO IMPLEMENTATION.


  METHOD get_logo_html.
    rv_html =
      |<img src="{ c_logo }" width="25px" height="25px" | &&
      |title="{ iv_title }" | &&
      |style="background-color:lightgrey;border-radius:6px;">|.
  ENDMETHOD.


  METHOD get_logo_mime.

    DATA:
      ls_key    TYPE wwwdatatab,
      lv_size_c TYPE wwwparams-value,
      lv_size   TYPE i,
      lt_w3mime TYPE STANDARD TABLE OF w3mime,
      ls_w3mime LIKE LINE OF lt_w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = c_mime.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_w3mime INTO ls_w3mime.
      CONCATENATE rv_xdata ls_w3mime-line INTO rv_xdata IN BYTE MODE.
    ENDLOOP.
    rv_xdata = rv_xdata(lv_size).

  ENDMETHOD.
ENDCLASS.
