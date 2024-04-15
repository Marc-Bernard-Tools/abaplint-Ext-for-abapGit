CLASS zcl_abaplint_abapgit_ext_issue DEFINITION
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
* Derive abapGit issue from abaplint annotation
************************************************************************
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_issue,
        obj_type    TYPE tadir-object,
        obj_name    TYPE tadir-obj_name,
        obj_subtype TYPE string,
        extension   TYPE string,
        program     TYPE progname,
        line        TYPE i,
        source      TYPE rswsourcet,
        level       TYPE string,
        title       TYPE string,
        url         TYPE string,
      END OF ty_issue,
      ty_issues TYPE STANDARD TABLE OF ty_issue WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !is_annotation TYPE zcl_abaplint_abapgit_ext_annot=>ty_annotation OPTIONAL
        !iv_issue      TYPE string OPTIONAL
        !iv_folder     TYPE string DEFAULT '/src/'
      RAISING
        zcx_abapgit_exception.

    METHODS get
      RETURNING
        VALUE(rs_issue) TYPE ty_issue
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_functab TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY.

    CONSTANTS:
      c_active TYPE c LENGTH 1 VALUE 'A' ##NO_TEXT.

    DATA:
      mv_folder     TYPE string,
      mv_issue      TYPE string,
      ms_issue      TYPE ty_issue,
      ms_annotation TYPE zcl_abaplint_abapgit_ext_annot=>ty_annotation.

    METHODS _folder_for_regex
      IMPORTING
        !iv_folder       TYPE string
      RETURNING
        VALUE(rv_folder) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS _get_issue_clas
      IMPORTING
        !is_issue       TYPE ty_issue
      RETURNING
        VALUE(rs_issue) TYPE ty_issue
      RAISING
        zcx_abapgit_exception.

    METHODS _get_issue_intf
      IMPORTING
        !is_issue       TYPE ty_issue
      RETURNING
        VALUE(rs_issue) TYPE ty_issue
      RAISING
        zcx_abapgit_exception.

    METHODS _get_issue_prog
      IMPORTING
        !is_issue       TYPE ty_issue
      RETURNING
        VALUE(rs_issue) TYPE ty_issue
      RAISING
        zcx_abapgit_exception.

    METHODS _get_issue_fugr
      IMPORTING
        !is_issue       TYPE ty_issue
      RETURNING
        VALUE(rs_issue) TYPE ty_issue
      RAISING
        zcx_abapgit_exception.

    METHODS _get_issue_type
      IMPORTING
        !is_issue       TYPE ty_issue
      RETURNING
        VALUE(rs_issue) TYPE ty_issue
      RAISING
        zcx_abapgit_exception.

    METHODS _parse
      IMPORTING
        !iv_issue       TYPE string
      RETURNING
        VALUE(rs_issue) TYPE ty_issue
      RAISING
        zcx_abapgit_exception.

    METHODS _read_class_line
      IMPORTING
        !iv_clsname TYPE seoclsname
        !iv_line    TYPE i
      EXPORTING
        !et_source  TYPE rswsourcet
        !ev_program TYPE progname
        !ev_line    TYPE i
      RAISING
        zcx_abapgit_exception.

    METHODS _read_class_include
      IMPORTING
        !iv_clsname       TYPE seoclsname
        !iv_clspart       TYPE seop_include_ext_app
      RETURNING
        VALUE(rv_program) TYPE progname
      RAISING
        zcx_abapgit_exception.

    METHODS _read_functions
      IMPORTING
        !iv_area          TYPE rs38l_area
      RETURNING
        VALUE(rt_functab) TYPE ty_functab
      RAISING
        zcx_abapgit_exception.

    METHODS _read_program
      IMPORTING
        !iv_program      TYPE progname
      RETURNING
        VALUE(rt_source) TYPE rswsourcet
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_issue IMPLEMENTATION.


  METHOD constructor.

    IF iv_issue IS INITIAL AND is_annotation IS INITIAL.
      zcx_abapgit_exception=>raise( 'Neither issue nor annotation supplied' ).
    ENDIF.

    ms_annotation = is_annotation.

    IF iv_issue IS INITIAL.
      mv_issue = to_upper( |{ ms_annotation-start_line } in { ms_annotation-path }| ).
    ELSE.
      mv_issue = to_upper( iv_issue ).
    ENDIF.

    ms_issue       = _parse( mv_issue ).
    ms_issue-level = ms_annotation-annotation_level.
    ms_issue-title = ms_annotation-title.
    ms_issue-url   = ms_annotation-message.

    mv_folder = _folder_for_regex( iv_folder ).

  ENDMETHOD.


  METHOD get.

    CASE ms_issue-obj_type.
      WHEN 'CLAS'.
        rs_issue = _get_issue_clas( ms_issue ).
      WHEN 'INTF'.
        rs_issue = _get_issue_intf( ms_issue ).
      WHEN 'PROG'.
        rs_issue = _get_issue_prog( ms_issue ).
      WHEN 'FUGR'.
        rs_issue = _get_issue_fugr( ms_issue ).
      WHEN 'TYPE'.
        rs_issue = _get_issue_type( ms_issue ).
      WHEN OTHERS.
        MOVE-CORRESPONDING ms_issue TO rs_issue.
    ENDCASE.

    IF rs_issue-extension = 'XML'.
      CLEAR rs_issue-source.
    ENDIF.

  ENDMETHOD.


  METHOD _folder_for_regex.

    IF strlen( iv_folder ) < 1.
      zcx_abapgit_exception=>raise( 'Missing starting folder' ).
    ENDIF.

    rv_folder = replace(
      val  = to_upper( iv_folder+1 )
      sub  = '/'
      with = '\/' ).

  ENDMETHOD.


  METHOD _get_issue_clas.

    MOVE-CORRESPONDING is_issue TO rs_issue.

    CASE to_lower( is_issue-obj_subtype ).
      WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_def.
        rs_issue-program = _read_class_include(
          iv_clsname = |{ is_issue-obj_name }|
          iv_clspart = seop_ext_class_locals_def ).
      WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_imp.
        rs_issue-program = _read_class_include(
          iv_clsname = |{ is_issue-obj_name }|
          iv_clspart = seop_ext_class_locals_imp ).
      WHEN zif_abapgit_oo_object_fnc=>c_parts-macros.
        rs_issue-program = _read_class_include(
          iv_clsname = |{ is_issue-obj_name }|
          iv_clspart = seop_ext_class_macros ).
      WHEN zif_abapgit_oo_object_fnc=>c_parts-testclasses.
        rs_issue-program = _read_class_include(
          iv_clsname = |{ is_issue-obj_name }|
          iv_clspart = seop_ext_class_testclasses ).
      WHEN OTHERS.
        _read_class_line(
          EXPORTING
            iv_clsname = |{ is_issue-obj_name }|
            iv_line    = is_issue-line
          IMPORTING
            ev_program = rs_issue-program
            ev_line    = rs_issue-line ).
    ENDCASE.

    IF rs_issue-program IS INITIAL.
      rs_issue-program = cl_oo_classname_service=>get_classpool_name( |{ is_issue-obj_name }| ).
    ENDIF.

    rs_issue-source = _read_program( rs_issue-program ).

  ENDMETHOD.


  METHOD _get_issue_fugr.

    DATA:
      lt_functab       TYPE ty_functab,
      lv_namespace     TYPE rs38l-namespace,
      lv_complete_area TYPE rs38l-area,
      lv_area          TYPE rs38l-area.

    FIELD-SYMBOLS <ls_functab> TYPE LINE OF ty_functab.

    MOVE-CORRESPONDING is_issue TO rs_issue.

    lt_functab = _read_functions( |{ is_issue-obj_name }| ).

    READ TABLE lt_functab ASSIGNING <ls_functab> WITH KEY funcname = is_issue-obj_subtype.
    IF sy-subrc = 0.
      rs_issue-program = <ls_functab>-include.
    ELSE.
      rs_issue-program = is_issue-obj_subtype.
    ENDIF.

    IF rs_issue-program IS INITIAL.
      lv_complete_area = |{ is_issue-obj_name }|.
      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        EXPORTING
          complete_area                = lv_complete_area
        IMPORTING
          namespace                    = lv_namespace
          group                        = lv_area
        EXCEPTIONS
          include_not_exists           = 1
          group_not_exists             = 2
          no_selections                = 3
          no_function_include          = 4
          no_function_pool             = 5
          delimiter_wrong_position     = 6
          no_customer_function_group   = 7
          no_customer_function_include = 8
          reserved_name_customer       = 9
          namespace_too_long           = 10
          area_length_error            = 11
          OTHERS                       = 12.
      CONCATENATE lv_namespace 'SAPL' lv_area INTO rs_issue-program.
    ENDIF.

    rs_issue-source = _read_program( rs_issue-program ).

  ENDMETHOD.


  METHOD _get_issue_intf.

    MOVE-CORRESPONDING is_issue TO rs_issue.

    rs_issue-program = cl_oo_classname_service=>get_intfsec_name( |{ is_issue-obj_name }| ).
    rs_issue-source = _read_program( rs_issue-program ).

  ENDMETHOD.


  METHOD _get_issue_prog.

    MOVE-CORRESPONDING is_issue TO rs_issue.

    rs_issue-program = is_issue-obj_name.
    rs_issue-source  = _read_program( rs_issue-program ).

  ENDMETHOD.


  METHOD _get_issue_type.

    MOVE-CORRESPONDING is_issue TO rs_issue.

    rs_issue-program = |%_C{ is_issue-obj_name }|.
    rs_issue-source = _read_program( rs_issue-program ).

  ENDMETHOD.


  METHOD _parse.

    DATA:
      lv_regex_type    TYPE string,
      lv_regex_subtype TYPE string,
      lv_line          TYPE string,
      lv_obj_type      TYPE string,
      lv_obj_subtype   TYPE string,
      lv_obj_name      TYPE string,
      lv_ext           TYPE string,
      lv_rest          TYPE string.

    lv_regex_subtype = '(\d*) IN ' && mv_folder && '(.*)\.(.*)\.(.*)\.(.*)'.
    lv_regex_type    = '(\d*) IN ' && mv_folder && '(.*)\.(.*)\.(.*)'.

    FIND REGEX lv_regex_subtype IN iv_issue
      SUBMATCHES lv_line lv_obj_name lv_obj_type lv_obj_subtype lv_ext.
    IF sy-subrc <> 0.
      FIND REGEX lv_regex_type IN iv_issue
        SUBMATCHES lv_line lv_obj_name lv_obj_type lv_ext.
    ENDIF.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Unable to identify object from issue description' ).
    ENDIF.

    DO.
      IF lv_obj_name CS '/'.
        SPLIT lv_obj_name AT '/' INTO lv_rest lv_obj_name.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    REPLACE ALL OCCURRENCES OF '#' IN lv_obj_name WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_obj_subtype WITH '/'.

    CLEAR rs_issue.
    rs_issue-obj_type    = lv_obj_type.
    rs_issue-obj_name    = lv_obj_name.
    rs_issue-obj_subtype = lv_obj_subtype.
    rs_issue-extension   = lv_ext.
    rs_issue-line        = lv_line.

  ENDMETHOD.


  METHOD _read_class_include.

    DATA: ls_include TYPE progstruc.

    ASSERT iv_clspart = seop_ext_class_locals_def
        OR iv_clspart = seop_ext_class_locals_imp
        OR iv_clspart = seop_ext_class_macros
        OR iv_clspart = seop_ext_class_testclasses.

    ls_include-rootname = iv_clsname.
    TRANSLATE ls_include-rootname USING ' ='.
    ls_include-categorya = iv_clspart(1).
    ls_include-codea     = iv_clspart+1(4).

    rv_program = ls_include.

  ENDMETHOD.


  METHOD _read_class_line.

    DATA:
      lx_error    TYPE REF TO cx_root,
      lo_instance TYPE REF TO cl_oo_factory,
      lo_source   TYPE REF TO cl_oo_clif_source,
      lo_scanner  TYPE REF TO cl_oo_source_scanner_class,
      ls_int      TYPE cl_oo_source_scanner_class=>type_source_interval,
      lt_methods  TYPE cl_oo_source_scanner_class=>type_method_implementations,
      ls_method   TYPE seocpdkey.

    TRY.
        lo_instance = cl_oo_factory=>create_instance( ).

        lo_source ?= lo_instance->create_clif_source(
          clif_name = iv_clsname
          version   = c_active ).

        lo_source->get_source( IMPORTING source = et_source ).

        lo_scanner ?= lo_source->get_scanner( ).

        ls_int = lo_scanner->get_public_section_interval( ).
        IF iv_line >= ls_int-begin-line AND iv_line < ls_int-end-line.
          ev_program = cl_oo_classname_service=>get_pubsec_name( iv_clsname ).
          ev_line = iv_line - ls_int-begin-line + 1.
          RETURN.
        ENDIF.

        ls_int = lo_scanner->get_protected_section_interval( ).
        IF iv_line >= ls_int-begin-line AND iv_line < ls_int-end-line.
          ev_program = cl_oo_classname_service=>get_prosec_name( iv_clsname ).
          ev_line = iv_line - ls_int-begin-line + 1.
          RETURN.
        ENDIF.

        ls_int = lo_scanner->get_private_section_interval( ).
        IF iv_line >= ls_int-begin-line AND iv_line < ls_int-end-line.
          ev_program = cl_oo_classname_service=>get_prisec_name( iv_clsname ).
          ev_line = iv_line - ls_int-begin-line + 1.
          RETURN.
        ENDIF.

        ls_method-clsname = iv_clsname.
        lt_methods = lo_scanner->get_method_implementations( ).
        LOOP AT lt_methods INTO ls_method-cpdname.
          ls_int = lo_scanner->get_method_impl_interval( ls_method-cpdname ).
          IF iv_line >= ls_int-begin-line AND iv_line < ls_int-end-line.
            ev_program = cl_oo_classname_service=>get_method_include( ls_method ).
            ev_line = iv_line - ls_int-begin-line + 1.
            RETURN.
          ENDIF.
        ENDLOOP.

      CATCH cx_root INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD _read_functions.

    FIELD-SYMBOLS <ls_functab> LIKE LINE OF rt_functab.

    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = iv_area
      TABLES
        functab                 = rt_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " The result can also contain function which are lowercase
    LOOP AT rt_functab ASSIGNING <ls_functab>.
      TRANSLATE <ls_functab> TO UPPER CASE.
    ENDLOOP.

    SORT rt_functab BY funcname ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_functab COMPARING funcname.

  ENDMETHOD.


  METHOD _read_program.

    DATA lv_msg LIKE LINE OF rt_source.

    " abapGit only deals with active objects which we read here
    " Note: The issue might be fixed already in inactive code
    READ REPORT iv_program INTO rt_source STATE c_active.
    IF sy-subrc <> 0.
      lv_msg = |Program { iv_program } does not exist in active version|.
      APPEND lv_msg TO rt_source.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
