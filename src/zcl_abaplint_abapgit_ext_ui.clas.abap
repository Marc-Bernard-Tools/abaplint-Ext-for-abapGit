CLASS zcl_abaplint_abapgit_ext_ui DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

************************************************************************
* abaplint Extension for abapGit
*
* https://github.com/Marc-Bernard-Tools/abaplint-Ext-for-abapGit
*
* Copyright 2023 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES:
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_renderable,
      zif_abapgit_gui_menu_provider.

    CLASS-METHODS create
      IMPORTING
        !iv_key         TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_check_run   TYPE string
        !iv_count_total TYPE string
      RETURNING
        VALUE(ri_page)  TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !iv_key         TYPE zif_abapgit_persistence=>ty_repo-key OPTIONAL
        !iv_check_run   TYPE string OPTIONAL
        !iv_count_total TYPE string
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        sort_1             TYPE string VALUE 'mbt_sort_1',
        sort_2             TYPE string VALUE 'mbt_sort_2',
        sort_3             TYPE string VALUE 'mbt_sort_3',
        jump_edit          TYPE string VALUE 'mbt_jump_edit',
        toggle_view_source TYPE string VALUE 'mbt_toggle_view_source',
        rules              TYPE string VALUE 'mbt_rules',
      END OF c_action.

    CONSTANTS:
      c_lines_before TYPE i VALUE 5,
      c_lines_after  TYPE i VALUE 5.

    DATA:
      mo_repo        TYPE REF TO zcl_abapgit_repo_online,
      mv_check_run   TYPE string,
      mv_count_total TYPE i,
      mt_issues      TYPE zcl_abaplint_abapgit_ext_issue=>ty_issues,
      mv_view_source TYPE abap_bool.

    METHODS _get_issues
      RETURNING
        VALUE(rt_issues) TYPE zcl_abaplint_abapgit_ext_issue=>ty_issues
      RAISING
        zcx_abapgit_exception.

    METHODS _render_footer
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS _render_issue
      IMPORTING
        !is_issue      TYPE zcl_abaplint_abapgit_ext_issue=>ty_issue
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS _render_issues
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS _render_source
      IMPORTING
        !is_issue      TYPE zcl_abaplint_abapgit_ext_issue=>ty_issue
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_ui IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    IF iv_key IS INITIAL.
      zcx_abapgit_exception=>raise( 'No repository key supplied' ).
    ELSE.
      mo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    ENDIF.

    IF iv_check_run IS INITIAL.
      zcx_abapgit_exception=>raise( 'No check run supplied' ).
    ELSE.
      mv_check_run   = iv_check_run.
      mv_count_total = iv_count_total.
    ENDIF.

    mt_issues = _get_issues( ).

    " Could be a user setting
    mv_view_source = abap_true.

    gui_services( )->cache_asset(
      iv_type    = 'image'
      iv_subtype = 'png'
      iv_url     = |{ zcl_abaplint_abapgit_ext_logo=>c_logo }|
      iv_xdata   = zcl_abaplint_abapgit_ext_logo=>get_logo_mime( ) ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abaplint_abapgit_ext_ui.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key         = iv_key
        iv_check_run   = iv_check_run
        iv_count_total = iv_count_total.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'abaplint Issues'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_jump_type TYPE tadir-object,
      lv_jump_name TYPE tadir-obj_name,
      lv_include   TYPE progname,
      lv_enclosing TYPE e071-obj_name,
      lv_line      TYPE i,
      lv_position  TYPE c LENGTH 10.

    CASE ii_event->mv_action.
      WHEN c_action-rules.

        rs_handled-page = zcl_abaplint_abapgit_ext_rules=>create( mo_repo->get_key( ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-toggle_view_source.

        mv_view_source = boolc( mv_view_source = abap_false ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-jump_edit.

        lv_jump_type = ii_event->query( )->get( 'TYPE' ).
        lv_jump_name = ii_event->query( )->get( 'NAME' ).
        lv_include   = ii_event->query( )->get( 'INCLUDE' ).
        lv_line      = ii_event->query( )->get( 'LINE' ).

        " Adjust for dynpros
        " Note: jump to line does not work for dynpros, unfortunately
        IF lv_jump_type = 'DYNP'.
          lv_enclosing = lv_include.
          CLEAR lv_include.
        ENDIF.

        " We could use zcl_abapgit_objects=>jump but we want to EDIT
        " and stay in same window
        lv_position = nmax(
          val1 = 1
          val2 = lv_line ).

        CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING
            operation           = 'EDIT'
            object_name         = lv_jump_name
            object_type         = lv_jump_type
            enclosing_object    = lv_enclosing
            include             = lv_include
            position            = lv_position
            in_new_window       = abap_false
          EXCEPTIONS
            not_executed        = 1
            invalid_object_type = 2
            OTHERS              = 3.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN c_action-sort_1.
        SORT mt_issues BY obj_type obj_name obj_subtype line url title.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-sort_2.
        SORT mt_issues BY obj_type obj_name url title obj_subtype line.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-sort_3.
        SORT mt_issues BY url title obj_type obj_name obj_subtype line.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    DATA:
      lo_sort_menu TYPE REF TO zcl_abapgit_html_toolbar,
      lo_view_menu TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT lo_sort_menu.

    lo_sort_menu->add(
      iv_txt = 'By Object, Sub-object, Line'
      iv_act = c_action-sort_1
    )->add(
      iv_txt = 'By Object, Check, Sub-object'
      iv_act = c_action-sort_2
    )->add(
      iv_txt = 'By Check, Object, Sub-object'
      iv_act = c_action-sort_3 ).

    CREATE OBJECT lo_view_menu.

    lo_view_menu->add(
      iv_txt = 'Source Code'
      iv_chk = mv_view_source
      iv_act = c_action-toggle_view_source ).

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'Sort'
      io_sub = lo_sort_menu
    )->add(
      iv_txt = 'View'
      io_sub = lo_view_menu
    )->add(
      iv_txt = 'Rules'
      iv_act = c_action-rules
    )->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="repo">' ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_false ) ).
    ri_html->add( '</div>' ).

    ri_html->add( _render_issues( ) ).

    ri_html->add( _render_footer( ) ).

  ENDMETHOD.


  METHOD _get_issues.

    DATA:
      lo_annotations TYPE REF TO zcl_abaplint_abapgit_ext_annot,
      ls_annotation  TYPE zcl_abaplint_abapgit_ext_annot=>ty_annotation,
      lt_annotations TYPE zcl_abaplint_abapgit_ext_annot=>ty_annotations,
      lo_issue       TYPE REF TO zcl_abaplint_abapgit_ext_issue,
      ls_issue       TYPE zcl_abaplint_abapgit_ext_issue=>ty_issue.

    CREATE OBJECT lo_annotations
      EXPORTING
        iv_url       = mo_repo->get_url( )
        iv_check_run = mv_check_run.

    lt_annotations = lo_annotations->get( ).

    LOOP AT lt_annotations INTO ls_annotation.

      CREATE OBJECT lo_issue
        EXPORTING
          is_annotation = ls_annotation.

      ls_issue = lo_issue->get( ).

      INSERT ls_issue INTO TABLE rt_issues.

    ENDLOOP.

    SORT rt_issues BY obj_type obj_name obj_subtype line url title.

  ENDMETHOD.


  METHOD _render_footer.

    DATA lv_url TYPE string.

    lv_url = zcl_abaplint_abapgit_ext_exit=>get_last_url( ).

    ri_html = zcl_abapgit_html=>create( ).

    IF lines( mt_issues ) = 0.

      ri_html->add( '<div class="dummydiv success">' ).
      ri_html->add( ri_html->icon( 'check' ) ).
      ri_html->add( 'No abaplint findings' ).
      ri_html->add( '</div>' ).

    ELSEIF lines( mt_issues ) < mv_count_total.

      ri_html->add( '<div class="dummydiv warning">' ).
      ri_html->add( ri_html->icon( 'exclamation-triangle' ) ).
      ri_html->add( |First { lines( mt_issues ) } of { mv_count_total } findings shown in list. | ).
      ri_html->add_a(
        iv_txt  = 'Show more...'
        iv_act  = |{ zif_abapgit_definitions=>c_action-url }?url={ lv_url }| ).
      ri_html->add( '</div>' ).

    ENDIF.

  ENDMETHOD.


  METHOD _render_issue.

    DATA:
      ls_mtdkey    TYPE seocpdkey,
      lv_subtype   TYPE string,
      lv_class     TYPE string,
      lv_icon      TYPE string,
      lv_jump_type TYPE tadir-object,
      lv_jump_name TYPE tadir-obj_name,
      lv_jump_prog TYPE progname,
      lv_obj_text  TYPE string,
      lv_obj_link  TYPE string,
      lv_msg_text  TYPE string,
      lv_msg_link  TYPE string,
      lv_msg_code  TYPE string,
      lv_rest      TYPE string.

    ri_html = zcl_abapgit_html=>create( ).

    CASE is_issue-level.
      WHEN 'failure'.
        lv_class = 'ci-error'.
        lv_icon  = ri_html->icon(
          iv_name = 'exclamation-circle/red'
          iv_hint = 'Failure' ).
      WHEN 'warning'.
        lv_class = 'ci-warning'.
        lv_icon  = ri_html->icon(
          iv_name = 'exclamation-triangle/yellow'
          iv_hint = 'Warning' ).
      WHEN OTHERS.
        lv_class = 'ci-info'.
    ENDCASE.

    " Default jump is to source
    lv_jump_type = 'PROG'.
    lv_jump_name = is_issue-obj_name.
    lv_jump_prog = is_issue-program.

    CASE is_issue-obj_type.
      WHEN 'CLAS'.
        lv_subtype = to_lower( is_issue-obj_subtype ).
        CASE lv_subtype.
          WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_def.
            lv_obj_text = |CLAS { is_issue-obj_name } : Local Definitions|.
          WHEN zif_abapgit_oo_object_fnc=>c_parts-locals_imp.
            lv_obj_text = |CLAS { is_issue-obj_name } : Local Implementations|.
          WHEN zif_abapgit_oo_object_fnc=>c_parts-macros.
            lv_obj_text = |CLAS { is_issue-obj_name } : Macros|.
          WHEN zif_abapgit_oo_object_fnc=>c_parts-testclasses.
            lv_obj_text = |CLAS { is_issue-obj_name } : Test Classes|.
          WHEN OTHERS.
            cl_oo_classname_service=>get_method_by_include(
              EXPORTING
                incname             = is_issue-program
              RECEIVING
                mtdkey              = ls_mtdkey
              EXCEPTIONS
                class_not_existing  = 1
                method_not_existing = 2
                OTHERS              = 3 ).
            IF sy-subrc = 0.
              lv_obj_text = |CLAS { is_issue-obj_name }->{ ls_mtdkey-cpdname }|.
            ELSE.
              lv_obj_text = |CLAS { is_issue-obj_name }|.
            ENDIF.
        ENDCASE.
      WHEN 'PROG'.
        " Screens
        IF is_issue-obj_subtype IS INITIAL.
          lv_obj_text = |PROG { is_issue-obj_name }|.
        ELSE.
          lv_obj_text = |PROG { is_issue-obj_name } Screen { is_issue-obj_subtype }|.
          lv_jump_type = 'DYNP'.
          lv_jump_name = is_issue-obj_subtype.
        ENDIF.
      WHEN 'FUGR'.
        " Function modules
        IF is_issue-obj_subtype IS INITIAL.
          lv_obj_text = |FUGR { is_issue-obj_name }|.
        ELSE.
          lv_obj_text = |FUGR { is_issue-obj_name } Function { is_issue-obj_subtype }|.
        ENDIF.
      WHEN OTHERS.
        lv_obj_text = |{ is_issue-obj_type } { is_issue-obj_name }|.
    ENDCASE.

    lv_msg_text = escape(
      val    = is_issue-title
      format = cl_abap_format=>e_html_text ).

    lv_msg_code = reverse( is_issue-url ).
    SPLIT lv_msg_code AT '/' INTO lv_msg_code lv_rest.
    lv_msg_code = reverse( lv_msg_code ).

    lv_msg_link = ri_html->a(
      iv_txt   = lv_msg_code
      iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ is_issue-url }|
      iv_class = 'url' ).

    lv_obj_text = |{ lv_obj_text } [ @{ is_issue-line } ]|.
    lv_obj_link = |{ c_action-jump_edit }?type={ lv_jump_type }&name={ lv_jump_name }|
               && |&line={ is_issue-line }&include={ lv_jump_prog }|.

    ri_html->add( |<li class="{ lv_class }">| ).
    ri_html->add_a(
      iv_txt = lv_obj_text
      iv_act = lv_obj_link
      iv_typ = zif_abapgit_html=>c_action_type-sapevent ).

    ri_html->add( '<span class="margin-v5">' ).
    ri_html->add( zcl_abaplint_abapgit_ext_logo=>get_logo_html( lv_msg_code ) ).
    ri_html->add( | { lv_msg_text } ({ lv_msg_link })| ).
    ri_html->add( '</span>' ).

    IF mv_view_source = abap_true.
      ri_html->add( _render_source( is_issue ) ).
    ENDIF.

    ri_html->add( |</li>| ).

  ENDMETHOD.


  METHOD _render_issues.

    DATA ls_issue LIKE LINE OF mt_issues.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( '<div class="ci-result"><ul>' ).

    LOOP AT mt_issues INTO ls_issue.

      ri_html->add( _render_issue( ls_issue ) ).

    ENDLOOP.

    ri_html->add( '</ul></div>' ).

  ENDMETHOD.


  METHOD _render_source.

    DATA:
      lv_source      LIKE LINE OF is_issue-source,
      lv_line        TYPE i,
      lv_class       TYPE string,
      lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter.

    ri_html = zcl_abapgit_html=>create( ).

    IF is_issue-source IS INITIAL.
      RETURN.
    ENDIF.

    " Assume all findings are in ABAP code
    lo_highlighter = zcl_abapgit_syntax_factory=>create( 'code.abap' ).
    ASSERT lo_highlighter IS NOT INITIAL.

    " Use same styles as diff pages
    ri_html->add( '<div class="diff_content" style="margin-bottom:20px">' ).
    ri_html->add( '<table class="diff_tab syntax-hl" i>' ).
    ri_html->add( '<thead class="nav_line">' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<th class="num"></th>' ).
    ri_html->add( '<th></th>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

    LOOP AT is_issue-source INTO lv_source FROM is_issue-line - c_lines_before TO is_issue-line + c_lines_after.
      lv_line = sy-tabix.
      lv_source = lo_highlighter->process_line( lv_source ).
      ri_html->add( '<tr>' ).
      IF lv_line = is_issue-line.
        CASE is_issue-level.
          WHEN 'failure'.
            lv_class = 'diff_del'. "red
          WHEN 'warning'.
            lv_class = 'diff_upd'. "yellow
          WHEN OTHERS.
            lv_class = 'diff_ins'. "green
        ENDCASE.
      ELSE.
        lv_class = 'diff_others'.
      ENDIF.
      ri_html->add( |<td class="num { lv_class }">{ lv_line }</td><td class="code { lv_class }">{ lv_source }</td>| ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    IF sy-subrc <> 0.
      ri_html->add( '<tr>' ).
      ri_html->add( '<td class="num diff_upd">0</td>' ).
      ri_html->add( '<td class="code diff_upd">Source location does not exist (anymore)</td>' ).
      ri_html->add( '</tr>' ).
    ENDIF.

    ri_html->add( '</table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
