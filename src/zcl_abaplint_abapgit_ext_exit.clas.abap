CLASS zcl_abaplint_abapgit_ext_exit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_user_exit_super
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

    CONSTANTS c_version TYPE string VALUE '2.0.0' ##NEEDED.

    TYPES ty_sha1 TYPE c LENGTH 40.

    METHODS:
      zif_abapgit_user_exit~on_event REDEFINITION,
      zif_abapgit_user_exit~wall_message_repo REDEFINITION.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_last_url
      RETURNING
        VALUE(rv_url) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_wall,
        commit TYPE string,
        html   TYPE REF TO zif_abapgit_html,
        url    TYPE string,
      END OF ty_wall.

    CONSTANTS:
      BEGIN OF c_action,
        go_abaplint TYPE string VALUE 'mbt_go_abaplint',
      END OF c_action.

    CONSTANTS:
      BEGIN OF c_git_status,
        queued      TYPE string VALUE 'queued',
        in_progress TYPE string VALUE 'in_progress',
        completed   TYPE string VALUE 'completed',
      END OF c_git_status.

    CONSTANTS:
      BEGIN OF c_git_conclusion,
        neutral         TYPE string VALUE 'neutral',
        success         TYPE string VALUE 'success',
        failure         TYPE string VALUE 'failure',
        action_required TYPE string VALUE 'action_required',
        cancelled       TYPE string VALUE 'cancelled',
        skipped         TYPE string VALUE 'skipped',
        stale           TYPE string VALUE 'stale',
        timed_out       TYPE string VALUE 'timed_out',
      END OF c_git_conclusion.

    CLASS-DATA:
      gv_div_attr TYPE string,
      gv_last_url TYPE string.

    DATA mt_wall TYPE HASHED TABLE OF ty_wall WITH UNIQUE KEY commit.

    METHODS _wall_message_abaplint
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        !is_check_run  TYPE zcl_abaplint_abapgit_ext_chkrn=>ty_check_run
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_exit IMPLEMENTATION.


  METHOD class_constructor.
    gv_div_attr = 'id="abaplint-message" style="padding-right: 10px; margin-top: 10px; float: left;"'.
  ENDMETHOD.


  METHOD get_last_url.
    rv_url = gv_last_url.
  ENDMETHOD.


  METHOD zif_abapgit_user_exit~on_event.

    IF ii_event->mv_action = c_action-go_abaplint.
      rs_handled-page  = zcl_abaplint_abapgit_ext_ui=>create(
        iv_key         = |{ ii_event->query( )->get( 'KEY' ) }|
        iv_check_run   = |{ ii_event->query( )->get( 'CHECKRUN' ) }|
        iv_count_total = |{ ii_event->query( )->get( 'TOTAL' ) }| ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_user_exit~wall_message_repo.

    DATA:
      lx_error       TYPE REF TO zcx_abapgit_exception,
      lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
      lt_files       TYPE zif_abapgit_git_definitions=>ty_files_tt,
      lv_commit      TYPE zcl_abaplint_abapgit_ext_exit=>ty_sha1,
      ls_wall        TYPE ty_wall,
      lo_check_run   TYPE REF TO zcl_abaplint_abapgit_ext_chkrn,
      ls_check_run   TYPE zcl_abaplint_abapgit_ext_chkrn=>ty_check_run,
      li_html        TYPE REF TO zif_abapgit_html.

    IF is_repo_meta-offline = abap_true.
      RETURN.
    ENDIF.

    TRY.
        lo_repo_online ?= zcl_abapgit_repo_srv=>get_instance( )->get( is_repo_meta-key ).

        " abaplint is only available on GitHub
        IF lo_repo_online->get_url( ) NS 'github.com'.
          RETURN.
        ENDIF.

        lv_commit = lo_repo_online->get_current_remote( ).

        " Check if repo contains abaplint rules
        lt_files = lo_repo_online->get_files_remote( ).
        LOOP AT lt_files TRANSPORTING NO FIELDS USING KEY file_path
          WHERE path = '/' AND filename CP 'abaplint*json'.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

      CATCH zcx_abapgit_exception.
        " If this fails, AG will show an error on the repo view anyway
        RETURN.
    ENDTRY.

    READ TABLE mt_wall INTO ls_wall WITH TABLE KEY commit = lv_commit.
    IF sy-subrc <> 0.

      TRY.
          CREATE OBJECT lo_check_run
            EXPORTING
              iv_url    = is_repo_meta-url
              iv_commit = lv_commit.

          ls_check_run = lo_check_run->get( ).

          IF ls_check_run IS INITIAL.
            ii_html->add( |<div { gv_div_attr }>No abaplint check run found. </div>| ).
            RETURN.
          ENDIF.

        CATCH zcx_abapgit_exception INTO lx_error.
          ii_html->add( |<div { gv_div_attr }>{ lx_error->get_text( ) }</div>| ).
          RETURN.
      ENDTRY.

      li_html = _wall_message_abaplint(
        iv_key       = is_repo_meta-key
        is_check_run = ls_check_run ).

      ls_wall-commit = lv_commit.
      ls_wall-html   = li_html.
      ls_wall-url    = ls_check_run-url.

      " Cache result of completed checkruns (others might change)
      IF ls_check_run-status = c_git_status-completed.
        INSERT ls_wall INTO TABLE mt_wall.
      ENDIF.

    ENDIF.

    ii_html->add( ls_wall-html->render( ) ).

    " Remember URL of last shown check run
    gv_last_url = ls_wall-url.

  ENDMETHOD.


  METHOD _wall_message_abaplint.

    DATA:
      lv_style   TYPE string,
      lv_act     TYPE string,
      lv_msg     TYPE string,
      lv_summary TYPE string.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( |<div { gv_div_attr }>| ).

    lv_act = |{ zif_abapgit_definitions=>c_action-url }?url={ is_check_run-url }|.

    CASE is_check_run-status.
      WHEN c_git_status-queued.
        ri_html->add_a(
          iv_txt = zcl_abapgit_html=>icon(
                     iv_name = 'circle-solid'
                     iv_hint = is_check_run-status )
          iv_act = lv_act ).
      WHEN c_git_status-in_progress.
        ri_html->add_a(
          iv_txt = zcl_abapgit_html=>icon(
                     iv_name  = 'circle-solid'
                     iv_class = 'warning'
                     iv_hint  = is_check_run-status )
          iv_act = lv_act ).
      WHEN c_git_status-completed.
        CASE is_check_run-conclusion.
          WHEN c_git_conclusion-neutral.
            ri_html->add_a(
              iv_txt = zcl_abapgit_html=>icon(
                         iv_name = 'circle-solid'
                         iv_hint = is_check_run-conclusion )
              iv_act = lv_act ).
          WHEN c_git_conclusion-success.
            ri_html->add_a(
              iv_txt = zcl_abapgit_html=>icon(
                         iv_name  = 'check'
                         iv_class = 'success'
                         iv_hint  = is_check_run-conclusion )
              iv_act = lv_act ).
          WHEN c_git_conclusion-failure
            OR c_git_conclusion-action_required
            OR c_git_conclusion-cancelled
            OR c_git_conclusion-skipped
            OR c_git_conclusion-stale
            OR c_git_conclusion-timed_out.
            ri_html->add_a(
              iv_txt  = zcl_abapgit_html=>icon(
                         iv_name  = 'times-solid'
                         iv_class = 'error'
                         iv_hint  = is_check_run-conclusion )
              iv_act = lv_act ).
          WHEN OTHERS.
            ri_html->add( |Unexpected value "{ is_check_run-conclusion }" for "conclusion"| ).
        ENDCASE.
      WHEN OTHERS.
        ri_html->add( |Unexpected value "{ is_check_run-status }" for "status"| ).
    ENDCASE.

    lv_msg = is_check_run-name.
    IF lv_msg <> is_check_run-app.
      lv_msg = |{ is_check_run-app } - { is_check_run-name }|.
    ENDIF.

    lv_summary = is_check_run-summary.
    IF lv_summary IS NOT INITIAL.
      REPLACE 'First 50 annotations shown, ' IN lv_summary WITH ''.

      lv_act = |{ c_action-go_abaplint }?| &&
               |key={ iv_key }&checkrun={ is_check_run-id }&total={ is_check_run-count_total }|.

      " todo, maybe better to show link only for failures
      lv_summary = ri_html->a(
        iv_txt = lv_summary
        iv_act = lv_act ).

      lv_msg = |{ lv_msg }: { lv_summary }|.
    ENDIF.

    ri_html->add( lv_msg ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
