CLASS zcl_abaplint_abapgit_ext_chkrn DEFINITION
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
* GitHub REST API for Checks - Check Run
* https://docs.github.com/en/rest/reference/checks
************************************************************************
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_check_run,
        app           TYPE string,
        version       TYPE string,
        status        TYPE string,
        conclusion    TYPE string,
        name          TYPE string,
        id            TYPE string,
        url           TYPE string,
        summary       TYPE string,
        display       TYPE string,
        count_issues  TYPE i,
        count_objects TYPE i,
        count_deps    TYPE i,
      END OF ty_check_run.

    METHODS constructor
      IMPORTING
        !iv_url    TYPE string
        !iv_commit TYPE zcl_abaplint_abapgit_ext_exit=>ty_sha1
      RAISING
        zcx_abapgit_exception.

    METHODS get
      RETURNING
        VALUE(rs_check_run) TYPE ty_check_run
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      mv_url    TYPE string,
      mv_commit TYPE zcl_abaplint_abapgit_ext_exit=>ty_sha1.

    METHODS format
      CHANGING
        cs_check_run TYPE ty_check_run.

ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_chkrn IMPLEMENTATION.


  METHOD constructor.
    mv_url    = iv_url.
    mv_commit = iv_commit.
  ENDMETHOD.


  METHOD format.

    DATA lv_count TYPE string ##NEEDED.

    REPLACE ALL OCCURRENCES OF
      cl_abap_char_utilities=>newline && cl_abap_char_utilities=>newline
      IN cs_check_run-summary WITH `, `.

    REPLACE 'First 50 annotations shown, ' IN cs_check_run-summary WITH ''.
    REPLACE ALL OCCURRENCES OF '"' IN cs_check_run-summary WITH ''.

    FIND REGEX 'abaplint ([0-9]+\.[0-9]+\.[0-9]+)' IN cs_check_run-summary
      SUBMATCHES cs_check_run-version ##REGEX_POSIX.
    IF sy-subrc <> 0.
      cs_check_run-version = '(unknown version)'.
    ENDIF.

    " Remove link to https://github.com/apps/abaplint/installations/new
    REPLACE REGEX ', \[adjust installations\].*' IN cs_check_run-summary WITH '' ##REGEX_POSIX.
    " Remove markdown table with detailed findings
    REPLACE REGEX '\|.*\|,' IN cs_check_run-summary WITH '' ##REGEX_POSIX.

    " 14 issues found
    FIND REGEX '([0-9]+) issues found' IN cs_check_run-summary SUBMATCHES lv_count ##REGEX_POSIX.
    IF sy-subrc = 0.
      cs_check_run-count_issues = lv_count.
    ENDIF.

    " abaplint <= 2.113
    " {"total":758,"normal":10,"dependencies":748} objects analyzed
    FIND REGEX 'total:([0-9]+)' IN cs_check_run-summary SUBMATCHES lv_count ##REGEX_POSIX.
    IF sy-subrc = 0.
      cs_check_run-count_objects = lv_count.
    ENDIF.
    FIND REGEX 'dependencies:([0-9]+)' IN cs_check_run-summary SUBMATCHES lv_count ##REGEX_POSIX.
    IF sy-subrc = 0.
      cs_check_run-count_deps = lv_count.
    ENDIF.

    " abaplint >= 2.114
    " 917 objects analyzed, including 749 dependencies
    FIND REGEX '([0-9]+) objects analyzed' IN cs_check_run-summary SUBMATCHES lv_count ##REGEX_POSIX.
    IF sy-subrc = 0.
      cs_check_run-count_objects = lv_count.
    ENDIF.
    FIND REGEX '([0-9]+) dependencies' IN cs_check_run-summary SUBMATCHES lv_count ##REGEX_POSIX.
    IF sy-subrc = 0.
      cs_check_run-count_deps = lv_count.
    ENDIF.

    cs_check_run-display = |{ cs_check_run-count_issues NUMBER = USER } |.

    IF cs_check_run-count_issues = 1.
      cs_check_run-display  = cs_check_run-display && 'issue'.
    ELSE.
      cs_check_run-display  = cs_check_run-display && 'issues'.
    ENDIF.

    cs_check_run-display = cs_check_run-display && |, { cs_check_run-count_objects NUMBER = USER } |.

    IF cs_check_run-count_objects = 1.
      cs_check_run-display = cs_check_run-display && 'object analyzed'.
    ELSE.
      cs_check_run-display = cs_check_run-display && 'objects analyzed'.
    ENDIF.

    IF cs_check_run-count_deps > 0.
      cs_check_run-display = cs_check_run-display && | ({ cs_check_run-count_deps NUMBER = USER }|.
      IF cs_check_run-count_deps = 1.
        cs_check_run-display = cs_check_run-display && ' dependency)'.
      ELSE.
        cs_check_run-display = cs_check_run-display && ' dependencies)'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get.

    DATA:
      li_json       TYPE REF TO zif_abapgit_ajson,
      lx_error      TYPE REF TO zcx_abapgit_ajson_error,
      lt_check_runs TYPE TABLE OF string,
      lv_check_run  TYPE string,
      lv_msg        TYPE string,
      lv_app        TYPE string,
      lv_name       TYPE string.

    TRY.
        li_json = zcl_abaplint_abapgit_ext_agent=>get_instance( mv_url )->get_check_runs( mv_commit ).

        lv_msg = li_json->get( '/message' ).

        IF lv_msg <> ''.
          zcx_abapgit_exception=>raise( |Error getting check runs: { lv_msg }| ).
        ENDIF.

        lt_check_runs = li_json->members( '/check_runs' ).

        LOOP AT lt_check_runs INTO lv_check_run.

          lv_app  = li_json->get( |/check_runs/{ lv_check_run }/app/name| ).
          lv_name = li_json->get( |/check_runs/{ lv_check_run }/name| ).

          " Only interested in abaplint run (not builds or abalint/observations)
          IF lv_app = 'abaplint' AND lv_name = 'abaplint'.

            rs_check_run-app          = lv_app.
            rs_check_run-name         = lv_name.
            rs_check_run-id           = li_json->get( |/check_runs/{ lv_check_run }/id| ).
            rs_check_run-status       = li_json->get( |/check_runs/{ lv_check_run }/status| ).
            rs_check_run-conclusion   = li_json->get( |/check_runs/{ lv_check_run }/conclusion| ).
            rs_check_run-url          = li_json->get( |/check_runs/{ lv_check_run }/html_url| ).
            rs_check_run-summary      = li_json->get( |/check_runs/{ lv_check_run }/output/summary| ).
            rs_check_run-count_issues = li_json->get( |/check_runs/{ lv_check_run }/output/annotation_count| ).

            " Some general error and not abaplint findings
            IF rs_check_run-summary CS 'error' AND rs_check_run-summary NS 'objects analyzed'.
              zcx_abapgit_exception=>raise( rs_check_run-summary ).
            ENDIF.

            format( CHANGING cs_check_run = rs_check_run ).

            EXIT. " >>>>

          ENDIF.

        ENDLOOP.

      CATCH zcx_abapgit_ajson_error INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
