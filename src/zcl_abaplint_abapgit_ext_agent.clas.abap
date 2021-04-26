CLASS zcl_abaplint_abapgit_ext_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      IMPORTING
        !iv_url            TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abaplint_abapgit_ext_agent
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS get_annotations
      IMPORTING
        !iv_check_run  TYPE string
      RETURNING
        VALUE(ri_json) TYPE REF TO zif_abapgit_ajson_reader
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_ajson_error .
    METHODS get_check_runs
      IMPORTING
        !iv_commit     TYPE zif_abapgit_definitions=>ty_sha1
      RETURNING
        VALUE(ri_json) TYPE REF TO zif_abapgit_ajson_reader
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_ajson_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance,
        url       TYPE string,
        instanace TYPE REF TO zcl_abaplint_abapgit_ext_agent,
      END OF ty_instance .

    CLASS-DATA:
      gt_instance TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY url .
    DATA mv_url TYPE string .
    DATA mo_agent TYPE REF TO zif_abapgit_http_agent .
ENDCLASS.



CLASS zcl_abaplint_abapgit_ext_agent IMPLEMENTATION.


  METHOD constructor.

    IF iv_url NS 'github.com'.
      zcx_abapgit_exception=>raise( 'abaplint Extension for abapGit only works with Github.com' ).
    ENDIF.

    mv_url = replace(
      val  = iv_url
      sub  = 'github.com'
      with = 'api.github.com/repos' ).

    mv_url = replace(
      val   = mv_url
      regex = '\.git$'
      with  = '' ).

    mo_agent = zcl_abapgit_factory=>get_http_agent( ).

    mo_agent->global_headers( )->set(
      iv_key = 'Accept'
      iv_val = 'application/vnd.github.v3+json' ).

    " Get auth token from repo
    IF zcl_abapgit_login_manager=>get( iv_url ) IS NOT INITIAL.
      mo_agent->global_headers( )->set(
        iv_key = 'Authorization'
        iv_val = zcl_abapgit_login_manager=>get( iv_url ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_annotations.

    DATA lv_url TYPE string.

    lv_url = mv_url && |/check-runs/{ iv_check_run }/annotations|.

    ri_json = mo_agent->request( lv_url )->json( ).

  ENDMETHOD.


  METHOD get_check_runs.

    DATA lv_url TYPE string.

    lv_url = mv_url && |/commits/{ iv_commit }/check-runs|.

    ri_json = mo_agent->request( lv_url )->json( ).

  ENDMETHOD.


  METHOD get_instance.

    DATA ls_instance LIKE LINE OF gt_instance.

    READ TABLE gt_instance INTO ls_instance WITH TABLE KEY url = iv_url.
    IF sy-subrc = 0.
      ro_instance = ls_instance-instanace.
    ELSE.
      CREATE OBJECT ro_instance
        EXPORTING
          iv_url = iv_url.

      ls_instance-url       = iv_url.
      ls_instance-instanace = ro_instance.
      INSERT ls_instance INTO TABLE gt_instance.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
