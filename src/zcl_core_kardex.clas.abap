CLASS zcl_core_kardex DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_rap_query_provider .

      METHODS:
      get_last_day_of_month
        IMPORTING iv_date       TYPE datum
        RETURNING VALUE(rv_end) TYPE datum.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CORE_KARDEX IMPLEMENTATION.


  METHOD get_last_day_of_month.
    DATA lv_next_month  TYPE datum.

    lv_next_month = iv_date.
    lv_next_month+6(2) = '01'.
    lv_next_month+4(2) = lv_next_month+4(2) + 1.

    IF lv_next_month+4(2) > '12'.
      lv_next_month+4(2) = '01'.
      lv_next_month+0(4) = lv_next_month+0(4) + 1.
    ENDIF.

    rv_end = lv_next_month - 1.
  ENDMETHOD.


  METHOD if_rap_query_provider~select.


    "--------------------------------------------------------------
    " 1. Declaración de variables
    "--------------------------------------------------------------
    DATA: lv_year      TYPE gjahr,
          lv_month     TYPE monat,
          lv_fecha_ini TYPE datum,
          lv_fecha_fin TYPE datum.

    DATA: lv_skip     TYPE i,
          lv_top      TYPE i,
          lv_max_rows TYPE i.

    "--------------------------------------------------------------
    " 2. Leer parámetros de entrada
    "--------------------------------------------------------------


    DATA(lt_parameters) = io_request->get_parameters( ).

    READ TABLE lt_parameters INTO DATA(ls_param)
     WITH KEY parameter_name = 'P_MONTH'.
    IF sy-subrc = 0.
      lv_month = ls_param-value.
    ENDIF.

    READ TABLE lt_parameters INTO ls_param
  WITH KEY parameter_name = 'P_YEAR'.
    IF sy-subrc = 0.
      lv_year = ls_param-value.
    ENDIF.

    DATA: lt_matnr_rng TYPE RANGE OF matnr,
          lt_werks_rng TYPE RANGE OF werks_d.

    TRY.
        DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_range).
        RETURN.
    ENDTRY.

    LOOP AT lt_filters INTO DATA(lo_filter).
      CASE lo_filter-name.
        WHEN 'MATERIAL'.
          LOOP AT lo_filter-range INTO DATA(ls_range).
            APPEND VALUE #( sign = ls_range-sign
                            option = ls_range-option
                            low = ls_range-low
                            high = ls_range-high ) TO lt_matnr_rng.
          ENDLOOP.

        WHEN 'PLANT'.
          LOOP AT lo_filter-range INTO ls_range.
            APPEND VALUE #( sign = ls_range-sign
                            option = ls_range-option
                            low = ls_range-low
                            high = ls_range-high ) TO lt_werks_rng.
          ENDLOOP.

      ENDCASE.
    ENDLOOP.


    DATA: lt_mov_mes TYPE STANDARD TABLE OF zc_kdx_kardex_v2 WITH EMPTY KEY.

    " Obtener primer y ultimo dia del mes

    lv_fecha_ini = |{ lv_year }{ lv_month }01|.
    lv_fecha_fin   = get_last_day_of_month( iv_date = lv_fecha_ini ).

    "--------------------------------------------------------------
    " 3. Obtener parámetros de paginación
    "--------------------------------------------------------------

    lv_skip     = io_request->get_paging( )->get_offset( ).
    lv_top      = io_request->get_paging( )->get_page_size( ).
    lv_max_rows = COND #( WHEN lv_top = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_top ).


    "--------------------------------------------------------------
    " 4. Obtener movimientos del mes
    "--------------------------------------------------------------
    TRY.
        SELECT
         material,
         plant,
         @lv_year  AS calendaryear,
         @lv_month AS calendarmonth,
         SUM(
           CASE
             WHEN debitcreditcode EQ 'S' THEN quantityinbaseunit
             WHEN debitcreditcode EQ 'H' THEN - quantityinbaseunit
             ELSE 0
           END
         ) AS quantityinbaseunit,
         SUM(
           CASE
             WHEN debitcreditcode EQ 'S' THEN totalgoodsmvtamtincccrcy
             WHEN debitcreditcode EQ 'H' THEN - totalgoodsmvtamtincccrcy
             ELSE 0
           END
         ) AS totalgoodsmvtamtincccrcy
         FROM i_materialdocumentitem_2
         WHERE postingdate BETWEEN @lv_fecha_ini AND @lv_fecha_fin
         AND   material IN @lt_matnr_rng
         AND   plant    IN @lt_werks_rng
         AND CONSUMPTIONPOSTING NOT IN ( 'A', 'E', 'P' )
*         AND goodsmovementrefdoctype not in ( 'B' )
*         AND goodsreceipttype NOT IN ( 'X' )
         AND IsAutomaticallyCreated NE 'X'
         AND STORAGELOCATION NE @space
         GROUP BY material, plant, fiscalyearperiod
         ORDER BY material, plant, fiscalyearperiod
         INTO TABLE @lt_mov_mes.


        "--------------------------------------------------------------
        " 5. Buscar última fecha anterior para saldo inicial
        "--------------------------------------------------------------
        SELECT material, plant, MAX( postingdate ) AS postingdate
          FROM i_materialdocumentitem_2
          WHERE postingdate < @lv_fecha_ini
*          WHERE postingdate < @lv_fecha_fin
          AND   material IN @lt_matnr_rng
          AND   plant    IN @lt_werks_rng
          GROUP BY material, plant
          INTO TABLE @DATA(lt_ult_fecha).

        " Eliminar materiales con movimiento
*        LOOP AT lt_ult_fecha INTO DATA(ls_fecha).
*          READ TABLE lt_mov_mes WITH KEY material = ls_fecha-material plant = ls_fecha-plant TRANSPORTING NO FIELDS.
*          IF sy-subrc = 0.
*            DELETE lt_ult_fecha WHERE material = ls_fecha-material AND plant = ls_fecha-plant.
*          ENDIF.
*        ENDLOOP.

        "----------------------------------------------------------
        " 4. MOVIMIENTOS HISTÓRICOS (SALDO INICIAL)
        "----------------------------------------------------------
        "--------------------------------------------------------------
        " 6. Obtener movimientos históricos (saldo inicial)
        "--------------------------------------------------------------
        TYPES: BEGIN OF ty_hist,
                 material                 TYPE i_materialdocumentitem_2-material,
                 plant                    TYPE i_materialdocumentitem_2-plant,
                 postingdate              TYPE i_materialdocumentitem_2-postingdate,
                 debitcreditcode          TYPE i_materialdocumentitem_2-debitcreditcode,
                 quantityinbaseunit       TYPE i_materialdocumentitem_2-quantityinbaseunit,
                 totalgoodsmvtamtincccrcy TYPE i_materialdocumentitem_2-totalgoodsmvtamtincccrcy,
               END OF ty_hist.

        DATA lt_hist TYPE STANDARD TABLE OF ty_hist.
        TYPES: zc_kdx_kardex_v2_tt TYPE STANDARD TABLE OF zc_kdx_kardex_v2 WITH EMPTY KEY.

        IF lt_ult_fecha IS NOT INITIAL.
          SELECT
           materialdocument,
           MATERIALDOCUMENTITEM,
          material,
          plant,
          postingdate,
          debitcreditcode,
          quantityinbaseunit,
          totalgoodsmvtamtincccrcy,
          goodsmovementrefdoctype,
          goodsreceipttype
           FROM i_materialdocumentitem_2
     FOR ALL ENTRIES IN @lt_ult_fecha
     WHERE material    = @lt_ult_fecha-material
              AND plant       = @lt_ult_fecha-plant
       AND postingdate <= @lt_ult_fecha-postingdate
       AND CONSUMPTIONPOSTING NOT IN ( 'A', 'E', 'P' )
       AND IsAutomaticallyCreated NE 'X'
       AND STORAGELOCATION NE @space
*              AND ISSUINGORRECEIVINGSTOCKTYPE NOT IN ('06')
     INTO TABLE @DATA(lt_hist_2).

*          LOOP AT lt_hist_2 INTO DATA(ls_hist_2).
*
*            DATA(lv_index) = sy-tabix.
*
*            IF  ls_hist_2-goodsmovementrefdoctype EQ 'B'
*            AND ls_hist_2-goodsreceipttype EQ 'X'.
*
*              DELETE lt_hist_2 INDEX lv_index.
*            ENDIF.
*          ENDLOOP.


          lt_hist = CORRESPONDING #( lt_hist_2 ).
*          SELECT
*                 material,
*                 plant,
*                 postingdate,
*                 debitcreditcode,
*                 quantityinbaseunit,
*                 totalgoodsmvtamtincccrcy FROM i_materialdocumentitem_2
*            FOR ALL ENTRIES IN @lt_ult_fecha
*            WHERE material    = @lt_ult_fecha-material
**              AND plant       = @lt_ult_fecha-plant
*              AND postingdate <= @lt_ult_fecha-postingdate
*            INTO TABLE @lt_hist.
        ENDIF.

        " Calcular saldos
        DATA lt_saldos_sinmov TYPE zc_kdx_kardex_v2_tt.
        LOOP AT lt_hist INTO DATA(ls_hist).
          DATA(lv_q_ini) = COND decfloat16(
                              WHEN ls_hist-debitcreditcode = 'S' THEN  ls_hist-quantityinbaseunit
                              WHEN ls_hist-debitcreditcode = 'H' THEN - ls_hist-quantityinbaseunit
                              ELSE 0 ).
          DATA(lv_v_ini) = COND decfloat16(
                              WHEN ls_hist-debitcreditcode = 'S' THEN  ls_hist-totalgoodsmvtamtincccrcy
                              WHEN ls_hist-debitcreditcode = 'H' THEN - ls_hist-totalgoodsmvtamtincccrcy
                              ELSE 0 ).

          READ TABLE lt_saldos_sinmov INTO DATA(ls_saldo_existente)
               WITH KEY material = ls_hist-material
                        plant    = ls_hist-plant.
          DATA(lv_idx) = sy-tabix.

          IF sy-subrc = 0 AND lv_idx > 0.
            " Ya existe, acumular y modificar con INDEX
            ls_saldo_existente-initialquantity += lv_q_ini.
            ls_saldo_existente-initialvalue    += lv_v_ini.
            MODIFY lt_saldos_sinmov FROM ls_saldo_existente INDEX lv_idx.
          ELSE.
            " Nuevo registro
            APPEND VALUE zc_kdx_kardex_v2(
              material        = ls_hist-material
              plant           = ls_hist-plant
              calendaryear    = lv_year
              calendarmonth   = lv_month
              initialquantity = lv_q_ini
              initialvalue    = lv_v_ini ) TO lt_saldos_sinmov.
          ENDIF.
        ENDLOOP.

        "--------------------------------------------------------------
        " 7. Armar resultado final
        "--------------------------------------------------------------
        DATA(lt_result) = VALUE zc_kdx_kardex_v2_tt( ).

        LOOP AT lt_saldos_sinmov INTO DATA(ls_mov).

           READ TABLE  lt_mov_mes INTO data(ls_sin) WITH KEY Material = ls_mov-Material
                                                                   plant    = ls_mov-Plant.

        if sy-subrc = 0.

          APPEND VALUE #( material                 = ls_mov-material
                          plant                    = ls_mov-plant
*                      postingdate              = ls_mov-postingdate
                          calendaryear             = lv_year
                          calendarmonth            = lv_month
                          quantityinbaseunit       = ls_sin-quantityinbaseunit + ls_mov-InitialQuantity "ls_mov-quantityinbaseunit
                          totalgoodsmvtamtincccrcy = ls_sin-totalgoodsmvtamtincccrcy + ls_mov-InitialValue "ls_mov-totalgoodsmvtamtincccrcy
*                          initialquantity          = 0
*                          initialvalue             = 0 ) TO lt_result.
                          initialquantity          = ls_mov-InitialQuantity "ls_sin-InitialQuantity
                          initialvalue             = ls_mov-InitialValue ) TO lt_result. "ls_sin-InitialValue ) TO lt_result.
          ELSE.
            APPEND VALUE #( material                 = ls_mov-material
                          plant                    = ls_mov-plant
*                      postingdate              = ls_mov-postingdate
                          calendaryear             = lv_year
                          calendarmonth            = lv_month
                          quantityinbaseunit       = 0
                          totalgoodsmvtamtincccrcy = 0 "ls_mov-totalgoodsmvtamtincccrcy
*                          initialquantity          = 0
*                          initialvalue             = 0 ) TO lt_result.
                          initialquantity          = ls_mov-InitialQuantity "ls_sin-InitialQuantity
                          initialvalue             = ls_mov-InitialValue ) TO lt_result. "ls_sin-InitialValue ) TO lt_result

         ENDIF.
        ENDLOOP.

*        APPEND LINES OF lt_saldos_sinmov TO lt_result.

        "--------------------------------------------------------------
        " 8. Paginación en memoria (solo si es necesario)
        "--------------------------------------------------------------
        DATA(lt_paged_result) = lt_result.
        IF lv_max_rows > 0.
          lt_paged_result = VALUE #( FOR idx = lv_skip + 1 THEN idx + 1
                                     WHILE idx <= lines( lt_result ) AND
                                           idx <= lv_skip + lv_max_rows
                                     LET ls_row = lt_result[ idx ] IN
                                     ( ls_row ) ).
        ENDIF.

        "--------------------------------------------------------------
        " 9. Retornar datos y total
        "--------------------------------------------------------------
        io_response->set_data( lt_paged_result ).

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records(
            iv_total_number_of_records = lines( lt_result )
          ).
        ENDIF.
        "--------------------------------------------------------------
        " 10. Manejo de excepciones
        "--------------------------------------------------------------
      CATCH cx_rap_query_provider INTO DATA(lx_query).
        " Registrar mensaje
        RAISE EXCEPTION lx_query.


*
*      CATCH cx_root INTO DATA(lx_root).
*RAISE EXCEPTION lx_root.
    ENDTRY.



  ENDMETHOD.
ENDCLASS.
