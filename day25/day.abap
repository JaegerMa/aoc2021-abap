CLASS grid DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t      TYPE string,
      value_list_t TYPE TABLE OF value_t WITH DEFAULT KEY,

      BEGIN OF point_t,
        x     TYPE i,
        y     TYPE i,
        value TYPE value_t,
        ref   TYPE REF TO value_t,
      END OF point_t,
      point_list_t TYPE TABLE OF point_t WITH DEFAULT KEY,

      BEGIN OF cell_t,
        col   TYPE i,
        value TYPE value_t,
      END OF cell_t,
      BEGIN OF row_t,
        row   TYPE i,
        cells TYPE SORTED TABLE OF cell_t WITH UNIQUE KEY primary_key COMPONENTS col,
      END OF row_t,

      grid_data_t TYPE SORTED TABLE OF row_t WITH UNIQUE KEY primary_key COMPONENTS row.

    DATA grid_data TYPE grid_data_t.
    DATA height TYPE i.
    DATA width TYPE i.

    METHODS:
      constructor
        IMPORTING height TYPE i
                  width  TYPE i,
      get
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE value_t,
      set
        IMPORTING x     TYPE i
                  y     TYPE i
                  value TYPE value_t,

      get_ref
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE REF TO value_t,
      get_point
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE point_t,
      get_all_points
        RETURNING VALUE(ret) TYPE REF TO point_list_t.

ENDCLASS.

TYPES:
  BEGIN OF change_t,
    from TYPE grid=>point_t,
    to   TYPE grid=>point_t,
  END OF change_t,
  change_list_t TYPE TABLE OF change_t WITH DEFAULT KEY.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/25` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

  PERFORM part1.




FORM load_input USING path TYPE string
             CHANGING input TYPE string.

  DATA input_bin TYPE xstring.
  OPEN DATASET path FOR INPUT IN BINARY MODE.
  READ DATASET path INTO input_bin.
  CLOSE DATASET path.

  DATA(line_break) = CONV xstring( '0A' ).
  DATA(zero) = CONV xstring( '00' ).
  SHIFT input_bin RIGHT DELETING TRAILING line_break IN BYTE MODE.
  SHIFT input_bin LEFT DELETING LEADING zero IN BYTE MODE.

  input = cl_http_utility=>if_http_utility~decode_utf8( input_bin ).

ENDFORM.

FORM part1.

  DATA grid TYPE REF TO grid.
  PERFORM read_grid CHANGING grid.

  DATA(iterations) = 0.
  DO.

    iterations = iterations + 1.

    DATA(change_count) = 0.
    PERFORM next_step USING grid CHANGING change_count.

    IF change_count = 0.
      EXIT.
    ENDIF.

  ENDDO.


  DATA(answer) = iterations.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM read_grid CHANGING grid TYPE REF TO grid.

  grid = NEW grid( height = lines( lines ) width = strlen( lines[ 1 ] ) ).

  DO lines( lines ) TIMES.
    DATA(y) = sy-index - 1.

    DATA(line) = lines[ y + 1 ].

    DO strlen( line ) TIMES.
      DATA(x) = sy-index - 1.

      DATA(char) = line+x(1).
      grid->set( x = x y = y value = char ).

    ENDDO.

  ENDDO.

ENDFORM.

FORM next_step USING grid TYPE REF TO grid
            CHANGING change_count TYPE i.

  DATA(points) = grid->get_all_points( ).

  DATA(changes) = VALUE change_list_t( ).
  LOOP AT points->* REFERENCE INTO DATA(point) WHERE value = `>`.

    DATA(destination_point) = grid->get_point( x = ( point->x + 1 ) MOD grid->width y = point->y ).
    IF destination_point-value <> `.`.
      CONTINUE.
    ENDIF.


    destination_point-ref->* = `#`.
    INSERT VALUE #( from = point->* to = destination_point ) INTO TABLE changes.

  ENDLOOP.

  change_count = change_count + lines( changes ).

  LOOP AT changes INTO DATA(change).
    change-from-ref->* = `.`.
    change-to-ref->* = `>`.
  ENDLOOP.


  changes = VALUE #( ).
  LOOP AT points->* REFERENCE INTO point WHERE value = `v`.

    destination_point = grid->get_point( x = point->x y = ( point->y + 1 ) MOD grid->height ).
    IF destination_point-value <> `.`.
      CONTINUE.
    ENDIF.


    destination_point-ref->* = `#`.
    INSERT VALUE #( from = point->* to = destination_point ) INTO TABLE changes.

  ENDLOOP.

  change_count = change_count + lines( changes ).

  LOOP AT changes INTO change.
    change-from-ref->* = `.`.
    change-to-ref->* = `v`.
  ENDLOOP.

ENDFORM.

CLASS grid IMPLEMENTATION.

  METHOD constructor.

    me->height = height.
    me->width = width.

  ENDMETHOD.


  METHOD get.

    DATA(row) = REF #( me->grid_data[ row = y ] OPTIONAL ).
    IF row IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(cell) = REF #( row->cells[ col = x ] OPTIONAL ).
    IF cell IS NOT BOUND.
      RETURN.
    ENDIF.


    ret = cell->value.

  ENDMETHOD.
  METHOD get_point.

    DATA(value_ref) = me->get_ref( x = x y = y ).
    ret = VALUE #( x = x
                   y = y
                   value = value_ref->*
                   ref = value_ref ).

  ENDMETHOD.
  METHOD get_all_points.

    DATA(points) = NEW point_list_t( ).

    LOOP AT me->grid_data REFERENCE INTO DATA(row).
      LOOP AT row->cells REFERENCE INTO DATA(cell).
        INSERT VALUE #( x = cell->col y = row->row value = cell->value ref = REF #( cell->value ) ) INTO TABLE points->*.
      ENDLOOP.
    ENDLOOP.

    ret = points.

  ENDMETHOD.

  METHOD set.

    DATA(ref) = me->get_ref( x = x y = y ).
    ref->* = value.

  ENDMETHOD.


  METHOD get_ref.

    DATA(row) = REF #( me->grid_data[ row = y ] OPTIONAL ).
    IF row IS NOT BOUND.
      INSERT VALUE #( row = y ) INTO TABLE me->grid_data REFERENCE INTO row.
    ENDIF.

    DATA(cell) = REF #( row->cells[ col = x ] OPTIONAL ).
    IF cell IS NOT BOUND.
      INSERT VALUE #( col = x ) INTO TABLE row->cells REFERENCE INTO cell.
    ENDIF.


    ret = REF #( cell->value ).

  ENDMETHOD.

ENDCLASS.
