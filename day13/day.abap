CLASS grid DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t      TYPE i,
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

    METHODS:
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


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/13` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

  DATA(grid) = NEW grid( ).
  LOOP AT lines INTO DATA(line).

    IF NOT matches( val = line regex = '^\d+,\d+$' ).
      CONTINUE.
    ENDIF.

    SPLIT line AT ',' INTO DATA(xstr) DATA(ystr).
    grid->set( x = CONV i( xstr ) y = CONV i( ystr ) value = 1 ).

  ENDLOOP.

  PERFORM part1.
  PERFORM part2.



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

  LOOP AT lines INTO DATA(line).

    IF line NS `fold`.
      CONTINUE.
    ENDIF.

    FIND REGEX '(x|y)=(\d+)' IN line SUBMATCHES DATA(axis) DATA(offsetstr).
    DATA(offset) = CONV i( offsetstr ).
    PERFORM fold USING grid axis offset.

    EXIT.

  ENDLOOP.

  DATA(count) = 0.
  DATA(points) = grid->get_all_points( ).
  LOOP AT points->* REFERENCE INTO DATA(point) WHERE value > 0.
    count = count + 1.
  ENDLOOP.


  DATA(answer) = count.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.

  LOOP AT lines INTO DATA(line).

    IF line NS `fold`.
      CONTINUE.
    ENDIF.

    FIND REGEX '(x|y)=(\d+)' IN line SUBMATCHES DATA(axis) DATA(offsetstr).
    DATA(offset) = CONV i( offsetstr ).
    PERFORM fold USING grid axis offset.

  ENDLOOP.


  DATA(max_x) = 0.
  DATA(max_y) = 0.
  DATA(points) = grid->get_all_points( ).
  LOOP AT points->* REFERENCE INTO DATA(point) WHERE value > 0.

    IF point->x > max_x.
      max_x = point->x.
    ENDIF.
    IF point->y > max_y.
      max_y = point->y.
    ENDIF.

  ENDLOOP.


  WRITE / 'Part 2:'.
  DO max_y + 1 TIMES.
    DATA(y) = sy-index - 1.

    DATA(out_line) = ``.
    DO max_x + 1 TIMES.
      DATA(x) = sy-index - 1.

      out_line = out_line && COND string( WHEN grid->get( x = x y = y ) > 0 THEN `X` ELSE ` `).

    ENDDO.
    WRITE / out_line.

  ENDDO.

ENDFORM.

FORM fold USING grid TYPE REF TO grid
                axis TYPE string
                offset TYPE i.

  DATA(points) = grid->get_all_points( ).
  IF axis = `x`.

    LOOP AT points->* REFERENCE INTO DATA(point) WHERE x > offset.

      DATA(newx) = ( ( point->x - offset ) * -1 ) + offset.
      DATA(new_point) = grid->get_point( x = newx y = point->y ).

      new_point-ref->* = new_point-value + point->value.
      point->ref->* = 0.

    ENDLOOP.

  ELSE.

    LOOP AT points->* REFERENCE INTO point WHERE y > offset.

      DATA(newy) = ( ( point->y - offset ) * -1 ) + offset.
      new_point = grid->get_point( x = point->x y = newy ).

      new_point-ref->* = new_point-value + point->value.
      point->ref->* = 0.

    ENDLOOP.

  ENDIF.

ENDFORM.


CLASS grid IMPLEMENTATION.

  METHOD get.

    DATA(ref) = me->get_ref( x = x y = y ).
    ret = ref->*.

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
