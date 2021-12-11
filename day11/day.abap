CLASS grid DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t      TYPE REF TO i,
      value_list_t TYPE TABLE OF value_t WITH DEFAULT KEY,

      BEGIN OF point_t,
        x     TYPE i,
        y     TYPE i,
        value TYPE value_t,
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
      get_neighbours_of
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE point_list_t.

ENDCLASS.

TYPES indexed_point_list_t TYPE SORTED TABLE OF grid=>point_t WITH UNIQUE KEY primary_key COMPONENTS x y.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/11` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

  DATA(field_width) = 10.
  DATA(field_height) = 10.
  DATA(field_size) = field_width * field_height.

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

  DATA(grid) = NEW grid( ).
  PERFORM read_field USING grid.

  DATA(total_flashes) = 0.
  DO 100 TIMES.

    DATA(flashed_points) = VALUE grid=>point_list_t( ).
    PERFORM step USING grid CHANGING flashed_points.
    total_flashes = total_flashes + lines( flashed_points ).

  ENDDO.


  DATA(answer) = total_flashes.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.

  DATA(grid) = NEW grid( ).
  PERFORM read_field USING grid.

  DATA(total_flashes) = 0.
  DATA step TYPE i.
  DO.

    step = sy-index.

    DATA(flashed_points) = VALUE grid=>point_list_t( ).
    PERFORM step USING grid CHANGING flashed_points.

    IF lines( flashed_points ) = field_size.
      EXIT.
    ENDIF.

  ENDDO.


  DATA(answer) = step.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.


FORM read_field USING grid TYPE REF TO grid.

  DO field_height TIMES.
    DATA(y) = sy-index - 1.

    DATA(line) = lines[ y + 1 ].
    DO field_width TIMES.
      DATA(x) = sy-index - 1.

      grid->set( x = x y = y value = NEW i( CONV i( line+x(1) ) ) ).

    ENDDO.

  ENDDO.

ENDFORM.

FORM step USING grid TYPE REF TO grid
       CHANGING flashed_points TYPE grid=>point_list_t.

  DATA flashed TYPE indexed_point_list_t.
  DATA flash_queue TYPE indexed_point_list_t.

  DO field_height TIMES.
    DATA(y) = sy-index - 1.

    DO field_width TIMES.
      DATA(x) = sy-index - 1.

      DATA(point) = grid->get_point( x = x y = y ).
      point-value->* = point-value->* + 1.
      IF point-value->* >= 10.
        INSERT point INTO TABLE flash_queue.
      ENDIF.

    ENDDO.

  ENDDO.

  WHILE lines( flash_queue ) <> 0.

    point = flash_queue[ lines( flash_queue ) ].
    DELETE flash_queue INDEX lines( flash_queue ).
    INSERT point INTO TABLE flashed.

    DATA(neighbours) = grid->get_neighbours_of( x = point-x y = point-y ).
    LOOP AT neighbours INTO DATA(neighbour).
      IF neighbour-x < 0 OR neighbour-x >= field_width OR neighbour-y < 0 OR neighbour-y >= field_height.
        CONTINUE.
      ENDIF.

      neighbour-value->* = neighbour-value->* + 1.
      IF neighbour-value->* >= 10
      AND NOT line_exists( flash_queue[ x = neighbour-x y = neighbour-y ] )
      AND NOT line_exists( flashed[ x = neighbour-x y = neighbour-y ] ).
        INSERT neighbour INTO TABLE flash_queue.
      ENDIF.

    ENDLOOP.

  ENDWHILE.


  LOOP AT flashed INTO DATA(flashed_point).
    flashed_point-value->* = 0.
  ENDLOOP.

  flashed_points = VALUE #( ).
  INSERT LINES OF flashed INTO TABLE flashed_points.

ENDFORM.



CLASS grid IMPLEMENTATION.

  METHOD get.

    DATA(ref) = me->get_ref( x = x y = y ).
    ret = ref->*.

  ENDMETHOD.
  METHOD get_point.

    ret = VALUE #( x = x
                   y = y
                   value = me->get( x = x y = y ) ).

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

  METHOD get_neighbours_of.

    ret = VALUE point_list_t(
        ( me->get_point( x = x - 1 y = y - 1 ) )
        ( me->get_point( x = x y = y - 1 ) )
        ( me->get_point( x = x + 1 y = y - 1 ) )
        ( me->get_point( x = x - 1 y = y ) )
        ( me->get_point( x = x + 1 y = y ) )
        ( me->get_point( x = x - 1 y = y + 1 ) )
        ( me->get_point( x = x y = y + 1 ) )
        ( me->get_point( x = x + 1 y = y + 1 ) )
    ).

  ENDMETHOD.

ENDCLASS.
