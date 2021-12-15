TYPES:
  BEGIN OF path_point_t,
    x    TYPE i,
    y    TYPE i,
    risk TYPE i,
  END OF path_point_t,
  path_point_ref_t      TYPE REF TO path_point_t,
  path_point_ref_list_t TYPE TABLE OF path_point_ref_t WITH DEFAULT KEY.

CLASS linked_list DEFINITION.

  PUBLIC SECTION.
    TYPES:
      value_t TYPE REF TO path_point_t,
      BEGIN OF node_t,
        value TYPE value_t,
        next  TYPE REF TO data,
      END OF node_t,
      node_ref_t TYPE REF TO node_t.

    DATA start TYPE node_ref_t.
    DATA end TYPE node_ref_t.
    DATA size TYPE i.


    METHODS:
      add
        IMPORTING value TYPE value_t,
      shift
        RETURNING VALUE(ret) TYPE value_t.

ENDCLASS.
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
    DATA size_x TYPE i.
    DATA size_y TYPE i.

    METHODS:
      constructor
        IMPORTING size_x TYPE i
                  size_y TYPE i,
      get
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE value_t,
      get_extended
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
        RETURNING VALUE(ret) TYPE point_t.

ENDCLASS.
CLASS point_map DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t TYPE i,
      BEGIN OF entry_t,
        x     TYPE i,
        y     TYPE i,
        value TYPE value_t,
      END OF entry_t,
      entries_t TYPE SORTED TABLE OF entry_t WITH UNIQUE KEY primary_key COMPONENTS x y.

    DATA entries TYPE entries_t.
    DATA preset TYPE value_t.

    METHODS:
      constructor
        IMPORTING preset TYPE value_t OPTIONAL,
      get
        IMPORTING x          TYPE i
                  y          TYPE i
        RETURNING VALUE(ret) TYPE value_t,
      set
        IMPORTING x     TYPE i
                  y     TYPE i
                  value TYPE value_t.

ENDCLASS.



START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/15` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).
  DATA risk_grid TYPE REF TO grid.
  PERFORM read_grid USING lines CHANGING risk_grid.

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

  DATA lowest_risk TYPE i.
  PERFORM find_lowest_risk USING risk_grid 99 99 100 100 CHANGING lowest_risk.

  DATA(answer) = lowest_risk.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.

  DATA lowest_risk TYPE i.
  PERFORM find_lowest_risk USING risk_grid 499 499 500 500 CHANGING lowest_risk.

  DATA(answer) = lowest_risk.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.



FORM read_grid USING lines TYPE string_table
            CHANGING grid TYPE REF TO grid.

  grid = NEW grid( size_x = strlen( lines[ 1 ] ) size_y = lines( lines ) ).

  LOOP AT lines INTO DATA(line).
    DATA(y) = sy-tabix - 1.

    DO strlen( line ) TIMES.
      DATA(x) = sy-index - 1.

      DATA(value) = CONV i( line+x(1) ).
      grid->set( x = x y = y value = value ).

    ENDDO.

  ENDLOOP.

ENDFORM.

FORM find_lowest_risk USING grid TYPE REF TO grid
                            dest_x TYPE i
                            dest_y TYPE i
                            grid_size_x TYPE i
                            grid_size_y TYPE i
                   CHANGING lowest_risk TYPE i.

  "Highest possible lowest risk.
  "Calculated by walking manhatten distance to the
  "destination and assuming every field is a 9.
  lowest_risk = ( dest_x + 1 ) + ( dest_y + 1 ) * 9.

  DATA(risk_cache) = NEW point_map( preset = lowest_risk ).
  DATA(queue) = NEW linked_list( ).

  queue->add( NEW path_point_t( x = 0 y = 0 risk = 0 ) ).
  WHILE queue->size > 0.

    DATA(current_point) = queue->shift( ).
    IF lowest_risk <= current_point->risk.
      CONTINUE.
    ENDIF.
    IF risk_cache->get( x = current_point->x y = current_point->y ) < current_point->risk.
      CONTINUE.
    ENDIF.

    risk_cache->set( x = current_point->x y = current_point->y value = current_point->risk ).


    IF current_point->x = dest_x AND current_point->y = dest_y.
      lowest_risk = current_point->risk.
      CONTINUE.
    ENDIF.


    DATA(next_points) = VALUE path_point_ref_list_t(
        ( NEW #( x = current_point->x - 1 y = current_point->y ) )
        ( NEW #( x = current_point->x + 1 y = current_point->y ) )
        ( NEW #( x = current_point->x y = current_point->y - 1 ) )
        ( NEW #( x = current_point->x y = current_point->y + 1 ) )
    ).
    LOOP AT next_points INTO DATA(next_point).

      IF next_point->x < 0 OR next_point->y < 0 OR next_point->x >= grid_size_x OR next_point->y >= grid_size_y.
        CONTINUE.
      ENDIF.

      next_point->risk = current_point->risk + grid->get_extended( x = next_point->x y = next_point->y ).
      IF next_point->risk >= lowest_risk.
        CONTINUE.
      ENDIF.
      IF next_point->risk >= risk_cache->get( x = next_point->x y = next_point->y ).
        CONTINUE.
      ENDIF.

      risk_cache->set( x = next_point->x y = next_point->y value = next_point->risk ).
      queue->add( next_point ).

    ENDLOOP.

  ENDWHILE.

ENDFORM.


CLASS linked_list IMPLEMENTATION.

  METHOD add.

    me->size = me->size + 1.

    DATA(node) = NEW node_t( value = value ).
    IF me->end IS BOUND.
      me->end->next = node.
    ENDIF.

    me->end = node.
    IF me->start IS NOT BOUND.
      me->start = node.
    ENDIF.

  ENDMETHOD.
  METHOD shift.

    IF me->start IS NOT BOUND.
      RETURN.
    ENDIF.


    DATA(node) = me->start.

    IF node->next IS BOUND.

      FIELD-SYMBOLS <node_ref> TYPE node_t.
      ASSIGN node->next->* TO <node_ref>.

      me->start = REF #( <node_ref> ).
    ELSE.
      me->start = VALUE #( ).
    ENDIF.

    IF me->end = node.
      me->end = VALUE #( ).
    ENDIF.

    me->size = me->size - 1.

    ret = node->value.

  ENDMETHOD.

ENDCLASS.
CLASS grid IMPLEMENTATION.

  METHOD constructor.

    me->size_x = size_x.
    me->size_y = size_y.

  ENDMETHOD.

  METHOD get.

    DATA(ref) = me->get_ref( x = x y = y ).
    ret = ref->*.

  ENDMETHOD.
  METHOD get_extended.

    DATA(newx) = x.
    DATA(newy) = y.
    DATA(risk_increase) = 0.

    WHILE newx >= me->size_x.
      newx = newx - me->size_x.
      risk_increase = risk_increase + 1.
    ENDWHILE.
    WHILE newy >= me->size_y.
      newy = newy - me->size_y.
      risk_increase = risk_increase + 1.
    ENDWHILE.

    DATA(ref) = me->get_ref( x = newx y = newy ).
    DATA(value) = ref->*.
    value = ( value - 1 + risk_increase ) MOD 9 + 1.

    ret = value.

  ENDMETHOD.
  METHOD get_point.

    DATA(value_ref) = me->get_ref( x = x y = y ).
    ret = VALUE #( x = x
                   y = y
                   value = value_ref->*
                   ref = value_ref ).


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
CLASS point_map IMPLEMENTATION.

  METHOD constructor.

    me->preset = preset.

  ENDMETHOD.

  METHOD get.

    DATA(entry) = REF #( me->entries[ x = x y = y ] OPTIONAL ).
    IF entry IS NOT BOUND.
      ret = me->preset.
      RETURN.
    ENDIF.

    ret = entry->value.

  ENDMETHOD.
  METHOD set.

    DATA(entry) = REF #( me->entries[ x = x y = y ] OPTIONAL ).
    IF entry IS NOT BOUND.
      INSERT VALUE #( x = x y = y ) INTO TABLE me->entries REFERENCE INTO entry.
    ENDIF.

    entry->value = value.

  ENDMETHOD.

ENDCLASS.
