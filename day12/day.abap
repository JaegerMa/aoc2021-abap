CLASS room DEFINITION.

  PUBLIC SECTION.

    TYPES room_list_t TYPE TABLE OF REF TO room WITH DEFAULT KEY.

    DATA id TYPE string.
    DATA is_big TYPE sap_bool.
    DATA connected_rooms TYPE room_list_t.

    METHODS:
      constructor
        IMPORTING id TYPE string.

ENDCLASS.
CLASS room_map DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF room_map_entry_t,
        id   TYPE string,
        room TYPE REF TO room,
      END OF room_map_entry_t,
      room_map_data_t TYPE HASHED TABLE OF room_map_entry_t WITH UNIQUE KEY primary_key COMPONENTS id.


    DATA rooms TYPE room_map_data_t.

    METHODS:
      add_connection
        IMPORTING room1_id TYPE string
                  room2_id TYPE string,
      get_room
        IMPORTING room_id    TYPE string
        RETURNING VALUE(ret) TYPE REF TO room.

ENDCLASS.

TYPES path_index_t TYPE HASHED TABLE OF string WITH UNIQUE KEY primary_key COMPONENTS table_line.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/12` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

  DATA(room_map) = NEW room_map( ).
  LOOP AT lines INTO DATA(line).
    SPLIT line AT `-` INTO DATA(room1_id) DATA(room2_id).
    room_map->add_connection( room1_id = room1_id room2_id = room2_id ).
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

  DATA(path_count) = 0.
  DATA(start) = room_map->get_room( `start` ).
  DATA(path_index) = VALUE path_index_t( ).
  PERFORM step USING start path_index abap_true CHANGING path_count.


  DATA(answer) = path_count.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.

  DATA(path_count) = 0.
  DATA(start) = room_map->get_room( `start` ).
  DATA(path_index) = VALUE path_index_t( ).
  PERFORM step USING start path_index abap_false CHANGING path_count.


  DATA(answer) = path_count.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.


FORM step USING room TYPE REF TO room
                path_index TYPE path_index_t
                small_room_used_twice TYPE sap_bool
       CHANGING path_count TYPE i.

  IF room->id = `end`.
    path_count = path_count + 1.
    RETURN.
  ENDIF.


  LOOP AT room->connected_rooms INTO DATA(next_room).

    IF next_room->id = `start`.
      CONTINUE.
    ENDIF.

    DATA(next_path_index) = REF #( path_index ).
    DATA(next_small_room_used_twice) = small_room_used_twice.
    IF next_room->is_big = abap_false.

      IF line_exists( path_index[ table_line = next_room->id ] ).

        IF small_room_used_twice = abap_true.
          CONTINUE.
        ELSE.
          next_small_room_used_twice = abap_true.
        ENDIF.

      ELSE.

        next_path_index = NEW #( path_index ).
        INSERT next_room->id INTO TABLE next_path_index->*.

      ENDIF.

    ENDIF.

    PERFORM step USING next_room next_path_index->* next_small_room_used_twice CHANGING path_count.

  ENDLOOP.

ENDFORM.



CLASS room IMPLEMENTATION.

  METHOD constructor.

    me->id = id.
    me->is_big = xsdbool( id CA `ABCDEFGHIJKLMNOPQRSTUVWXYZ` ).

  ENDMETHOD.

ENDCLASS.
CLASS room_map IMPLEMENTATION.

  METHOD add_connection.

    DATA(room1) = me->get_room( room1_id ).
    DATA(room2) = me->get_room( room2_id ).

    INSERT room1 INTO TABLE room2->connected_rooms.
    INSERT room2 INTO TABLE room1->connected_rooms.

  ENDMETHOD.

  METHOD get_room.

    DATA(entry) = REF #( me->rooms[ id = room_id ] OPTIONAL ).
    IF entry IS BOUND.
      ret = entry->room.
    ELSE.
      DATA(new_room) = NEW room( room_id ).
      INSERT VALUE #( id = room_id room = new_room ) INTO TABLE me->rooms.
      ret = new_room.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
