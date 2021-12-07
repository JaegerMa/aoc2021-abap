TYPES:
  BEGIN OF map_entry_t,
    key   TYPE i,
    value TYPE int8,
  END OF map_entry_t,
  map_t TYPE SORTED TABLE OF map_entry_t WITH UNIQUE KEY primary_key COMPONENTS key,

  BEGIN OF ocean_state_t,
    fish_count        TYPE int8,
    new_fish_map      TYPE map_t,
    new_fish_once_map TYPE map_t,
    generation        TYPE i,
  END OF ocean_state_t.



START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/6` CHANGING input.

  SPLIT input AT `,` INTO TABLE DATA(input_numbers_str).
  DATA(input_numbers) = VALUE int4_table( FOR input_number_str IN input_numbers_str ( CONV i( input_number_str ) ) ).

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

  DATA state TYPE ocean_state_t.
  PERFORM init_gen USING input_numbers CHANGING state.

  DO 80 TIMES.
    PERFORM next_gen CHANGING state.
  ENDDO.

  DATA(answer) = state-fish_count.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM part2.

  DATA state TYPE ocean_state_t.
  PERFORM init_gen USING input_numbers CHANGING state.

  DO 256 TIMES.
    PERFORM next_gen CHANGING state.
  ENDDO.

  DATA(answer) = state-fish_count.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM init_gen USING init_state TYPE int4_table
           CHANGING state TYPE ocean_state_t.

  state = VALUE #( fish_count = lines( init_state )
                   new_fish_map = VALUE #( ( key = 0 value = 0 )
                                           ( key = 1 value = 0 )
                                           ( key = 2 value = 0 )
                                           ( key = 3 value = 0 )
                                           ( key = 4 value = 0 )
                                           ( key = 5 value = 0 )
                                           ( key = 6 value = 0 ) )
  new_fish_once_map = VALUE #( )
  generation = -1 ).

  LOOP AT input_numbers INTO DATA(input_number).

    DATA(new_fish_once_entry) = REF #( state-new_fish_once_map[ key = input_number ] OPTIONAL ).
    IF new_fish_once_entry IS NOT BOUND.
      INSERT VALUE #( key = input_number value = 0 ) INTO TABLE state-new_fish_once_map REFERENCE INTO new_fish_once_entry.
    ENDIF.

    new_fish_once_entry->value = new_fish_once_entry->value + 1.

  ENDLOOP.

ENDFORM.
FORM next_gen CHANGING state TYPE ocean_state_t.

  DATA(gen) = state-generation + 1.

  DATA(new_fish_entry) = REF #( state-new_fish_map[ key = gen MOD 7 ] ).
  DATA(new_fish) = new_fish_entry->value.
  DATA(new_fish_once) = VALUE #( state-new_fish_once_map[ key = gen ]-value DEFAULT 0 ).

  state-fish_count = state-fish_count + new_fish + new_fish_once.
  state-generation = gen.

  INSERT VALUE #( key = gen + 9 value = new_fish + new_fish_once ) INTO TABLE state-new_fish_once_map.
  new_fish_entry->value = new_fish_entry->value + new_fish_once.

ENDFORM.
