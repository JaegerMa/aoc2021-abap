TYPES element_position_t TYPE i.

CONSTANTS element_position_house TYPE element_position_t VALUE 0.
CONSTANTS element_position_hallway TYPE element_position_t VALUE 1.

TYPES:
  "While pos contains only whether the element
  "is in a house or in the hallway,
  "pos2 contains the horizontal position
  "and pos3 the vertical starting position inside the house.
  "pos and pos2 are always updated, pos3 never
  BEGIN OF element_t,
    name             TYPE string,
    pos              TYPE element_position_t,
    pos2             TYPE i,
    pos3             TYPE i,
    destination_pos2 TYPE i,
    energy_required  TYPE i,
  END OF element_t,
  element_list_t TYPE TABLE OF element_t WITH DEFAULT KEY,

  BEGIN OF state_t,
    elements       TYPE element_list_t,
    houses         TYPE int4_table,
    hallway        TYPE int4_table,
    energy_used    TYPE i,
    house_depth    TYPE i,
    hallway_length TYPE i,
  END OF state_t,

  BEGIN OF energy_cache_line_t,
    state_id    TYPE string,
    energy_used TYPE i,
  END OF energy_cache_line_t,
  energy_cache_t TYPE HASHED TABLE OF energy_cache_line_t WITH UNIQUE KEY primary_key COMPONENTS state_id.



START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/23` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

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

  DATA(house_lines) = VALUE string_table( ( lines[ 3 ] )
                                          ( lines[ 4 ] ) ).

  DATA initial_state TYPE state_t.
  PERFORM read_initial_state USING house_lines CHANGING initial_state.

  DATA(min_energy) = 1000000000.
  DATA cache TYPE energy_cache_t.

  PERFORM next_step USING initial_state CHANGING cache min_energy.

  DATA(answer) = min_energy.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.

  DATA(house_lines) = VALUE string_table( ( lines[ 3 ] )
                                          ( `#D#C#B#A#` )
                                          ( `#D#B#A#C#` )
                                          ( lines[ 4 ] ) ).

  DATA initial_state TYPE state_t.
  PERFORM read_initial_state USING house_lines CHANGING initial_state.

  DATA(min_energy) = 1000000000.
  DATA cache TYPE energy_cache_t.

  PERFORM next_step USING initial_state CHANGING cache min_energy.

  DATA(answer) = min_energy.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.



"Wow, this whole function are only 10 lines in JavaScript ._.
FORM read_initial_state USING house_lines TYPE string_table
                     CHANGING state TYPE state_t.

  DATA elements TYPE element_list_t.

  DATA(house_depth) = lines( house_lines ).
  LOOP AT house_lines INTO DATA(house_line).

    "The vertical position is stored in pos3.
    "It's 1-based and starts with the lowest element.
    DATA(vertical_position) = house_depth - sy-tabix + 1.

    SPLIT house_line AT `#` INTO TABLE DATA(element_chars).
    DELETE element_chars WHERE table_line CO space.

    LOOP AT element_chars INTO DATA(element_char).
      "The house IDs correspond to the hallway ID and are 0-based.
      "This makes the ID of the first house being 2, the second 4
      "and the third and fourth 6 and 8.
      DATA(house_id) = sy-tabix * 2.

      "While sy-tabix is 1-based, find's return value is 0-based
      DATA(destination_pos2) = ( find( sub = element_char val = `ABCD` ) + 1 ) * 2.
      DATA(energy_required) = ipow( base = 10 exp = find( sub = element_char val = `ABCD` ) ).

      DATA(element) = VALUE element_t(
          name = element_char
          pos = element_position_house
          pos2 = house_id
          pos3 = vertical_position
          destination_pos2 = destination_pos2
          energy_required = energy_required
      ).

      INSERT element INTO TABLE elements.

    ENDLOOP.

  ENDLOOP.

  state = VALUE state_t(
      elements = elements
      houses = VALUE #(
          ( 100 ) ( 100 )
          ( house_depth * -1 )
          ( 100 )
          ( house_depth * -1 )
          ( 100 )
          ( house_depth * -1 )
          ( 100 )
          ( house_depth * -1 )
          ( 100 ) ( 100 )
      )
      hallway = VALUE #( ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) ( 0 ) )
      energy_used = 0
      house_depth = house_depth
      hallway_length = 11
  ).

ENDFORM.

FORM next_step USING state TYPE state_t
            CHANGING cache TYPE energy_cache_t
                     min_energy TYPE i.

  IF state-energy_used >= min_energy.
    RETURN.
  ENDIF.

  DATA state_id TYPE string.
  PERFORM get_state_id USING state CHANGING state_id.
  DATA(cache_entry) = REF #( cache[ state_id = state_id ] OPTIONAL ).
  IF cache_entry IS BOUND AND cache_entry->energy_used <= state-energy_used.
    RETURN.
  ENDIF.

  IF cache_entry IS BOUND.
    cache_entry->energy_used = state-energy_used.
  ELSE.
    INSERT VALUE #( state_id = state_id energy_used = state-energy_used ) INTO TABLE cache.
  ENDIF.


  DATA is_finished TYPE sap_bool.
  PERFORM is_finished USING state CHANGING is_finished.
  IF is_finished = abap_true.
    min_energy = state-energy_used.
    RETURN.
  ENDIF.



  LOOP AT state-elements REFERENCE INTO DATA(element).
    "Let's keep that ID 1-based as it's only used to
    "access the elements table of state-structures
    "and not used for calculations.
    DATA(element_id) = sy-tabix.

    "Element is already in its destination house
    IF element->pos = element_position_house AND element->pos2 = element->destination_pos2 AND state-houses[ element->pos2 + 1 ] >= 0.
      CONTINUE.
    ENDIF.
    "Element is blocked by other elements in the house
    IF element->pos = element_position_house AND state-houses[ element->pos2 + 1 ] <> element->pos3 * -1.
      CONTINUE.
    ENDIF.
    "Destination house hasn't been emptied yet
    IF element->pos = element_position_hallway AND state-houses[ element->destination_pos2 + 1 ] < 0.
      CONTINUE.
    ENDIF.

    "Element is blocked by another element in the hallway
    IF element->pos = element_position_hallway.

      DATA blocking_element TYPE REF TO element_t.
      blocking_element = VALUE #( ).
      LOOP AT state-elements REFERENCE INTO blocking_element WHERE pos = element_position_hallway
                                                               AND ( ( pos2 > element->pos2 AND pos2 < element->destination_pos2 )
                                                                  OR ( pos2 < element->pos2 AND pos2 > element->destination_pos2 ) ).
        EXIT.
      ENDLOOP.
      IF blocking_element IS BOUND.
        CONTINUE.
      ENDIF.

    ENDIF.


    DATA new_state TYPE state_t.
    IF element->pos = element_position_house.

      DATA(hallway_id) = element->pos2 + 1.
      WHILE hallway_id < state-hallway_length AND state-hallway[ hallway_id + 1 ] = 0.

        "Yep, I've used a house occupancy of 100 as magic number. Blame me.
        IF state-houses[ hallway_id + 1 ] <> 100.
          hallway_id = hallway_id + 1.
          CONTINUE.
        ENDIF.

        PERFORM move_out USING state element->* element_id hallway_id CHANGING new_state.
        PERFORM next_step USING new_state CHANGING cache min_energy.

        hallway_id = hallway_id + 1.

      ENDWHILE.

      hallway_id = element->pos2 - 1.
      WHILE hallway_id >= 0 AND state-hallway[ hallway_id + 1 ] = 0.

        IF state-houses[ hallway_id + 1 ] <> 100.
          hallway_id = hallway_id - 1.
          CONTINUE.
        ENDIF.

        PERFORM move_out USING state element->* element_id hallway_id CHANGING new_state.
        PERFORM next_step USING new_state CHANGING cache min_energy.

        hallway_id = hallway_id - 1.

      ENDWHILE.

    ELSE.

      PERFORM move_in USING state element->* element_id CHANGING new_state.
      PERFORM next_step USING new_state CHANGING cache min_energy.

    ENDIF.

  ENDLOOP.

ENDFORM.
FORM get_state_id USING state TYPE state_t
               CHANGING state_id TYPE string.

  DATA(parts) = VALUE string_table( FOR element IN state-elements ( |{ element-name }{ element-pos }{ element-pos2 }| ) ).
  state_id = concat_lines_of( parts ).

ENDFORM.
FORM is_finished USING state TYPE state_t
              CHANGING finished TYPE sap_bool.

  LOOP AT state-elements REFERENCE INTO DATA(element).

    IF element->pos <> element_position_house.
      finished = abap_false.
      RETURN.
    ENDIF.
    IF element->pos2 <> element->destination_pos2.
      finished = abap_false.
      RETURN.
    ENDIF.

  ENDLOOP.

  finished = abap_true.

ENDFORM.
FORM move_out USING state TYPE state_t
                    element TYPE element_t
                    element_id TYPE i
                    new_hallway_id TYPE i
           CHANGING new_state TYPE state_t.

  "No need to clone anything manually
  "While internal tables lie on the heap,
  "they're still treated like stack elements
  "and are cloned on copy operations.
  new_state = state.

  DATA(new_element) = REF #( new_state-elements[ element_id ] ).
  new_state-hallway[ new_hallway_id + 1 ] = 1.
  new_element->pos = element_position_hallway.
  new_element->pos2 = new_hallway_id.
  new_state-energy_used = new_state-energy_used
    + element-energy_required * ( abs( element-pos2 - new_hallway_id )
                                 + ( state-house_depth - element-pos3 + 1 ) ).
  new_state-houses[ element-pos2 + 1 ] = state-houses[ element-pos2 + 1 ] + 1.

ENDFORM.
FORM move_in USING state TYPE state_t
                   element TYPE element_t
                   element_id TYPE i
          CHANGING new_state TYPE state_t.

  new_state = state.

  DATA(new_element) = REF #( new_state-elements[ element_id ] ).
  new_state-hallway[ element-pos2 + 1 ] = 0.
  new_element->pos = element_position_house.
  new_element->pos2 = element-destination_pos2.
  new_state-energy_used = new_state-energy_used
    + element-energy_required * ( abs( element-pos2 - element-destination_pos2 )
                                 + ( state-house_depth - state-houses[ element-destination_pos2 + 1 ] ) ).
  new_state-houses[ element-destination_pos2 + 1 ] = state-houses[ element-destination_pos2 + 1 ] + 1.

ENDFORM.
