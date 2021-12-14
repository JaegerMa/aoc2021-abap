TYPES:
  BEGIN OF mutation_t,
    source   TYPE string,
    inserted TYPE string,
  END OF mutation_t,
  mutation_map_t TYPE HASHED TABLE OF mutation_t WITH UNIQUE KEY primary_key COMPONENTS source.


CLASS map DEFINITION.

  PUBLIC SECTION.

    TYPES:
      value_t TYPE int8,
      BEGIN OF entry_t,
        key   TYPE string,
        value TYPE value_t,
      END OF entry_t,
      entries_t TYPE HASHED TABLE OF entry_t WITH UNIQUE KEY primary_key COMPONENTS key.

    DATA entries TYPE entries_t.

    METHODS:
      get
        IMPORTING key        TYPE string
        RETURNING VALUE(ret) TYPE REF TO entry_t.

ENDCLASS.


START-OF-SELECTION.

  DATA input TYPE string.
  PERFORM load_input USING `/tmp/14` CHANGING input.


  SPLIT input AT |\n| INTO TABLE DATA(lines).

  DATA(mutations) = VALUE mutation_map_t( ).
  LOOP AT lines INTO DATA(line) WHERE table_line CS `->`.
    SPLIT line AT ` -> ` INTO DATA(source) DATA(inserted).
    INSERT VALUE #( source = source inserted = inserted ) INTO TABLE mutations.
  ENDLOOP.

  DATA(base_state) = lines[ 1 ].

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

  DATA pair_map TYPE REF TO map.
  DATA char_map TYPE REF TO map.

  PERFORM generate_state USING base_state CHANGING char_map pair_map.

  DO 10 TIMES.
    PERFORM next_state USING char_map pair_map mutations.
  ENDDO.

  DATA checksum TYPE int8.
  PERFORM get_state_checksum USING char_map CHANGING checksum.

  DATA(answer) = checksum.
  WRITE / 'Part 1:'.
  WRITE CONV string( answer ).

ENDFORM.
FORM part2.

  DATA pair_map TYPE REF TO map.
  DATA char_map TYPE REF TO map.

  PERFORM generate_state USING base_state CHANGING char_map pair_map.

  DO 40 TIMES.
    PERFORM next_state USING char_map pair_map mutations.
  ENDDO.

  DATA checksum TYPE int8.
  PERFORM get_state_checksum USING char_map CHANGING checksum.

  DATA(answer) = checksum.
  WRITE / 'Part 2:'.
  WRITE CONV string( answer ).

ENDFORM.

FORM generate_state USING base TYPE string
                CHANGING char_map TYPE REF TO map
                         pair_map TYPE REF TO map.

  pair_map = NEW map( ).
  char_map = NEW map( ).
  DO strlen( base_state ) - 1 TIMES.

    DATA(offset) = sy-index - 1.
    DATA(pair_key) = base+offset(2).
    DATA(pair_entry) = pair_map->get( pair_key ).
    pair_entry->value = pair_entry->value + 1.

  ENDDO.
  DO strlen( base_state ) TIMES.

    offset = sy-index - 1.
    DATA(char) = base+offset(1).
    DATA(char_entry) = char_map->get( char ).
    char_entry->value = char_entry->value + 1.

  ENDDO.

ENDFORM.
FORM next_state USING char_map TYPE REF TO map
                      pair_map TYPE REF TO map
                      mutations TYPE mutation_map_t.

  DATA(old_pairs) = pair_map->entries.
  LOOP AT old_pairs INTO DATA(pair).

    DATA(mutation) = REF #( mutations[ source = pair-key ] OPTIONAL ).
    IF mutation IS NOT BOUND.
      CONTINUE.
    ENDIF.


    DATA(old_occurence) = pair_map->get( pair-key ).
    old_occurence->value = old_occurence->value - pair-value.

    DATA(char_occcurence) = char_map->get( mutation->inserted ).
    char_occcurence->value = char_occcurence->value + pair-value.

    DATA(new_occurence1) = pair_map->get( pair-key(1) && mutation->inserted ).
    new_occurence1->value = new_occurence1->value + pair-value.

    DATA(new_occurence2) = pair_map->get( mutation->inserted && pair-key+1(1) ).
    new_occurence2->value = new_occurence2->value + pair-value.

  ENDLOOP.

ENDFORM.
FORM get_state_checksum USING char_map TYPE REF TO map
                     CHANGING checksum TYPE int8.

  TYPES occurences_t TYPE TABLE OF int8 WITH DEFAULT KEY.
  DATA(occurences) = VALUE occurences_t( FOR entry IN char_map->entries ( entry-value ) ).
  SORT occurences.

  checksum = occurences[ lines( occurences ) ] - occurences[ 1 ].

ENDFORM.



CLASS map IMPLEMENTATION.

  METHOD get.

    DATA(entry) = REF #( me->entries[ key = key ] OPTIONAL ).
    IF entry IS NOT BOUND.
      INSERT VALUE #( key = key ) INTO TABLE me->entries REFERENCE INTO entry.
    ENDIF.

    ret = entry.

  ENDMETHOD.

ENDCLASS.
